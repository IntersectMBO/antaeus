{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
-- Not using all CardanoEra
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Spec.BabbageFeatures where

import Cardano.Api qualified as C
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Time.Clock qualified as Time
import Data.Time.Clock.POSIX qualified as Time
import Hedgehog qualified as H
import Hedgehog.Internal.Property (MonadTest)
import Helpers.Common (makeAddress, toShelleyBasedEra)
import Helpers.Query qualified as Q
import Helpers.Test (assert)
import Helpers.TestData (TestInfo (..), TestParams (..))
import Helpers.Testnet qualified as TN
import Helpers.Tx qualified as Tx
import Helpers.Utils qualified as U
import PlutusLedgerApi.V1.Interval qualified as P
import PlutusLedgerApi.V1.Time qualified as P
import PlutusLedgerApi.V2 qualified as PlutusV2
import PlutusScripts.Basic.V_1_0 qualified as PS_1_0
import PlutusScripts.Basic.V_1_1 qualified as PS_1_1
import PlutusScripts.Helpers qualified as PS
import PlutusScripts.V2TxInfo qualified as PS (
  checkV2TxInfoAssetIdV2,
  checkV2TxInfoMintWitnessV2,
  checkV2TxInfoRedeemer,
  txInfoData,
  txInfoFee,
  txInfoInputs,
  txInfoMint,
  txInfoOutputs,
  txInfoSigs,
 )

checkTxInfoV2TestInfo =
  TestInfo
    { testName = "checkTxInfoV2Test"
    , testDescription =
        "Check each attribute of the TxInfo from the V2 ScriptContext in a single transaction"
    , test = checkTxInfoV2Test
    }

checkTxInfoV2Test
  :: (MonadIO m, MonadTest m)
  => TN.TestEnvironmentOptions era
  -> TestParams era
  -> m (Maybe String)
checkTxInfoV2Test networkOptions TestParams{..} = do
  era <- TN.eraFromOptionsM networkOptions
  startTime <- liftIO Time.getPOSIXTime
  (w1SKey, w1VKey, w1Address) <- TN.w1All tempAbsPath networkId
  let sbe = toShelleyBasedEra era

  -- build a transaction

  txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address
  txInAsTxOut@(C.TxOut _ txInValue _ _) <-
    Q.getTxOutAtAddress era localNodeConnectInfo w1Address txIn "txInAsTxOut <- getTxOutAtAddress"

  let tokenValues = C.valueFromList [(PS.checkV2TxInfoAssetIdV2, 1), (PS_1_0.alwaysSucceedAssetIdV2, 2)]
      executionUnits1 = C.ExecutionUnits{C.executionSteps = 1_000_000_000, C.executionMemory = 10_000_000}
      executionUnits2 = C.ExecutionUnits{C.executionSteps = 1_000_000_000, C.executionMemory = 4_000_000}
      collateral = Tx.txInsCollateral era [txIn]
      totalLovelace = C.txOutValueToLovelace txInValue
      fee = 2_500_000 :: C.Lovelace
      amountPaid = 10_000_000
      amountReturned = totalLovelace - amountPaid - fee
      datum = PS.toScriptData (42 :: Integer)

      txOut1 = Tx.txOutWithDatumInTx era (C.lovelaceToValue amountPaid <> tokenValues) w1Address datum
      txOut2 = Tx.txOut era (C.lovelaceToValue amountReturned) w1Address

      lowerBound =
        P.fromMilliSeconds $
          P.DiffMilliSeconds $
            U.posixToMilliseconds $
              Time.utcTimeToPOSIXSeconds $
                -- subtract 10 seconds from the lower bound so it is well before the testnet start time
                Time.addUTCTime (-10) $
                  fromJust mTime -- before slot 1
      upperBound =
        P.fromMilliSeconds $
          P.DiffMilliSeconds $
            U.posixToMilliseconds startTime
              + 600_000 -- ~10mins after slot 1 (to account for testnet init time)
      timeRange = P.interval lowerBound upperBound :: P.POSIXTimeRange

      expTxInfoInputs = PS.txInfoInputs era (txIn, txInAsTxOut)
      expTxInfoReferenceInputs = PS.txInfoInputs era (txIn, txInAsTxOut)
      expTxInfoOutputs = PS.txInfoOutputs era [txOut1, txOut2]
      expTxInfoFee = PS.txInfoFee fee
      expTxInfoMint = PS.txInfoMint tokenValues
      expDCert = [] -- not testing any staking registration certificate
      expWdrl = PlutusV2.fromList [] -- not testing any staking reward withdrawal
      expTxInfoSigs = PS.txInfoSigs [w1VKey]
      expTxInfoRedeemers = PS_1_0.alwaysSucceedPolicyTxInfoRedeemerV2
      expTxInfoData = PS.txInfoData [datum]
      expTxInfoValidRange = timeRange

      redeemer =
        PS.checkV2TxInfoRedeemer
          [expTxInfoInputs]
          [expTxInfoReferenceInputs]
          expTxInfoOutputs
          expTxInfoFee
          expTxInfoMint
          expDCert
          expWdrl
          expTxInfoValidRange
          expTxInfoSigs
          expTxInfoRedeemers
          expTxInfoData
      mintWitnesses =
        Map.fromList
          [ PS.checkV2TxInfoMintWitnessV2 sbe redeemer executionUnits1
          , PS_1_0.alwaysSucceedMintWitnessV2' sbe executionUnits2
          ]

      txBodyContent =
        (Tx.emptyTxBodyContent sbe pparams)
          { C.txIns = Tx.pubkeyTxIns [txIn]
          , C.txInsReference = Tx.txInsReference era [txIn]
          , C.txInsCollateral = collateral
          , C.txMintValue = Tx.txMintValue era tokenValues mintWitnesses
          , C.txOuts = [txOut1, txOut2]
          , C.txFee = Tx.txFee era fee
          , C.txValidityLowerBound = Tx.txValidityLowerBound era 1
          , C.txValidityUpperBound = Tx.txValidityUpperBound era 2700
          , -- \^ ~9min range (200ms slots)
            -- \^ Babbage era onwards cannot have upper slot beyond epoch boundary (10_000 slot epoch)
            C.txExtraKeyWits = Tx.txExtraKeyWits era [w1VKey]
          }
  txbody <- Tx.buildRawTx sbe txBodyContent
  kw <- Tx.signTx sbe txbody (C.WitnessPaymentKey w1SKey)
  let signedTx = C.makeSignedTransaction [kw] txbody

  Tx.submitTx sbe localNodeConnectInfo signedTx

  let expectedTxIn = Tx.txIn (Tx.txId signedTx) 0
  resultTxOut <-
    Q.getTxOutAtAddress
      era
      localNodeConnectInfo
      w1Address
      expectedTxIn
      "resultTxOut <- getTxOutAtAddress"
  txOutHasTokenValue <- Q.txOutHasValue resultTxOut tokenValues
  assert "txOut has tokens" txOutHasTokenValue

referenceScriptMintTestInfo =
  TestInfo
    { testName = "referenceScriptMintTest"
    , testDescription = "Mint tokens by referencing an input containing a Plutus policy as witness"
    , test = referenceScriptMintTest
    }

referenceScriptMintTest
  :: (MonadTest m, MonadIO m)
  => TN.TestEnvironmentOptions era
  -> TestParams era
  -> m (Maybe String)
referenceScriptMintTest networkOptions TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
  era <- TN.eraFromOptionsM networkOptions
  (w1SKey, w1Address) <- TN.w1 tempAbsPath networkId
  let sbe = toShelleyBasedEra era

  -- build a transaction to hold reference script

  txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address

  let refScriptLovelaceValue = C.lovelaceToValue 20_000_000
      refScriptTxOut = case era of
        C.AlonzoEra -> error "Alonzo era is unsupported in this test"
        C.BabbageEra ->
          Tx.txOutWithRefScript
            era
            refScriptLovelaceValue
            w1Address
            (PS.unPlutusScriptV2 PS_1_0.alwaysSucceedPolicyScriptV2)
        C.ConwayEra ->
          Tx.txOutWithRefScript
            era
            refScriptLovelaceValue
            w1Address
            (PS.unPlutusScriptV3 PS_1_1.alwaysSucceedPolicyScriptV3)
      otherTxOut = Tx.txOut era (C.lovelaceToValue 5_000_000) w1Address

      txBodyContent =
        (Tx.emptyTxBodyContent sbe pparams)
          { C.txIns = Tx.pubkeyTxIns [txIn]
          , C.txOuts = [refScriptTxOut, otherTxOut]
          }

  signedTx <- Tx.buildTx era localNodeConnectInfo txBodyContent w1Address w1SKey
  Tx.submitTx sbe localNodeConnectInfo signedTx
  let refScriptTxIn = Tx.txIn (Tx.txId signedTx) 0
      otherTxIn = Tx.txIn (Tx.txId signedTx) 1
  Q.waitForTxInAtAddress era localNodeConnectInfo w1Address refScriptTxIn "waitForTxInAtAddress"

  -- build a transaction to mint token using reference script

  let (tokenValues, mintWitnesses) = case era of
        C.BabbageEra ->
          ( C.valueFromList [(PS_1_0.alwaysSucceedAssetIdV2, 6)]
          , Map.fromList [PS_1_0.alwaysSucceedMintWitnessV2 sbe (Just refScriptTxIn)]
          )
        C.ConwayEra ->
          ( C.valueFromList [(PS_1_1.alwaysSucceedAssetIdV3, 6)]
          , Map.fromList [PS_1_1.alwaysSucceedMintWitnessV3 sbe (Just refScriptTxIn)]
          )
      collateral = Tx.txInsCollateral era [otherTxIn]
      txOut = Tx.txOut era (C.lovelaceToValue 3_000_000 <> tokenValues) w1Address

      txBodyContent2 =
        (Tx.emptyTxBodyContent sbe pparams)
          { C.txIns = Tx.pubkeyTxIns [otherTxIn]
          , C.txInsCollateral = collateral
          , C.txInsReference = Tx.txInsReference era [refScriptTxIn]
          , C.txMintValue = Tx.txMintValue era tokenValues mintWitnesses
          , C.txOuts = [txOut]
          }

  signedTx2 <- Tx.buildTx era localNodeConnectInfo txBodyContent2 w1Address w1SKey
  Tx.submitTx sbe localNodeConnectInfo signedTx2
  let expectedTxIn = Tx.txIn (Tx.txId signedTx2) 0
  -- Query for txo and assert it contains newly minted token
  resultTxOut <-
    Q.getTxOutAtAddress era localNodeConnectInfo w1Address expectedTxIn "getTxOutAtAddress"
  txOutHasTokenValue <- Q.txOutHasValue resultTxOut tokenValues
  assert "txOut has tokens" txOutHasTokenValue

referenceScriptInlineDatumSpendTestInfo =
  TestInfo
    { testName = "referenceScriptInlineDatumSpendTest"
    , testDescription =
        "Spend funds locked by script by using inline datum and referencing an input containing a "
          ++ "Plutus script as witness"
    , test = referenceScriptInlineDatumSpendTest
    }

referenceScriptInlineDatumSpendTest
  :: (MonadIO m, MonadTest m)
  => TN.TestEnvironmentOptions era
  -> TestParams era
  -> m (Maybe String)
referenceScriptInlineDatumSpendTest
  networkOptions
  TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
    era <- TN.eraFromOptionsM networkOptions
    (w1SKey, w1Address) <- TN.w1 tempAbsPath networkId
    let sbe = toShelleyBasedEra era

    -- build a transaction to hold reference script

    txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address

    let refScriptLovelaceValue = C.lovelaceToValue 20_000_000
        refScriptTxOut = case era of
          C.AlonzoEra -> error "Alonzo era is unsupported in this test"
          C.BabbageEra ->
            Tx.txOutWithRefScript
              era
              refScriptLovelaceValue
              w1Address
              (PS.unPlutusScriptV2 PS_1_0.alwaysSucceedSpendScriptV2)
          C.ConwayEra ->
            Tx.txOutWithRefScript
              era
              refScriptLovelaceValue
              w1Address
              (PS.unPlutusScriptV3 PS_1_1.alwaysSucceedSpendScriptV3)
        otherTxOut = Tx.txOut era (C.lovelaceToValue 5_000_000) w1Address
        scriptAddress = case era of
          C.BabbageEra -> makeAddress (Right PS_1_0.alwaysSucceedSpendScriptHashV2) networkId
          C.ConwayEra -> makeAddress (Right PS_1_1.alwaysSucceedSpendScriptHashV3) networkId
        scriptTxOut = Tx.txOutWithInlineDatum era (C.lovelaceToValue 10_000_000) scriptAddress (PS.toScriptData ())
        txBodyContent =
          (Tx.emptyTxBodyContent sbe pparams)
            { C.txIns = Tx.pubkeyTxIns [txIn]
            , C.txOuts = [refScriptTxOut, otherTxOut, scriptTxOut]
            }

    signedTx <- Tx.buildTx era localNodeConnectInfo txBodyContent w1Address w1SKey
    Tx.submitTx sbe localNodeConnectInfo signedTx
    let refScriptTxIn = Tx.txIn (Tx.txId signedTx) 0
        otherTxIn = Tx.txIn (Tx.txId signedTx) 1
        txInAtScript = Tx.txIn (Tx.txId signedTx) 2
    Q.waitForTxInAtAddress era localNodeConnectInfo w1Address refScriptTxIn "waitForTxInAtAddress"

    -- build a transaction to mint token using reference script

    let witness = case era of
          C.BabbageEra -> PS_1_0.alwaysSucceedSpendWitnessV2 sbe (Just refScriptTxIn) Nothing
          C.ConwayEra -> PS_1_1.alwaysSucceedSpendWitnessV3 sbe (Just refScriptTxIn) Nothing
        scriptTxIn = Tx.txInWitness txInAtScript witness
        collateral = Tx.txInsCollateral era [otherTxIn]
        adaValue = C.lovelaceToValue 4_200_000
        txOut = Tx.txOut era adaValue w1Address

        txBodyContent2 =
          (Tx.emptyTxBodyContent sbe pparams)
            { C.txIns = [scriptTxIn]
            , C.txInsReference = Tx.txInsReference era [refScriptTxIn]
            , C.txInsCollateral = collateral
            , C.txOuts = [txOut]
            }

    signedTx2 <- Tx.buildTx era localNodeConnectInfo txBodyContent2 w1Address w1SKey
    Tx.submitTx sbe localNodeConnectInfo signedTx2
    let expectedTxIn = Tx.txIn (Tx.txId signedTx2) 0
    -- Query for txo and assert it contains newly minted token
    resultTxOut <-
      Q.getTxOutAtAddress era localNodeConnectInfo w1Address expectedTxIn "getTxOutAtAddress"
    txOutHasAdaValue <- Q.txOutHasValue resultTxOut adaValue
    assert "txOut has tokens" txOutHasAdaValue

referenceScriptDatumHashSpendTestInfo =
  TestInfo
    { testName = "referenceScriptDatumHashSpendTest"
    , testDescription =
        "Spend funds locked by script by providing datum in txbody and referencing an input "
          ++ "containing a Plutus script as witness"
    , test = referenceScriptDatumHashSpendTest
    }

referenceScriptDatumHashSpendTest
  :: (MonadIO m, MonadTest m)
  => TN.TestEnvironmentOptions era
  -> TestParams era
  -> m (Maybe String)
referenceScriptDatumHashSpendTest networkOptions TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
  era <- TN.eraFromOptionsM networkOptions
  (w1SKey, w1Address) <- TN.w1 tempAbsPath networkId
  let sbe = toShelleyBasedEra era

  -- build a transaction to hold reference script

  txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address

  let refScriptLovelaceValue = C.lovelaceToValue 20_000_000
      refScriptTxOut = case era of
        C.AlonzoEra -> error "Alonzo era is unsupported in this test"
        C.BabbageEra ->
          Tx.txOutWithRefScript
            era
            refScriptLovelaceValue
            w1Address
            (PS.unPlutusScriptV2 PS_1_0.alwaysSucceedSpendScriptV2)
        C.ConwayEra ->
          Tx.txOutWithRefScript
            era
            refScriptLovelaceValue
            w1Address
            (PS.unPlutusScriptV3 PS_1_1.alwaysSucceedSpendScriptV3)
      otherTxOut = Tx.txOut era (C.lovelaceToValue 5_000_000) w1Address
      scriptAddress = case era of
        C.BabbageEra -> makeAddress (Right PS_1_0.alwaysSucceedSpendScriptHashV2) networkId
        C.ConwayEra -> makeAddress (Right PS_1_1.alwaysSucceedSpendScriptHashV3) networkId
      datum = PS.toScriptData ()
      scriptTxOut = Tx.txOutWithDatumHash era (C.lovelaceToValue 10_000_000) scriptAddress datum

      txBodyContent =
        (Tx.emptyTxBodyContent sbe pparams)
          { C.txIns = Tx.pubkeyTxIns [txIn]
          , C.txOuts = [refScriptTxOut, otherTxOut, scriptTxOut]
          }

  signedTx <- Tx.buildTx era localNodeConnectInfo txBodyContent w1Address w1SKey
  Tx.submitTx sbe localNodeConnectInfo signedTx
  let refScriptTxIn = Tx.txIn (Tx.txId signedTx) 0
      otherTxIn = Tx.txIn (Tx.txId signedTx) 1
      txInAtScript = Tx.txIn (Tx.txId signedTx) 2
  Q.waitForTxInAtAddress era localNodeConnectInfo w1Address refScriptTxIn "waitForTxInAtAddress"

  -- build a transaction to mint token using reference script

  let scriptTxIn = case era of
        C.BabbageEra ->
          Tx.txInWitness
            txInAtScript
            (PS_1_0.alwaysSucceedSpendWitnessV2 sbe (Just refScriptTxIn) (Just datum))
        C.ConwayEra ->
          Tx.txInWitness
            txInAtScript
            (PS_1_1.alwaysSucceedSpendWitnessV3 sbe (Just refScriptTxIn) (Just datum))
      collateral = Tx.txInsCollateral era [otherTxIn]
      adaValue = C.lovelaceToValue 4_200_000
      txOut = Tx.txOut era adaValue w1Address

      txBodyContent2 =
        (Tx.emptyTxBodyContent sbe pparams)
          { C.txIns = [scriptTxIn]
          , C.txInsReference = Tx.txInsReference era [refScriptTxIn]
          , C.txInsCollateral = collateral
          , C.txOuts = [txOut]
          }

  signedTx2 <- Tx.buildTx era localNodeConnectInfo txBodyContent2 w1Address w1SKey
  Tx.submitTx sbe localNodeConnectInfo signedTx2
  let expectedTxIn = Tx.txIn (Tx.txId signedTx2) 0
  -- Query for txo and assert it contains newly minted token
  resultTxOut <-
    Q.getTxOutAtAddress era localNodeConnectInfo w1Address expectedTxIn "getTxOutAtAddress"
  txOutHasAdaValue <- Q.txOutHasValue resultTxOut adaValue
  assert "txOut has tokens" txOutHasAdaValue

inlineDatumSpendTestInfo =
  TestInfo
    { testName = "inlineDatumSpendTest"
    , testDescription =
        "Spend funds locked by script by using inline datum and providing Plutus script in "
          ++ "transaction as witness"
    , test = inlineDatumSpendTest
    }

inlineDatumSpendTest
  :: (MonadIO m, MonadTest m)
  => TN.TestEnvironmentOptions era
  -> TestParams era
  -> m (Maybe String)
inlineDatumSpendTest networkOptions TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
  era <- TN.eraFromOptionsM networkOptions
  (w1SKey, w1Address) <- TN.w1 tempAbsPath networkId
  let sbe = toShelleyBasedEra era

  -- build a transaction to hold inline datum at script address

  txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address

  let scriptAddress = case era of
        C.BabbageEra -> makeAddress (Right PS_1_0.alwaysSucceedSpendScriptHashV2) networkId
        C.ConwayEra -> makeAddress (Right PS_1_1.alwaysSucceedSpendScriptHashV3) networkId
      scriptTxOut = Tx.txOutWithInlineDatum era (C.lovelaceToValue 10_000_000) scriptAddress (PS.toScriptData ())
      otherTxOut = Tx.txOut era (C.lovelaceToValue 5_000_000) w1Address

      txBodyContent =
        (Tx.emptyTxBodyContent sbe pparams)
          { C.txIns = Tx.pubkeyTxIns [txIn]
          , C.txOuts = [scriptTxOut, otherTxOut]
          }

  signedTx <- Tx.buildTx era localNodeConnectInfo txBodyContent w1Address w1SKey
  Tx.submitTx sbe localNodeConnectInfo signedTx
  let txInAtScript = Tx.txIn (Tx.txId signedTx) 0
      otherTxIn = Tx.txIn (Tx.txId signedTx) 1
  Q.waitForTxInAtAddress era localNodeConnectInfo scriptAddress txInAtScript "waitForTxInAtAddress"

  -- build a transaction to mint token using reference script

  let
    -- without reference script
    scriptTxIn = case era of
      C.BabbageEra -> Tx.txInWitness txInAtScript (PS_1_0.alwaysSucceedSpendWitnessV2 sbe Nothing Nothing)
      C.ConwayEra -> Tx.txInWitness txInAtScript (PS_1_1.alwaysSucceedSpendWitnessV3 sbe Nothing Nothing)
    collateral = Tx.txInsCollateral era [otherTxIn]
    adaValue = C.lovelaceToValue 4_200_000
    txOut = Tx.txOut era adaValue w1Address

    txBodyContent2 =
      (Tx.emptyTxBodyContent sbe pparams)
        { C.txIns = [scriptTxIn]
        , C.txInsCollateral = collateral
        , C.txOuts = [txOut]
        }

  signedTx2 <- Tx.buildTx era localNodeConnectInfo txBodyContent2 w1Address w1SKey
  Tx.submitTx sbe localNodeConnectInfo signedTx2
  let expectedTxIn = Tx.txIn (Tx.txId signedTx2) 0
  -- Query for txo and assert it contains newly minted token
  resultTxOut <-
    Q.getTxOutAtAddress era localNodeConnectInfo w1Address expectedTxIn "getTxOutAtAddress"
  txOutHasAdaValue <- Q.txOutHasValue resultTxOut adaValue
  assert "txOut has tokens" txOutHasAdaValue

referenceInputWithV1ScriptErrorTestInfo =
  TestInfo
    { testName = "referenceInputWithV1ScriptErrorTest"
    , testDescription =
        "ReferenceInputsNotSupported error occurs when executing a V1 script whilst referencing an input"
    , test = referenceInputWithV1ScriptErrorTest
    }

referenceInputWithV1ScriptErrorTest
  :: (MonadIO m, MonadTest m)
  => TN.TestEnvironmentOptions era
  -> TestParams era
  -> m (Maybe String)
referenceInputWithV1ScriptErrorTest
  networkOptions
  TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
    era <- TN.eraFromOptionsM networkOptions
    (w1SKey, w1Address) <- TN.w1 tempAbsPath networkId
    let sbe = toShelleyBasedEra era

    txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address

    let tokenValues = C.valueFromList [(PS_1_0.alwaysSucceedAssetIdV1, 1)]
        mintWitnesses = Map.fromList [PS_1_0.alwaysSucceedMintWitnessV1 sbe Nothing]
        collateral = Tx.txInsCollateral era [txIn]
        txOut = Tx.txOut era (C.lovelaceToValue 3_000_000 <> tokenValues) w1Address

        txBodyContent =
          (Tx.emptyTxBodyContent sbe pparams)
            { C.txIns = Tx.pubkeyTxIns [txIn]
            , C.txInsCollateral = collateral
            , C.txInsReference = Tx.txInsReference era [txIn]
            , C.txMintValue = Tx.txMintValue era tokenValues mintWitnesses
            , C.txOuts = [txOut]
            }

    eitherTx <-
      Tx.buildTxWithError
        era
        localNodeConnectInfo
        txBodyContent
        w1Address
        Nothing
        [C.WitnessPaymentKey w1SKey]
    let expError = "ReferenceInputsNotSupported"
    -- why is this validity interval error? https://github.com/input-output-hk/cardano-node/issues/5080
    assert expError $ Tx.isTxBodyErrorValidityInterval expError eitherTx

referenceScriptOutputWithV1ScriptErrorTestInfo =
  TestInfo
    { testName = "referenceScriptOutputWithV1ScriptErrorTest"
    , testDescription =
        "ReferenceScriptsNotSupported error occurs when executing a V1 script whilst creating "
          ++ "an output including a reference script"
    , test = referenceScriptOutputWithV1ScriptErrorTest
    }

referenceScriptOutputWithV1ScriptErrorTest
  :: (MonadIO m, MonadTest m)
  => TN.TestEnvironmentOptions era
  -> TestParams era
  -> m (Maybe String)
referenceScriptOutputWithV1ScriptErrorTest
  networkOptions
  TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
    era <- TN.eraFromOptionsM networkOptions
    (w1SKey, w1Address) <- TN.w1 tempAbsPath networkId
    let sbe = toShelleyBasedEra era

    txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address

    let tokenValues = C.valueFromList [(PS_1_0.alwaysSucceedAssetIdV1, 1)]
        mintWitnesses = Map.fromList [PS_1_0.alwaysSucceedMintWitnessV1 sbe Nothing]
        collateral = Tx.txInsCollateral era [txIn]
        txOut =
          Tx.txOutWithRefScript
            era
            (C.lovelaceToValue 3_000_000 <> tokenValues)
            w1Address
            (PS.unPlutusScriptV2 PS_1_0.alwaysSucceedSpendScriptV2)

        txBodyContent =
          (Tx.emptyTxBodyContent sbe pparams)
            { C.txIns = Tx.pubkeyTxIns [txIn]
            , C.txInsCollateral = collateral
            , C.txMintValue = Tx.txMintValue era tokenValues mintWitnesses
            , C.txOuts = [txOut]
            }

    eitherTx <-
      Tx.buildTxWithError
        era
        localNodeConnectInfo
        txBodyContent
        w1Address
        Nothing
        [C.WitnessPaymentKey w1SKey]
    H.annotate $ show eitherTx
    let expError = "ReferenceScriptsNotSupported"
    -- why is this validity interval error? https://github.com/input-output-hk/cardano-node/issues/5080
    assert expError $ Tx.isTxBodyErrorValidityInterval expError eitherTx

inlineDatumOutputWithV1ScriptErrorTestInfo =
  TestInfo
    { testName = "inlineDatumOutputWithV1ScriptErrorTest"
    , testDescription =
        "InlineDatumsNotSupported error occurs when executing a V1 script whilst creating"
          ++ "an output including an inline datum"
    , test = inlineDatumOutputWithV1ScriptErrorTest
    }

inlineDatumOutputWithV1ScriptErrorTest
  :: (MonadIO m, MonadTest m)
  => TN.TestEnvironmentOptions era
  -> TestParams era
  -> m (Maybe String)
inlineDatumOutputWithV1ScriptErrorTest
  networkOptions
  TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
    era <- TN.eraFromOptionsM networkOptions
    (w1SKey, w1Address) <- TN.w1 tempAbsPath networkId
    let sbe = toShelleyBasedEra era

    txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address

    let tokenValues = C.valueFromList [(PS_1_0.alwaysSucceedAssetIdV1, 1)]
        mintWitnesses = Map.fromList [PS_1_0.alwaysSucceedMintWitnessV1 sbe Nothing]
        collateral = Tx.txInsCollateral era [txIn]
        txOut =
          Tx.txOutWithInlineDatum
            era
            (C.lovelaceToValue 3_000_000 <> tokenValues)
            w1Address
            (PS.toScriptData ())

        txBodyContent =
          (Tx.emptyTxBodyContent sbe pparams)
            { C.txIns = Tx.pubkeyTxIns [txIn]
            , C.txInsCollateral = collateral
            , C.txMintValue = Tx.txMintValue era tokenValues mintWitnesses
            , C.txOuts = [txOut]
            }

    eitherTx <-
      Tx.buildTxWithError
        era
        localNodeConnectInfo
        txBodyContent
        w1Address
        Nothing
        [C.WitnessPaymentKey w1SKey]
    H.annotate $ show eitherTx
    let expError = "InlineDatumsNotSupported"
    -- why is this validity interval error? https://github.com/input-output-hk/cardano-node/issues/5080
    assert expError $ Tx.isTxBodyErrorValidityInterval expError eitherTx

returnCollateralWithTokensValidScriptTestInfo =
  TestInfo
    { testName = "returnCollateralWithTokensValidScriptTest"
    , testDescription =
        "Check it is possible to provide collateral input containing tokens if return collateral"
          ++ "is being used to return them"
    , test = returnCollateralWithTokensValidScriptTest
    }

returnCollateralWithTokensValidScriptTest
  :: (MonadIO m, MonadTest m)
  => TN.TestEnvironmentOptions era
  -> TestParams era
  -> m (Maybe String)
returnCollateralWithTokensValidScriptTest
  networkOptions
  TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
    era <- TN.eraFromOptionsM networkOptions
    (w1SKey, w1Address) <- TN.w1 tempAbsPath networkId
    let sbe = toShelleyBasedEra era

    txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address

    -- build and submit transaction to create output containing some tokens.

    let (tokenValues, mintWitnesses) = case era of
          C.AlonzoEra -> error "Alonzo era is unsupported in this test"
          C.BabbageEra ->
            ( C.valueFromList [(PS_1_0.alwaysSucceedAssetIdV2, 10)]
            , Map.fromList [PS_1_0.alwaysSucceedMintWitnessV2 sbe Nothing]
            )
          C.ConwayEra ->
            ( C.valueFromList [(PS_1_0.alwaysSucceedAssetIdV2, 10), (PS_1_1.alwaysSucceedAssetIdV3, 10)]
            , Map.fromList
                [ PS_1_0.alwaysSucceedMintWitnessV2 sbe Nothing
                , PS_1_1.alwaysSucceedMintWitnessV3 sbe Nothing
                ]
            )
        collateral = Tx.txInsCollateral era [txIn]
        txOut =
          Tx.txOutWithInlineDatum
            era
            (C.lovelaceToValue 5_000_000 <> tokenValues)
            w1Address
            (PS.toScriptData ())

        txBodyContent =
          (Tx.emptyTxBodyContent sbe pparams)
            { C.txIns = Tx.pubkeyTxIns [txIn]
            , C.txInsCollateral = collateral
            , C.txMintValue = Tx.txMintValue era tokenValues mintWitnesses
            , C.txOuts = [txOut]
            }

    signedTx <- Tx.buildTx era localNodeConnectInfo txBodyContent w1Address w1SKey
    Tx.submitTx sbe localNodeConnectInfo signedTx
    let txIn2 = Tx.txIn (Tx.txId signedTx) 0
    Q.waitForTxInAtAddress era localNodeConnectInfo w1Address txIn2 "waitForTxInAtAddress"

    -- build and submit transaction with tokens in collateral input.
    -- This is allowed because using return collateral feature.

    let tokenValues2 = case era of
          C.BabbageEra -> C.valueFromList [(PS_1_0.alwaysSucceedAssetIdV2, 20)]
          C.ConwayEra -> C.valueFromList [(PS_1_0.alwaysSucceedAssetIdV2, 20), (PS_1_1.alwaysSucceedAssetIdV3, 20)]
        collateral2 = Tx.txInsCollateral era [txIn2]
        txOut2 =
          Tx.txOutWithInlineDatum
            era
            (C.lovelaceToValue 2_000_000 <> tokenValues2)
            w1Address
            (PS.toScriptData ())
        colReturnTxOut = Tx.txOut era (C.lovelaceToValue 3_000_000 <> tokenValues) w1Address

        txBodyContent2 =
          (Tx.emptyTxBodyContent sbe pparams)
            { C.txIns = Tx.pubkeyTxIns [txIn2]
            , C.txInsCollateral = collateral2
            , C.txMintValue = Tx.txMintValue era tokenValues mintWitnesses
            , C.txOuts = [txOut2]
            , C.txReturnCollateral = Tx.txReturnCollateral era colReturnTxOut
            }

    signedTx2 <- Tx.buildTx era localNodeConnectInfo txBodyContent2 w1Address w1SKey
    Tx.submitTx sbe localNodeConnectInfo signedTx2
    let expectedTxIn = Tx.txIn (Tx.txId signedTx2) 0
    -- Query for txo and assert it contains newly minted token
    resultTxOut <-
      Q.getTxOutAtAddress era localNodeConnectInfo w1Address expectedTxIn "getTxOutAtAddress"
    txOutHasTokenValue <- Q.txOutHasValue resultTxOut tokenValues2
    assert "txOut has tokens" txOutHasTokenValue

submitWithInvalidScriptThenCollateralIsTakenAndReturnedTestInfo =
  TestInfo
    { testName = "submitWithInvalidScriptThenCollateralIsTakenAndReturnedTest"
    , testDescription =
        "Submit a failing script when using total and return collateral in tx body."
          ++ "Check that ada and tokens from collateral input are returned in the collateral output."
          ++ "and that the regular input is not consumed."
    , test = submitWithInvalidScriptThenCollateralIsTakenAndReturnedTest
    }
submitWithInvalidScriptThenCollateralIsTakenAndReturnedTest
  :: (MonadIO m, MonadTest m)
  => TN.TestEnvironmentOptions era
  -> TestParams era
  -> m (Maybe String)
submitWithInvalidScriptThenCollateralIsTakenAndReturnedTest
  networkOptions
  TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
    era <- TN.eraFromOptionsM networkOptions
    (w1SKey, w1Address) <- TN.w1 tempAbsPath networkId
    let sbe = toShelleyBasedEra era

    txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address

    -- build and submit transaction to create output containing some tokens.

    let (tokenValues, mintWitnesses) = case era of
          C.AlonzoEra -> error "Alonzo era is unsupported in this test"
          C.BabbageEra ->
            ( C.valueFromList [(PS_1_0.alwaysSucceedAssetIdV2, 10)]
            , Map.fromList [PS_1_0.alwaysSucceedMintWitnessV2 sbe Nothing]
            )
          C.ConwayEra ->
            ( C.valueFromList [(PS_1_0.alwaysSucceedAssetIdV2, 10), (PS_1_1.alwaysSucceedAssetIdV3, 10)]
            , Map.fromList
                [PS_1_0.alwaysSucceedMintWitnessV2 sbe Nothing, PS_1_1.alwaysSucceedMintWitnessV3 sbe Nothing]
            )
        collateral = Tx.txInsCollateral era [txIn]
        txOutAmount = 10_000_000
        txOut = Tx.txOut era (C.lovelaceToValue txOutAmount <> tokenValues) w1Address

        txBodyContent =
          (Tx.emptyTxBodyContent sbe pparams)
            { C.txIns = Tx.pubkeyTxIns [txIn]
            , C.txInsCollateral = collateral
            , C.txMintValue = Tx.txMintValue era tokenValues mintWitnesses
            , C.txOuts = [txOut]
            }

    signedTx <- Tx.buildTx era localNodeConnectInfo txBodyContent w1Address w1SKey
    Tx.submitTx sbe localNodeConnectInfo signedTx
    let collateralTxIn = Tx.txIn (Tx.txId signedTx) 0
    Q.waitForTxInAtAddress era localNodeConnectInfo w1Address collateralTxIn "waitForTxInAtAddress"

    -- build and submit transaction with failing script

    txIn2 <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address

    let (tokenValues2, mintWitnesses2) = case era of
          C.BabbageEra ->
            ( C.valueFromList [(PS_1_0.alwaysFailsAssetIdV2, 1)]
            , Map.fromList [PS_1_0.alwaysFailsMintWitnessV2 sbe Nothing]
            )
          C.ConwayEra ->
            ( C.valueFromList [(PS_1_1.alwaysFailsAssetIdV3, 1)]
            , Map.fromList [PS_1_1.alwaysSucceedMintWitnessV3 sbe Nothing]
            )
        collateral2 = Tx.txInsCollateral era [collateralTxIn]
        txOut1 = Tx.txOut era (C.lovelaceToValue 2_000_000) w1Address
        txOut2 = Tx.txOut era (C.lovelaceToValue 3_000_000 <> tokenValues2) w1Address
        colReturnAmount = 4_000_000
        colReturnValue = C.lovelaceToValue colReturnAmount <> tokenValues
        colReturnTxOut = Tx.txOut era colReturnValue w1Address
        totalCollateralAmount = txOutAmount - colReturnAmount

        txBodyContent2 =
          (Tx.emptyTxBodyContent sbe pparams)
            { C.txIns = Tx.pubkeyTxIns [txIn2]
            , C.txInsCollateral = collateral2
            , C.txMintValue = Tx.txMintValue era tokenValues2 mintWitnesses2
            , C.txOuts = [txOut1, txOut2]
            , C.txReturnCollateral = Tx.txReturnCollateral era colReturnTxOut
            , C.txTotalCollateral = Tx.txTotalCollateral era totalCollateralAmount
            , C.txScriptValidity = Tx.txScriptValidity era C.ScriptInvalid
            }

    signedTx2 <- Tx.buildTx era localNodeConnectInfo txBodyContent2 w1Address w1SKey
    Tx.submitTx sbe localNodeConnectInfo signedTx2

    -- Query for return collateral txo and assert presence of ada and tokens from the first tx
    let expectedTxIn = Tx.txIn (Tx.txId signedTx2) 3 -- collateral return index is n outputs (including change)
    resultTxOut <-
      Q.getTxOutAtAddress era localNodeConnectInfo w1Address expectedTxIn "getTxOutAtAddress"
    txOutHasAdaAndTokenValue <- Q.txOutHasValue resultTxOut colReturnValue
    a1 <- assert "txOut has tokens" txOutHasAdaAndTokenValue
    -- Query collateral input and assert it has been spent
    collateralSpent <- not <$> Q.isTxOutAtAddress era localNodeConnectInfo w1Address collateralTxIn
    a2 <- assert "collateral spent" collateralSpent
    -- Query regular tx input and assert it has not been spent
    txInNotSpent <- Q.isTxOutAtAddress era localNodeConnectInfo w1Address txIn2
    a3 <- assert "txIn not spent" txInNotSpent
    U.concatMaybes [a1, a2, a3]

-- TODO: access datum in reference input in plutus script
