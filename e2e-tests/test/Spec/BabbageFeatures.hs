{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-} -- Not using all CardanoEra
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE RecordWildCards     #-}

module Spec.BabbageFeatures(
    checkTxInfoV2Test,
    referenceScriptMintTest,
    referenceScriptInlineDatumSpendTest,
    referenceScriptDatumHashSpendTest,
    inlineDatumSpendTest,
    referenceInputWithV1ScriptErrorTest,
    referenceScriptOutputWithV1ScriptErrorTest,
    inlineDatumOutputWithV1ScriptErrorTest,
    returnCollateralWithTokensValidScriptTest,
    submitWithInvalidScriptThenCollateralIsTakenAndReturnedTest
    ) where

import Cardano.Api qualified as C
import Data.Map qualified as Map

import Hedgehog qualified as H

import CardanoTestnet qualified as TN
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Time.Clock.POSIX qualified as Time
import Hedgehog.Internal.Property (MonadTest)
import Helpers.Common (makeAddress)
import Helpers.Query qualified as Q
import Helpers.Test (TestParams (TestParams, localNodeConnectInfo, networkId, pparams, tempAbsPath))
import Helpers.Testnet qualified as TN
import Helpers.Tx qualified as Tx
import Helpers.Utils qualified as U
import Plutus.V1.Ledger.Interval qualified as PlutusV1
import Plutus.V1.Ledger.Time qualified as PlutusV1
import Plutus.V2.Ledger.Api qualified as PlutusV2
import PlutusScripts.Always qualified as PS
import PlutusScripts.Helpers qualified as PS
import PlutusScripts.V2TxInfo (checkV2TxInfoAssetIdV2, checkV2TxInfoMintWitnessV2, checkV2TxInfoRedeemer, txInfoData,
                               txInfoFee, txInfoInputs, txInfoMint, txInfoOutputs, txInfoSigs)

-- | Test must be first to run after new testnet is initialised due to slot timing
checkTxInfoV2Test :: (MonadIO m , MonadTest m) =>
  Either TN.LocalNodeOptions TN.TestnetOptions ->
  TestParams ->
  POSIXTime ->
  m ()
checkTxInfoV2Test networkOptions TestParams{..} preTestnetTime = do

  C.AnyCardanoEra era <- TN.eraFromOptions networkOptions
  startTime <- liftIO Time.getPOSIXTime
  (w1SKey, w1VKey, w1Address) <- TN.w1 tempAbsPath networkId

  -- build a transaction

  txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address
  txInAsTxOut@(C.TxOut _ txInValue _ _) <- Q.getTxOutAtAddress era localNodeConnectInfo w1Address txIn "txInAsTxOut <- getTxOutAtAddress"

  let
    tokenValues = C.valueFromList [(checkV2TxInfoAssetIdV2, 1), (PS.alwaysSucceedAssetIdV2, 2)]
    executionUnits1 = C.ExecutionUnits {C.executionSteps = 1_000_000_000, C.executionMemory = 10_000_000 }
    executionUnits2 = C.ExecutionUnits {C.executionSteps = 1_000_000_000, C.executionMemory = 4_000_000 }
    collateral = Tx.txInsCollateral era [txIn]
    totalLovelace = C.txOutValueToLovelace txInValue
    fee = 2_500_000 :: C.Lovelace
    amountPaid = 10_000_000
    amountReturned = totalLovelace - amountPaid - fee
    datum = PS.toScriptData (42 ::Integer)

    txOut1 = Tx.txOutWithDatumInTx era (C.lovelaceToValue amountPaid <> tokenValues) w1Address datum
    txOut2 = Tx.txOut era (C.lovelaceToValue amountReturned) w1Address

    lowerBound = PlutusV1.fromMilliSeconds
      $ PlutusV1.DiffMilliSeconds $ U.posixToMilliseconds preTestnetTime -- before slot 1
    upperBound = PlutusV1.fromMilliSeconds
      $ PlutusV1.DiffMilliSeconds $ U.posixToMilliseconds startTime + 600_000 -- ~10mins after slot 1 (to account for testnet init time)
    timeRange = PlutusV1.interval lowerBound upperBound :: PlutusV1.POSIXTimeRange

    expTxInfoInputs          = txInfoInputs (txIn, txInAsTxOut)
    expTxInfoReferenceInputs = txInfoInputs (txIn, txInAsTxOut)
    expTxInfoOutputs         = txInfoOutputs [ txOut1, txOut2 ]
    expTxInfoFee             = txInfoFee fee
    expTxInfoMint            = txInfoMint tokenValues
    expDCert                 = []                   -- not testing any staking registration certificate
    expWdrl                  = PlutusV2.fromList [] -- not testing any staking reward withdrawal
    expTxInfoSigs            = txInfoSigs [w1VKey]
    expTxInfoRedeemers       = PS.alwaysSucceedPolicyTxInfoRedeemerV2
    expTxInfoData            = txInfoData [datum]
    expTxInfoValidRange      = timeRange

    redeemer = checkV2TxInfoRedeemer [expTxInfoInputs] [expTxInfoReferenceInputs] expTxInfoOutputs expTxInfoFee expTxInfoMint
               expDCert expWdrl expTxInfoValidRange expTxInfoSigs expTxInfoRedeemers expTxInfoData
    mintWitnesses = Map.fromList [checkV2TxInfoMintWitnessV2 era redeemer executionUnits1, PS.alwaysSucceedMintWitnessV2' era executionUnits2]

    txBodyContent = (Tx.emptyTxBodyContent era pparams)
      { C.txIns = Tx.pubkeyTxIns [txIn]
      , C.txInsReference = Tx.txInsReference era [txIn]
      , C.txInsCollateral = collateral
      , C.txMintValue = Tx.txMintValue era tokenValues mintWitnesses
      , C.txOuts = [txOut1, txOut2]
      , C.txFee = Tx.txFee era fee
      , C.txValidityRange = Tx.txValidityRange era 1 2700 -- ~9min range (200ms slots). Babbage era onwards cannot have upper slot beyond epoch boundary (10_000 slot epoch).
      , C.txExtraKeyWits = Tx.txExtraKeyWits era [w1VKey]
      }
  txbody <- Tx.buildRawTx era txBodyContent
  kw <- Tx.signTx era txbody w1SKey
  let signedTx = C.makeSignedTransaction [kw] txbody

  Tx.submitTx era localNodeConnectInfo signedTx

  let expectedTxIn = Tx.txIn (Tx.txId signedTx) 0
  resultTxOut <- Q.getTxOutAtAddress era localNodeConnectInfo w1Address expectedTxIn "resultTxOut <- getTxOutAtAddress "
  txOutHasTokenValue <- Q.txOutHasValue resultTxOut tokenValues
  H.assert txOutHasTokenValue

  H.success

referenceScriptMintTest :: (MonadTest m, MonadIO m) =>
  Either TN.LocalNodeOptions TN.TestnetOptions ->
  TestParams ->
  m ()
referenceScriptMintTest networkOptions TestParams{..} = do

  C.AnyCardanoEra era <- TN.eraFromOptions networkOptions
  (w1SKey, _, w1Address) <- TN.w1 tempAbsPath networkId

  -- build a transaction to hold reference script

  txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address

  let
    refScriptTxOut = Tx.txOutWithRefScript era (C.lovelaceToValue 20_000_000) w1Address
      (PS.unPlutusScriptV2 PS.alwaysSucceedPolicyScriptV2)
    otherTxOut = Tx.txOut era (C.lovelaceToValue 5_000_000) w1Address

    txBodyContent = (Tx.emptyTxBodyContent era pparams)
      { C.txIns = Tx.pubkeyTxIns [txIn]
      , C.txOuts = [refScriptTxOut, otherTxOut]
      }

  signedTx <- Tx.buildTx era txBodyContent w1Address w1SKey networkId
  Tx.submitTx era localNodeConnectInfo signedTx
  let refScriptTxIn = Tx.txIn (Tx.txId signedTx) 0
      otherTxIn   = Tx.txIn (Tx.txId signedTx) 1
  Q.waitForTxInAtAddress era localNodeConnectInfo w1Address refScriptTxIn "waitForTxInAtAddress"

  -- build a transaction to mint token using reference script

  let
    tokenValues = C.valueFromList [(PS.alwaysSucceedAssetIdV2, 6)]
    mintWitnesses = Map.fromList [PS.alwaysSucceedMintWitnessV2 era (Just refScriptTxIn)]
    collateral = Tx.txInsCollateral era [otherTxIn]
    txOut = Tx.txOut era (C.lovelaceToValue 3_000_000 <> tokenValues) w1Address

    txBodyContent2 = (Tx.emptyTxBodyContent era pparams)
      { C.txIns = Tx.pubkeyTxIns [otherTxIn]
      , C.txInsCollateral = collateral
      , C.txInsReference = Tx.txInsReference era [refScriptTxIn]
      , C.txMintValue = Tx.txMintValue era tokenValues mintWitnesses
      , C.txOuts = [txOut]
      }

  signedTx2 <- Tx.buildTx era txBodyContent2 w1Address w1SKey networkId
  Tx.submitTx era localNodeConnectInfo signedTx2
  let expectedTxIn = Tx.txIn (Tx.txId signedTx2) 0
  -- Query for txo and assert it contains newly minted token
  resultTxOut <- Q.getTxOutAtAddress era localNodeConnectInfo w1Address expectedTxIn "getTxOutAtAddress"
  txOutHasTokenValue <- Q.txOutHasValue resultTxOut tokenValues
  H.assert txOutHasTokenValue

  H.success

referenceScriptInlineDatumSpendTest :: (MonadIO m , MonadTest m) =>
  Either TN.LocalNodeOptions TN.TestnetOptions ->
  TestParams ->
  m ()
referenceScriptInlineDatumSpendTest networkOptions TestParams{..} = do

  C.AnyCardanoEra era <- TN.eraFromOptions networkOptions
  (w1SKey, _, w1Address) <- TN.w1 tempAbsPath networkId

  -- build a transaction to hold reference script

  txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address

  let
    refTxOut      = Tx.txOutWithRefScript era (C.lovelaceToValue 20_000_000) w1Address
                     (PS.unPlutusScriptV2 PS.alwaysSucceedSpendScriptV2)
    otherTxOut    = Tx.txOut era (C.lovelaceToValue 5_000_000) w1Address
    scriptAddress = makeAddress (Right PS.alwaysSucceedSpendScriptHashV2) networkId
    scriptTxOut   = Tx.txOutWithInlineDatum era (C.lovelaceToValue 10_000_000) scriptAddress (PS.toScriptData ())

    txBodyContent = (Tx.emptyTxBodyContent era pparams)
      { C.txIns = Tx.pubkeyTxIns [txIn]
      , C.txOuts = [refTxOut, otherTxOut, scriptTxOut]
      }

  signedTx <- Tx.buildTx era txBodyContent w1Address w1SKey networkId
  Tx.submitTx era localNodeConnectInfo signedTx
  let refScriptTxIn = Tx.txIn (Tx.txId signedTx) 0
      otherTxIn     = Tx.txIn (Tx.txId signedTx) 1
      txInAtScript  = Tx.txIn (Tx.txId signedTx) 2
  Q.waitForTxInAtAddress era localNodeConnectInfo w1Address refScriptTxIn "waitForTxInAtAddress"

  -- build a transaction to mint token using reference script

  let
    scriptTxIn = Tx.txInWitness txInAtScript (PS.alwaysSucceedSpendWitnessV2 era (Just refScriptTxIn) Nothing)
    collateral = Tx.txInsCollateral era [otherTxIn]
    adaValue = C.lovelaceToValue 4_200_000
    txOut = Tx.txOut era adaValue w1Address

    txBodyContent2 = (Tx.emptyTxBodyContent era pparams)
      { C.txIns = [scriptTxIn]
      , C.txInsReference = Tx.txInsReference era [refScriptTxIn]
      , C.txInsCollateral = collateral
      , C.txOuts = [txOut]
      }

  signedTx2 <- Tx.buildTx era txBodyContent2 w1Address w1SKey networkId
  Tx.submitTx era localNodeConnectInfo signedTx2
  let expectedTxIn = Tx.txIn (Tx.txId signedTx2) 0
  -- Query for txo and assert it contains newly minted token
  resultTxOut <- Q.getTxOutAtAddress era localNodeConnectInfo w1Address expectedTxIn "getTxOutAtAddress"
  txOutHasAdaValue <- Q.txOutHasValue resultTxOut adaValue
  H.assert txOutHasAdaValue

  H.success

referenceScriptDatumHashSpendTest :: (MonadIO m , MonadTest m) =>
  Either TN.LocalNodeOptions TN.TestnetOptions ->
  TestParams ->
  m ()
referenceScriptDatumHashSpendTest networkOptions TestParams{..} = do

  C.AnyCardanoEra era <- TN.eraFromOptions networkOptions
  (w1SKey, _, w1Address) <- TN.w1 tempAbsPath networkId

  -- build a transaction to hold reference script

  txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address

  let
    refTxOut      = Tx.txOutWithRefScript era (C.lovelaceToValue 20_000_000) w1Address
                     (PS.unPlutusScriptV2 PS.alwaysSucceedSpendScriptV2)
    otherTxOut    = Tx.txOut era (C.lovelaceToValue 5_000_000) w1Address
    scriptAddress = makeAddress (Right PS.alwaysSucceedSpendScriptHashV2) networkId
    datum         = PS.toScriptData ()
    scriptTxOut   = Tx.txOutWithDatumHash era (C.lovelaceToValue 10_000_000) scriptAddress datum

    txBodyContent = (Tx.emptyTxBodyContent era pparams)
      { C.txIns = Tx.pubkeyTxIns [txIn]
      , C.txOuts = [refTxOut, otherTxOut, scriptTxOut]
      }

  signedTx <- Tx.buildTx era txBodyContent w1Address w1SKey networkId
  Tx.submitTx era localNodeConnectInfo signedTx
  let refScriptTxIn = Tx.txIn (Tx.txId signedTx) 0
      otherTxIn     = Tx.txIn (Tx.txId signedTx) 1
      txInAtScript  = Tx.txIn (Tx.txId signedTx) 2
  Q.waitForTxInAtAddress era localNodeConnectInfo w1Address refScriptTxIn "waitForTxInAtAddress"

  -- build a transaction to mint token using reference script

  let
    scriptTxIn = Tx.txInWitness txInAtScript $ PS.alwaysSucceedSpendWitnessV2 era (Just refScriptTxIn) (Just datum)
    collateral = Tx.txInsCollateral era [otherTxIn]
    adaValue = C.lovelaceToValue 4_200_000
    txOut = Tx.txOut era adaValue w1Address

    txBodyContent2 = (Tx.emptyTxBodyContent era pparams)
      { C.txIns = [scriptTxIn]
      , C.txInsReference = Tx.txInsReference era [refScriptTxIn]
      , C.txInsCollateral = collateral
      , C.txOuts = [txOut]
      }

  signedTx2 <- Tx.buildTx era txBodyContent2 w1Address w1SKey networkId
  Tx.submitTx era localNodeConnectInfo signedTx2
  let expectedTxIn = Tx.txIn (Tx.txId signedTx2) 0
  -- Query for txo and assert it contains newly minted token
  resultTxOut <- Q.getTxOutAtAddress era localNodeConnectInfo w1Address expectedTxIn "getTxOutAtAddress"
  txOutHasAdaValue <- Q.txOutHasValue resultTxOut adaValue
  H.assert txOutHasAdaValue

  H.success

inlineDatumSpendTest :: (MonadIO m , MonadTest m) =>
  Either TN.LocalNodeOptions TN.TestnetOptions ->
  TestParams ->
  m ()
inlineDatumSpendTest networkOptions TestParams{..} = do

  C.AnyCardanoEra era <- TN.eraFromOptions networkOptions
  (w1SKey, _, w1Address) <- TN.w1 tempAbsPath networkId

  -- build a transaction to hold inline datum at script address

  txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address

  let
    scriptAddress = makeAddress (Right PS.alwaysSucceedSpendScriptHashV2) networkId
    scriptTxOut   = Tx.txOutWithInlineDatum era (C.lovelaceToValue 10_000_000) scriptAddress (PS.toScriptData ())
    otherTxOut    = Tx.txOut era (C.lovelaceToValue 5_000_000) w1Address

    txBodyContent = (Tx.emptyTxBodyContent era pparams)
      { C.txIns = Tx.pubkeyTxIns [txIn]
      , C.txOuts = [scriptTxOut, otherTxOut]
      }

  signedTx <- Tx.buildTx era txBodyContent w1Address w1SKey networkId
  Tx.submitTx era localNodeConnectInfo signedTx
  let txInAtScript  = Tx.txIn (Tx.txId signedTx) 0
      otherTxIn     = Tx.txIn (Tx.txId signedTx) 1
  Q.waitForTxInAtAddress era localNodeConnectInfo scriptAddress txInAtScript "waitForTxInAtAddress"

  -- build a transaction to mint token using reference script

  let
    scriptTxIn = Tx.txInWitness txInAtScript (PS.alwaysSucceedSpendWitnessV2 era Nothing Nothing)  -- without reference script
    collateral = Tx.txInsCollateral era [otherTxIn]
    adaValue = C.lovelaceToValue 4_200_000
    txOut = Tx.txOut era adaValue w1Address

    txBodyContent2 = (Tx.emptyTxBodyContent era pparams)
      { C.txIns = [scriptTxIn]
      , C.txInsCollateral = collateral
      , C.txOuts = [txOut]
      }

  signedTx2 <- Tx.buildTx era txBodyContent2 w1Address w1SKey networkId
  Tx.submitTx era localNodeConnectInfo signedTx2
  let expectedTxIn = Tx.txIn (Tx.txId signedTx2) 0
  -- Query for txo and assert it contains newly minted token
  resultTxOut <- Q.getTxOutAtAddress era localNodeConnectInfo w1Address expectedTxIn "getTxOutAtAddress"
  txOutHasAdaValue <- Q.txOutHasValue resultTxOut adaValue
  H.assert txOutHasAdaValue

  H.success

referenceInputWithV1ScriptErrorTest :: (MonadIO m , MonadTest m) =>
  Either TN.LocalNodeOptions TN.TestnetOptions ->
  TestParams ->
  m ()
referenceInputWithV1ScriptErrorTest networkOptions TestParams{..} = do

  C.AnyCardanoEra era <- TN.eraFromOptions networkOptions
  (w1SKey, _, w1Address) <- TN.w1 tempAbsPath networkId

  txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address

  let
    tokenValues = C.valueFromList [(PS.alwaysSucceedAssetIdV1, 1)]
    mintWitnesses = Map.fromList [PS.alwaysSucceedMintWitnessV1 era Nothing]
    collateral = Tx.txInsCollateral era [txIn]
    txOut = Tx.txOut era (C.lovelaceToValue 3_000_000 <> tokenValues) w1Address

    txBodyContent = (Tx.emptyTxBodyContent era pparams)
      { C.txIns = Tx.pubkeyTxIns [txIn]
      , C.txInsCollateral = collateral
      , C.txInsReference = Tx.txInsReference era [txIn]
      , C.txMintValue = Tx.txMintValue era tokenValues mintWitnesses
      , C.txOuts = [txOut]
      }

  eitherTx <- Tx.buildTx' era txBodyContent w1Address w1SKey networkId
  H.assert $ Tx.isTxBodyErrorValidityInterval "ReferenceInputsNotSupported" eitherTx -- why is this validity interval error? https://github.com/input-output-hk/cardano-node/issues/5080
  H.success

referenceScriptOutputWithV1ScriptErrorTest :: (MonadIO m , MonadTest m) =>
  Either TN.LocalNodeOptions TN.TestnetOptions ->
  TestParams ->
  m ()
referenceScriptOutputWithV1ScriptErrorTest networkOptions TestParams{..} = do

  C.AnyCardanoEra era <- TN.eraFromOptions networkOptions
  (w1SKey, _, w1Address) <- TN.w1 tempAbsPath networkId

  txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address

  let
    tokenValues = C.valueFromList [(PS.alwaysSucceedAssetIdV1, 1)]
    mintWitnesses = Map.fromList [PS.alwaysSucceedMintWitnessV1 era Nothing]
    collateral = Tx.txInsCollateral era [txIn]
    txOut = Tx.txOutWithRefScript era (C.lovelaceToValue 3_000_000 <> tokenValues) w1Address
              (PS.unPlutusScriptV2 PS.alwaysSucceedSpendScriptV2)

    txBodyContent = (Tx.emptyTxBodyContent era pparams)
      { C.txIns = Tx.pubkeyTxIns [txIn]
      , C.txInsCollateral = collateral
      , C.txMintValue = Tx.txMintValue era tokenValues mintWitnesses
      , C.txOuts = [txOut]
      }

  eitherTx <- Tx.buildTx' era txBodyContent w1Address w1SKey networkId
  H.annotate $ show eitherTx
  H.assert $ Tx.isTxBodyErrorValidityInterval "ReferenceScriptsNotSupported" eitherTx -- why is this validity interval error? https://github.com/input-output-hk/cardano-node/issues/5080
  H.success

inlineDatumOutputWithV1ScriptErrorTest :: (MonadIO m , MonadTest m) =>
  Either TN.LocalNodeOptions TN.TestnetOptions ->
  TestParams ->
  m ()
inlineDatumOutputWithV1ScriptErrorTest networkOptions TestParams{..} = do

  C.AnyCardanoEra era <- TN.eraFromOptions networkOptions
  (w1SKey, _, w1Address) <- TN.w1 tempAbsPath networkId

  txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address

  let
    tokenValues = C.valueFromList [(PS.alwaysSucceedAssetIdV1, 1)]
    mintWitnesses = Map.fromList [PS.alwaysSucceedMintWitnessV1 era Nothing]
    collateral = Tx.txInsCollateral era [txIn]
    txOut = Tx.txOutWithInlineDatum era (C.lovelaceToValue 3_000_000 <> tokenValues) w1Address (PS.toScriptData ())

    txBodyContent = (Tx.emptyTxBodyContent era pparams)
      { C.txIns = Tx.pubkeyTxIns [txIn]
      , C.txInsCollateral = collateral
      , C.txMintValue = Tx.txMintValue era tokenValues mintWitnesses
      , C.txOuts = [txOut]
      }

  eitherTx <- Tx.buildTx' era txBodyContent w1Address w1SKey networkId
  H.annotate $ show eitherTx
  H.assert $ Tx.isTxBodyErrorValidityInterval "InlineDatumsNotSupported" eitherTx -- why is this validity interval error? https://github.com/input-output-hk/cardano-node/issues/5080
  H.success

returnCollateralWithTokensValidScriptTest :: (MonadIO m , MonadTest m) =>
  Either TN.LocalNodeOptions TN.TestnetOptions ->
  TestParams ->
  m ()
returnCollateralWithTokensValidScriptTest networkOptions TestParams{..} = do

  C.AnyCardanoEra era <- TN.eraFromOptions networkOptions
  (w1SKey, _, w1Address) <- TN.w1 tempAbsPath networkId

  txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address

  -- build and submit transaction to create output containing some tokens.

  let
    tokenValues = C.valueFromList [(PS.alwaysSucceedAssetIdV2, 10)]
    mintWitnesses = Map.fromList [PS.alwaysSucceedMintWitnessV2 era Nothing]
    collateral = Tx.txInsCollateral era [txIn]
    txOut = Tx.txOutWithInlineDatum era (C.lovelaceToValue 5_000_000 <> tokenValues) w1Address (PS.toScriptData ())

    txBodyContent = (Tx.emptyTxBodyContent era pparams)
      { C.txIns = Tx.pubkeyTxIns [txIn]
      , C.txInsCollateral = collateral
      , C.txMintValue = Tx.txMintValue era tokenValues mintWitnesses
      , C.txOuts = [txOut]
      }

  signedTx <- Tx.buildTx era txBodyContent w1Address w1SKey networkId
  Tx.submitTx era localNodeConnectInfo signedTx
  let txIn2 = Tx.txIn (Tx.txId signedTx) 0
  Q.waitForTxInAtAddress era localNodeConnectInfo w1Address txIn2 "waitForTxInAtAddress"

  -- build and submit transaction with tokens in collateral input.
  -- This is allowed because using return collateral feature.

  let
    tokenValues2 = C.valueFromList [(PS.alwaysSucceedAssetIdV2, 20)]
    collateral2 = Tx.txInsCollateral era [txIn2]
    txOut2 = Tx.txOutWithInlineDatum era (C.lovelaceToValue 2_000_000 <> tokenValues2) w1Address (PS.toScriptData ())
    colReturnTxOut = Tx.txOut era (C.lovelaceToValue 3_000_000 <> tokenValues) w1Address

    txBodyContent2 = (Tx.emptyTxBodyContent era pparams)
      { C.txIns = Tx.pubkeyTxIns [txIn2]
      , C.txInsCollateral = collateral2
      , C.txMintValue = Tx.txMintValue era tokenValues mintWitnesses
      , C.txOuts = [txOut2]
      , C.txReturnCollateral = Tx.txReturnCollateral era colReturnTxOut
      }

  signedTx2 <- Tx.buildTx era txBodyContent2 w1Address w1SKey networkId
  Tx.submitTx era localNodeConnectInfo signedTx2
  let expectedTxIn = Tx.txIn (Tx.txId signedTx2) 0
  -- Query for txo and assert it contains newly minted token
  resultTxOut <- Q.getTxOutAtAddress era localNodeConnectInfo w1Address expectedTxIn "getTxOutAtAddress"
  txOutHasTokenValue <- Q.txOutHasValue resultTxOut tokenValues2
  H.assert txOutHasTokenValue

  H.success

-- Submit a failing script when using total and return collateral in tx body.
-- Check that ada and tokens from collateral input are returned in the collateral output.
-- Also check that regular input is not consumed.
submitWithInvalidScriptThenCollateralIsTakenAndReturnedTest :: (MonadIO m , MonadTest m) =>
  Either TN.LocalNodeOptions TN.TestnetOptions ->
  TestParams ->
  m ()
submitWithInvalidScriptThenCollateralIsTakenAndReturnedTest networkOptions TestParams{..} = do

  C.AnyCardanoEra era <- TN.eraFromOptions networkOptions
  (w1SKey, _, w1Address) <- TN.w1 tempAbsPath networkId

  txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address

  -- build and submit transaction to create output containing some tokens.

  let
    tokenValues = C.valueFromList [(PS.alwaysSucceedAssetIdV2, 10)]
    mintWitnesses = Map.fromList [PS.alwaysSucceedMintWitnessV2 era Nothing]
    collateral = Tx.txInsCollateral era [txIn]
    txOutAmount = 10_000_000
    txOut = Tx.txOut era (C.lovelaceToValue txOutAmount <> tokenValues) w1Address

    txBodyContent = (Tx.emptyTxBodyContent era pparams)
      { C.txIns = Tx.pubkeyTxIns [txIn]
      , C.txInsCollateral = collateral
      , C.txMintValue = Tx.txMintValue era tokenValues mintWitnesses
      , C.txOuts = [txOut]
      }

  signedTx <- Tx.buildTx era txBodyContent w1Address w1SKey networkId
  Tx.submitTx era localNodeConnectInfo signedTx
  let collateralTxIn = Tx.txIn (Tx.txId signedTx) 0
  Q.waitForTxInAtAddress era localNodeConnectInfo w1Address collateralTxIn "waitForTxInAtAddress"

  -- build and submit transaction with failing script

  txIn2 <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address

  let
    tokenValues2 = C.valueFromList [(PS.alwaysFailsAssetIdV2, 1)]
    mintWitnesses2 = Map.fromList [PS.alwaysFailsMintWitnessV2 era Nothing]
    collateral2 = Tx.txInsCollateral era [collateralTxIn]
    txOut1 = Tx.txOut era (C.lovelaceToValue 2_000_000) w1Address
    txOut2 = Tx.txOut era (C.lovelaceToValue 3_000_000 <> tokenValues2) w1Address
    colReturnAmount = 4_000_000
    colReturnValue = C.lovelaceToValue colReturnAmount <> tokenValues
    colReturnTxOut = Tx.txOut era colReturnValue w1Address
    totalCollateralAmount = txOutAmount - colReturnAmount

    txBodyContent2 = (Tx.emptyTxBodyContent era pparams)
      { C.txIns = Tx.pubkeyTxIns [txIn2]
      , C.txInsCollateral = collateral2
      , C.txMintValue = Tx.txMintValue era tokenValues2 mintWitnesses2
      , C.txOuts = [txOut1, txOut2]
      , C.txReturnCollateral = Tx.txReturnCollateral era colReturnTxOut
      , C.txTotalCollateral = Tx.txTotalCollateral era totalCollateralAmount
      , C.txScriptValidity = Tx.txScriptValidity era C.ScriptInvalid
      }

  signedTx2 <- Tx.buildTx era txBodyContent2 w1Address w1SKey networkId
  Tx.submitTx era localNodeConnectInfo signedTx2

  -- Query for return collateral txo and assert presence of ada and tokens from the first tx
  let expectedTxIn = Tx.txIn (Tx.txId signedTx2) 3 -- collateral return index is n outputs (including change)
  resultTxOut <- Q.getTxOutAtAddress era localNodeConnectInfo w1Address expectedTxIn "getTxOutAtAddress"
  txOutHasAdaAndTokenValue <- Q.txOutHasValue resultTxOut colReturnValue
  H.assert txOutHasAdaAndTokenValue
  -- Query collateral input and assert it has been spent
  collateralSpent <- not <$> Q.isTxOutAtAddress era localNodeConnectInfo w1Address collateralTxIn
  H.assert collateralSpent
  -- Query regular tx input and assert it has not been spent
  txInNotSpent <- Q.isTxOutAtAddress era localNodeConnectInfo w1Address txIn2
  H.assert txInNotSpent

  H.success

-- TODO: access datum in reference input in plutus script
