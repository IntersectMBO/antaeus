{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Spec.ConwayFeatures where

import Cardano.Api qualified as C
import Cardano.Api.Ledger qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.Crypto.Hash qualified as Crypto
import Cardano.Ledger.SafeHash qualified as L
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString qualified as BS
import Data.Map (singleton)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Time.Clock.POSIX qualified as Time
import Hedgehog qualified as H
import Hedgehog.Extras qualified as H
import Hedgehog.Internal.Property (MonadTest, (===))
import Helpers.Common (toConwayEraOnwards, toShelleyBasedEra)
import Helpers.Query qualified as Q
import Helpers.Test (assert)
import Helpers.TestData (TestInfo (..), TestParams (..))
import Helpers.Testnet qualified as TN
import Helpers.Tx qualified as Tx
import Helpers.Utils qualified as U
import PlutusLedgerApi.V1.Interval qualified as P
import PlutusLedgerApi.V1.Time qualified as P
import PlutusLedgerApi.V2 qualified as PlutusV2
import PlutusScripts.Always.V_1_0 qualified as PS_1_0
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

-- NOTE: Does not yet check V3 TxInfo fields
checkTxInfoV3TestInfo =
  TestInfo
    { testName = "checkTxInfoV3Test"
    , testDescription =
        "Check each attribute of the TxInfo from the V3 ScriptContext in a single transaction"
    , test = checkTxInfoV3Test
    }

checkTxInfoV3Test
  :: (MonadIO m, MonadTest m)
  => Either TN.LocalNodeOptions TN.TestnetOptions
  -> TestParams
  -> m (Maybe String)
checkTxInfoV3Test networkOptions TestParams{..} = do
  C.AnyCardanoEra era <- TN.eraFromOptions networkOptions
  startTime <- liftIO Time.getPOSIXTime
  (w1SKey, w1VKey, w1Address) <- TN.w1All networkOptions tempAbsPath networkId

  -- build a transaction

  txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address
  txInAsTxOut@(C.TxOut _ txInValue _ _) <-
    Q.getTxOutAtAddress era localNodeConnectInfo w1Address txIn "txInAsTxOut <- getTxOutAtAddress"

  -- TODO: use V3 scripts here and check for V3 TxInfo fields
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
              fromJust mTime -- before slot 1
      upperBound =
        P.fromMilliSeconds $
          P.DiffMilliSeconds $
            U.posixToMilliseconds startTime + 600_000 -- ~10mins after slot 1 (to account for testnet init time)
      timeRange = P.interval lowerBound upperBound :: P.POSIXTimeRange

      expTxInfoInputs = PS.txInfoInputs (txIn, txInAsTxOut)
      expTxInfoReferenceInputs = PS.txInfoInputs (txIn, txInAsTxOut)
      expTxInfoOutputs = PS.txInfoOutputs [txOut1, txOut2]
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
          [ PS.checkV2TxInfoMintWitnessV2 era redeemer executionUnits1
          , PS_1_0.alwaysSucceedMintWitnessV2' era executionUnits2
          ]

      txBodyContent =
        (Tx.emptyTxBodyContent pparams)
          { C.txIns = Tx.pubkeyTxIns [txIn]
          , C.txInsReference = Tx.txInsReference era [txIn]
          , C.txInsCollateral = collateral
          , C.txMintValue = Tx.txMintValue era tokenValues mintWitnesses
          , C.txOuts = [txOut1, txOut2]
          , C.txFee = Tx.txFee era fee
          , C.txValidityRange = Tx.txValidityRange era 1 2700
          , -- \^ ~9min range (200ms slots)
            -- \^ Babbage era onwards cannot have upper slot beyond epoch boundary (10_000 slot epoch)
            C.txExtraKeyWits = Tx.txExtraKeyWits era [w1VKey]
          }
  txbody <- Tx.buildRawTx era txBodyContent
  kw <- Tx.signTx era txbody (C.WitnessPaymentKey w1SKey)
  let signedTx = C.makeSignedTransaction [kw] txbody

  Tx.submitTx era localNodeConnectInfo signedTx

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

constitutionProposalAndVoteTestInfo =
  TestInfo
    { testName = "constitutionProposalAndVoteTest"
    , testDescription = "Propose and vote on new constitution"
    , test = constitutionProposalAndVoteTest
    }

constitutionProposalAndVoteTest
  :: (MonadTest m, MonadIO m)
  => Either TN.LocalNodeOptions TN.TestnetOptions
  -> TestParams
  -> m (Maybe String)
constitutionProposalAndVoteTest networkOptions TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
  C.AnyCardanoEra era <- TN.eraFromOptions networkOptions
  (w1SKey, w1Address) <- TN.w1 networkOptions tempAbsPath networkId
  let sbe = toShelleyBasedEra era
      ceo = toConwayEraOnwards era

  (pool1SKey, pool1StakeKeyHash, stakePoolKeyHash) <- TN.pool1 tempAbsPath

  -- wait for next epoch to start before proposing governance action
  currentEpoch <-
    Q.waitForNextEpoch era localNodeConnectInfo =<< Q.getCurrentEpoch era localNodeConnectInfo
  H.annotate $ show currentEpoch

  -- check no existing constitution hash (why not Nothing?)
  existingConstitutionHash <- Q.getConstitutionAnchorHashAsString era localNodeConnectInfo
  existingConstitutionHash === "\"0000000000000000000000000000000000000000000000000000000000000000\""

  -- define a new constituion
  let constitutionPath = tempAbsPath <> "/constituion.txt"
  H.writeFile constitutionPath "a new way of life"
  constituionBS <- H.evalIO $ BS.readFile constitutionPath
  let
    constituionHash = show (Crypto.hashWith id constituionBS :: Crypto.Hash Crypto.Blake2b_256 BS.ByteString)
    constitutionUrl = U.unsafeFromMaybe $ C.textToUrl "https://example.com/constituion.txt"
    anchor = C.createAnchor constitutionUrl constituionBS
  H.annotate constituionHash

  -- build a transaction to propose the constituion

  txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address

  let txOut1 = Tx.txOut era (C.lovelaceToValue 2_000_000) w1Address
      txOut2 = Tx.txOut era (C.lovelaceToValue 3_000_000) w1Address
      proposal =
        C.createProposalProcedure
          sbe
          (C.toShelleyNetwork networkId)
          1_000_000
          pool1StakeKeyHash
          (C.ProposeNewConstitution C.SNothing anchor)
          anchor

      txBodyContent =
        (Tx.emptyTxBodyContent pparams)
          { C.txIns = Tx.pubkeyTxIns [txIn]
          , C.txGovernanceActions = C.TxGovernanceActions ceo [proposal]
          , C.txOuts = [txOut1, txOut2]
          }

  signedTx <- Tx.buildTx era localNodeConnectInfo txBodyContent w1Address w1SKey
  H.annotate $ show signedTx
  Tx.submitTx era localNodeConnectInfo signedTx
  let txIn1@(C.TxIn txInId1 _txInIx1) = Tx.txIn (Tx.txId signedTx) 0
      _txIn2 = Tx.txIn (Tx.txId signedTx) 1
      txIn3 = Tx.txIn (Tx.txId signedTx) 2 -- change output
  resultTxOut <-
    Q.getTxOutAtAddress era localNodeConnectInfo w1Address txIn1 "getTxOutAtAddress"
  H.annotate $ show resultTxOut

  -- vote on the constituion
  let txOut3 = Tx.txOut era (C.lovelaceToValue 4_000_000) w1Address
      gAID = C.shelleyBasedEraConstraints sbe $ C.createGovernanceActionId txInId1 0
      vote = C.createVotingProcedure sbe C.Yes Nothing

      txBodyContent2 =
        (Tx.emptyTxBodyContent pparams)
          { C.txIns = Tx.pubkeyTxIns [txIn3]
          , C.txVotes = C.TxVotes ceo (singleton (C.VoterSpo stakePoolKeyHash, C.GovernanceActionId gAID) vote)
          , C.txOuts = [txOut3]
          }

  signedTx2 <-
    Tx.buildTxWithWitnessOverride
      era
      localNodeConnectInfo
      txBodyContent2
      w1Address
      (Just 2)
      [C.WitnessPaymentKey w1SKey, C.WitnessStakePoolKey pool1SKey]
  H.annotate $ show signedTx2
  Tx.submitTx era localNodeConnectInfo signedTx2
  let expectedTxIn2 = Tx.txIn (Tx.txId signedTx2) 0
  resultTxOut2 <-
    Q.getTxOutAtAddress era localNodeConnectInfo w1Address expectedTxIn2 "getTxOutAtAddress"
  H.annotate $ show resultTxOut2

  -- wait for next epoch before asserting for new constitution
  Q.waitForNextEpoch_ era localNodeConnectInfo currentEpoch

  -- wait 1 second at start of epoch to guarentee constitution is enacted
  liftIO $ threadDelay 1_000_000

  -- check new constituion is enacted
  newConstitutionHash <- Q.getConstitutionAnchorHashAsString era localNodeConnectInfo
  H.annotate newConstitutionHash -- debug
  constituionHash === newConstitutionHash -- debug assertion
  assert "expected constitution hash matches query result" (constituionHash == newConstitutionHash)
