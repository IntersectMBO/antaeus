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
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString qualified as BS
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Ratio ((%))
import Data.Time.Clock.POSIX qualified as Time
import Hedgehog qualified as H
import Hedgehog.Extras qualified as H
import Hedgehog.Internal.Property (MonadTest, (===))
import Helpers.Committee
import Helpers.Common (toConwayEraOnwards, toShelleyBasedEra)
import Helpers.DRep (
  DRep (DRep, dRepKeyHash, dRepRegCert, dRepSKey, dRepStakeCred, dRepVotingCredential),
 )
import Helpers.Query qualified as Q
import Helpers.Staking (Staking (Staking, stakeCred, stakeRegCert, stakeSKey))
import Helpers.Test (assert, success)
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
  => Either (TN.LocalNodeOptions era) (TN.TestnetOptions era)
  -> TestParams era
  -> m (Maybe String)
checkTxInfoV3Test networkOptions TestParams{..} = do
  era <- TN.eraFromOptionsM networkOptions
  startTime <- liftIO Time.getPOSIXTime
  (w1SKey, w1VKey, w1Address) <- TN.w1All tempAbsPath networkId

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
        (Tx.emptyTxBodyContent era pparams)
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
  txbody <- Tx.buildRawTx era txBodyContent
  kw <- Tx.signTx (toShelleyBasedEra era) txbody (C.WitnessPaymentKey w1SKey)
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

registerStakingTestInfo staking =
  TestInfo
    { testName = "registerStakingTest"
    , testDescription = "Register a stake address (for vote delegation)"
    , test = registerStakingTest staking
    }
registerStakingTest
  :: (MonadTest m, MonadIO m)
  => Staking era
  -> Either (TN.LocalNodeOptions era) (TN.TestnetOptions era)
  -> TestParams era
  -> m (Maybe String)
registerStakingTest
  Staking{..}
  networkOptions
  TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
    era <- TN.eraFromOptionsM networkOptions
    (w1SKey, w1Address) <- TN.w1 tempAbsPath networkId

    w1StakeRegTxIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address
    let
      adaValue = C.lovelaceToValue 2_000_000
      w1StakeRegTxOut = Tx.txOut era adaValue w1Address
      w1StakeRegTxBodyContent =
        (Tx.emptyTxBodyContent era pparams)
          { C.txIns = Tx.pubkeyTxIns [w1StakeRegTxIn]
          , C.txCertificates = Tx.txCertificates era [stakeRegCert] stakeCred
          , C.txOuts = [w1StakeRegTxOut]
          }
    signedW1StakeRegTx1 <-
      Tx.buildTxWithWitnessOverride
        era
        localNodeConnectInfo
        w1StakeRegTxBodyContent
        w1Address
        (Just 2) -- witnesses
        [C.WitnessPaymentKey w1SKey, C.WitnessStakeKey stakeSKey]
    Tx.submitTx era localNodeConnectInfo signedW1StakeRegTx1
    let expTxIn = Tx.txIn (Tx.txId signedW1StakeRegTx1) 1 -- change output
    w1StakeRegResultTxOut <-
      Q.getTxOutAtAddress era localNodeConnectInfo w1Address expTxIn "getTxOutAtAddress"
    H.annotate $ show w1StakeRegResultTxOut
    success

registerDRepTestInfo dRep =
  TestInfo
    { testName = "registerDRepTest"
    , testDescription = "Register a DRep address (for voting)"
    , test = registerDRepTest dRep
    }
registerDRepTest
  :: (MonadTest m, MonadIO m)
  => DRep era
  -> Either (TN.LocalNodeOptions era) (TN.TestnetOptions era)
  -> TestParams era
  -> m (Maybe String)
registerDRepTest
  DRep{..}
  networkOptions
  TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
    era <- TN.eraFromOptionsM networkOptions
    (w1SKey, w1Address) <- TN.w1 tempAbsPath networkId

    dRepRegTxIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address
    let
      regDRepTxOut = Tx.txOut era (C.lovelaceToValue 2_000_000) w1Address
      regDRepTxBodyContent =
        (Tx.emptyTxBodyContent era pparams)
          { C.txIns = Tx.pubkeyTxIns [dRepRegTxIn]
          , C.txCertificates = Tx.txCertificates era [dRepRegCert] dRepStakeCred
          , C.txOuts = [regDRepTxOut]
          }
    signedRegDRepTx <- Tx.buildTx era localNodeConnectInfo regDRepTxBodyContent w1Address w1SKey
    Tx.submitTx era localNodeConnectInfo signedRegDRepTx
    let expTxIn = Tx.txIn (Tx.txId signedRegDRepTx) 0
    regDRepResultTxOut <-
      Q.getTxOutAtAddress era localNodeConnectInfo w1Address expTxIn "getTxOutAtAddress"
    H.annotate $ show regDRepResultTxOut
    success

registerCommitteeTestInfo staking committee =
  TestInfo
    { testName = "registerCommitteeTest"
    , testDescription = "Register a committee member (for voting)"
    , test = registerCommitteeTest staking committee
    }
registerCommitteeTest
  :: (MonadTest m, MonadIO m)
  => Staking era
  -> Committee era
  -> Either (TN.LocalNodeOptions era) (TN.TestnetOptions era)
  -> TestParams era
  -> m (Maybe String)
registerCommitteeTest
  Staking{..}
  Committee{..}
  networkOptions
  TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
    era <- TN.eraFromOptionsM networkOptions
    (w1SKey, w1Address) <- TN.w1 tempAbsPath networkId

    -- register committee
    committeeRegTxIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address
    let
      committeeRegTxOut = Tx.txOut era (C.lovelaceToValue 2_000_000) w1Address
      committeeRegTxBodyContent =
        (Tx.emptyTxBodyContent era pparams)
          { C.txIns = Tx.pubkeyTxIns [committeeRegTxIn]
          , C.txCertificates = Tx.txCertificates era [committeeHotKeyAuthCert] stakeCred
          , C.txOuts = [committeeRegTxOut]
          }
    signedCommitteeRegTx <-
      Tx.buildTxWithWitnessOverride
        era
        localNodeConnectInfo
        committeeRegTxBodyContent
        w1Address
        (Just 2)
        [C.WitnessPaymentKey w1SKey, C.WitnessCommitteeColdKey committeeColdSKey]
    Tx.submitTx era localNodeConnectInfo signedCommitteeRegTx
    let expTxIn = Tx.txIn (Tx.txId signedCommitteeRegTx) 0
    regDRepResultTxOut <-
      Q.getTxOutAtAddress era localNodeConnectInfo w1Address expTxIn "getTxOutAtAddress"
    H.annotate $ show regDRepResultTxOut
    success

delegateToDRepTestInfo dRep staking =
  TestInfo
    { testName = "delegateToDRepTest"
    , testDescription = "Delegate stake to DRep (for vote delegation)"
    , test = delegateToDRepTest dRep staking
    }
delegateToDRepTest
  :: (MonadTest m, MonadIO m)
  => DRep era
  -> Staking era
  -> Either (TN.LocalNodeOptions era) (TN.TestnetOptions era)
  -> TestParams era
  -> m (Maybe String)
delegateToDRepTest
  DRep{..}
  Staking{..}
  networkOptions
  TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
    era <- TN.eraFromOptionsM networkOptions
    (w1SKey, w1Address) <- TN.w1 tempAbsPath networkId
    let ceo = toConwayEraOnwards era

    stakeDelgTxIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address
    let
      dRepCred = C.DRepCredential $ C.KeyHashObj dRepKeyHash
      dRepDelegatee = C.DelegVote $ C.conwayEraOnwardsConstraints ceo dRepCred
      w1StakeDelgReqs = C.StakeDelegationRequirementsConwayOnwards ceo stakeCred dRepDelegatee
      w1StakeDelgCert = C.makeStakeAddressDelegationCertificate w1StakeDelgReqs

      stakeDelegTxOut = Tx.txOut era (C.lovelaceToValue 2_000_000) w1Address
      stakeDelegTxBodyContent =
        (Tx.emptyTxBodyContent era pparams)
          { C.txIns = Tx.pubkeyTxIns [stakeDelgTxIn]
          , C.txCertificates = Tx.txCertificates era [w1StakeDelgCert] stakeCred
          , C.txOuts = [stakeDelegTxOut]
          }
    signedStakeDelegTx <-
      Tx.buildTxWithWitnessOverride
        era
        localNodeConnectInfo
        stakeDelegTxBodyContent
        w1Address
        (Just 2)
        [C.WitnessPaymentKey w1SKey, C.WitnessStakeKey stakeSKey]
    Tx.submitTx era localNodeConnectInfo signedStakeDelegTx
    let expTxIn = Tx.txIn (Tx.txId signedStakeDelegTx) 0
    stakeDelegResultTxOut <-
      Q.getTxOutAtAddress era localNodeConnectInfo w1Address expTxIn "getTxOutAtAddress"
    H.annotate $ show stakeDelegResultTxOut
    success

constitutionProposalAndVoteTestInfo dRep =
  TestInfo
    { testName = "constitutionProposalAndVoteTest"
    , testDescription = "Propose and vote on new constitution"
    , test = constitutionProposalAndVoteTest dRep
    }
constitutionProposalAndVoteTest
  :: (MonadTest m, MonadIO m)
  => DRep era
  -> Either (TN.LocalNodeOptions era) (TN.TestnetOptions era)
  -> TestParams era
  -> m (Maybe String)
constitutionProposalAndVoteTest
  DRep{..}
  networkOptions
  TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
    era <- TN.eraFromOptionsM networkOptions
    (w1SKey, w1Address) <- TN.w1 tempAbsPath networkId
    (pool1SKey, pool1StakeKeyHash) <- TN.pool1 tempAbsPath
    let sbe = toShelleyBasedEra era
        ceo = toConwayEraOnwards era

    currentEpoch0 <- Q.getCurrentEpoch era localNodeConnectInfo
    H.annotate $ show currentEpoch0 ++ " should be Epoch0"

    -- wait for next epoch to start before proposing governance action
    currentEpoch1 <-
      Q.waitForNextEpoch era localNodeConnectInfo "currentEpoch1"
        =<< Q.getCurrentEpoch era localNodeConnectInfo
    H.annotate $ show currentEpoch1 ++ " should be Epoch1"

    -- check no existing constitution hash
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

    tx1In <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address
    let
      tx1Out1 = Tx.txOut era (C.lovelaceToValue 2_000_000) w1Address
      tx1Out2 = Tx.txOut era (C.lovelaceToValue 3_000_000) w1Address
      proposal =
        C.createProposalProcedure
          sbe
          (C.toShelleyNetwork networkId)
          0 -- govActionDeposit
          pool1StakeKeyHash
          (C.ProposeNewConstitution C.SNothing anchor)
          anchor

      tx1BodyContent =
        (Tx.emptyTxBodyContent era pparams)
          { C.txIns = Tx.pubkeyTxIns [tx1In]
          , C.txProposalProcedures = C.forEraInEonMaybe era (`C.Featured` [proposal])
          , C.txOuts = [tx1Out1, tx1Out2]
          }

    signedTx1 <- Tx.buildTx era localNodeConnectInfo tx1BodyContent w1Address w1SKey
    Tx.submitTx era localNodeConnectInfo signedTx1
    let _tx2In1@(C.TxIn tx2InId1 _tx2InIx1) = Tx.txIn (Tx.txId signedTx1) 0
        _tx2In2 = Tx.txIn (Tx.txId signedTx1) 1
        tx2In3 = Tx.txIn (Tx.txId signedTx1) 2 -- change output
    result1TxOut <-
      Q.getTxOutAtAddress era localNodeConnectInfo w1Address tx2In3 "getTxOutAtAddress"
    H.annotate $ show result1TxOut

    -- vote on the constituion

    let tx2Out1 = Tx.txOut era (C.lovelaceToValue 4_000_000) w1Address
        votingProcedures = Tx.buildVotingProcedures sbe ceo tx2InId1 0 dRepVotingCredential
    let tx2BodyContent =
          (Tx.emptyTxBodyContent era pparams)
            { C.txIns = Tx.pubkeyTxIns [tx2In3]
            , C.txVotingProcedures = C.forEraInEonMaybe era (`C.Featured` votingProcedures)
            , C.txOuts = [tx2Out1]
            }

    let castDrep (C.DRepSigningKey sk) = C.PaymentSigningKey sk

    signedTx2 <-
      Tx.buildTxWithWitnessOverride
        era
        localNodeConnectInfo
        tx2BodyContent
        w1Address
        (Just 3) -- witnesses
        [ C.WitnessPaymentKey w1SKey
        , C.WitnessStakePoolKey pool1SKey
        , C.WitnessPaymentKey (castDrep dRepSKey)
        ]
    Tx.submitTx era localNodeConnectInfo signedTx2
    let result2TxIn = Tx.txIn (Tx.txId signedTx2) 0
    result2TxOut <-
      Q.getTxOutAtAddress era localNodeConnectInfo w1Address result2TxIn "getTxOutAtAddress"
    H.annotate $ show result2TxOut

    currentEpoch1' <- Q.getCurrentEpoch era localNodeConnectInfo
    H.annotate $ show currentEpoch1' ++ " should still be Epoch1"

    -- wait for next epoch before asserting for new constitution
    currentEpoch2 <-
      Q.waitForNextEpoch era localNodeConnectInfo "currentEpoch2"
        =<< Q.getCurrentEpoch era localNodeConnectInfo

    H.annotate $ show currentEpoch2 ++ " should be Epoch2"
    currentEpoch3 <-
      Q.waitForNextEpoch era localNodeConnectInfo "currentEpoch3"
        =<< Q.getCurrentEpoch era localNodeConnectInfo
    H.annotate $ show currentEpoch3 ++ " should be Epoch3"

    -- wait 2 seconds at start of epoch to account for any delay with constitution enactment
    liftIO $ threadDelay 2_000_000

    -- check new constituion is enacted
    newConstitutionHash <- Q.getConstitutionAnchorHashAsString era localNodeConnectInfo
    constituionHash === newConstitutionHash -- debug assertion
    assert "expected constitution hash matches query result" (constituionHash == newConstitutionHash)

committeeProposalAndVoteTestInfo dRep committee =
  TestInfo
    { testName = "committeeProposalAndVoteTest"
    , testDescription = "Propose and vote on new constitutional committee"
    , test = committeeProposalAndVoteTest dRep committee
    }
committeeProposalAndVoteTest
  :: (MonadTest m, MonadIO m)
  => DRep era
  -> Committee era
  -> Either (TN.LocalNodeOptions era) (TN.TestnetOptions era)
  -> TestParams era
  -> m (Maybe String)
committeeProposalAndVoteTest
  DRep{..}
  Committee{..}
  networkOptions
  TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
    era <- TN.eraFromOptionsM networkOptions
    (w1SKey, w1Address) <- TN.w1 tempAbsPath networkId
    (pool1SKey, pool1StakeKeyHash) <- TN.pool1 tempAbsPath
    let sbe = toShelleyBasedEra era
        ceo = toConwayEraOnwards era

    currentEpoch3 <- Q.getCurrentEpoch era localNodeConnectInfo
    H.annotate $ show currentEpoch3 ++ " should be Epoch3"

    -- wait for next epoch to start before proposing governance action
    currentEpoch4 <-
      Q.waitForNextEpoch era localNodeConnectInfo "currentEpoch4"
        =<< Q.getCurrentEpoch era localNodeConnectInfo
    H.annotate $ show currentEpoch4 ++ " should be Epoch4"

    -- build a transaction to propose the new committee

    let anchorUrl = C.textToUrl "https://example.com/committee.txt"
        anchor = C.createAnchor (U.unsafeFromMaybe anchorUrl) "new committee"
    tx1In <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address
    let
      tx1Out1 = Tx.txOut era (C.lovelaceToValue 2_000_000) w1Address
      tx1Out2 = Tx.txOut era (C.lovelaceToValue 3_000_000) w1Address
      prevConstitutionalCommittee = []
      newConstitutionalCommittee = Map.singleton committeeColdKeyHash (currentEpoch4 + 1)
      quorum = 1 % 1
      proposal =
        C.createProposalProcedure
          sbe
          (C.toShelleyNetwork networkId)
          0 -- govActionDeposit
          pool1StakeKeyHash
          (C.ProposeNewCommittee C.SNothing prevConstitutionalCommittee newConstitutionalCommittee quorum)
          anchor

      tx1BodyContent =
        (Tx.emptyTxBodyContent era pparams)
          { C.txIns = Tx.pubkeyTxIns [tx1In]
          , C.txProposalProcedures = C.forEraInEonMaybe era (`C.Featured` [proposal])
          , C.txOuts = [tx1Out1, tx1Out2]
          }

    signedTx1 <- Tx.buildTx era localNodeConnectInfo tx1BodyContent w1Address w1SKey
    Tx.submitTx era localNodeConnectInfo signedTx1
    let _tx2In1@(C.TxIn tx2InId1 _tx2InIx1) = Tx.txIn (Tx.txId signedTx1) 0
        _tx2In2 = Tx.txIn (Tx.txId signedTx1) 1
        tx2In3 = Tx.txIn (Tx.txId signedTx1) 2 -- change output
    result1TxOut <-
      Q.getTxOutAtAddress era localNodeConnectInfo w1Address tx2In3 "getTxOutAtAddress"
    H.annotate $ show result1TxOut

    -- vote on the committee

    let tx2Out1 = Tx.txOut era (C.lovelaceToValue 4_000_000) w1Address
        votingProcedures = Tx.buildVotingProcedures sbe ceo tx2InId1 0 dRepVotingCredential
    let tx2BodyContent =
          (Tx.emptyTxBodyContent era pparams)
            { C.txIns = Tx.pubkeyTxIns [tx2In3]
            , C.txVotingProcedures = C.forEraInEonMaybe era (`C.Featured` votingProcedures)
            , C.txOuts = [tx2Out1]
            }

    let castDrep (C.DRepSigningKey sk) = C.PaymentSigningKey sk

    signedTx2 <-
      Tx.buildTxWithWitnessOverride
        era
        localNodeConnectInfo
        tx2BodyContent
        w1Address
        (Just 3) -- witnesses
        [ C.WitnessPaymentKey w1SKey
        , C.WitnessStakePoolKey pool1SKey
        , C.WitnessPaymentKey (castDrep dRepSKey)
        ]
    Tx.submitTx era localNodeConnectInfo signedTx2
    let result2TxIn = Tx.txIn (Tx.txId signedTx2) 0
    result2TxOut <-
      Q.getTxOutAtAddress era localNodeConnectInfo w1Address result2TxIn "getTxOutAtAddress"
    H.annotate $ show result2TxOut
    success -- TODO: check new committee is enacted
