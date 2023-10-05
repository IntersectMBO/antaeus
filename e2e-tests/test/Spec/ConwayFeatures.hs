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
import Cardano.Ledger.Conway.Governance qualified as L
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString qualified as BS
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
  => Either (TN.LocalNodeOptions era) (TN.TestnetOptions era)
  -> TestParams era
  -> m (Maybe String)
checkTxInfoV3Test networkOptions TestParams{..} = do
  era <- TN.eraFromOptions networkOptions
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

constitutionProposalAndVoteTestInfo =
  TestInfo
    { testName = "constitutionProposalAndVoteTest"
    , testDescription = "Propose and vote on new constitution"
    , test = constitutionProposalAndVoteTest
    }

constitutionProposalAndVoteTest
  :: (MonadTest m, MonadIO m)
  => Either (TN.LocalNodeOptions era) (TN.TestnetOptions era)
  -> TestParams era
  -> m (Maybe String)
constitutionProposalAndVoteTest networkOptions TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
  era <- TN.eraFromOptions networkOptions
  (w1SKey, w1Address) <- TN.w1 tempAbsPath networkId
  (pool1SKey, _pool1VKey, _pool1VKeyHash, pool1StakeKeyHash, _pool1VrfKeyHash) <- TN.pool1 tempAbsPath
  let sbe = toShelleyBasedEra era
      ceo = toConwayEraOnwards era

  -- generate stake keys, address and certificate for w1 (TODO: move to w1All)

  w1StakeSKey <- liftIO $ C.generateSigningKey C.AsStakeKey
  let
    w1StakeVKey = C.getVerificationKey w1StakeSKey
    -- w1ExtendedAddr = makeAddressWithStake (Left w1VKey) (Just w1StakeVKey) networkId
    w1StakeCred = C.StakeCredentialByKey $ C.verificationKeyHash w1StakeVKey
    w1StakeDeposit = C.Lovelace 0 -- protocolParamStakePoolDeposit
    w1StakeReqs = C.StakeAddrRegistrationConway ceo w1StakeDeposit w1StakeCred
    w1StakeRegCert = C.makeStakeAddressRegistrationCertificate w1StakeReqs

  -- submit tx to register w1 stake

  w1StakeRegTxIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address
  let
    w1StakeRegTxOut = Tx.txOut era (C.lovelaceToValue 2_000_000) w1Address
    w1StakeRegTxBodyContent =
      (Tx.emptyTxBodyContent era pparams)
        { C.txIns = Tx.pubkeyTxIns [w1StakeRegTxIn]
        , C.txCertificates = Tx.txCertificates era [w1StakeRegCert] w1StakeCred
        , C.txOuts = [w1StakeRegTxOut]
        }
  signedW1StakeRegTx1 <-
    Tx.buildTxWithWitnessOverride
      era
      localNodeConnectInfo
      w1StakeRegTxBodyContent
      w1Address
      (Just 2)
      [C.WitnessPaymentKey w1SKey, C.WitnessStakeKey w1StakeSKey]
  H.annotate $ show signedW1StakeRegTx1 -- DEBUG
  Tx.submitTx era localNodeConnectInfo signedW1StakeRegTx1
  let dRepRegTxIn = Tx.txIn (Tx.txId signedW1StakeRegTx1) 1 -- change output
  w1StakeRegResultTxOut <-
    Q.getTxOutAtAddress era localNodeConnectInfo w1Address dRepRegTxIn "getTxOutAtAddress"
  H.annotate $ show w1StakeRegResultTxOut

  -- generate DRep keys and produce DRep certificate (TODO: move to helper)
  dRepSkey <- liftIO $ C.generateSigningKey C.AsDRepKey
  let
    dRepVKey = C.getVerificationKey dRepSkey
    C.DRepKeyHash drepKeyHash = C.verificationKeyHash dRepVKey
    drep1StakeCred = C.StakeCredentialByKey . C.StakeKeyHash $ C.coerceKeyRole drepKeyHash
    drep1VotingCredential = U.unsafeFromRight $ C.toVotingCredential ceo drep1StakeCred
    dRepDeposit = C.Lovelace 0 -- dRepDeposit
    dRepRegReqs = C.DRepRegistrationRequirements ceo drep1VotingCredential dRepDeposit
    dRepRegCert = C.makeDrepRegistrationCertificate dRepRegReqs Nothing

  -- submit tx to register a DRep

  let
    regDRepTxOut = Tx.txOut era (C.lovelaceToValue 2_000_000) w1Address
    regDRepTxBodyContent =
      (Tx.emptyTxBodyContent era pparams)
        { C.txIns = Tx.pubkeyTxIns [dRepRegTxIn]
        , C.txCertificates = Tx.txCertificates era [dRepRegCert] drep1StakeCred
        , C.txOuts = [regDRepTxOut]
        }
  signedRegDRepTx <- Tx.buildTx era localNodeConnectInfo regDRepTxBodyContent w1Address w1SKey
  H.annotate $ show signedRegDRepTx -- DEBUG
  Tx.submitTx era localNodeConnectInfo signedRegDRepTx
  let stakeDelgTxIn = Tx.txIn (Tx.txId signedRegDRepTx) 1 -- change output
  regDRepResultTxOut <-
    Q.getTxOutAtAddress era localNodeConnectInfo w1Address stakeDelgTxIn "getTxOutAtAddress"
  H.annotate $ show regDRepResultTxOut

  -- delegate w1 stake to DRep
  let
    dRepCred = C.DRepCredential $ C.KeyHashObj drepKeyHash
    dRepDelegatee = C.DelegVote $ C.conwayEraOnwardsConstraints ceo dRepCred
    w1StakeDelgReqs = C.StakeDelegationRequirementsConwayOnwards ceo w1StakeCred dRepDelegatee
    w1StakeDelgCert = C.makeStakeAddressDelegationCertificate w1StakeDelgReqs

    stakeDelegTxOut = Tx.txOut era (C.lovelaceToValue 2_000_000) w1Address
    stakeDelegTxBodyContent =
      (Tx.emptyTxBodyContent era pparams)
        { C.txIns = Tx.pubkeyTxIns [stakeDelgTxIn]
        , C.txCertificates = Tx.txCertificates era [w1StakeDelgCert] w1StakeCred
        , C.txOuts = [stakeDelegTxOut]
        }
  signedStakeDelegTx <-
    Tx.buildTxWithWitnessOverride
      era
      localNodeConnectInfo
      stakeDelegTxBodyContent
      w1Address
      (Just 2)
      [C.WitnessPaymentKey w1SKey, C.WitnessStakeKey w1StakeSKey]
  H.annotate $ show signedStakeDelegTx -- DEBUG
  Tx.submitTx era localNodeConnectInfo signedStakeDelegTx
  let tx1In = Tx.txIn (Tx.txId signedStakeDelegTx) 1 -- change output
  stakeDelegResultTxOut <-
    Q.getTxOutAtAddress era localNodeConnectInfo w1Address tx1In "getTxOutAtAddress"
  H.annotate $ show stakeDelegResultTxOut

  -- REMOVE
  currentEpoch0 <- Q.getCurrentEpoch era localNodeConnectInfo
  H.annotate $ show currentEpoch0 ++ " should be Epoch0"

  -- wait for next epoch to start before proposing governance action
  currentEpoch1 <-
    Q.waitForNextEpoch era localNodeConnectInfo =<< Q.getCurrentEpoch era localNodeConnectInfo
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

  let
    tx1Out1 = Tx.txOut era (C.lovelaceToValue 2_000_000) w1Address
    tx1Out2 = Tx.txOut era (C.lovelaceToValue 3_000_000) w1Address
    proposal =
      C.createProposalProcedure
        sbe
        (C.toShelleyNetwork networkId)
        0 -- 1_000_000
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
      votingProcedures = C.shelleyBasedEraConstraints sbe $ do
        -- TODO: move producing voting procedure to helper function
        let gAID = C.createGovernanceActionId tx2InId1 0
            voteProcedure = C.createVotingProcedure ceo C.Yes Nothing
            drepVoter = L.DRepVoter (C.unVotingCredential drep1VotingCredential)
        C.singletonVotingProcedures ceo drepVoter gAID (C.unVotingProcedure voteProcedure)
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
      (Just 3)
      [ C.WitnessPaymentKey w1SKey
      , C.WitnessStakePoolKey pool1SKey
      , C.WitnessPaymentKey (castDrep dRepSkey)
      ]
  H.annotate $ show signedTx2 -- DEBUG
  Tx.submitTx era localNodeConnectInfo signedTx2
  let result2TxIn = Tx.txIn (Tx.txId signedTx2) 0
  result2TxOut <-
    Q.getTxOutAtAddress era localNodeConnectInfo w1Address result2TxIn "getTxOutAtAddress"
  H.annotate $ show result2TxOut

  -- REMOVE
  currentEpoch1' <- Q.getCurrentEpoch era localNodeConnectInfo
  H.annotate $ show currentEpoch1' ++ " should still be Epoch1"

  -- wait for next epoch before asserting for new constitution
  currentEpoch2 <-
    Q.waitForNextEpoch era localNodeConnectInfo =<< Q.getCurrentEpoch era localNodeConnectInfo
  H.annotate $ show currentEpoch2 ++ " should be Epoch2"
  currentEpoch3 <-
    Q.waitForNextEpoch era localNodeConnectInfo =<< Q.getCurrentEpoch era localNodeConnectInfo
  H.annotate $ show currentEpoch3 ++ " should be Epoch3"

  -- wait 2 seconds at start of epoch for stability - guarentees constitution is enacted
  liftIO $ threadDelay 2_000_000

  -- check new constituion is enacted
  newConstitutionHash <- Q.getConstitutionAnchorHashAsString era localNodeConnectInfo
  constituionHash === newConstitutionHash -- REMOVE debug assertion
  assert "expected constitution hash matches query result" (constituionHash == newConstitutionHash)

-- REMOVE
-- currentEpoch3' <- Q.getCurrentEpoch era localNodeConnectInfo
-- H.annotate $ show currentEpoch3' ++ " should still be Epoch3"
-- H.failure
