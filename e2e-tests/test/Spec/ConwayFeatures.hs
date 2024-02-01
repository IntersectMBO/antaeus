{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Spec.ConwayFeatures where

import Cardano.Api qualified as C
import Cardano.Api.Ledger qualified as C
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Shelley qualified as C
import Cardano.Crypto.Hash qualified as Crypto
import Cardano.Ledger.BaseTypes qualified as L
import Cardano.Ledger.Conway.PParams qualified as L
import Cardano.Ledger.Core qualified as L
import Control.Concurrent (threadDelay)
import Control.Lens ((.~))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString qualified as BS
import Data.Function ((&))
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Maybe.Strict qualified as StrictMaybe
import Data.Ratio ((%))
import Data.Time.Clock.POSIX qualified as Time
import GHC.Num (Natural)
import Hedgehog qualified as H
import Hedgehog.Extras qualified as H
import Hedgehog.Internal.Property (MonadTest, (===))
import Helpers.Committee (Committee (..))
import Helpers.Committee qualified as CC
import Helpers.Common (showKeyOrScript, toConwayEraOnwards, toShelleyBasedEra)
import Helpers.DRep (DRep (..))
import Helpers.DRep qualified as DRep
import Helpers.Query qualified as Q
import Helpers.StakePool (StakePool (..), makeStakePoolRetireCertification)
import Helpers.Staking (
  Staking (Staking, stakeCred, stakeDelegationPool, stakeRegCert, stakeSKey, stakeUnregCert),
  stakeDelegCert,
 )
import Helpers.Test (assert, success)
import Helpers.TestData (TestInfo (..), TestParams (..))
import Helpers.Testnet qualified as TN
import Helpers.Tx qualified as Tx
import Helpers.Utils qualified as U
import PlutusLedgerApi.V1.Interval qualified as P
import PlutusLedgerApi.V1.Time qualified as P
import PlutusLedgerApi.V2 qualified as PlutusV2
import PlutusScripts.Basic.V_1_0 qualified as PS_1_0
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

-- TODO: finish implementing regression checks
checkTxInfoV3TestInfo =
  TestInfo
    { testName = "checkTxInfoV3Test"
    , testDescription =
        "Check each attribute of the TxInfo from the V3 ScriptContext in a single transaction"
    , test = checkTxInfoV3Test
    }

checkTxInfoV3Test
  :: (MonadIO m, MonadTest m)
  => TN.TestEnvironmentOptions era
  -> TestParams era
  -> m (Maybe String)
checkTxInfoV3Test networkOptions TestParams{..} = do
  era <- TN.eraFromOptionsM networkOptions
  startTime <- liftIO Time.getPOSIXTime
  (w1SKey, _w1VKey, w1VKeyHash, w1Address) <- TN.w1All networkOptions tempAbsPath networkId
  let sbe = toShelleyBasedEra era

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

      expTxInfoInputs = PS.txInfoInputs era (txIn, txInAsTxOut)
      expTxInfoReferenceInputs = PS.txInfoInputs era (txIn, txInAsTxOut)
      expTxInfoOutputs = PS.txInfoOutputs era [txOut1, txOut2]
      expTxInfoFee = PS.txInfoFee fee
      expTxInfoMint = PS.txInfoMint tokenValues
      expDCert = [] -- not testing any staking registration certificate
      expWdrl = PlutusV2.fromList [] -- not testing any staking reward withdrawal
      expTxInfoSigs = PS.txInfoSigs [w1VKeyHash]
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
            C.txExtraKeyWits = Tx.txExtraKeyWits era [w1VKeyHash]
          }
  txbody <- Tx.buildRawTx sbe txBodyContent
  kw <- Tx.signTx sbe txbody w1SKey
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

registerStakePoolTestInfo stakePool =
  TestInfo
    { testName = "registerStakePoolTest"
    , testDescription = "Register a stake pool (for voting)"
    , test = registerStakePoolTest stakePool
    }
registerStakePoolTest
  :: (MonadTest m, MonadIO m)
  => StakePool era
  -> TN.TestEnvironmentOptions era
  -> TestParams era
  -> m (Maybe String)
registerStakePoolTest
  StakePool{..}
  networkOptions
  TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
    era <- TN.eraFromOptionsM networkOptions
    (w1SKey, w1Address) <- TN.w1 networkOptions tempAbsPath networkId
    let sbe = toShelleyBasedEra era

    sPRegTxIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address
    let
      regSPTxOut = Tx.txOut era (C.lovelaceToValue 2_000_000) w1Address
      regSPTxBodyContent =
        (Tx.emptyTxBodyContent sbe pparams)
          { C.txIns = Tx.pubkeyTxIns [sPRegTxIn]
          , C.txCertificates = Tx.txCertificates era [sPRegCert] [sPStakeCred]
          , C.txOuts = [regSPTxOut]
          }
    signedRegSPTx <-
      Tx.buildTxWithWitnessOverride
        era
        localNodeConnectInfo
        regSPTxBodyContent
        w1Address
        (Just 3)
        [w1SKey, C.WitnessStakePoolKey sPSKey, C.WitnessStakeKey sPRewardKey]
    Tx.submitTx sbe localNodeConnectInfo signedRegSPTx
    let expTxIn = Tx.txIn (Tx.txId signedRegSPTx) 0
    regDRepResultTxOut <-
      Q.getTxOutAtAddress era localNodeConnectInfo w1Address expTxIn "getTxOutAtAddress"
    H.annotate $ show regDRepResultTxOut
    success

registerStakingTestInfo staking =
  TestInfo
    { testName = "registerStakingTest"
    , testDescription = "Register a stake address (for vote delegation)"
    , test = registerStakingTest staking
    }
registerStakingTest
  :: (MonadTest m, MonadIO m)
  => Staking era
  -> TN.TestEnvironmentOptions era
  -> TestParams era
  -> m (Maybe String)
registerStakingTest
  Staking{..}
  networkOptions
  TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
    era <- TN.eraFromOptionsM networkOptions
    (w1SKey, w1Address) <- TN.w1 networkOptions tempAbsPath networkId
    let sbe = toShelleyBasedEra era

    w1StakeRegTxIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address
    let
      adaValue = C.lovelaceToValue 2_000_000
      w1StakeRegTxOut = Tx.txOut era adaValue w1Address
      w1StakeRegTxBodyContent =
        (Tx.emptyTxBodyContent sbe pparams)
          { C.txIns = Tx.pubkeyTxIns [w1StakeRegTxIn]
          , C.txCertificates = Tx.txCertificates era [stakeRegCert] [stakeCred]
          , C.txOuts = [w1StakeRegTxOut]
          }
    signedW1StakeRegTx1 <-
      Tx.buildTxWithWitnessOverride
        era
        localNodeConnectInfo
        w1StakeRegTxBodyContent
        w1Address
        (Just 2) -- witnesses
        [w1SKey, C.WitnessStakeKey stakeSKey]
    Tx.submitTx sbe localNodeConnectInfo signedW1StakeRegTx1
    let expTxIn = Tx.txIn (Tx.txId signedW1StakeRegTx1) 1 -- change output
    w1StakeRegResultTxOut <-
      Q.getTxOutAtAddress era localNodeConnectInfo w1Address expTxIn "getTxOutAtAddress"
    H.annotate $ show w1StakeRegResultTxOut
    success

registerDRepTestInfo :: DRep era -> TestInfo era
registerDRepTestInfo dRep =
  TestInfo
    { testName = "registerDRepTest (" ++ showKeyOrScript dRep ++ ")"
    , testDescription = "Register a " ++ showKeyOrScript dRep ++ " DRep (for voting)"
    , test = registerDRepTest dRep
    }
registerDRepTest
  :: (MonadTest m, MonadIO m)
  => DRep era
  -> TN.TestEnvironmentOptions era
  -> TestParams era
  -> m (Maybe String)
registerDRepTest KeyDRep{kDRepRegCert = c} = registerDRep c
registerDRepTest ScriptDRep{sDRepRegCert = c} = registerDRep c
registerDRep
  :: (MonadTest m, MonadIO m)
  => C.Certificate era
  -> TN.TestEnvironmentOptions era
  -> TestParams era
  -> m (Maybe String)
registerDRep dRepRegCert networkOptions TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
  era <- TN.eraFromOptionsM networkOptions
  (w1SKey, w1Address) <- TN.w1 networkOptions tempAbsPath networkId
  let sbe = toShelleyBasedEra era

  dRepRegTxIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address
  let
    regDRepTxOut = Tx.txOut era (C.lovelaceToValue 2_000_000) w1Address
    -- TODO: add DRep script witness (once ledger supports it)
    regDRepTxBodyContent =
      (Tx.emptyTxBodyContent sbe pparams)
        { C.txIns = Tx.pubkeyTxIns [dRepRegTxIn]
        , C.txCertificates = Tx.txCertificates era [dRepRegCert] []
        , C.txOuts = [regDRepTxOut]
        }
  -- TODO: add DRep key witness (if KeyDRep)
  signedRegDRepTx <- Tx.buildTx era localNodeConnectInfo regDRepTxBodyContent w1Address w1SKey
  Tx.submitTx sbe localNodeConnectInfo signedRegDRepTx
  let expTxIn = Tx.txIn (Tx.txId signedRegDRepTx) 0
  regDRepResultTxOut <-
    Q.getTxOutAtAddress era localNodeConnectInfo w1Address expTxIn "getTxOutAtAddress"
  H.annotate $ show regDRepResultTxOut
  success

registerCommitteeTestInfo committee =
  TestInfo
    { testName = "registerCommitteeTest"
    , testDescription = "Register a committee member (for voting)"
    , test = registerCommitteeTest committee
    }
registerCommitteeTest
  :: (MonadTest m, MonadIO m)
  => Committee era
  -> TN.TestEnvironmentOptions era
  -> TestParams era
  -> m (Maybe String)
registerCommitteeTest
  Committee{..}
  networkOptions
  TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
    era <- TN.eraFromOptionsM networkOptions
    (w1SKey, w1Address) <- TN.w1 networkOptions tempAbsPath networkId
    let sbe = toShelleyBasedEra era

    -- register committee
    committeeRegTxIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address
    let
      committeeRegTxOut = Tx.txOut era (C.lovelaceToValue 2_000_000) w1Address
      committeeRegTxBodyContent =
        (Tx.emptyTxBodyContent sbe pparams)
          { C.txIns = Tx.pubkeyTxIns [committeeRegTxIn]
          , C.txCertificates = Tx.txCertificates era [committeeHotKeyAuthCert] []
          , C.txOuts = [committeeRegTxOut]
          }
    signedCommitteeRegTx <-
      Tx.buildTxWithWitnessOverride
        era
        localNodeConnectInfo
        committeeRegTxBodyContent
        w1Address
        (Just 2)
        [w1SKey, C.WitnessCommitteeColdKey committeeColdSKey]
    Tx.submitTx sbe localNodeConnectInfo signedCommitteeRegTx
    let expTxIn = Tx.txIn (Tx.txId signedCommitteeRegTx) 0
    regDRepResultTxOut <-
      Q.getTxOutAtAddress era localNodeConnectInfo w1Address expTxIn "getTxOutAtAddress"
    H.annotate $ show regDRepResultTxOut
    success

delegateToDRepTestInfo keyDRep staking =
  TestInfo
    { testName = "delegateToDRepTest (" ++ showKeyOrScript keyDRep ++ ")"
    , testDescription = "Delegate stake to " ++ showKeyOrScript keyDRep ++ " DRep (for vote delegation)"
    , test = delegateToDRepTest keyDRep staking
    }
delegateToDRepTest
  :: (MonadTest m, MonadIO m)
  => DRep era
  -> Staking era
  -> TN.TestEnvironmentOptions era
  -> TestParams era
  -> m (Maybe String)
delegateToDRepTest KeyDRep{kDRepLedgerCred = c} = delegateToDRep c
delegateToDRepTest ScriptDRep{sDRepLedgerCred = c} = delegateToDRep c
delegateToDRep
  dRepLedgerCred
  Staking{..}
  networkOptions
  TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
    era <- TN.eraFromOptionsM networkOptions
    (w1SKey, w1Address) <- TN.w1 networkOptions tempAbsPath networkId
    let sbe = toShelleyBasedEra era

    stakeDelgTxIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address
    let
      voteDelgCert = DRep.voteDelegateCert (toConwayEraOnwards era) dRepLedgerCred stakeCred

      stakeDelegTxOut = Tx.txOut era (C.lovelaceToValue 2_000_000) w1Address
      stakeDelegTxBodyContent =
        (Tx.emptyTxBodyContent sbe pparams)
          { C.txIns = Tx.pubkeyTxIns [stakeDelgTxIn]
          , C.txCertificates = Tx.txCertificates era [voteDelgCert] [stakeCred]
          , C.txOuts = [stakeDelegTxOut]
          }
    signedStakeDelegTx <-
      Tx.buildTxWithWitnessOverride
        era
        localNodeConnectInfo
        stakeDelegTxBodyContent
        w1Address
        (Just 2)
        [w1SKey, C.WitnessStakeKey stakeSKey]
    Tx.submitTx sbe localNodeConnectInfo signedStakeDelegTx
    let expTxIn = Tx.txIn (Tx.txId signedStakeDelegTx) 0
    stakeDelegResultTxOut <-
      Q.getTxOutAtAddress era localNodeConnectInfo w1Address expTxIn "getTxOutAtAddress"
    H.annotate $ show stakeDelegResultTxOut
    success

delegateToStakePoolTestInfo staking =
  TestInfo
    { testName = "delegateToStakePoolTest"
    , testDescription = "Delegate stake to SPO (for staking)"
    , test = delegateToStakePoolTest staking
    }
delegateToStakePoolTest
  :: (MonadTest m, MonadIO m)
  => Staking era
  -> TN.TestEnvironmentOptions era
  -> TestParams era
  -> m (Maybe String)
delegateToStakePoolTest
  Staking{..}
  networkOptions
  TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
    era <- TN.eraFromOptionsM networkOptions
    (w1SKey, w1Address) <- TN.w1 networkOptions tempAbsPath networkId
    let ceo = toConwayEraOnwards era
        sbe = toShelleyBasedEra era

    stakeDelgTxIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address
    let
      w1StakeDelgCert = stakeDelegCert ceo (sPLedgerKeyHash stakeDelegationPool) stakeCred

      stakeDelegTxOut = Tx.txOut era (C.lovelaceToValue 2_000_000) w1Address
      stakeDelegTxBodyContent =
        (Tx.emptyTxBodyContent sbe pparams)
          { C.txIns = Tx.pubkeyTxIns [stakeDelgTxIn]
          , C.txCertificates = Tx.txCertificates era [w1StakeDelgCert] [stakeCred]
          , C.txOuts = [stakeDelegTxOut]
          }
    signedStakeDelegTx <-
      Tx.buildTxWithWitnessOverride
        era
        localNodeConnectInfo
        stakeDelegTxBodyContent
        w1Address
        (Just 2)
        [w1SKey, C.WitnessStakeKey stakeSKey]
    Tx.submitTx sbe localNodeConnectInfo signedStakeDelegTx
    let expTxIn = Tx.txIn (Tx.txId signedStakeDelegTx) 0
    stakeDelegResultTxOut <-
      Q.getTxOutAtAddress era localNodeConnectInfo w1Address expTxIn "getTxOutAtAddress"
    H.annotate $ show stakeDelegResultTxOut
    success

constitutionProposalAndVoteTestInfo committee kDRep sDRep staking =
  TestInfo
    { testName = "constitutionProposalAndVoteTest"
    , testDescription = "Propose and vote on new constitution"
    , test = constitutionProposalAndVoteTest committee kDRep sDRep staking
    }
constitutionProposalAndVoteTest
  :: (MonadTest m, MonadIO m)
  => Committee era
  -> DRep era
  -> DRep era
  -> Staking era
  -> TN.TestEnvironmentOptions era
  -> TestParams era
  -> m (Maybe String)
constitutionProposalAndVoteTest
  Committee{..}
  KeyDRep{..}
  ScriptDRep{} -- will be used
  Staking{..}
  networkOptions
  TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
    era <- TN.eraFromOptionsM networkOptions
    (w1SKey, w1Address) <- TN.w1 networkOptions tempAbsPath networkId
    let sbe = toShelleyBasedEra era
        ceo = toConwayEraOnwards era

    currentEpoch1 <- Q.getCurrentEpoch era localNodeConnectInfo
    H.annotate $ show currentEpoch1

    -- wait for next epoch to start before proposing governance action
    currentEpoch2 <-
      Q.waitForNextEpoch era localNodeConnectInfo "currentEpoch2"
        =<< Q.getCurrentEpoch era localNodeConnectInfo
    H.annotate $ show currentEpoch2

    -- check no existing constitution hash
    existingConstitutionHash <- Q.getConstitutionAnchorHashAsString era localNodeConnectInfo
    existingConstitutionHash === "\"0000000000000000000000000000000000000000000000000000000000000000\""

    -- define a new constituion
    let constitutionPath = tempAbsPath <> "/constituion.txt"
    H.writeFile constitutionPath "a new way of life"
    constituionBS <- H.evalIO $ BS.readFile constitutionPath
    -- TODO: add constitution script (proposal policy) once implemented in cardano-api
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
          (sPStakeKeyHash stakeDelegationPool)
          (C.ProposeNewConstitution C.SNothing anchor)
          anchor

      {-- TODO: check proposal onchain with minting policy (once ledger supports it)
        plutusProposalProcedure = fromCardanoProposal sbe proposal
        -- proposalProcedureItems' = C.fromProposalProcedure sbe proposal
        -- plutusProposalProcedure' = fromCardanoProposal proposalProcedureItems'
        tokenValues = C.valueFromList [(PS_1_1.verifyProposalProceduresAssetIdV3, 1)]
        mintWitness = Map.fromList [PS_1_1.verifyProposalProceduresMintWitnessV3 sbe [plutusProposalProcedure]]
        --}

      tx1BodyContent =
        (Tx.emptyTxBodyContent sbe pparams)
          { C.txIns = Tx.pubkeyTxIns [tx1In]
          , C.txProposalProcedures = C.forEraInEonMaybe era (`C.Featured` [proposal])
          , C.txOuts = [tx1Out1, tx1Out2]
          }

    signedTx1 <- Tx.buildTx era localNodeConnectInfo tx1BodyContent w1Address w1SKey
    Tx.submitTx sbe localNodeConnectInfo signedTx1
    let _tx2In1@(C.TxIn tx2InId1 _tx2InIx1) = Tx.txIn (Tx.txId signedTx1) 0
        _tx2In2 = Tx.txIn (Tx.txId signedTx1) 1
        tx2In3 = Tx.txIn (Tx.txId signedTx1) 2 -- change output
    result1TxOut <-
      Q.getTxOutAtAddress era localNodeConnectInfo w1Address tx2In3 "getTxOutAtAddress"
    H.annotate $ show result1TxOut

    -- vote on the constituion

    let tx2Out1 = Tx.txOut era (C.lovelaceToValue 4_000_000) w1Address
        -- NOTE : sDRepVoter uses alwaysSucceed
        -- tokenValues = C.valueFromList [(PS_1_0.alwaysSucceedAssetIdV2, 10)]
        -- mintWitness = Map.fromList [PS_1_0.alwaysSucceedMintWitnessV2 era Nothing]
        -- collateral = Tx.txInsCollateral era [tx2In3]
        -- SPO not allowed to vote on constitution
        votes = [(committeeVoter, C.Yes), (kDRepVoter, C.Yes)] -- , (sDRepVoter, C.Yes)]
        -- TODO: build votes in plutus format as redeemer
        votingProcedures = Tx.buildVotingProcedures sbe ceo tx2InId1 0 votes
    let tx2BodyContent =
          (Tx.emptyTxBodyContent sbe pparams)
            { C.txIns = Tx.pubkeyTxIns [tx2In3]
            , -- , C.txInsCollateral = collateral
              -- , C.txMintValue = Tx.txMintValue era tokenValues mintWitness
              C.txVotingProcedures = C.forEraInEonMaybe era (`C.Featured` votingProcedures)
            , C.txOuts = [tx2Out1]
            }

    signedTx2 <-
      Tx.buildTxWithWitnessOverride
        era
        localNodeConnectInfo
        tx2BodyContent
        w1Address
        (Just 3) -- witnesses
        [w1SKey, DRep.castDRep kDRepSKey, CC.castCommittee committeeHotSKey]
    Tx.submitTx sbe localNodeConnectInfo signedTx2
    let result2TxIn = Tx.txIn (Tx.txId signedTx2) 0
    result2TxOut <-
      Q.getTxOutAtAddress era localNodeConnectInfo w1Address result2TxIn "getTxOutAtAddress"
    H.annotate $ show result2TxOut

    -- wait for next epoch before asserting for new constitution
    currentEpoch3 <-
      Q.waitForNextEpoch era localNodeConnectInfo "currentEpoch3"
        =<< Q.getCurrentEpoch era localNodeConnectInfo
    H.annotate $ show currentEpoch3

    currentEpoch4 <-
      Q.waitForNextEpoch era localNodeConnectInfo "currentEpoch4"
        =<< Q.getCurrentEpoch era localNodeConnectInfo
    H.annotate $ show currentEpoch4

    -- wait 2 seconds at start of epoch to account for any delay with constitution enactment
    liftIO $ threadDelay 2_000_000

    -- check new constituion is enacted
    newConstitutionHash <- Q.getConstitutionAnchorHashAsString era localNodeConnectInfo
    constituionHash === newConstitutionHash -- debug assertion
    assert "expected constitution hash matches query result" (constituionHash == newConstitutionHash)
constitutionProposalAndVoteTest _ ScriptDRep{} _ _ _ _ = error "ScriptDRep in wrong arg"
constitutionProposalAndVoteTest _ _ KeyDRep{} _ _ _ = error "KeyDRep in wrong arg"

committeeProposalAndVoteTestInfo committee dRep staking =
  TestInfo
    { testName = "committeeProposalAndVoteTest"
    , testDescription = "Propose and vote on new constitutional committee"
    , test = committeeProposalAndVoteTest committee dRep staking
    }
committeeProposalAndVoteTest
  :: (MonadTest m, MonadIO m)
  => Committee era
  -> DRep era
  -> Staking era
  -> TN.TestEnvironmentOptions era
  -> TestParams era
  -> m (Maybe String)
committeeProposalAndVoteTest
  Committee{..}
  KeyDRep{..}
  Staking{..}
  networkOptions
  TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
    era <- TN.eraFromOptionsM networkOptions
    (w1SKey, w1Address) <- TN.w1 networkOptions tempAbsPath networkId
    let sbe = toShelleyBasedEra era
        ceo = toConwayEraOnwards era

    currentEpoch1 <- Q.getCurrentEpoch era localNodeConnectInfo
    H.annotate $ show currentEpoch1

    -- wait for next epoch to start before proposing governance action
    currentEpoch2 <-
      Q.waitForNextEpoch era localNodeConnectInfo "currentEpoch2"
        =<< Q.getCurrentEpoch era localNodeConnectInfo
    H.annotate $ show currentEpoch2

    -- build a transaction to propose the new committee

    let anchorUrl = C.textToUrl "https://example.com/committee.txt"
        anchor = C.createAnchor (U.unsafeFromMaybe anchorUrl) "new committee"
    tx1In <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address
    let
      tx1Out1 = Tx.txOut era (C.lovelaceToValue 2_000_000) w1Address
      tx1Out2 = Tx.txOut era (C.lovelaceToValue 3_000_000) w1Address
      prevConstitutionalCommittee = []
      newConstitutionalCommittee = Map.singleton committeeColdKeyHash (currentEpoch2 + 1)
      quorum = 1 % 1
      proposal =
        C.createProposalProcedure
          sbe
          (C.toShelleyNetwork networkId)
          0 -- govActionDeposit
          (sPStakeKeyHash stakeDelegationPool)
          (C.ProposeNewCommittee C.SNothing prevConstitutionalCommittee newConstitutionalCommittee quorum)
          anchor

      tx1BodyContent =
        (Tx.emptyTxBodyContent sbe pparams)
          { C.txIns = Tx.pubkeyTxIns [tx1In]
          , C.txProposalProcedures = C.forEraInEonMaybe era (`C.Featured` [proposal])
          , C.txOuts = [tx1Out1, tx1Out2]
          }

    signedTx1 <- Tx.buildTx era localNodeConnectInfo tx1BodyContent w1Address w1SKey
    Tx.submitTx sbe localNodeConnectInfo signedTx1
    let _tx2In1@(C.TxIn tx2InId1 _tx2InIx1) = Tx.txIn (Tx.txId signedTx1) 0
        _tx2In2 = Tx.txIn (Tx.txId signedTx1) 1
        tx2In3 = Tx.txIn (Tx.txId signedTx1) 2 -- change output
    result1TxOut <-
      Q.getTxOutAtAddress era localNodeConnectInfo w1Address tx2In3 "getTxOutAtAddress"
    H.annotate $ show result1TxOut

    -- vote on the committee

    let tx2Out1 = Tx.txOut era (C.lovelaceToValue 4_000_000) w1Address
        -- committee member not allowed to vote on committee update
        votes = [(kDRepVoter, C.Yes), (sPVoter stakeDelegationPool, C.Yes)]
        votingProcedures = Tx.buildVotingProcedures sbe ceo tx2InId1 0 votes
    let tx2BodyContent =
          (Tx.emptyTxBodyContent sbe pparams)
            { C.txIns = Tx.pubkeyTxIns [tx2In3]
            , C.txVotingProcedures = C.forEraInEonMaybe era (`C.Featured` votingProcedures)
            , C.txOuts = [tx2Out1]
            }

    signedTx2 <-
      Tx.buildTxWithWitnessOverride
        era
        localNodeConnectInfo
        tx2BodyContent
        w1Address
        (Just 3) -- witnesses
        [w1SKey, C.WitnessStakePoolKey (sPSKey stakeDelegationPool), DRep.castDRep kDRepSKey]
    Tx.submitTx sbe localNodeConnectInfo signedTx2
    let result2TxIn = Tx.txIn (Tx.txId signedTx2) 0
    result2TxOut <-
      Q.getTxOutAtAddress era localNodeConnectInfo w1Address result2TxIn "getTxOutAtAddress"
    H.annotate $ show result2TxOut
    success -- TODO: check new committee is enacted (will influence future voting)
committeeProposalAndVoteTest _ ScriptDRep{} _ _ _ = error "committeeProposalAndVoteTest: ScriptDRep not supported"

noConfidenceProposalAndVoteTestInfo dRep staking =
  TestInfo
    { testName = "noConfidenceProposalAndVoteTest"
    , testDescription = "Propose and vote on a motion of no-confidence"
    , test = noConfidenceProposalAndVoteTest dRep staking
    }
noConfidenceProposalAndVoteTest
  :: (MonadTest m, MonadIO m)
  => DRep era
  -> Staking era
  -> TN.TestEnvironmentOptions era
  -> TestParams era
  -> m (Maybe String)
noConfidenceProposalAndVoteTest
  KeyDRep{..}
  Staking{..}
  networkOptions
  TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
    era <- TN.eraFromOptionsM networkOptions
    (w1SKey, w1Address) <- TN.w1 networkOptions tempAbsPath networkId
    let sbe = toShelleyBasedEra era
        ceo = toConwayEraOnwards era

    currentEpoch1 <- Q.getCurrentEpoch era localNodeConnectInfo
    H.annotate $ show currentEpoch1

    -- wait for next epoch to start before proposing governance action
    currentEpoch2 <-
      Q.waitForNextEpoch era localNodeConnectInfo "currentEpoch2"
        =<< Q.getCurrentEpoch era localNodeConnectInfo
    H.annotate $ show currentEpoch2

    -- build a transaction to propose the motion of no-confidence

    let anchorUrl = C.textToUrl "https://example.com/no_confidence.txt"
        anchor = C.createAnchor (U.unsafeFromMaybe anchorUrl) "motion of no confidence"
    tx1In <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address
    let
      tx1Out1 = Tx.txOut era (C.lovelaceToValue 2_000_000) w1Address
      tx1Out2 = Tx.txOut era (C.lovelaceToValue 3_000_000) w1Address
      proposal =
        C.createProposalProcedure
          sbe
          (C.toShelleyNetwork networkId)
          0 -- govActionDeposit
          (sPStakeKeyHash stakeDelegationPool)
          (C.MotionOfNoConfidence C.SNothing)
          anchor

      tx1BodyContent =
        (Tx.emptyTxBodyContent sbe pparams)
          { C.txIns = Tx.pubkeyTxIns [tx1In]
          , C.txProposalProcedures = C.forEraInEonMaybe era (`C.Featured` [proposal])
          , C.txOuts = [tx1Out1, tx1Out2]
          }

    signedTx1 <- Tx.buildTx era localNodeConnectInfo tx1BodyContent w1Address w1SKey
    Tx.submitTx sbe localNodeConnectInfo signedTx1
    let _tx2In1@(C.TxIn tx2InId1 _tx2InIx1) = Tx.txIn (Tx.txId signedTx1) 0
        _tx2In2 = Tx.txIn (Tx.txId signedTx1) 1
        tx2In3 = Tx.txIn (Tx.txId signedTx1) 2 -- change output
    result1TxOut <-
      Q.getTxOutAtAddress era localNodeConnectInfo w1Address tx2In3 "getTxOutAtAddress"
    H.annotate $ show result1TxOut

    -- vote on the motion of no-confidence

    let tx2Out1 = Tx.txOut era (C.lovelaceToValue 4_000_000) w1Address
        -- committee not allowed to vote on motion of no-confidence
        votes = [(kDRepVoter, C.Yes), (sPVoter stakeDelegationPool, C.Yes)]
        votingProcedures = Tx.buildVotingProcedures sbe ceo tx2InId1 0 votes
    let tx2BodyContent =
          (Tx.emptyTxBodyContent sbe pparams)
            { C.txIns = Tx.pubkeyTxIns [tx2In3]
            , C.txVotingProcedures = C.forEraInEonMaybe era (`C.Featured` votingProcedures)
            , C.txOuts = [tx2Out1]
            }

    signedTx2 <-
      Tx.buildTxWithWitnessOverride
        era
        localNodeConnectInfo
        tx2BodyContent
        w1Address
        (Just 3) -- witnesses
        [w1SKey, C.WitnessStakePoolKey (sPSKey stakeDelegationPool), DRep.castDRep kDRepSKey]
    Tx.submitTx sbe localNodeConnectInfo signedTx2
    let result2TxIn = Tx.txIn (Tx.txId signedTx2) 0
    result2TxOut <-
      Q.getTxOutAtAddress era localNodeConnectInfo w1Address result2TxIn "getTxOutAtAddress"
    H.annotate $ show result2TxOut
    success -- TODO: check motion of no-confidence is enacted
noConfidenceProposalAndVoteTest ScriptDRep{} _ _ _ =
  error "noConfidenceProposalAndVoteTest: ScriptDRep not yet supported"

parameterChangeProposalAndVoteTestInfo committee dRep staking =
  TestInfo
    { testName = "parameterChangeProposalAndVoteTest"
    , testDescription = "Propose and vote on a change to the protocol parameters"
    , test = parameterChangeProposalAndVoteTest committee dRep staking
    }
parameterChangeProposalAndVoteTest
  :: (MonadTest m, MonadIO m, L.ConwayEraPParams (C.ShelleyLedgerEra era))
  => Committee era
  -> DRep era
  -> Staking era
  -> TN.TestEnvironmentOptions era
  -> TestParams era
  -> m (Maybe String)
parameterChangeProposalAndVoteTest
  Committee{..}
  KeyDRep{..}
  Staking{..}
  networkOptions
  TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
    era <- TN.eraFromOptionsM networkOptions
    (w1SKey, w1Address) <- TN.w1 networkOptions tempAbsPath networkId
    let sbe = toShelleyBasedEra era
        ceo = toConwayEraOnwards era

    currentEpoch1 <- Q.getCurrentEpoch era localNodeConnectInfo
    H.annotate $ show currentEpoch1

    -- wait for next epoch to start before proposing governance action
    currentEpoch2 <-
      Q.waitForNextEpoch era localNodeConnectInfo "currentEpoch2"
        =<< Q.getCurrentEpoch era localNodeConnectInfo
    H.annotate $ show currentEpoch2

    -- build a transaction to propose a change to the protocol parameters

    let anchorUrl = C.textToUrl "https://example.com/pparameters.txt"
        anchor = C.createAnchor (U.unsafeFromMaybe anchorUrl) "protocol parameters"
    tx1In <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address
    let
      tx1Out1 = Tx.txOut era (C.lovelaceToValue 2_000_000) w1Address
      tx1Out2 = Tx.txOut era (C.lovelaceToValue 3_000_000) w1Address
      -- protocol parameter update to change the committee minimum size to 1
      pparamsUpdate = L.emptyPParamsUpdate & L.ppuCommitteeMinSizeL .~ StrictMaybe.SJust (1 :: Natural)
      proposal =
        C.createProposalProcedure
          sbe
          (C.toShelleyNetwork networkId)
          0 -- govActionDeposit
          (sPStakeKeyHash stakeDelegationPool)
          (C.UpdatePParams C.SNothing pparamsUpdate)
          anchor

      tx1BodyContent =
        (Tx.emptyTxBodyContent sbe pparams)
          { C.txIns = Tx.pubkeyTxIns [tx1In]
          , C.txProposalProcedures = C.forEraInEonMaybe era (`C.Featured` [proposal])
          , C.txOuts = [tx1Out1, tx1Out2]
          }

    signedTx1 <- Tx.buildTx era localNodeConnectInfo tx1BodyContent w1Address w1SKey
    Tx.submitTx sbe localNodeConnectInfo signedTx1
    let _tx2In1@(C.TxIn tx2InId1 _tx2InIx1) = Tx.txIn (Tx.txId signedTx1) 0
        _tx2In2 = Tx.txIn (Tx.txId signedTx1) 1
        tx2In3 = Tx.txIn (Tx.txId signedTx1) 2 -- change output
    result1TxOut <-
      Q.getTxOutAtAddress era localNodeConnectInfo w1Address tx2In3 "getTxOutAtAddress"
    H.annotate $ show result1TxOut

    -- vote on the updated protocol parameters

    let tx2Out1 = Tx.txOut era (C.lovelaceToValue 4_000_000) w1Address
        votes = [(committeeVoter, C.Yes), (kDRepVoter, C.Yes)] -- SPO not allowed to vote on protocol parameters update
        votingProcedures = Tx.buildVotingProcedures sbe ceo tx2InId1 0 votes
    let tx2BodyContent =
          (Tx.emptyTxBodyContent sbe pparams)
            { C.txIns = Tx.pubkeyTxIns [tx2In3]
            , C.txVotingProcedures = C.forEraInEonMaybe era (`C.Featured` votingProcedures)
            , C.txOuts = [tx2Out1]
            }

    signedTx2 <-
      Tx.buildTxWithWitnessOverride
        era
        localNodeConnectInfo
        tx2BodyContent
        w1Address
        (Just 3) -- witnesses
        [w1SKey, (DRep.castDRep kDRepSKey), (CC.castCommittee committeeHotSKey)]
    Tx.submitTx sbe localNodeConnectInfo signedTx2
    let result2TxIn = Tx.txIn (Tx.txId signedTx2) 0
    result2TxOut <-
      Q.getTxOutAtAddress era localNodeConnectInfo w1Address result2TxIn "getTxOutAtAddress"
    H.annotate $ show result2TxOut
    success -- TODO: check protocol parameter update is enacted
parameterChangeProposalAndVoteTest _ ScriptDRep{} _ _ _ =
  error "parameterChangeProposalAndVoteTest: ScriptDRep not yet supported"

treasuryWithdrawalProposalAndVoteTestInfo committee dRep staking =
  TestInfo
    { testName = "treasuryWithdrawalProposalAndVoteTest"
    , testDescription = "Propose and vote on a treasury withdrawal"
    , test = treasuryWithdrawalProposalAndVoteTest committee dRep staking
    }
treasuryWithdrawalProposalAndVoteTest
  :: (MonadTest m, MonadIO m)
  => Committee era
  -> DRep era
  -> Staking era
  -> TN.TestEnvironmentOptions era
  -> TestParams era
  -> m (Maybe String)
treasuryWithdrawalProposalAndVoteTest
  Committee{..}
  KeyDRep{..}
  Staking{..}
  networkOptions
  TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
    era <- TN.eraFromOptionsM networkOptions
    (w1SKey, w1Address) <- TN.w1 networkOptions tempAbsPath networkId
    let sbe = toShelleyBasedEra era
        ceo = toConwayEraOnwards era

    currentEpoch1 <- Q.getCurrentEpoch era localNodeConnectInfo
    H.annotate $ show currentEpoch1

    -- wait for next epoch to start before proposing governance action
    currentEpoch2 <-
      Q.waitForNextEpoch era localNodeConnectInfo "currentEpoch2"
        =<< Q.getCurrentEpoch era localNodeConnectInfo
    H.annotate $ show currentEpoch2

    -- build a transaction to propose a treasury withdrawal

    let anchorUrl = C.textToUrl "https://example.com/treasury_withdrawal.txt"
        anchor = C.createAnchor (U.unsafeFromMaybe anchorUrl) "treasury withdrawal"
    tx1In <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address
    let
      tx1Out1 = Tx.txOut era (C.lovelaceToValue 2_000_000) w1Address
      tx1Out2 = Tx.txOut era (C.lovelaceToValue 3_000_000) w1Address
      tWithdrawal = [(L.Testnet, stakeCred, 10_000_000 :: C.Lovelace)]
      proposal =
        C.createProposalProcedure
          sbe
          (C.toShelleyNetwork networkId)
          0 -- govActionDeposit
          (sPStakeKeyHash stakeDelegationPool)
          (C.TreasuryWithdrawal tWithdrawal)
          anchor

      tx1BodyContent =
        (Tx.emptyTxBodyContent sbe pparams)
          { C.txIns = Tx.pubkeyTxIns [tx1In]
          , C.txProposalProcedures = C.forEraInEonMaybe era (`C.Featured` [proposal])
          , C.txOuts = [tx1Out1, tx1Out2]
          }

    signedTx1 <- Tx.buildTx era localNodeConnectInfo tx1BodyContent w1Address w1SKey
    Tx.submitTx sbe localNodeConnectInfo signedTx1
    let _tx2In1@(C.TxIn tx2InId1 _tx2InIx1) = Tx.txIn (Tx.txId signedTx1) 0
        _tx2In2 = Tx.txIn (Tx.txId signedTx1) 1
        tx2In3 = Tx.txIn (Tx.txId signedTx1) 2 -- change output
    result1TxOut <-
      Q.getTxOutAtAddress era localNodeConnectInfo w1Address tx2In3 "getTxOutAtAddress"
    H.annotate $ show result1TxOut

    -- vote on the treasury withdrawal

    let tx2Out1 = Tx.txOut era (C.lovelaceToValue 4_000_000) w1Address
        votes = [(committeeVoter, C.Yes), (kDRepVoter, C.Yes)] -- SPO not allowed to vote on treasury withdrawal
        votingProcedures = Tx.buildVotingProcedures sbe ceo tx2InId1 0 votes
    let tx2BodyContent =
          (Tx.emptyTxBodyContent sbe pparams)
            { C.txIns = Tx.pubkeyTxIns [tx2In3]
            , C.txVotingProcedures = C.forEraInEonMaybe era (`C.Featured` votingProcedures)
            , C.txOuts = [tx2Out1]
            }

    signedTx2 <-
      Tx.buildTxWithWitnessOverride
        era
        localNodeConnectInfo
        tx2BodyContent
        w1Address
        (Just 3) -- witnesses
        [w1SKey, (DRep.castDRep kDRepSKey), (CC.castCommittee committeeHotSKey)]
    Tx.submitTx sbe localNodeConnectInfo signedTx2
    let result2TxIn = Tx.txIn (Tx.txId signedTx2) 0
    result2TxOut <-
      Q.getTxOutAtAddress era localNodeConnectInfo w1Address result2TxIn "getTxOutAtAddress"
    H.annotate $ show result2TxOut
    success -- TODO: check treasury withdrawal is enacted
treasuryWithdrawalProposalAndVoteTest _ ScriptDRep{} _ _ _ =
  error "treasuryWithdrawalProposalAndVoteTest: ScriptDRep not yet supported"

mkProtocolVersionOrErr :: (Natural, Natural) -> L.ProtVer
mkProtocolVersionOrErr (majorProtVer, minorProtVer) =
  case (`L.ProtVer` minorProtVer) <$> L.mkVersion majorProtVer of
    Just v -> v
    Nothing ->
      error $ "mkProtocolVersionOrErr: invalid protocol version " <> show (majorProtVer, minorProtVer)

hardForkProposalAndVoteTestInfo committee dRep staking =
  TestInfo
    { testName = "hardForkProposalAndVoteTest"
    , testDescription = "Propose and vote on a hard fork"
    , test = hardForkProposalAndVoteTest committee dRep staking
    }
hardForkProposalAndVoteTest
  :: (MonadTest m, MonadIO m)
  => Committee era
  -> DRep era
  -> Staking era
  -> TN.TestEnvironmentOptions era
  -> TestParams era
  -> m (Maybe String)
hardForkProposalAndVoteTest
  Committee{..}
  KeyDRep{..}
  Staking{..}
  networkOptions
  TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
    era <- TN.eraFromOptionsM networkOptions
    (w1SKey, w1Address) <- TN.w1 networkOptions tempAbsPath networkId
    let sbe = toShelleyBasedEra era
        ceo = toConwayEraOnwards era

    currentEpoch1 <- Q.getCurrentEpoch era localNodeConnectInfo
    H.annotate $ show currentEpoch1

    -- wait for next epoch to start before proposing governance action
    currentEpoch2 <-
      Q.waitForNextEpoch era localNodeConnectInfo "currentEpoch2"
        =<< Q.getCurrentEpoch era localNodeConnectInfo
    H.annotate $ show currentEpoch2

    -- build a transaction to propose a treasury withdrawal

    let anchorUrl = C.textToUrl "https://example.com/hard_fork.txt"
        anchor = C.createAnchor (U.unsafeFromMaybe anchorUrl) "hard fork"
    tx1In <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address
    pvNat :: Natural <- toEnum <$> TN.pvFromOptions networkOptions
    let
      tx1Out1 = Tx.txOut era (C.lovelaceToValue 2_000_000) w1Address
      tx1Out2 = Tx.txOut era (C.lovelaceToValue 3_000_000) w1Address
      nextPv = mkProtocolVersionOrErr (pvNat + 1, 2)
      proposal =
        C.createProposalProcedure
          sbe
          (C.toShelleyNetwork networkId)
          0 -- govActionDeposit
          (sPStakeKeyHash stakeDelegationPool)
          (C.InitiateHardfork C.SNothing nextPv)
          anchor

      tx1BodyContent =
        (Tx.emptyTxBodyContent sbe pparams)
          { C.txIns = Tx.pubkeyTxIns [tx1In]
          , C.txProposalProcedures = C.forEraInEonMaybe era (`C.Featured` [proposal])
          , C.txOuts = [tx1Out1, tx1Out2]
          }

    signedTx1 <- Tx.buildTx era localNodeConnectInfo tx1BodyContent w1Address w1SKey
    Tx.submitTx sbe localNodeConnectInfo signedTx1
    let _tx2In1@(C.TxIn tx2InId1 _tx2InIx1) = Tx.txIn (Tx.txId signedTx1) 0
        _tx2In2 = Tx.txIn (Tx.txId signedTx1) 1
        tx2In3 = Tx.txIn (Tx.txId signedTx1) 2 -- change output
    result1TxOut <-
      Q.getTxOutAtAddress era localNodeConnectInfo w1Address tx2In3 "getTxOutAtAddress"
    H.annotate $ show result1TxOut

    -- vote on the hard fork

    let tx2Out1 = Tx.txOut era (C.lovelaceToValue 4_000_000) w1Address
        votes = [(committeeVoter, C.Yes), (kDRepVoter, C.Yes), (sPVoter stakeDelegationPool, C.Yes)]
        votingProcedures = Tx.buildVotingProcedures sbe ceo tx2InId1 0 votes
    let tx2BodyContent =
          (Tx.emptyTxBodyContent sbe pparams)
            { C.txIns = Tx.pubkeyTxIns [tx2In3]
            , C.txVotingProcedures = C.forEraInEonMaybe era (`C.Featured` votingProcedures)
            , C.txOuts = [tx2Out1]
            }

    signedTx2 <-
      Tx.buildTxWithWitnessOverride
        era
        localNodeConnectInfo
        tx2BodyContent
        w1Address
        (Just 4) -- witnesses
        [ w1SKey
        , C.WitnessStakePoolKey (sPSKey stakeDelegationPool)
        , DRep.castDRep kDRepSKey
        , CC.castCommittee committeeHotSKey
        ]
    Tx.submitTx sbe localNodeConnectInfo signedTx2
    let result2TxIn = Tx.txIn (Tx.txId signedTx2) 0
    result2TxOut <-
      Q.getTxOutAtAddress era localNodeConnectInfo w1Address result2TxIn "getTxOutAtAddress"
    H.annotate $ show result2TxOut
    success -- TODO: check hard fork is enacted
hardForkProposalAndVoteTest _ ScriptDRep{} _ _ _ = error "hardForkProposalAndVoteTest: ScriptDRep not yet supported"

infoProposalAndVoteTestInfo committee dRep staking =
  TestInfo
    { testName = "infoProposalAndVoteTest"
    , testDescription = "Propose and vote on an Info action"
    , test = infoProposalAndVoteTest committee dRep staking
    }
infoProposalAndVoteTest
  :: (MonadTest m, MonadIO m)
  => Committee era
  -> DRep era
  -> Staking era
  -> TN.TestEnvironmentOptions era
  -> TestParams era
  -> m (Maybe String)
infoProposalAndVoteTest
  Committee{..}
  KeyDRep{..}
  Staking{..}
  networkOptions
  TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
    era <- TN.eraFromOptionsM networkOptions
    (w1SKey, w1Address) <- TN.w1 networkOptions tempAbsPath networkId
    let sbe = toShelleyBasedEra era
        ceo = toConwayEraOnwards era

    currentEpoch1 <- Q.getCurrentEpoch era localNodeConnectInfo
    H.annotate $ show currentEpoch1

    -- wait for next epoch to start before proposing governance action
    currentEpoch2 <-
      Q.waitForNextEpoch era localNodeConnectInfo "currentEpoch2"
        =<< Q.getCurrentEpoch era localNodeConnectInfo
    H.annotate $ show currentEpoch2

    -- build a transaction to propose an Info action

    let anchorUrl = C.textToUrl "https://example.com/info.txt"
        anchor = C.createAnchor (U.unsafeFromMaybe anchorUrl) "Info"
    tx1In <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address
    let
      tx1Out1 = Tx.txOut era (C.lovelaceToValue 2_000_000) w1Address
      tx1Out2 = Tx.txOut era (C.lovelaceToValue 3_000_000) w1Address
      proposal =
        C.createProposalProcedure
          sbe
          (C.toShelleyNetwork networkId)
          0 -- govActionDeposit
          (sPStakeKeyHash stakeDelegationPool)
          C.InfoAct
          anchor

      tx1BodyContent =
        (Tx.emptyTxBodyContent sbe pparams)
          { C.txIns = Tx.pubkeyTxIns [tx1In]
          , C.txProposalProcedures = C.forEraInEonMaybe era (`C.Featured` [proposal])
          , C.txOuts = [tx1Out1, tx1Out2]
          }

    signedTx1 <- Tx.buildTx era localNodeConnectInfo tx1BodyContent w1Address w1SKey
    Tx.submitTx sbe localNodeConnectInfo signedTx1
    let _tx2In1@(C.TxIn tx2InId1 _tx2InIx1) = Tx.txIn (Tx.txId signedTx1) 0
        _tx2In2 = Tx.txIn (Tx.txId signedTx1) 1
        tx2In3 = Tx.txIn (Tx.txId signedTx1) 2 -- change output
    result1TxOut <-
      Q.getTxOutAtAddress era localNodeConnectInfo w1Address tx2In3 "getTxOutAtAddress"
    H.annotate $ show result1TxOut

    -- vote on the hard fork

    let tx2Out1 = Tx.txOut era (C.lovelaceToValue 4_000_000) w1Address
        votes = [(committeeVoter, C.Yes), (kDRepVoter, C.Yes), (sPVoter stakeDelegationPool, C.Yes)]
        votingProcedures = Tx.buildVotingProcedures sbe ceo tx2InId1 0 votes
    let tx2BodyContent =
          (Tx.emptyTxBodyContent sbe pparams)
            { C.txIns = Tx.pubkeyTxIns [tx2In3]
            , C.txVotingProcedures = C.forEraInEonMaybe era (`C.Featured` votingProcedures)
            , C.txOuts = [tx2Out1]
            }

    signedTx2 <-
      Tx.buildTxWithWitnessOverride
        era
        localNodeConnectInfo
        tx2BodyContent
        w1Address
        (Just 4) -- witnesses
        [ w1SKey
        , C.WitnessStakePoolKey (sPSKey stakeDelegationPool)
        , DRep.castDRep kDRepSKey
        , CC.castCommittee committeeHotSKey
        ]
    Tx.submitTx sbe localNodeConnectInfo signedTx2
    let result2TxIn = Tx.txIn (Tx.txId signedTx2) 0
    result2TxOut <-
      Q.getTxOutAtAddress era localNodeConnectInfo w1Address result2TxIn "getTxOutAtAddress"
    H.annotate $ show result2TxOut
    success -- TODO: check hard fork is enacted
infoProposalAndVoteTest _ ScriptDRep{} _ _ _ = error "infoProposalAndVoteTest: ScriptDRep not yet supported"

unregisterDRepTestInfo dRep =
  TestInfo
    { testName = "unregisterDRepTest (" ++ showKeyOrScript dRep ++ ")"
    , testDescription = "Unregister " ++ showKeyOrScript dRep ++ "DRep"
    , test = unregisterDRepTest dRep
    }
unregisterDRepTest
  :: (MonadTest m, MonadIO m)
  => DRep era
  -> TN.TestEnvironmentOptions era
  -> TestParams era
  -> m (Maybe String)
unregisterDRepTest dRep@KeyDRep{} = unregisterDRep dRep
unregisterDRepTest dRep@ScriptDRep{} = unregisterDRep dRep
unregisterDRep
  dRep
  networkOptions
  TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
    era <- TN.eraFromOptionsM networkOptions
    (w1SKey, w1Address) <- TN.w1 networkOptions tempAbsPath networkId
    let sbe = toShelleyBasedEra era

    unRegDRepTxIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address
    let
      unRegDRepTxOut = Tx.txOut era (C.lovelaceToValue 2_000_000) w1Address
      -- TODO: add DRep script witness
      unRegDRepTxBodyContent =
        (Tx.emptyTxBodyContent sbe pparams)
          { C.txIns = Tx.pubkeyTxIns [unRegDRepTxIn]
          , C.txCertificates = Tx.txCertificates era [kDRepUnregCert dRep] []
          , C.txOuts = [unRegDRepTxOut]
          }
      unRegDRepTxWitnesses =
        case dRep of
          KeyDRep{} -> [w1SKey, DRep.castDRep (kDRepSKey dRep)]
          ScriptDRep{} -> [w1SKey]

    signedDRepUnregTx <-
      Tx.buildTxWithWitnessOverride
        era
        localNodeConnectInfo
        unRegDRepTxBodyContent
        w1Address
        (Just $ fromIntegral $ length unRegDRepTxWitnesses)
        unRegDRepTxWitnesses
    Tx.submitTx sbe localNodeConnectInfo signedDRepUnregTx
    let expTxIn = Tx.txIn (Tx.txId signedDRepUnregTx) 0
    stakeDelegResultTxOut <-
      Q.getTxOutAtAddress era localNodeConnectInfo w1Address expTxIn "getTxOutAtAddress"
    H.annotate $ show stakeDelegResultTxOut
    success

unregisterStakingTestInfo staking =
  TestInfo
    { testName = "unregisterStakingTest"
    , testDescription = "Unregister DRep"
    , test = unregisterStakingTest staking
    }
unregisterStakingTest
  :: (MonadTest m, MonadIO m)
  => Staking era
  -> TN.TestEnvironmentOptions era
  -> TestParams era
  -> m (Maybe String)
unregisterStakingTest
  Staking{..}
  networkOptions
  TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
    era <- TN.eraFromOptionsM networkOptions
    (w1SKey, w1Address) <- TN.w1 networkOptions tempAbsPath networkId
    let sbe = toShelleyBasedEra era

    stakeDelgTxIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address
    let
      stakeDelegTxOut = Tx.txOut era (C.lovelaceToValue 2_000_000) w1Address
      stakeDelegTxBodyContent =
        (Tx.emptyTxBodyContent sbe pparams)
          { C.txIns = Tx.pubkeyTxIns [stakeDelgTxIn]
          , C.txCertificates = Tx.txCertificates era [stakeUnregCert] [stakeCred]
          , C.txOuts = [stakeDelegTxOut]
          }

    signedStakeUnregTx <-
      Tx.buildTxWithWitnessOverride
        era
        localNodeConnectInfo
        stakeDelegTxBodyContent
        w1Address
        (Just 2)
        [w1SKey, C.WitnessStakeKey stakeSKey]
    Tx.submitTx sbe localNodeConnectInfo signedStakeUnregTx
    let expTxIn = Tx.txIn (Tx.txId signedStakeUnregTx) 0
    stakeDelegResultTxOut <-
      Q.getTxOutAtAddress era localNodeConnectInfo w1Address expTxIn "getTxOutAtAddress"
    H.annotate $ show stakeDelegResultTxOut
    success

retireStakePoolTestInfo stakePool =
  TestInfo
    { testName = "retireStakePoolTestInfo"
    , testDescription = "Retire stake pool"
    , test = retireStakePoolTest stakePool
    }
retireStakePoolTest
  :: (MonadTest m, MonadIO m)
  => StakePool era
  -> TN.TestEnvironmentOptions era
  -> TestParams era
  -> m (Maybe String)
retireStakePoolTest
  stakePool@StakePool{..}
  networkOptions
  TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
    era <- TN.eraFromOptionsM networkOptions
    (w1SKey, w1Address) <- TN.w1 networkOptions tempAbsPath networkId
    let ceo = toConwayEraOnwards era
        sbe = toShelleyBasedEra era

    currentEpoch <- Q.getCurrentEpoch era localNodeConnectInfo
    H.annotate $ show currentEpoch

    stakeDelgTxIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address
    let
      retireSPCert = makeStakePoolRetireCertification ceo stakePool (currentEpoch + 1)
      stakeDelegTxOut = Tx.txOut era (C.lovelaceToValue 2_000_000) w1Address
      stakeDelegTxBodyContent =
        (Tx.emptyTxBodyContent sbe pparams)
          { C.txIns = Tx.pubkeyTxIns [stakeDelgTxIn]
          , C.txCertificates = Tx.txCertificates era [retireSPCert] [sPStakeCred]
          , C.txOuts = [stakeDelegTxOut]
          }

    signedPoolRetireTx <-
      Tx.buildTxWithWitnessOverride
        era
        localNodeConnectInfo
        stakeDelegTxBodyContent
        w1Address
        (Just 2)
        [w1SKey, C.WitnessStakePoolKey sPSKey]
    Tx.submitTx sbe localNodeConnectInfo signedPoolRetireTx
    let expTxIn = Tx.txIn (Tx.txId signedPoolRetireTx) 0
    stakeDelegResultTxOut <-
      Q.getTxOutAtAddress era localNodeConnectInfo w1Address expTxIn "getTxOutAtAddress"
    H.annotate $ show stakeDelegResultTxOut
    success

-- TODO: test with script using txInfoCurrentTreasuryAmount (Just and Nothing)
-- TODO: test with script using txInfoTreasuryDonation (Just and Nothing)
