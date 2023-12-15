{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Helpers.DRep where

import Cardano.Api qualified as C
import Cardano.Api.Ledger qualified as C
import Cardano.Api.Shelley qualified as C hiding (Voter)
import Control.Monad.IO.Class (MonadIO, liftIO)

data DRep era
  = KeyDRep
      { dRepSKey :: C.SigningKey C.DRepKey
      , dRepCred :: C.Credential 'C.DRepRole (C.EraCrypto (C.ShelleyLedgerEra era))
      , dRepLedgerCred :: C.DRep (C.EraCrypto (C.ShelleyLedgerEra era))
      , dRepRegCert :: C.Certificate era
      , dRepUnregCert :: C.Certificate era
      , dRepVoter :: C.Voter (C.EraCrypto (C.ShelleyLedgerEra era))
      }
  | ScriptDRep
      { dRepCred :: C.Credential 'C.DRepRole (C.EraCrypto (C.ShelleyLedgerEra era))
      , dRepLedgerCred :: C.DRep (C.EraCrypto (C.ShelleyLedgerEra era))
      , dRepRegCert :: C.Certificate era
      , dRepUnregCert :: C.Certificate era
      , dRepVoter :: C.Voter (C.EraCrypto (C.ShelleyLedgerEra era))
      }
  deriving (Show)

generateDRepKeyCredentialsAndCertificate
  :: (MonadIO m)
  => C.ConwayEraOnwards era
  -> m (DRep era)
generateDRepKeyCredentialsAndCertificate ceo = do
  dRepSkey <- liftIO $ C.generateSigningKey C.AsDRepKey
  let C.DRepKeyHash dRepKeyHash = C.verificationKeyHash $ C.getVerificationKey dRepSkey
      dRepVotingCredential = C.conwayEraOnwardsConstraints ceo $ C.KeyHashObj dRepKeyHash
  buildDRep ceo dRepVotingCredential (Just dRepSkey)

produceDRepScriptCredentialsAndCertificate
  :: (MonadIO m)
  => C.ConwayEraOnwards era
  -> C.ScriptHash
  -> m (DRep era)
produceDRepScriptCredentialsAndCertificate ceo scriptHash = do
  let dRepVotingCredential = C.conwayEraOnwardsConstraints ceo $ C.ScriptHashObj (C.toShelleyScriptHash scriptHash)
  buildDRep ceo dRepVotingCredential Nothing

buildDRep
  :: (MonadIO m)
  => C.ConwayEraOnwards era
  -> C.Credential 'C.DRepRole (C.EraCrypto (C.ShelleyLedgerEra era))
  -> Maybe (C.SigningKey C.DRepKey)
  -> m (DRep era)
buildDRep ceo dRepVotingCredential mDRepSKey = do
  let
    dRepDeposit = C.Lovelace 0
    dRepRegReqs = C.DRepRegistrationRequirements ceo dRepVotingCredential dRepDeposit
    dRepRegCert = C.makeDrepRegistrationCertificate dRepRegReqs Nothing
    dRepUnregReqs = C.DRepUnregistrationRequirements ceo dRepVotingCredential 0
    dRepUnregCert = C.makeDrepUnregistrationCertificate dRepUnregReqs
    dRepVoter = C.DRepVoter dRepVotingCredential
  return $ case mDRepSKey of
    Nothing ->
      ScriptDRep
        dRepVotingCredential
        (C.DRepCredential dRepVotingCredential)
        dRepRegCert
        dRepUnregCert
        dRepVoter
    Just dRepSKey ->
      KeyDRep
        dRepSKey
        dRepVotingCredential
        (C.DRepCredential dRepVotingCredential)
        dRepRegCert
        dRepUnregCert
        dRepVoter

alwaysAbstainDRep
  :: (MonadIO m)
  => C.ConwayEraOnwards era
  -> m (C.DRep (C.EraCrypto (C.ShelleyLedgerEra era)))
alwaysAbstainDRep ceo = pure $ C.conwayEraOnwardsConstraints ceo C.DRepAlwaysAbstain

alwaysNoConfidenceDRep
  :: (MonadIO m)
  => C.ConwayEraOnwards era
  -> m (C.DRep (C.EraCrypto (C.ShelleyLedgerEra era)))
alwaysNoConfidenceDRep ceo = pure $ C.conwayEraOnwardsConstraints ceo C.DRepAlwaysNoConfidence

castDRep :: C.SigningKey C.DRepKey -> C.SigningKey C.PaymentKey
castDRep (C.DRepSigningKey sk) = C.PaymentSigningKey sk

voteDelegateCert
  :: C.ConwayEraOnwards era
  -> C.DRep (C.EraCrypto (C.ShelleyLedgerEra era))
  -> C.StakeCredential
  -> C.Certificate era
voteDelegateCert ceo dRepLedgerCred stakeCred = do
  let dRepDelegatee = C.DelegVote $ C.conwayEraOnwardsConstraints ceo dRepLedgerCred
      stakeDelgReqs = C.StakeDelegationRequirementsConwayOnwards ceo stakeCred dRepDelegatee
  C.makeStakeAddressDelegationCertificate stakeDelgReqs

stakeAndVoteCert
  :: C.ConwayEraOnwards era
  -> C.DRep (C.EraCrypto (C.ShelleyLedgerEra era))
  -> C.KeyHash 'C.StakePool C.StandardCrypto
  -> C.StakeCredential
  -> C.Certificate era
stakeAndVoteCert ceo dRepLedgerCred stakePoolKeyHash stakeCred = do
  let stakePoolDelegatee = C.DelegStakeVote (C.conwayEraOnwardsConstraints ceo stakePoolKeyHash) dRepLedgerCred
  C.makeStakeAddressAndDRepDelegationCertificate ceo stakeCred stakePoolDelegatee 0
