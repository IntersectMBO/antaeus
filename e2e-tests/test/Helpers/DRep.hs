{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Helpers.DRep where

import Cardano.Api qualified as C
import Cardano.Api.Ledger (Voter)
import Cardano.Api.Ledger qualified as C
import Cardano.Api.Shelley qualified as C
import Control.Monad.IO.Class (MonadIO, liftIO)

data DRep era = DRep
  { dRepSKey :: C.SigningKey C.DRepKey
  , dRepCred :: C.Credential 'C.DRepRole (C.EraCrypto (C.ShelleyLedgerEra era))
  , dRepLedgerCred :: C.DRep (C.EraCrypto (C.ShelleyLedgerEra era))
  , dRepRegCert :: C.Certificate era
  , dRepUnregCert :: C.Certificate era
  , dRepVoter :: Voter (C.EraCrypto (C.ShelleyLedgerEra era))
  }
  deriving (Show)

generateDRepKeyCredentialsAndCertificate
  :: (MonadIO m)
  => C.ConwayEraOnwards era
  -> m (DRep era)
generateDRepKeyCredentialsAndCertificate ceo = do
  dRepSkey <- liftIO $ C.generateSigningKey C.AsDRepKey
  let
    C.DRepKeyHash dRepKeyHash = C.verificationKeyHash $ C.getVerificationKey dRepSkey
    dRepVotingCredential = C.conwayEraOnwardsConstraints ceo $ C.KeyHashObj dRepKeyHash
    dRepDeposit = C.Lovelace 0 -- dRepDeposit
    dRepRegReqs = C.DRepRegistrationRequirements ceo dRepVotingCredential dRepDeposit
    dRepRegCert = C.makeDrepRegistrationCertificate dRepRegReqs Nothing
    dRepUnregReqs = C.DRepUnregistrationRequirements ceo dRepVotingCredential 0
    dRepUnregCert = C.makeDrepUnregistrationCertificate dRepUnregReqs
    dRepVoter = C.DRepVoter dRepVotingCredential
  return $
    DRep
      dRepSkey
      dRepVotingCredential
      (C.DRepCredential dRepVotingCredential)
      dRepRegCert
      dRepUnregCert
      dRepVoter

castDRep :: C.SigningKey C.DRepKey -> C.SigningKey C.PaymentKey
castDRep (C.DRepSigningKey sk) = C.PaymentSigningKey sk

stakeDelegateCert
  :: C.ConwayEraOnwards era
  -> C.DRep (C.EraCrypto (C.ShelleyLedgerEra era))
  -> C.StakeCredential
  -> C.Certificate era
stakeDelegateCert ceo dRepLedgerCred stakeCred = do
  let dRepDelegatee = C.DelegVote $ C.conwayEraOnwardsConstraints ceo dRepLedgerCred
      w1StakeDelgReqs = C.StakeDelegationRequirementsConwayOnwards ceo stakeCred dRepDelegatee
  C.makeStakeAddressDelegationCertificate w1StakeDelgReqs
