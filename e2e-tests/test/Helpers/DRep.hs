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
  , dRepKeyHash :: C.KeyHash 'C.DRepRole C.StandardCrypto
  , dRepRegCert :: C.Certificate era
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
    dRepVoter = C.DRepVoter dRepVotingCredential
  return $ DRep dRepSkey dRepKeyHash dRepRegCert dRepVoter
