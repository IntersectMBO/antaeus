{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Helpers.DRep where

import Cardano.Api qualified as C
import Cardano.Api.Ledger qualified as C
import Cardano.Api.Shelley qualified as C
import Control.Monad.IO.Class (MonadIO, liftIO)
import Helpers.Utils qualified as U

data DRep era = DRep
  { dRepSKey :: C.SigningKey C.DRepKey
  , dRepKeyHash :: C.KeyHash 'C.DRepRole C.StandardCrypto
  , dRepStakeCred :: C.StakeCredential
  , dRepVotingCredential :: C.VotingCredential era
  , dRepRegCert :: C.Certificate era
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
    dRepStakeCred = C.StakeCredentialByKey . C.StakeKeyHash $ C.coerceKeyRole dRepKeyHash
    dRepVotingCredential = U.unsafeFromRight $ C.toVotingCredential ceo dRepStakeCred
    dRepDeposit = C.Lovelace 0 -- dRepDeposit
    dRepRegReqs = C.DRepRegistrationRequirements ceo dRepVotingCredential dRepDeposit
    dRepRegCert = C.makeDrepRegistrationCertificate dRepRegReqs Nothing
  return $ DRep dRepSkey dRepKeyHash dRepStakeCred dRepVotingCredential dRepRegCert
