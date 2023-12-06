{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Helpers.Staking where

import Cardano.Api qualified as C
import Cardano.Api.Ledger (Voter)
import Cardano.Api.Ledger qualified as C
import Cardano.Api.Shelley qualified as C
import Control.Monad.IO.Class (MonadIO, liftIO)

data Staking era = Staking
  { stakeSKey :: C.SigningKey C.StakeKey
  , stakeCred :: C.StakeCredential
  , stakeRegCert :: C.Certificate era
  , stakePoolVoter :: Voter (C.EraCrypto (C.ShelleyLedgerEra era)) -- pool to delegate to
  }
  deriving (Show)

generateStakeKeyCredentialAndCertificate
  :: (MonadIO m)
  => C.ConwayEraOnwards era
  -> Voter (C.EraCrypto (C.ShelleyLedgerEra era))
  -> m (Staking era)
generateStakeKeyCredentialAndCertificate ceo spoVoter = do
  stakeSKey <- liftIO $ C.generateSigningKey C.AsStakeKey
  let
    stakeCred = C.StakeCredentialByKey $ C.verificationKeyHash $ C.getVerificationKey stakeSKey
    stakeDeposit = C.Lovelace 0 -- keyDeposit
    stakeReqs = C.StakeAddrRegistrationConway ceo stakeDeposit stakeCred
    stakeRegCert = C.makeStakeAddressRegistrationCertificate stakeReqs
  return $ Staking stakeSKey stakeCred stakeRegCert spoVoter
