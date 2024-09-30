{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Helpers.Committee where

import Cardano.Api qualified as C
import Cardano.Api.Ledger qualified as C
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Shelley qualified as C hiding (Voter)
import Cardano.Ledger.Keys qualified as Keys
import Control.Monad.IO.Class (MonadIO, liftIO)

data Committee era = Committee
  { committeeColdSKey :: C.SigningKey C.CommitteeColdKey
  , commiteeColdVKey :: C.VerificationKey C.CommitteeColdKey
  , committeeColdKeyHash :: C.Hash C.CommitteeColdKey
  , committeeHotSKey :: C.SigningKey C.CommitteeHotKey
  , committeeHotKeyAuthCert :: C.Certificate era
  , committeeVoter :: C.Voter (C.EraCrypto (C.ShelleyLedgerEra era))
  }
  deriving (Show)

generateCommitteeKeysAndCertificate
  :: forall m era. (MonadIO m) => C.ConwayEraOnwards era -> m (Committee era)
generateCommitteeKeysAndCertificate era = do
  -- generate committee cold key
  committeeColdSKey <- liftIO $ C.generateSigningKey C.AsCommitteeColdKey
  -- generate committee hot key
  committeeHotSKey <- liftIO $ C.generateSigningKey C.AsCommitteeHotKey

  let
    commiteeColdVKey@(C.CommitteeColdVerificationKey ccColdVKey) =
      C.getVerificationKey committeeColdSKey
    committeeColdKeyHash = C.verificationKeyHash commiteeColdVKey

    C.CommitteeHotVerificationKey ccHotVKey =
      C.getVerificationKey committeeHotSKey

    -- produce committee hot key authorization certificate
    coldKeyHash =
      C.conwayEraOnwardsConstraints era $ L.KeyHashObj $ Keys.hashKey ccColdVKey
    hotKeyHash =
      C.conwayEraOnwardsConstraints era $ L.KeyHashObj $ Keys.hashKey ccHotVKey
    committeeHotRequirements =
      C.CommitteeHotKeyAuthorizationRequirements era coldKeyHash hotKeyHash
    committeeHotKeyAuthCert =
      C.makeCommitteeHotKeyAuthorizationCertificate committeeHotRequirements

    -- produce committee voter
    C.CommitteeHotKeyHash committeeHotHash =
      C.verificationKeyHash $ C.getVerificationKey committeeHotSKey
    committeeVotingCredential =
      C.conwayEraOnwardsConstraints era $ C.KeyHashObj committeeHotHash
    committeeVoter = C.CommitteeVoter committeeVotingCredential
  pure
    Committee
      { committeeColdSKey
      , commiteeColdVKey
      , committeeColdKeyHash
      , committeeHotSKey
      , committeeHotKeyAuthCert
      , committeeVoter
      }

keyAsWitness :: C.SigningKey C.CommitteeHotKey -> C.ShelleyWitnessSigningKey
keyAsWitness (C.CommitteeHotSigningKey key) =
  C.WitnessPaymentKey (C.PaymentSigningKey key)
