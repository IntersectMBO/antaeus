{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Helpers.Common where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Helpers.DRep qualified as DRep

data KeyOrScript a = Key a | Script a

instance Show (KeyOrScript (DRep.DRep era)) where
  show (Key DRep.KeyDRep{}) = "key"
  show (Script DRep.ScriptDRep{}) = "script"

showKeyOrScript :: DRep.DRep era -> String
showKeyOrScript kDRep@DRep.KeyDRep{} = show $ Key kDRep
showKeyOrScript sDRep@DRep.ScriptDRep{} = show $ Script sDRep

toShelleyBasedEra :: C.CardanoEra era -> C.ShelleyBasedEra era
toShelleyBasedEra era =
  case era of
    C.AlonzoEra -> C.ShelleyBasedEraAlonzo
    C.BabbageEra -> C.ShelleyBasedEraBabbage
    C.ConwayEra -> C.ShelleyBasedEraConway
    _ -> error "Must use Alonzo, Babbage or Conway era"

toMaryEraOnwards :: C.CardanoEra era -> C.MaryEraOnwards era
toMaryEraOnwards era =
  case era of
    C.AlonzoEra -> C.MaryEraOnwardsAlonzo
    C.BabbageEra -> C.MaryEraOnwardsBabbage
    C.ConwayEra -> C.MaryEraOnwardsConway
    _ -> error "Must use Alonzo, Babbage or Conway era"

toConwayEraOnwards :: C.CardanoEra era -> C.ConwayEraOnwards era
toConwayEraOnwards era =
  case era of
    C.ConwayEra -> C.ConwayEraOnwardsConway
    _ -> error "Must use Conway era"

-- | Treat CardanoEra as ShelleyBased
withIsShelleyBasedEra :: C.CardanoEra era -> ((C.IsShelleyBasedEra era) => r) -> r
withIsShelleyBasedEra era r =
  case era of
    C.AlonzoEra -> r
    C.BabbageEra -> r
    C.ConwayEra -> r
    _ -> error "Must use Alonzo, Babbage or Conway era"

makeAddress
  :: Either (C.VerificationKey C.PaymentKey) C.ScriptHash
  -> C.NetworkId
  -> C.Address C.ShelleyAddr
makeAddress ePkSh = makeAddressWithStake ePkSh Nothing

-- | Make a payment or script address
makeAddressWithStake
  :: Either (C.VerificationKey C.PaymentKey) C.ScriptHash
  -> Maybe (C.VerificationKey C.StakeKey)
  -> C.NetworkId
  -> C.Address C.ShelleyAddr
makeAddressWithStake (Left paymentKey) mStakeVKey nId =
  C.makeShelleyAddress
    nId
    (C.PaymentCredentialByKey $ C.verificationKeyHash paymentKey)
    ( case mStakeVKey of
        Nothing -> C.NoStakeAddress
        Just stakeVKey -> C.StakeAddressByValue $ C.StakeCredentialByKey $ C.verificationKeyHash stakeVKey
    )
makeAddressWithStake (Right scriptHash) _mStakeVKey nId =
  C.makeShelleyAddress nId (C.PaymentCredentialByScript scriptHash) C.NoStakeAddress
