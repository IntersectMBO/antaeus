{-# LANGUAGE RankNTypes #-}

module Helpers.Common where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C

-- | Any CardanoEra with CardanoMode
toEraInCardanoMode :: C.CardanoEra era -> C.EraInMode era C.CardanoMode
toEraInCardanoMode era = fromMaybe $ C.toEraInMode era C.CardanoMode
  where
    fromMaybe Nothing = error $ "No mode for this era " ++ show era ++ " in CardanoMode"
    fromMaybe (Just eim) = eim

toShelleyBasedEra :: C.CardanoEra era -> C.ShelleyBasedEra era
toShelleyBasedEra era =
  case era of
    C.AlonzoEra -> C.ShelleyBasedEraAlonzo
    C.BabbageEra -> C.ShelleyBasedEraBabbage
    C.ConwayEra -> C.ShelleyBasedEraConway
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
