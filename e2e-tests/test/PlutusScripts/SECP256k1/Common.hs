{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
-- Not using all CardanoEra
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module PlutusScripts.SECP256k1.Common where

import Cardano.Api qualified as C
import PlutusScripts.Helpers (
  bytesFromHex,
  toScriptData,
 )
import PlutusTx qualified
import PlutusTx.Builtins qualified as BI
import PlutusTx.Prelude qualified as P

---- SECP256k1 ----

data Secp256Params = Secp256Params
  { vkey :: P.BuiltinByteString
  , msg :: P.BuiltinByteString
  , sig :: P.BuiltinByteString
  }

PlutusTx.unstableMakeIsData ''Secp256Params
PlutusTx.makeLift ''Secp256Params

-- Schnorr minting policy --

-- Use redeemer once PlutusV3 is fully implemented in the ledger
-- {-# INLINEABLE mkVerifySchnorrPolicy #-}
-- mkVerifySchnorrPolicy :: Secp256Params -> sc -> Bool
-- mkVerifySchnorrPolicy Secp256Params{..} _sc = BI.verifySchnorrSecp256k1Signature vkey msg sig

{-# INLINEABLE mkVerifySchnorrPolicy #-}
mkVerifySchnorrPolicy :: Secp256Params -> P.BuiltinData -> P.BuiltinData -> Bool
mkVerifySchnorrPolicy Secp256Params{..} _r _sc = BI.verifySchnorrSecp256k1Signature vkey msg sig

schnorrAssetName :: C.AssetName
schnorrAssetName = C.AssetName "Schnorr"

verifySchnorrParams :: Secp256Params
verifySchnorrParams =
  Secp256Params
    { vkey =
        BI.toBuiltin $ bytesFromHex "599de3e582e2a3779208a210dfeae8f330b9af00a47a7fb22e9bb8ef596f301b"
    , msg =
        BI.toBuiltin $
          bytesFromHex
            ( "3030303030303030303030303030303030303030303030303030303030303030"
                <> "3030303030303030303030303030303030303030303030303030303030303030"
            )
    , sig =
        BI.toBuiltin $
          bytesFromHex
            ( "5a56da88e6fd8419181dec4d3dd6997bab953d2fc71ab65e23cfc9e7e3d1a310"
                <> "613454a60f6703819a39fdac2a410a094442afd1fc083354443e8d8bb4461a9b"
            )
    }

verifySchnorrRedeemer :: C.HashableScriptData
verifySchnorrRedeemer = toScriptData verifySchnorrParams

-- ECDSA minting policy --

-- Use redeemer once PlutusV3 is fully implemented in the ledger
-- {-# INLINEABLE mkVerifyEcdsaPolicy #-}
-- mkVerifyEcdsaPolicy :: Secp256Params -> sc -> Bool
-- mkVerifyEcdsaPolicy Secp256Params{..} _sc = BI.verifyEcdsaSecp256k1Signature vkey msg sig

{-# INLINEABLE mkVerifyEcdsaPolicy #-}
mkVerifyEcdsaPolicy :: Secp256Params -> P.BuiltinData -> P.BuiltinData -> Bool
mkVerifyEcdsaPolicy Secp256Params{..} _r _sc = BI.verifyEcdsaSecp256k1Signature vkey msg sig

ecdsaAssetName :: C.AssetName
ecdsaAssetName = C.AssetName "ECDSA"

verifyEcdsaParams :: Secp256Params
verifyEcdsaParams =
  Secp256Params
    { vkey =
        BI.toBuiltin $ bytesFromHex "0392d7b94bc6a11c335a043ee1ff326b6eacee6230d3685861cd62bce350a172e0"
    , msg =
        BI.toBuiltin $ bytesFromHex "16e0bf1f85594a11e75030981c0b670370b3ad83a43f49ae58a2fd6f6513cde9"
    , sig =
        BI.toBuiltin $
          bytesFromHex
            ( "5fb12954b28be6456feb080cfb8467b6f5677f62eb9ad231de7a575f4b685751"
                <> "2754fb5ef7e0e60e270832e7bb0e2f0dc271012fa9c46c02504aa0e798be6295"
            )
    }

verifyEcdsaRedeemer :: C.HashableScriptData
verifyEcdsaRedeemer = toScriptData verifyEcdsaParams
