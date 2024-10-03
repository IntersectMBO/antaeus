{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module PlutusScripts.SECP256k1.V_1_1 where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Helpers.ScriptUtils qualified as U
import PlutusCore.Version (plcVersion110)
import PlutusLedgerApi.Common (SerialisedScript, serialiseCompiledCode)
import PlutusScripts.Helpers (
  mintScriptWitness,
  plutusL3,
  policyIdV3,
  writeSerialisedScript,
 )
import PlutusScripts.SECP256k1.Common (
  Secp256Params (..),
  ecdsaAssetName,
  schnorrAssetName,
  verifyEcdsaParams,
  verifyEcdsaRedeemer,
  verifySchnorrParams,
  verifySchnorrRedeemer,
 )
import PlutusTx qualified as P
import PlutusTx.Builtins qualified as BI
import PlutusTx.Prelude qualified as P

-- Schnorr minting policy --

verifyEcdsaPolicyV12 :: SerialisedScript
verifyEcdsaPolicyV12 =
  serialiseCompiledCode $
    $$(P.compile [||mkVerifyEcdsaPolicy||])
      `P.unsafeApplyCode` P.liftCode plcVersion110 verifyEcdsaParams
  where
    {-# INLINEABLE mkVerifyEcdsaPolicy #-}
    mkVerifyEcdsaPolicy :: Secp256Params -> P.BuiltinData -> P.BuiltinData -> ()
    mkVerifyEcdsaPolicy Secp256Params{..} _redeemer _sc =
      U.check $ P.verifyEcdsaSecp256k1Signature vkey msg sig

verifySchnorrPolicyV3 :: SerialisedScript
verifySchnorrPolicyV3 =
  serialiseCompiledCode $
    $$(P.compile [||mkVerifySchnorrPolicy||])
      `P.unsafeApplyCode` P.liftCode plcVersion110 verifySchnorrParams
  where
    {-# INLINEABLE mkVerifySchnorrPolicy #-}
    mkVerifySchnorrPolicy :: Secp256Params -> P.BuiltinData -> P.BuiltinUnit
    mkVerifySchnorrPolicy Secp256Params{..} _sc =
      P.check $ BI.verifySchnorrSecp256k1Signature vkey msg sig

verifySchnorrPolicyScriptV3 :: C.PlutusScript C.PlutusScriptV3
verifySchnorrPolicyScriptV3 = C.PlutusScriptSerialised verifySchnorrPolicyV3

writeVerifySchnorrPolicyScriptV3 :: IO ()
writeVerifySchnorrPolicyScriptV3 =
  writeSerialisedScript "verifySchnorrPolicyScriptV3" verifySchnorrPolicyScriptV3

verifySchnorrAssetIdV3 :: C.AssetId
verifySchnorrAssetIdV3 =
  C.AssetId (policyIdV3 verifySchnorrPolicyV3) schnorrAssetName

verifySchnorrMintWitnessV3
  :: C.ShelleyBasedEra era
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
verifySchnorrMintWitnessV3 era =
  ( policyIdV3 verifySchnorrPolicyV3
  , mintScriptWitness
      era
      plutusL3
      (Left verifySchnorrPolicyScriptV3)
      verifySchnorrRedeemer
  )

-- ECDSA minting policy --

verifyEcdsaPolicy :: SerialisedScript
verifyEcdsaPolicy =
  serialiseCompiledCode $
    $$(P.compile [||mkVerifyEcdsaPolicy||])
      `P.unsafeApplyCode` P.liftCode plcVersion110 verifyEcdsaParams
  where
    {-# INLINEABLE mkVerifyEcdsaPolicy #-}
    mkVerifyEcdsaPolicy :: Secp256Params -> P.BuiltinData -> P.BuiltinUnit
    mkVerifyEcdsaPolicy Secp256Params{..} _sc =
      P.check $ BI.verifyEcdsaSecp256k1Signature vkey msg sig

verifyEcdsaPolicyScriptV3 :: C.PlutusScript C.PlutusScriptV3
verifyEcdsaPolicyScriptV3 = C.PlutusScriptSerialised verifyEcdsaPolicy

writeVerifyEcdsaPolicyScriptV3 :: IO ()
writeVerifyEcdsaPolicyScriptV3 =
  writeSerialisedScript "verifyEcdsaPolicyScriptV3" verifyEcdsaPolicyScriptV3

verifyEcdsaAssetIdV3 :: C.AssetId
verifyEcdsaAssetIdV3 = C.AssetId (policyIdV3 verifyEcdsaPolicy) ecdsaAssetName

verifyEcdsaMintWitnessV3
  :: C.ShelleyBasedEra era
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
verifyEcdsaMintWitnessV3 sbe =
  ( policyIdV3 verifyEcdsaPolicy
  , mintScriptWitness
      sbe
      plutusL3
      (Left verifyEcdsaPolicyScriptV3)
      verifyEcdsaRedeemer
  )
