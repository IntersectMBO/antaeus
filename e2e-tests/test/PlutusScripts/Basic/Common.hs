{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -O0 #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}

module PlutusScripts.Basic.Common where

import PlutusLedgerApi.V1 qualified as P
import PlutusLedgerApi.V1.Interval qualified as P
import PlutusLedgerApi.V1.Value qualified as P
import PlutusLedgerApi.V3 qualified as PV3
import PlutusTx.Prelude qualified as P

-- AlwaysSucceeds minting policy --

{-# INLINEABLE mkAlwaysSucceedPolicy #-}
mkAlwaysSucceedPolicy :: P.BuiltinData -> P.BuiltinData -> ()
mkAlwaysSucceedPolicy _datum _sc = ()

-- AlwaysSucceeds validator --

{-# INLINEABLE mkAlwaysSucceedSpend #-}
mkAlwaysSucceedSpend :: P.BuiltinData -> P.BuiltinData -> P.BuiltinData -> ()
mkAlwaysSucceedSpend _datum _redeemer _sc = ()

-- AlwaysFails minting policy --

{-# INLINEABLE mkAlwaysFailsPolicy #-}
mkAlwaysFailsPolicy :: P.BuiltinData -> P.BuiltinData -> ()
mkAlwaysFailsPolicy _datum _sc = P.check $ P.error ()

-- Mint token name policy --

{-# INLINEABLE mkMintTokenNamePolicyV3 #-}
mkMintTokenNamePolicyV3 :: P.TokenName -> PV3.ScriptContext -> Bool
mkMintTokenNamePolicyV3 tn ctx = P.traceIfFalse "wrong token name" checkTokenName
  where
    info :: PV3.TxInfo
    info = PV3.scriptContextTxInfo ctx

    -- TODO: Use builtin when available in PV3
    ownCurrencySymbol :: PV3.ScriptContext -> PV3.CurrencySymbol
    ownCurrencySymbol PV3.ScriptContext{PV3.scriptContextPurpose = PV3.Minting cs} = cs
    ownCurrencySymbol _ = P.traceError "Lh"

    checkTokenName :: Bool
    checkTokenName = P.valueOf (PV3.txInfoMint info) (ownCurrencySymbol ctx) tn P.> 0

-- Time range policy --

{-# INLINEABLE mkTimeRangePolicyV3 #-}
mkTimeRangePolicyV3 :: P.POSIXTime -> PV3.ScriptContext -> Bool
mkTimeRangePolicyV3 dl ctx = (P.to dl) `P.contains` range
  where
    info :: PV3.TxInfo
    info = PV3.scriptContextTxInfo ctx

    range :: P.POSIXTimeRange
    range = PV3.txInfoValidRange info

-- Witness redeemer policy --

{-# INLINEABLE mkWitnessRedeemerPolicyV3 #-}
mkWitnessRedeemerPolicyV3 :: P.PubKeyHash -> PV3.ScriptContext -> Bool
mkWitnessRedeemerPolicyV3 pkh ctx = P.traceIfFalse "not signed by redeemer pubkeyhash" checkWitness
  where
    info :: PV3.TxInfo
    info = PV3.scriptContextTxInfo ctx

    -- TODO: Use builtin when available in PV3
    txSignedBy :: PV3.TxInfo -> PV3.PubKeyHash -> Bool
    txSignedBy PV3.TxInfo{PV3.txInfoSignatories} k = case P.find ((P.==) k) txInfoSignatories of
      P.Just _ -> P.True
      P.Nothing -> P.False

    checkWitness :: Bool
    checkWitness = txSignedBy info pkh
