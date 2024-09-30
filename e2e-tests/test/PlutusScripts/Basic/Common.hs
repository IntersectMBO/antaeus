{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -O0 #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}

module PlutusScripts.Basic.Common where

import PlutusLedgerApi.V1.Interval (contains)
import PlutusLedgerApi.V1.Value (valueOf)
import PlutusLedgerApi.V3 (
  CurrencySymbol,
  POSIXTime,
  POSIXTimeRange,
  PubKeyHash,
  ScriptContext (
    ScriptContext,
    scriptContextScriptInfo,
    scriptContextTxInfo
  ),
  ScriptInfo (MintingScript),
  TokenName,
  TxInfo (TxInfo, txInfoMint, txInfoSignatories, txInfoValidRange),
  to,
 )
import PlutusTx.Prelude

-- Mint token name policy --

{-# INLINEABLE mkMintTokenNamePolicyV3 #-}
mkMintTokenNamePolicyV3 :: TokenName -> ScriptContext -> Bool
mkMintTokenNamePolicyV3 tn ctx = traceIfFalse "wrong token name" checkTokenName
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    -- TODO: Use builtin when available in PV3
    ownCurrencySymbol :: ScriptContext -> CurrencySymbol
    ownCurrencySymbol
      ScriptContext{scriptContextScriptInfo = MintingScript cs} = cs
    ownCurrencySymbol _ = traceError "Lh"

    checkTokenName :: Bool
    checkTokenName = valueOf (txInfoMint info) (ownCurrencySymbol ctx) tn > 0

-- Time range policy --

{-# INLINEABLE mkTimeRangePolicyV3 #-}
mkTimeRangePolicyV3 :: POSIXTime -> ScriptContext -> Bool
mkTimeRangePolicyV3 dl ctx = to dl `contains` range
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    range :: POSIXTimeRange
    range = txInfoValidRange info

-- Witness redeemer policy --

{-# INLINEABLE mkWitnessRedeemerPolicyV3 #-}
mkWitnessRedeemerPolicyV3 :: PubKeyHash -> ScriptContext -> Bool
mkWitnessRedeemerPolicyV3 pkh ctx =
  traceIfFalse "not signed by redeemer pubkeyhash" checkWitness
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    -- TODO: Use builtin when available in PV3
    txSignedBy :: TxInfo -> PubKeyHash -> Bool
    txSignedBy TxInfo{txInfoSignatories} k =
      case find ((==) k) txInfoSignatories of
        Just _ -> True
        Nothing -> False

    checkWitness :: Bool
    checkWitness = txSignedBy info pkh
