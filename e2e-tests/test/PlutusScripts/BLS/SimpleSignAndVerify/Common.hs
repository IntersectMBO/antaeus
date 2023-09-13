{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# HLINT ignore "Use underscore" #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
-- Not using all CardanoEra
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module PlutusScripts.BLS.SimpleSignAndVerify.Common where

import PlutusTx qualified
import PlutusTx.Prelude qualified as P

data BlsParams = BlsParams
  { privKey :: Integer -- 32 bit private key
  , message :: P.BuiltinByteString
  }
PlutusTx.unstableMakeIsData ''BlsParams

redeemerParams :: BlsParams
redeemerParams =
  BlsParams
    { -- sha256 hash of the phrase "I am a secret key" as an Integer
      privKey = 50166937291276222007610100461546392414157570314060957244808461481762532157524
    , message = "I am a message"
    }

-- BLS 12 381 simple verify with private key minting policy --

{-# INLINEABLE verifyBlsSimpleScript #-}
verifyBlsSimpleScript
  :: BlsParams
  -> sc
  -> Bool
verifyBlsSimpleScript BlsParams{..} _sc = do
  let
    -- calculate public key
    pubKey = P.bls12_381_G1_scalarMul privKey P.bls12_381_G1_generator

    -- Hash this msg to the G2
    msgToG2 = P.bls12_381_G2_hashToGroup message P.emptyByteString

    -- Create signature artifact in G2 with private key
    sigma = P.bls12_381_G2_scalarMul privKey msgToG2

  -- verify the msg with signature sigma with the check e(g1,sigma)=e(pub,msgToG2)
  P.bls12_381_finalVerify
    (P.bls12_381_millerLoop P.bls12_381_G1_generator sigma)
    (P.bls12_381_millerLoop pubKey msgToG2)
