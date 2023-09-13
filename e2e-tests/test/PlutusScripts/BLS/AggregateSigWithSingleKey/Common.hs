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

module PlutusScripts.BLS.AggregateSigWithSingleKey.Common where

import PlutusScripts.Helpers (
  bytesFromHex,
 )
import PlutusTx qualified
import PlutusTx.Prelude qualified as P

data BlsParams = BlsParams
  { messages :: [P.BuiltinByteString]
  , pubKey :: P.BuiltinByteString
  , aggregateSignature :: P.BuiltinByteString
  }
PlutusTx.unstableMakeIsData ''BlsParams

redeemerParams :: BlsParams
redeemerParams =
  BlsParams
    { messages =
        [ P.toBuiltin $ bytesFromHex "2ba037cdb63cb5a7277dc5d6dc549e4e28a15c70670f0e97787c170485829264"
        , P.toBuiltin $ bytesFromHex "ecbf14bddeb68410f423e8849e0ce35c10d20a802bbc3d9a6ca01c386279bf01"
        , P.toBuiltin $ bytesFromHex "e8f75f478cb0d159db767341602fa02d3e01c3d9aacf9b686eccf1bb5ff4c8fd"
        , P.toBuiltin $ bytesFromHex "21473e89d50f51f9a1ced2390c72ee7e37f15728e61d1fb2c8c839495e489052"
        , P.toBuiltin $ bytesFromHex "8c146d00fe2e1caec31b159fc42dcd7e06865c6fa5267c6ca9c5284e651e175a"
        , P.toBuiltin $ bytesFromHex "362f469b6e722347de959f76533315542ffa440d37cde8862da3b3331e53b60d"
        , P.toBuiltin $ bytesFromHex "73baeb620e63a2e646ea148974350aa337491e5f5fc087cb429173d1eeb74f5a"
        , P.toBuiltin $ bytesFromHex "73acc6c3d72b59b8bf5ab58cdcf76aa001689aac938a75b1bb25d77b5382898c"
        , P.toBuiltin $ bytesFromHex "4e73ba04bae3a083c8a2109f15b8c4680ae4ba1c70df5b513425349a77e95d3b"
        , P.toBuiltin $ bytesFromHex "565825a0227d45068e61eb90aa1a4dc414c0976911a52d46b39f40c5849e5abe"
        ]
    , pubKey =
        P.toBuiltin $
          bytesFromHex
            ( "97c919babda8d928d771d107a69adfd85a75cee2cedc4afa"
                <> "4c0a7e902f38b340ea21a701a46df825210dd6942632b46c"
            )
    , aggregateSignature =
        P.toBuiltin $
          bytesFromHex
            ( "b425291f423235b022cdd038e1a3cbdcc73b5a4470251634abb874c7585a3a05b8ea54ceb93286edb0e9184bf9a852a1"
                <> "138c6dd860e4b756c63dff65c433a6c5aa06834f00ac5a1a1acf6bedc44bd4354f9d36d4f20f66318f39116428fabb88"
            )
    }

---- Aggregate BLS signature with same key, different messages, with PK over G1 ----

{-
  * hashed_msg_i = G2HashToCurve(msg_i, "BLS_SIG_BLS12381G2_XMD:SHA-256_SSWU_RO_NUL_") for i in [1, 10]
  * pk_deser = G1Decompress(pk)
  * aggr_sig_deser = G2Decompress(aggr_sig)
  * aggr_msg = sum_{i\in[1,10]} hashed_msg_i
  * Check that pairing(pk_deser, aggr_msg) = pairing(G1Generator, aggr_sig_deser)
-}
{-# INLINEABLE aggregateSigSingleKeyG1 #-}
aggregateSigSingleKeyG1
  :: P.BuiltinByteString
  -> BlsParams
  -> sc
  -> Bool
aggregateSigSingleKeyG1 dst BlsParams{..} _sc = do
  let
    hashedMsgs = P.map (\x -> P.bls12_381_G2_hashToGroup x dst) messages
    pkDeser = P.bls12_381_G1_uncompress pubKey
    aggrSigDeser = P.bls12_381_G2_uncompress aggregateSignature
    aggrMsg = foldl1' P.bls12_381_G2_add hashedMsgs

  P.bls12_381_finalVerify
    (P.bls12_381_millerLoop pkDeser aggrMsg)
    (P.bls12_381_millerLoop P.bls12_381_G1_generator aggrSigDeser)
  where
    -- PlutusTx.Foldable has no foldl1
    foldl1' :: (a -> a -> a) -> [a] -> a
    foldl1' _ [] = P.traceError "foldr1: empty list"
    foldl1' _ [_] = P.traceError "foldr1: only one element in list"
    foldl1' f (x : xs) = P.foldl f x xs
