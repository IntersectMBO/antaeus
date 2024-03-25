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

module PlutusScripts.BLS.SchnorrG2.Common where

import PlutusScripts.Helpers (
  bytesFromHex,
 )
import PlutusTx qualified
import PlutusTx.Builtins qualified as BI
import PlutusTx.Prelude qualified as P

data BlsParams = BlsParams
  { message :: P.BuiltinByteString
  , pubKey :: P.BuiltinByteString
  , signature :: (P.BuiltinByteString, P.BuiltinByteString)
  }
PlutusTx.unstableMakeIsData ''BlsParams

redeemerParams :: BlsParams
redeemerParams =
  BlsParams
    { message =
        P.toBuiltin $ bytesFromHex "2b71175d0486006a33f14bc4e1fe711a3d4a3a3549b230013240e8f80e54372f"
    , pubKey =
        P.toBuiltin $
          bytesFromHex
            ( "88370a4b4ddc627613b0396498fb068f1c1ff8f2aa6b946a9fc65f930d24394ddc45042e602094f6a88d49a8a037e781"
                <> "08dce014586ff5ff5744842f382e3917d180c7eb969585748d20ae8c6e07ca786e8da7ea2c7bdef5ae1becebe4da59ad"
            )
    , signature =
        ( P.toBuiltin $
            bytesFromHex
              ( "964851eb8823492c8720bf8c515b87043f5bab648000e63cfb6fc6fcdf6709061e0035c315cd23d239866471dea907d9"
                  <> "1568b69297dc8c4360f65d0bd399c2de19781c13bbf3a82ff1fcab8ac9f688ed96d6f2ea9a8ed057e76f0347d858ae22"
              )
        , P.toBuiltin $ bytesFromHex "2c5a22cb1e2fb77586c0c6908060b38107675a6277b8a61b1d6394a162af6718"
        )
    }

---- BLS Schnorr signature in G2 ----

{-
  * hash = Sha256(A || pk || msg)[..16]
  * pk_deser = G2Decompress(pk)
  * A_deser = G2Decompress(A)
  * r_deser = IntegerFromBytes(r)
  * Check that r_deser * G2Generator = A_deser + c * pk_deser
-}
{-# INLINEABLE verifySchnorrG2Script #-}
verifySchnorrG2Script
  :: P.BuiltinByteString
  -> BlsParams
  -> sc
  -> Bool
verifySchnorrG2Script bs16Null BlsParams{..} _sc = do
  let
    uncompressedG2 = P.bls12_381_G2_uncompress P.bls12_381_G2_compressed_generator
    a = P.fst signature
    r = P.snd signature
    c =
      BI.byteStringToInteger
        False
        ( P.sliceByteString
            0
            16
            (P.sha2_256 (a `P.appendByteString` pubKey `P.appendByteString` message))
            `P.appendByteString` bs16Null
        )
    pkDeser = P.bls12_381_G2_uncompress pubKey
    aDeser = P.bls12_381_G2_uncompress a
    rDeser = BI.byteStringToInteger False r
  (rDeser `P.bls12_381_G2_scalarMul` uncompressedG2)
    P.== (aDeser `P.bls12_381_G2_add` (c `P.bls12_381_G2_scalarMul` pkDeser))
    -- additional check using negation is for testing the function
    -- it can be removed to improve performance
    && (rDeser `P.bls12_381_G2_scalarMul` uncompressedG2)
      `P.bls12_381_G2_add` (P.bls12_381_G2_neg aDeser)
      `P.bls12_381_G2_equals` (c `P.bls12_381_G2_scalarMul` pkDeser)
