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

module PlutusScripts.BLS.SchnorrG1.Common where

import PlutusScripts.BLS.Common (byteStringToInteger)
import PlutusScripts.Helpers (
  bytesFromHex,
 )
import PlutusTx qualified
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
        P.toBuiltin $ bytesFromHex "0558db9aff738e5421439601e7f30e88b74f43b80c1d172b5d371ce0dc05c912"
    , pubKey =
        P.toBuiltin $
          bytesFromHex
            ( "b91cacee903a53383c504e9e9a39e57d1eaa6403d5d38fc9"
                <> "496e5007d54ca92d106d1059f09461972aa98514d07000ae"
            )
    , signature =
        ( P.toBuiltin $
            bytesFromHex
              "8477e8491acc1cfbcf675acf7cf6b92e027cad7dd604a0e8205703aa2cc590066c1746f89e10d492d0230e6620c29726"
        , P.toBuiltin $ bytesFromHex "4e908280c0100cfe53501171ffa93528b9e2bb551d1025decb4a5b416a0aee53"
        )
    }

---- BLS Schnorr signature in G1 ----

{-
  * c = Sha256(A || pk || msg)[..16]
  * pk_deser = G1Decompress(pk)
  * A_deser = G1Decompress(A)
  * r_deser = IntegerFromBytes(r)
  * Check that r_deser * G1Generator = A_deser + c * pk_deser
-}
{-# INLINEABLE verifySchnorrG1Script #-}
verifySchnorrG1Script
  :: P.BuiltinByteString
  -> BlsParams
  -> sc
  -> Bool
verifySchnorrG1Script bs16Null BlsParams{..} _sc = do
  let
    a = P.fst signature
    r = P.snd signature
    c =
      byteStringToInteger
        ( P.sliceByteString
            0
            16
            (P.sha2_256 (a `P.appendByteString` pubKey `P.appendByteString` message))
            `P.appendByteString` bs16Null
        )
    pkDeser = P.bls12_381_G1_uncompress pubKey
    aDeser = P.bls12_381_G1_uncompress a
    rDeser = byteStringToInteger r
  (rDeser `P.bls12_381_G1_scalarMul` P.bls12_381_G1_generator)
    `P.bls12_381_G1_equals` (aDeser `P.bls12_381_G1_add` (c `P.bls12_381_G1_scalarMul` pkDeser))
    -- additional check using negation is for testing the function
    -- it can be removed to improve performance
    && (rDeser `P.bls12_381_G1_scalarMul` P.bls12_381_G1_generator)
      `P.bls12_381_G1_add` (P.bls12_381_G1_neg aDeser)
      P.== (c `P.bls12_381_G1_scalarMul` pkDeser)
