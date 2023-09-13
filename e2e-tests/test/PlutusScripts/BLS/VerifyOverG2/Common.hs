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

module PlutusScripts.BLS.VerifyOverG2.Common where

import PlutusScripts.Helpers (bytesFromHex)
import PlutusTx qualified
import PlutusTx.Builtins qualified as BI
import PlutusTx.Prelude qualified as P

data BlsParams = BlsParams
  { message :: P.BuiltinByteString
  , pubKey :: P.BuiltinByteString
  , signature :: P.BuiltinByteString
  }
PlutusTx.unstableMakeIsData ''BlsParams

redeemerParams :: BlsParams
redeemerParams =
  BlsParams
    { message =
        BI.toBuiltin $ bytesFromHex "5032ec38bbc5da98ee0c6f568b872a65a08abf251deb21bb4b56e5d8821e68aa"
    , pubKey =
        BI.toBuiltin $
          bytesFromHex
            ( "b4953c4ba10c4d4196f90169e76faf154c260ed73fc77bb65dc3be31e0cec614a7287cda94195343676c2c57494f0e65"
                <> "1527e6504c98408e599a4eb96f7c5a8cfb85d2fdc772f28504580084ef559b9b623bc84ce30562ed320f6b7f65245ad4"
            )
    , signature =
        BI.toBuiltin $
          bytesFromHex
            ( "a9d4de7b0b2805fe52bccb86415ef7b8ffecb313c3c25404"
                <> "4dfc1bdc531d3eae999d87717822a052692140774bd7245c"
            )
    }

---- BLS signature with the public key over G2 ----

{-
  * pk_deser = G2Decompress(pk)
  * sig_deser = G1Decompress(sig)
  * hashed_msg = G1HashToCurve(msg, "BLS_SIG_BLS12381G2_XMD:SHA-256_SSWU_RO_NUL_")
  * Check that pairing(pk_deser, hashed_msg) = pairing(G1Generator, sig_deser)
-}
{-# INLINEABLE verifySigG2Script #-}
verifySigG2Script
  :: P.BuiltinByteString
  -> BlsParams
  -> sc
  -> Bool
verifySigG2Script dst BlsParams{..} _sc = do
  let
    pkDeser = BI.bls12_381_G2_uncompress pubKey
    sigDeser = BI.bls12_381_G1_uncompress signature
    hashedMsg = BI.bls12_381_G1_hashToGroup message dst

  BI.bls12_381_finalVerify
    (BI.bls12_381_millerLoop hashedMsg pkDeser)
    (BI.bls12_381_millerLoop sigDeser P.bls12_381_G2_generator)
