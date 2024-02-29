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

module PlutusScripts.BLS.VerifyOverG1.Common where

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
        BI.toBuiltin $ bytesFromHex "3e00ef2f895f40d67f5bb8e81f09a5a12c840ec3ce9a7f3b181be188ef711a1e"
    , pubKey =
        BI.toBuiltin $
          bytesFromHex
            ( "aa04a34d4db073e41505ebb84eee16c0094fde9fa22ec974"
                <> "adb36e5b3df5b2608639f091bff99b5f090b3608c3990173"
            )
    , signature =
        BI.toBuiltin $
          bytesFromHex
            ( "808ccec5435a63ae01e10d81be2707ab55cd0dfc235dfdf9f70ad32799e42510d67c9f61d98a6578a96a76cf6f4c105d"
                <> "09262ec1d86b06515360b290e7d52d347e48438de2ea2233f3c72a0c2221ed2da5e115367bca7a2712165032340e0b29"
            )
    }

---- BLS signature with the public key over G1 ----
{-
  * pk_deser = G1Decompress(pk)
  * sig_deser = G2Decompress(sig)
  * hashed_msg = G2HashToCurve(msg, "BLS_SIG_BLS12381G2_XMD:SHA-256_SSWU_RO_NUL_")
  * Check that pairing(pk_deser, hashed_msg) = pairing(G1Generator, sig_deser)
-}
{-# INLINEABLE verifySigG1Script #-}
verifySigG1Script
  :: P.BuiltinByteString
  -> BlsParams
  -> sc
  -> Bool
verifySigG1Script dst BlsParams{..} _sc = do
  let
    pkDeser = BI.bls12_381_G1_uncompress pubKey
    sigDeser = BI.bls12_381_G2_uncompress signature
    hashedMsg = BI.bls12_381_G2_hashToGroup message dst

  BI.bls12_381_finalVerify
    (BI.bls12_381_millerLoop pkDeser hashedMsg)
    (BI.bls12_381_millerLoop (P.bls12_381_G1_uncompress P.bls12_381_G1_compressed_generator) sigDeser)
