-- editorconfig-checker-disable-file
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -O0 #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}

module PlutusScripts.BLS.Vrf.Common where

import PlutusScripts.BLS.Common (byteStringToInteger)
import PlutusTx qualified
import PlutusTx.Prelude qualified as P

{-# INLINEABLE vrfPrivKey #-}
vrfPrivKey :: Integer
vrfPrivKey = 50166937291276222007610100461546392414157570314060957244808461481762532157524 :: Integer

{-# INLINEABLE vrfMessage #-}
vrfMessage :: P.BuiltinByteString
vrfMessage = "I am a message"

data VrfProof = VrfProof
  { vrfProofGamma :: P.BuiltinBLS12_381_G2_Element
  , vrfProofC :: P.BuiltinByteString
  , vrfProofS :: Integer
  }
PlutusTx.unstableMakeIsData ''VrfProof

data VrfProofWithOutput = VrfProofWithOutput
  { vrfProofOutput :: P.BuiltinByteString
  , vrfProofProof :: VrfProof
  }
PlutusTx.unstableMakeIsData ''VrfProofWithOutput

data BlsParams = BlsParams
  { pubKey :: P.BuiltinBLS12_381_G2_Element
  , message :: P.BuiltinByteString
  , proofWithOutput :: VrfProofWithOutput
  }
PlutusTx.unstableMakeIsData ''BlsParams

{-# INLINEABLE redeemerParams #-}
redeemerParams :: BlsParams
redeemerParams =
  BlsParams
    { pubKey =
        P.bls12_381_G2_scalarMul vrfPrivKey (P.bls12_381_G2_uncompress P.bls12_381_G2_compressed_generator)
    , message = vrfMessage
    , proofWithOutput = generateVrfProofWithOutput vrfPrivKey vrfMessage
    }

---- Verify BLS VRF ----

{- | A basic VRF using G2 (using G1 would be cheaper)
  for reference see https://datatracker.ietf.org/doc/html/draft-irtf-cfrg-vrf-15#name-elliptic-curve-vrf-ecvrf
  and more readable and mathematical in nature see https://eprint.iacr.org/2017/099.pdf.
-}
{-# INLINEABLE verifyBlsVrfScript #-}
verifyBlsVrfScript
  :: BlsParams
  -> sc
  -> Bool
verifyBlsVrfScript (BlsParams pubKey message (VrfProofWithOutput beta (VrfProof gamma c s))) _sc = do
  let
    uncompressedG2 = P.bls12_381_G2_uncompress P.bls12_381_G2_compressed_generator
    -- cofactor of G2
    f :: Integer =
      305502333931268344200999753193121504214466019254188142667664032982267604182971884026507427359259977847832272839041692990889188039904403802465579155252111

    -- The proof of that the VRF hash of input alpha under our priv key is beta
    -- To verify a VRF hash given an
    --        input alpha
    --        output beta
    --        proof pi (gamma, c, s)
    --        pubkey pub
    -- do the following calculation
    u =
      P.bls12_381_G2_add
        (P.bls12_381_G2_scalarMul (byteStringToInteger c) pubKey)
        (P.bls12_381_G2_scalarMul s uncompressedG2)
    h = P.bls12_381_G2_hashToGroup message P.emptyByteString
    v =
      P.bls12_381_G2_add
        (P.bls12_381_G2_scalarMul (byteStringToInteger c) gamma)
        (P.bls12_381_G2_scalarMul s h)

  -- and check

  c
    P.== ( P.sha2_256
            P.. P.mconcat
            P.$ P.bls12_381_G2_compress
            P.<$> [uncompressedG2, h, pubKey, gamma, u, v]
         )
    P.&& beta
    P.== (P.sha2_256 P.. P.bls12_381_G2_compress P.$ P.bls12_381_G2_scalarMul f gamma)

-- used offchain to generate the vrf proof output
generateVrfProofWithOutput :: Integer -> P.BuiltinByteString -> VrfProofWithOutput
generateVrfProofWithOutput privKey message = do
  let
    uncompressedG2 = P.bls12_381_G2_uncompress P.bls12_381_G2_compressed_generator
    -- calculate public key
    pub = P.bls12_381_G2_scalarMul privKey uncompressedG2

    -- hash this msg to G2
    h = P.bls12_381_G2_hashToGroup message P.emptyByteString

    -- define first element of the proof of correct VRF
    gamma = P.bls12_381_G2_scalarMul privKey h
    -- gammaComp = Tx.bls12_381_G2_compress gamma

    -- for this signed hash with preimage alpha, define a ephemeral interger (for each signature take a new one)
    -- Random 32 byte int
    k = 108204667002115086588596846168569722098834602153875763359385781912495445631691 :: Integer

    -- define second element of the proof of correct VRF
    -- the paper notes that this can actually be truncated to 128 bits without loss of the 128 bits security.
    -- truncating this will allow for smaller proof sizes.
    c =
      P.sha2_256 . mconcat $
        P.bls12_381_G2_compress
          <$> [ uncompressedG2
              , h
              , pub
              , gamma
              , P.bls12_381_G2_scalarMul k uncompressedG2
              , P.bls12_381_G2_scalarMul k h
              ]

    -- define the third and last element of a proof of correct VRF
    s =
      (k - (byteStringToInteger c) * privKey)
        `P.modulo` 52435875175126190479447740508185965837690552500527637822603658699938581184513

    -- cofactor of G2
    f =
      305502333931268344200999753193121504214466019254188142667664032982267604182971884026507427359259977847832272839041692990889188039904403802465579155252111
        :: Integer

    -- create our VRF hash output
    beta = P.sha2_256 . P.bls12_381_G2_compress $ P.bls12_381_G2_scalarMul f gamma

  VrfProofWithOutput beta (VrfProof gamma c s)
