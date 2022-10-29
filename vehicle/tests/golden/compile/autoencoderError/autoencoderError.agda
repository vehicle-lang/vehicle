-- WARNING: This file was generated automatically by Vehicle
-- and should not be modified manually!
-- Metadata
--  - Agda version: 2.6.2
--  - AISEC version: 0.1.0
--  - Time generated: ???

{-# OPTIONS --allow-exec #-}

open import Vehicle
open import Vehicle.Utils
open import Vehicle.Data.Tensor
open import Data.Product
open import Data.Integer as ℤ using (ℤ)
open import Data.Rational as ℚ using (ℚ)
open import Data.Fin as Fin using (Fin; #_)
open import Data.List.Base

module autoencoderError-output where

postulate encode : Tensor ℚ (5 ∷ []) → Tensor ℚ (2 ∷ [])

postulate decode : Tensor ℚ (2 ∷ []) → Tensor ℚ (5 ∷ [])

epsilon : Tensor ℚ (5 ∷ [])
epsilon = λ (i : Fin 5) → ℤ.+ 1 ℚ./ 10

abstract
  identity : ∀ (x : Tensor ℚ (5 ∷ [])) → ∀ (i : Fin 5) → sub x epsilon i ℚ.≤ decode (encode x) i × decode (encode x) i ℚ.≤ add x epsilon i
  identity = checkSpecification record
    { proofCache   = "/home/matthew/Code/AISEC/vehicle/proofcache.vclp"
    }