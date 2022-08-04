-- WARNING: This file was generated automatically by Vehicle
-- and should not be modified manually!
-- Metadata
--  - Agda version: 2.6.2
--  - AISEC version: 0.1.0.1
--  - Time generated: ???

{-# OPTIONS --allow-exec #-}

open import Vehicle
open import Vehicle.Data.Tensor
open import Data.Rational as ℚ using (ℚ)
open import Data.Fin as Fin using (Fin; #_)
open import Data.List
open import Data.Vec.Functional

module monotonicity-output where

postulate f : Tensor ℚ (1 ∷ []) → Tensor ℚ (1 ∷ [])

abstract
  monotonic : ∀ (x1 : ℚ) → ∀ (x2 : ℚ) → x1 ℚ.≤ x2 → f (x1 ∷ []) (# 0) ℚ.≤ f (x2 ∷ []) (# 0)
  monotonic = checkSpecification record
    { proofCache   = "/home/matthew/Code/AISEC/vehicle/proofcache.vclp"
    }