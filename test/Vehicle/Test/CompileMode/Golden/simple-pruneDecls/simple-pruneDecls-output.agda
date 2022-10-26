-- WARNING: This file was generated automatically by Vehicle
-- and should not be modified manually!
-- Metadata
--  - Agda version: 2.6.2
--  - AISEC version: 0.1.0.1
--  - Time generated: ???

{-# OPTIONS --allow-exec #-}

open import Vehicle
open import Data.Integer as ℤ using (ℤ)
open import Data.Rational as ℚ using (ℚ)
open import Data.Fin as Fin using (Fin; #_)
open import Data.Vec.Functional renaming ([] to []ᵥ; _∷_ to _∷ᵥ_)

module simple-pruneDecls-output where

postulate g : Vector ℚ 2 → Vector ℚ 1

abstract
  p2 : ∀ (x : Vector ℚ 2) → g x (# 0) ℚ.≥ ℤ.+ 0 ℚ./ 1
  p2 = checkSpecification record
    { proofCache   = "/home/matthew/Code/AISEC/vehicle/proofcache.vclp"
    }
