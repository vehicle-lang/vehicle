-- WARNING: This file was generated automatically by Vehicle
-- and should not be modified manually!
-- Metadata
--  - Agda version: 2.6.2
--  - AISEC version: 0.1.0.1
--  - Time generated: ???

{-# OPTIONS --allow-exec #-}

open import Vehicle
open import Vehicle.Data.Tensor
open import Data.Product
open import Data.Integer as ℤ using (ℤ)
open import Data.Rational as ℚ using (ℚ)
open import Data.Bool as 𝔹 using (Bool; true; false; if_then_else_)
open import Data.Fin as Fin using (Fin; #_)
open import Data.List
open import Data.Vec.Functional
open import Relation.Nullary
open import Relation.Nullary.Decidable

module simple-if-temp-output where

postulate f : Tensor ℚ (1 ∷ []) → Tensor ℚ (1 ∷ [])

abstract
  prop1 : ∀ (x : ℚ) → if ⌊ x ℚ.>? ℤ.+ 0 ℚ./ 1 ⌋ then f (x ∷ []) (# 0) ℚ.> ℤ.+ 0 ℚ./ 1 else f (x ∷ []) (# 0) ℚ.≤ ℤ.+ 0 ℚ./ 1
  prop1 = checkSpecification record
    { proofCache   = "/home/matthew/Code/AISEC/vehicle/proofcache.vclp"
    }

abstract
  prop3 : ∃ λ (x : ℚ) → if ⌊ f (x ∷ []) (# 0) ℚ.>? ℤ.+ 0 ℚ./ 1 ⌋ then x ℚ.≥ ℤ.+ 0 ℚ./ 1 else x ℚ.< ℤ.+ 0 ℚ./ 1
  prop3 = checkSpecification record
    { proofCache   = "/home/matthew/Code/AISEC/vehicle/proofcache.vclp"
    }