-- WARNING: This file was generated automatically by Vehicle
-- and should not be modified manually!
-- Metadata
--  - Agda version: 2.6.2
--  - AISEC version: 0.1.0.1
--  - Time generated: ???

{-# OPTIONS --allow-exec #-}

open import Vehicle
open import Vehicle.Data.Tensor
open import Data.Integer as ℤ using (ℤ)
open import Data.Rational as ℚ using (ℚ)
open import Data.Fin as Fin using (Fin; #_)
open import Data.List
open import Data.Vec.Functional

module simple-tensor-output where

postulate f : Tensor ℚ (2 ∷ (2 ∷ [])) → Tensor ℚ (2 ∷ (2 ∷ []))

zeroD : Tensor ℚ []
zeroD = ℤ.+ 5 ℚ./ 2

oneD : Tensor ℚ (2 ∷ [])
oneD = zeroD ∷ (ℤ.+ 1 ℚ./ 1 ∷ [])

twoD : Tensor ℚ (2 ∷ (2 ∷ []))
twoD = oneD ∷ ((ℤ.+ 2 ℚ./ 1 ∷ (ℤ.+ 3 ℚ./ 1 ∷ [])) ∷ [])

lookup2D : ℚ
lookup2D = twoD (# 0) (# 1)

addition : Tensor ℚ (2 ∷ (2 ∷ []))
addition = addVector twoD twoD

subtraction : Tensor ℚ (2 ∷ (2 ∷ []))
subtraction = subVector twoD twoD

abstract
  property : ∀ (i : Fin 2) → ∀ (j : Fin 2) → addVector (f subtraction) addition i j ℚ.≥ ℤ.+ 0 ℚ./ 1
  property = checkSpecification record
    { proofCache   = "/home/matthew/Code/AISEC/vehicle/proofcache.vclp"
    }