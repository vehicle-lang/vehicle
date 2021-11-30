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
open import Data.Nat as ℕ using (ℕ)
open import Data.Rational as ℚ using (ℚ)
open import Data.Rational as ℝ using () renaming (ℚ to ℝ)
open import Data.Fin as Fin using (#_)
open import Data.List
open import Data.List.Relation.Unary.All as List

module andGate-output where

private
  VEHICLE_PROJECT_FILE = "TODO_projectFile"

andGate : Tensor ℝ (2 ∷ []) → Tensor ℝ (1 ∷ [])
andGate = evaluate record
  { projectFile = VEHICLE_PROJECT_FILE
  ; networkUUID = "TODO_networkUUID"
  }

Truthy : ℝ → Set
Truthy x = x ℝ.≥ 1 ℚ./ 2

Falsey : ℝ → Set
Falsey x = x ℝ.≤ 1 ℚ./ 2

ValidInput : Tensor ℝ (2 ∷ []) → Set
ValidInput x = List.All (λ (i : ℕ) → 0 ℝ.≤ x i × x i ℝ.≤ 1) (0 ∷ (1 ∷ []) : List ℕ)

CorrectOutput : Tensor ℝ (2 ∷ []) → Set
CorrectOutput x = let y = andGate x in (Truthy (x (# 0)) × Truthy (x (# 1)) → Truthy (y (# 0))) × ((Truthy (x (# 0)) × Falsey (x (# 1)) → Falsey (y (# 0))) × ((Falsey (x (# 0)) × Truthy (x (# 1)) → Falsey (y (# 0))) × (Falsey (x (# 0)) × Falsey (x (# 1)) → Falsey (y (# 0)))))

abstract
  andGateCorrect : ∀ (x : Tensor ℝ (2 ∷ [])) → ValidInput x → CorrectOutput x
  andGateCorrect = checkProperty record
    { projectFile  = VEHICLE_PROJECT_FILE
    ; propertyUUID = "TODO_propertyUUID"
    }