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
open import Data.Integer as ℤ using (ℤ)
open import Data.Rational as ℚ using (ℚ)
open import Data.Fin as Fin using (Fin; #_)
open import Data.List
open import Data.Vec.Functional
open import Relation.Binary.PropositionalEquality

module mnist-robustness-output where

Image : Set
Image = Tensor ℚ (28 ∷ (28 ∷ []))

Label : Set
Label = Fin 10

ValidImage : Image → Set
ValidImage x = ∀ (i : Fin 28) → ∀ (j : Fin 28) → ℤ.+ 0 ℚ./ 1 ℚ.≤ x i j × x i j ℚ.≤ ℤ.+ 1 ℚ./ 1

postulate mnist : Image → Vector ℚ 10

Advises : Image → (Label → Set)
Advises x i = ∀ (j : Fin 10) → j ≢ i → mnist x i ℚ.> mnist x j

postulate epsilon : ℚ

BoundedByEpsilon : Image → Set
BoundedByEpsilon x = ∀ (i : Fin 28) → ∀ (j : Fin 28) → ℚ.- epsilon ℚ.≤ x i j × x i j ℚ.≤ epsilon

RobustAround : Image → (Label → Set)
RobustAround image label = ∀ (pertubation : Vector (Vector ℚ 28) 28) → let perturbedImage = subVector image pertubation in BoundedByEpsilon pertubation × ValidImage perturbedImage → Advises perturbedImage label

postulate n : ℕ

postulate trainingImages : Vector Image n

postulate trainingLabels : Vector Label n

abstract
  robust : λ (i : Fin n) → RobustAround (trainingImages i) (trainingLabels i)
  robust = checkSpecification record
    { proofCache   = "/home/matthew/Code/AISEC/vehicle/proofcache.vclp"
    }