-- WARNING: This file was generated automatically by Vehicle
-- and should not be modified manually!
-- Metadata
--  - Agda version: 2.6.2
--  - AISEC version: 0.1.0.1
--  - Time generated: ???

{-# OPTIONS --allow-exec #-}

open import Vehicle
open import Data.Unit
open import Data.Empty
open import Data.Product
open import Data.Nat as ℕ using (ℕ)
open import Data.Fin as Fin using (Fin; #_)
open import Data.List
open import Data.List.Relation.Unary.All as List
open import Function.Base
open import Relation.Binary.PropositionalEquality

module simple-let-temp-output where

abstract
  e1 : let x = ⊤ in let y = ⊥ in x × y
  e1 = checkSpecification record
    { proofCache   = "/home/matthew/Code/AISEC/vehicle/proofcache.vclp"
    }

abstract
  e2 : let y = # 1 in y Fin.≥ # 2
  e2 = checkSpecification record
    { proofCache   = "/home/matthew/Code/AISEC/vehicle/proofcache.vclp"
    }

e3 : ℕ
e3 = let y = 1 ℕ.+ 1 in y

lamLet : ℕ → ℕ
lamLet x = let y = x in y

abstract
  forallLet : ∀ (x : Fin 2) → let y = x in y ≡ # 1
  forallLet = checkSpecification record
    { proofCache   = "/home/matthew/Code/AISEC/vehicle/proofcache.vclp"
    }

abstract
  letForall : let y = # 1 in ∀ (x : Fin 2) → y ≡ x
  letForall = checkSpecification record
    { proofCache   = "/home/matthew/Code/AISEC/vehicle/proofcache.vclp"
    }

abstract
  forallInLet : List.All (λ (x : ℕ) → let y = x in y ≡ 1) (List ℕ ∋ 1 ∷ [])
  forallInLet = checkSpecification record
    { proofCache   = "/home/matthew/Code/AISEC/vehicle/proofcache.vclp"
    }

abstract
  letForallIn : let y = 1 in List.All (λ (x : ℕ) → y ≡ x) (List ℕ ∋ 1 ∷ [])
  letForallIn = checkSpecification record
    { proofCache   = "/home/matthew/Code/AISEC/vehicle/proofcache.vclp"
    }