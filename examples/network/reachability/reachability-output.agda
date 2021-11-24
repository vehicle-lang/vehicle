-- WARNING: This file was generated automatically by Vehicle
-- and should not be modified manually!
-- Metadata
--  - Agda version: 2.6.2
--  - AISEC version: 0.1.0.1
--  - Time generated: ???

open import AISEC.Utils
open import Data.Real as ℝ using (ℝ)
open import Data.List
open import Relation.Binary.PropositionalEquality

module MyTestModule where

private
  VEHICLE_PROJECT_FILE = TODO/vehicle/path

f : Tensor ℝ (2 ∷ []) → Tensor ℝ (1 ∷ [])
f = evaluate record
  { projectFile = VEHICLE_PROJECT_FILE
  ; networkUUID = NETWORK_UUID
  }

abstract
  reachable : ∃ (x : Tensor ℝ (2 ∷ [])) → let y = f x in y 0 ≡ 0
  reachable = checkProperty record
    { projectFile  = VEHICLE_PROJECT_FILE
    ; propertyUUID = ????
    }