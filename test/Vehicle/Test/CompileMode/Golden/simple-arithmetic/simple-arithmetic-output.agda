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

module simple-arithmetic-output where

precedence : ℚ → (ℚ → (ℚ → ℚ))
precedence x y z = (x ℚ.+ (ℤ.+ 2 ℚ./ 1) ℚ.* y) ℚ.- z ℚ.÷ y
