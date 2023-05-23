------------------------------------------------------------------------
-- Utility methods
------------------------------------------------------------------------
-- Should be pushed to the standard library

{-# OPTIONS --allow-exec #-}

open import Algebra.Core using (Op₂)
open import Level using (Level)
open import Data.Bool.Base using (T; Bool; if_then_else_; not; _∨_)
open import Data.Char.Properties using (_≟_)
open import Data.String using (String; _++_; lines; toList)
open import Data.Nat.Base as ℕ using (ℕ; suc)
open import Data.Vec.Base using (Vec; []; _∷_)
open import Data.Vec.Functional as Vector using (Vector)
open import Data.Vec.Recursive using (_^_; toVec)
open import Data.Product using (_,_)
open import Data.Float.Base using (Float; _≤ᵇ_)
open import Data.List.Base using ([]; _∷_)
open import Data.List.Relation.Binary.Infix.Heterogeneous.Properties using (infix?)
open import Data.Unit.Base using (⊤; tt)
open import Data.Rational.Base as ℚ using (ℚ)
open import Relation.Nullary using (does)
open import Relation.Binary.Core using (Rel)

module Vehicle.Utils where

_⇒_ : Op₂ Bool
x ⇒ y = not x ∨ y

_≤_ :  Rel Float _
x ≤ y = T (x ≤ᵇ y)

_⊆_ : String → String → Bool
s ⊆ t = does (infix? _≟_ (toList s) (toList t))

record HasAdd {a} (A : Set a) : Set a where
  constructor hasAdd
  infixl 6 _⊕_
  field
    _⊕_ : A → A → A

open HasAdd {{...}} public

instance
  addNat : HasAdd ℕ
  addNat = hasAdd ℕ._+_

  addRat : HasAdd ℚ
  addRat = hasAdd ℚ._+_

  addVector : ∀ {a} {A : Set a} {{_ : HasAdd A}} {n} → HasAdd (Vector A n)
  addVector = hasAdd (Vector.zipWith _⊕_)



record HasSub {a} (A : Set a) : Set a where
  constructor hasSub
  infixl 6 _⊖_
  field
    _⊖_ : A → A → A

open HasSub {{...}} public

instance
  subRat : HasSub ℚ
  subRat = hasSub ℚ._-_

  subVector : ∀ {a} {A : Set a} {{_ : HasSub A}} {n} → HasSub (Vector A n)
  subVector = hasSub (Vector.zipWith _⊖_)
