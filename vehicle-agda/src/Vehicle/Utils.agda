------------------------------------------------------------------------
-- Utility methods
------------------------------------------------------------------------
-- Should be pushed to the standard library

{-# OPTIONS --allow-exec #-}

open import Algebra.Core using (Op₂)
open import Level using (Level; 0ℓ)
open import Data.Bool.Base using (T; Bool; true; false; _∧_; if_then_else_; not; _∨_)
open import Data.Char.Properties as Char using (_≟_)
open import Data.String using (String; _++_; lines; toList)
open import Data.Integer.Base as ℤ using (ℤ; +_; -[1+_])
open import Data.Nat.Base as ℕ using (ℕ; suc)
open import Data.Fin using (Fin; zero; suc)
open import Data.Vec.Base using (Vec; []; _∷_)
open import Data.Vec.Functional as Vector using (Vector)
open import Data.Vec.Recursive using (_^_; toVec)
open import Data.Product using (_,_)
open import Data.Float.Base using (Float; _≤ᵇ_)
open import Data.List.Base using ([]; _∷_)
open import Data.List.Relation.Binary.Infix.Heterogeneous.Properties using (infix?)
open import Data.Rational.Base as ℚ using (ℚ; ↥_; ↧_)
open import Relation.Nullary using (does)
open import Relation.Binary.Core using (Rel)
open import Relation.Binary.TypeClasses

module Vehicle.Utils where

_⇒_ : Op₂ Bool
x ⇒ y = not x ∨ y
{-
_≤_ :  Rel Float _
x ≤ y = T (x ≤ᵇ y)
-}
_⊆_ : String → String → Bool
s ⊆ t = does (infix? Char._≟_ (toList s) (toList t))

infix  4 _ℤ<ᵇ_ _ℚ<ᵇ_

_ℤ<ᵇ_ : ℤ → ℤ → Bool
+ n ℤ<ᵇ + m = n ℕ.<ᵇ m
+ n ℤ<ᵇ -[1+ m ] = false
-[1+ n ] ℤ<ᵇ + m = true
-[1+ n ] ℤ<ᵇ -[1+ m ] = m ℕ.<ᵇ n

_ℚ<ᵇ_ : ℚ → ℚ → Bool
p ℚ<ᵇ q = (↥ p ℤ.* ↧ q) ℤ<ᵇ (↥ q ℤ.* ↧ p)


------------------------------------------------------------------------
-- Type-classes
------------------------------------------------------------------------

record HasAdd {a} (A : Set a) : Set a where
  constructor hasAdd
  infixl 6 _⊕_
  field
    _⊕_ : A → A → A

open HasAdd {{...}} public

record HasSub {a} (A : Set a) : Set a where
  constructor hasSub
  infixl 6 _⊖_
  field
    _⊖_ : A → A → A

open HasSub {{...}} public

------------------------------------------------------------------------
-- ℕ
------------------------------------------------------------------------

instance
  addNat : HasAdd ℕ
  addNat = hasAdd ℕ._+_

------------------------------------------------------------------------
-- ℚ
------------------------------------------------------------------------

instance
  subRat : HasSub ℚ
  subRat = hasSub ℚ._-_

  addRat : HasAdd ℚ
  addRat = hasAdd ℚ._+_

------------------------------------------------------------------------
-- Tensor
------------------------------------------------------------------------

foreachVector : ∀ {n} {A : Set} → (Fin n → A) → Vector A n
foreachVector v = v


open import Function.Nary.NonDependent
open import Data.Product.Nary.NonDependent
open import Data.Unit.Polymorphic using (tt)
open import Function using (const; _∘′_; _∘_)

uniformLevels : ∀ n (l : Level) → Levels n
uniformLevels n l = ltabulate n (const l)

uniformSets : ∀ n {a} → Set a → Sets n (uniformLevels n a)
uniformSets ℕ.zero A = tt
uniformSets (suc n) A = A , uniformSets n A

stabulate : ∀ n → (f : Fin n → Level) → (g : (i : Fin n) → Set (f i)) → Sets n (ltabulate n f)
stabulate ℕ.zero f g = _
stabulate (suc n) f g = g zero , stabulate n (f ∘′ suc) (λ u → g (suc u))

open import Relation.Binary.PropositionalEquality

substLevel : ∀ {a b} → a ≡ b → Set a → Set b
substLevel refl t = t

tabulate[0ℓ]≡0ℓ : ∀ n → ⨆ n (ltabulate n (const 0ℓ)) ≡ 0ℓ
tabulate[0ℓ]≡0ℓ ℕ.zero = refl
tabulate[0ℓ]≡0ℓ (suc n) = tabulate[0ℓ]≡0ℓ n

foreachNary : ∀ {n} → (Fin n → Set) → Set (⨆ n (ltabulate n (const 0ℓ)))
foreachNary {n} f = Product n (stabulate n (const 0ℓ) f)
