

module RationalUtils where

open import Algebra
import Algebra.Morphism.RingMonomorphism as RingMonomorphisms
open import Data.Rational as ℚ
open import Data.Rational.Properties
open import Data.Nat using (z≤n; s≤s)
open import Data.Integer using (+≤+; +<+; +_; -[1+_]; +[1+_]; +0)
open import Data.Product using (_×_; _,_)
open import Data.Sum
open import Function.Base using (_∘_; _∘′_)
open import Level using (0ℓ)
open import Relation.Binary
open import Relation.Binary.PropositionalEquality
open import Relation.Nullary using (yes; no)
open import Relation.Nullary.Negation using (contradiction)

open import Vehicle.Data.Tensor

open import Data.Maybe.Base using (just; nothing; decToMaybe)
open import Tactic.RingSolver.Core.AlmostCommutativeRing
open import Tactic.RingSolver.NonReflective (fromCommutativeRing +-*-commutativeRing (λ x → decToMaybe (0ℚ ℚ.≟ x)))

open ≤-Reasoning

private
  variable
    p q : ℚ

private
  module +-*-Monomorphism = RingMonomorphisms toℚᵘ-isRingMonomorphism-+-*

------------------------------------------------------------------------
-- Things to work out how to put in standard library

2ℚ = + 2 / 1
3ℚ = + 3 / 1

≰⇒≥ : _≰_ ⇒ _≥_
≰⇒≥ = <⇒≤ ∘′ ≰⇒>

p≤0⇒∣p∣≡-p : p ≤ 0ℚ → ∣ p ∣ ≡ - p
p≤0⇒∣p∣≡-p {mkℚ +[1+ n ] _ _} p≤0 = contradiction (nonPositive p≤0) λ()
p≤0⇒∣p∣≡-p {mkℚ +0       _ _} _   = refl
p≤0⇒∣p∣≡-p {mkℚ -[1+ n ] _ _} _   = refl

neg-involutive : Involutive _≡_ (-_)
neg-involutive (mkℚ +[1+ n ] _ _) = refl
neg-involutive (mkℚ +0       _ _) = refl
neg-involutive (mkℚ -[1+ n ] _ _) = refl

p≤∣p∣ : ∀ p → p ≤ ∣ p ∣
p≤∣p∣ p with 0ℚ ≤? p
... | yes 0≤p = ≤-reflexive (sym (0≤p⇒∣p∣≡p 0≤p))
... | no  0≰p = ≤-trans (≰⇒≥ 0≰p) (0≤∣p∣ p)

-p≤∣p∣ : ∀ p → - p ≤ ∣ p ∣
-p≤∣p∣ p with 0ℚ ≤? p
... | yes 0≤p = ≤-trans (neg-antimono-≤ 0≤p) (0≤∣p∣ p)
... | no  0≰p = ≤-reflexive (sym (p≤0⇒∣p∣≡-p (≰⇒≥ 0≰p)))

-p≤q⇒-q≤p : - p ≤ q → - q ≤ p
-p≤q⇒-q≤p {p} {q} = subst (- q ≤_) (neg-involutive p) ∘ neg-antimono-≤

p≤-q⇒q≤-p : p ≤ - q → q ≤ - p
p≤-q⇒q≤-p {p} {q} = subst (_≤ - p) (neg-involutive q) ∘ neg-antimono-≤

-p<q⇒-q<p : - p < q → - q < p
-p<q⇒-q<p {p} {q} = subst (- q <_) (neg-involutive p) ∘ neg-antimono-<

p<-q⇒q<-p : p < - q → q < - p
p<-q⇒q<-p {p} {q} = subst (_< - p) (neg-involutive q) ∘ neg-antimono-<

postulate p-[p+q]≡q : ∀ p q → p - (p + q) ≡ q

postulate p+q-p≡q : ∀ p q → p + q - p ≡ q

postulate p-q+q≡p : ∀ p q → p - q + q ≡ p

postulate *-monoʳ-≤ : ∀ r → Positive r → (r *_) Preserves _≤_ ⟶ _≤_

postulate p<r-q⇒p+q<r : ∀ p q r → p < r - q → p + q < r

∣p∣≤q⇒-q≤p≤q : ∀ p → ∣ p ∣ ≤ q → - q ≤ p × p ≤ q
∣p∣≤q⇒-q≤p≤q p ∣p∣≤q =
  -p≤q⇒-q≤p (≤-trans (-p≤∣p∣ p) ∣p∣≤q)
  , ≤-trans (p≤∣p∣ p) ∣p∣≤q

-p≤q≤p⇒∣q∣≤p : - p ≤ q → q ≤ p → ∣ q ∣ ≤ p
-p≤q≤p⇒∣q∣≤p {p} {q} -q≤p q≤p with ∣p∣≡p∨∣p∣≡-p q
... | inj₁ ∣q∣≡q  = subst (_≤ p) (sym ∣q∣≡q) q≤p
... | inj₂ ∣q∣≡-q = subst (_≤ p) (sym ∣q∣≡-q) (-p≤q⇒-q≤p -q≤p)

∣p∣<q⇒-q<p<q : ∀ p → ∣ p ∣ < q → - q < p × p < q
∣p∣<q⇒-q<p<q p ∣p∣<q =
  -p<q⇒-q<p (≤-<-trans (-p≤∣p∣ p) ∣p∣<q)
  , ≤-<-trans (p≤∣p∣ p) ∣p∣<q

-p<q<p⇒∣q∣<p : - p < q → q < p → ∣ q ∣ < p
-p<q<p⇒∣q∣<p {p} {q} -p<q q<p with ∣p∣≡p∨∣p∣≡-p q
... | inj₁ ∣q∣≡q  = subst (_< p) (sym ∣q∣≡q) q<p
... | inj₂ ∣q∣≡-q = subst (_< p) (sym ∣q∣≡-q) (-p<q⇒-q<p -p<q)

p+q-q≡p : ∀ p q → p + q - q ≡ p
p+q-q≡p p q = begin-equality
  p + q - q   ≡⟨ +-assoc p q (- q) ⟩
  p + (q - q) ≡⟨ cong (λ v → p + v) (+-inverseʳ q) ⟩
  p + 0ℚ      ≡⟨ +-identityʳ p ⟩
  p           ∎

+-isCommutativeSemigroup : IsCommutativeSemigroup _≡_ _+_
+-isCommutativeSemigroup = isCommutativeSemigroup
  where open IsCommutativeMonoid +-0-isCommutativeMonoid

+-commutativeSemigroup : CommutativeSemigroup 0ℓ 0ℓ
+-commutativeSemigroup = record
  { isCommutativeSemigroup = +-isCommutativeSemigroup
  }

2*p≡p+p : ∀ p → 2ℚ * p ≡ p + p
2*p≡p+p p = begin-equality
  2ℚ * p          ≡⟨⟩
  (1ℚ + 1ℚ) * p   ≡⟨ *-distribʳ-+ p 1ℚ 1ℚ ⟩
  1ℚ * p + 1ℚ * p ≡⟨ cong₂ _+_ (*-identityˡ p) (*-identityˡ p) ⟩
  p + p           ∎

2p+p≡3p : ∀ p → 2ℚ * p + p ≡ 3ℚ * p
2p+p≡3p p = begin-equality
  2ℚ * p + p      ≡⟨ cong ((2ℚ * p) ℚ.+_) (sym (*-identityˡ p)) ⟩
  2ℚ * p + 1ℚ * p ≡⟨ sym (*-distribʳ-+ p 2ℚ 1ℚ) ⟩
  (2ℚ + 1ℚ) * p   ∎
  where open ≤-Reasoning


lem1 : ∀ a b c d e f g → a + (b + c) + d + (e + f + f - g) - (e + f + f - g) ≡
                         c + (a + f + (e + b + d + f)) - g - (f + f + (e - g))
lem1 = solve 7 (λ a b c d e f g → (a ⊕ (b ⊕ c) ⊕ d ⊕ (e ⊕ f ⊕ f ⊕ (⊝ g)) ⊕ (⊝ (e ⊕ f ⊕ f ⊕ (⊝ g))) ,
                                  c ⊕ (a ⊕ f ⊕ (e ⊕ b ⊕ d ⊕ f)) ⊕ (⊝ g) ⊕ (⊝ (f ⊕ f ⊕ (e ⊕ (⊝ g)))))) refl
