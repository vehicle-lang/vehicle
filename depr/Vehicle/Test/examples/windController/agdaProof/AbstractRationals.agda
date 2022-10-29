

module AbstractRationals where

open import Algebra
open import Data.Integer.Base using (+0)
open import Data.Maybe.Base using (just; nothing; decToMaybe)
open import Data.Rational.Base as ℚ public hiding (_+_; _*_; _-_)
open import Data.Rational.Properties as ℚ public
  using (module ≤-Reasoning; <⇒≤)
open import Relation.Binary
open import Relation.Binary.PropositionalEquality
open import Function.Base
open import Data.Product using (_×_; _,_)

open import Tactic.RingSolver.Core.AlmostCommutativeRing
open import Tactic.RingSolver.NonReflective (fromCommutativeRing ℚ.+-*-commutativeRing (λ x → decToMaybe (0ℚ ℚ.≟ x)))

open import RationalUtils as ℚ public using (2ℚ; 3ℚ)

infixl 7 _*_
infixl 6 _+_ _-_

abstract
  _+_ : ℚ → ℚ → ℚ
  _+_ = ℚ._+_

  _*_ : ℚ → ℚ → ℚ
  _*_ = ℚ._*_

  _-_ : ℚ → ℚ → ℚ
  _-_ = ℚ._-_

  +-eq : _+_ ≡ ℚ._+_
  +-eq = refl

  *-eq : _*_ ≡ ℚ._*_
  *-eq = refl

  neg-eq : _-_ ≡ ℚ._-_
  neg-eq = refl

  p+q-p≡q : ∀ p q → p + q - p ≡ q
  p+q-p≡q = ℚ.p+q-p≡q

  p+q-q≡p : ∀ p q → p + q - q ≡ p
  p+q-q≡p = ℚ.p+q-q≡p

  p-q+q≡p : ∀ p q → p - q + q ≡ p
  p-q+q≡p = ℚ.p-q+q≡p

  p-[p+q]≡q : ∀ p q → p - (p + q) ≡ q
  p-[p+q]≡q = ℚ.p-[p+q]≡q

  ∣p-q∣≤∣p∣+∣q∣ : ∀ p q → ∣ p - q ∣ ≤ ∣ p ∣ + ∣ q ∣
  ∣p-q∣≤∣p∣+∣q∣ = ℚ.∣p-q∣≤∣p∣+∣q∣

  ∣p∣≤q⇒-q≤p≤q : ∀ p {q} → ∣ p ∣ ≤ q → - q ≤ p × p ≤ q
  ∣p∣≤q⇒-q≤p≤q p = ℚ.∣p∣≤q⇒-q≤p≤q p

  -p<q<p⇒∣q∣<p : ∀ {p q} → - p < q → q < p → ∣ q ∣ < p
  -p<q<p⇒∣q∣<p = ℚ.-p<q<p⇒∣q∣<p

  2*p≡p+p : ∀ p → 2ℚ * p ≡ p + p
  2*p≡p+p = ℚ.2*p≡p+p

  +-assoc : Associative _≡_ _+_
  +-assoc = ℚ.+-assoc

  +-monoˡ-≤ : ∀ r → (_+ r) Preserves _≤_ ⟶ _≤_
  +-monoˡ-≤ = ℚ.+-monoˡ-≤

  +-monoʳ-≤ : ∀ r → (_+_ r) Preserves _≤_ ⟶ _≤_
  +-monoʳ-≤ = ℚ.+-monoʳ-≤

  +-mono-≤ : _+_ Preserves₂ _≤_ ⟶ _≤_ ⟶ _≤_
  +-mono-≤ = ℚ.+-mono-≤

  *-monoʳ-≤ : ∀ r → Positive r → (r *_) Preserves _≤_ ⟶ _≤_
  *-monoʳ-≤ = ℚ.*-monoʳ-≤

  ∣p+q∣≤∣p∣+∣q∣ : ∀ p q → ∣ p + q ∣ ≤ ∣ p ∣ + ∣ q ∣
  ∣p+q∣≤∣p∣+∣q∣ = ℚ.∣p+q∣≤∣p∣+∣q∣

  ∣p*q∣≡∣p∣*∣q∣ : ∀ p q → ∣ p * q ∣ ≡ ∣ p ∣ * ∣ q ∣
  ∣p*q∣≡∣p∣*∣q∣ = ℚ.∣p*q∣≡∣p∣*∣q∣

  p<r-q⇒p+q<r : ∀ p q r → p < r - q → p + q < r
  p<r-q⇒p+q<r = ℚ.p<r-q⇒p+q<r

  2p+p≡3p : ∀ p → 2ℚ * p + p ≡ 3ℚ * p
  2p+p≡3p p = begin-equality
    2ℚ * p + p      ≡⟨ cong (2ℚ * p +_) (sym (ℚ.*-identityˡ p)) ⟩
    2ℚ * p + 1ℚ * p ≡⟨ sym (ℚ.*-distribʳ-+ p 2ℚ 1ℚ) ⟩
    (2ℚ + 1ℚ) * p   ∎
    where open ≤-Reasoning


  lem1 : ∀ a b c d e f g → a + (b + c) + d + (e + f + f - g) - (e + f + f - g) ≡
                           c + (a + f + (e + b + d + f)) - g - (f + f + (e - g))
  lem1 = solve 7 (λ a b c d e f g → (a ⊕ (b ⊕ c) ⊕ d ⊕ (e ⊕ f ⊕ f ⊕ (⊝ g)) ⊕ (⊝ (e ⊕ f ⊕ f ⊕ (⊝ g))) ,
                                    c ⊕ (a ⊕ f ⊕ (e ⊕ b ⊕ d ⊕ f)) ⊕ (⊝ g) ⊕ (⊝ (f ⊕ f ⊕ (e ⊕ (⊝ g)))))) refl
