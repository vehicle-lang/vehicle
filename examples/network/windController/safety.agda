

module safety where

open import Algebra
open import Data.List
open import Data.List.Relation.Unary.All using (All; []; _∷_)
open import Data.Rational
open import Data.Rational.Properties
open import Data.Nat using (z≤n)
open import Data.Integer using (+≤+; +_)
open import Data.Vec using (_∷_)
open import Level using (0ℓ)
open import Relation.Binary.PropositionalEquality

open import Vehicle.Data.Tensor

open ≤-Reasoning

------------------------------------------------------------------------
-- Things to work out how to put in standard library

2ℚ = + 2 / 1
3ℚ = + 3 / 1

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

2*x≡x+x : ∀ p → 2ℚ * p ≡ p + p
2*x≡x+x p = begin-equality
  2ℚ * p          ≡⟨⟩
  (1ℚ + 1ℚ) * p   ≡⟨ *-distribʳ-+ p 1ℚ 1ℚ ⟩
  1ℚ * p + 1ℚ * p ≡⟨ cong₂ _+_ (*-identityˡ p) (*-identityˡ p) ⟩
  p + p           ∎

open import Algebra.Properties.CommutativeSemigroup +-commutativeSemigroup

------------------------------------------------------------------------
-- The controller

postulate controller : ℚ → ℚ → ℚ

postulate controller-lem : ∀ x y → ∣ x ∣ ≤ 3ℚ → ∣ controller x y + 2ℚ * x - y ∣ ≤ 2ℚ

------------------------------------------------------------------------
-- The model

record State : Set where
  no-eta-equality
  field
    windSpeed : ℚ
    position  : ℚ
    velocity  : ℚ

open State

initialState : State
initialState = record
  { windSpeed = 0ℚ
  ; position  = 0ℚ
  ; velocity  = 0ℚ
  }

nextState : ℚ → State → State
nextState changeInWindSpeed s = record
  { windSpeed = newWindSpeed
  ; position  = newPosition
  ; velocity  = velocity s + controller newPosition (position s)
  }
  where
  newWindSpeed = windSpeed s + changeInWindSpeed
  newPosition  = position s + velocity s + newWindSpeed

finalState : List ℚ → State
finalState xs = foldr nextState initialState xs

------------------------------------------------------------------------
-- Definition and proof of safety

stateSum : State → ℚ
stateSum s = position s + velocity s + windSpeed s

Safe : State → Set
Safe s = ∣ position s ∣ ≤ 3ℚ

Good : State → Set
Good s = ∣ stateSum s ∣ ≤ 2ℚ

LowWind : ℚ → Set
LowWind x = ∣ x ∣ ≤ 1ℚ

-- A state being good will imply the next state is safe

good⇒nextSafe : ∀ dw → ∣ dw ∣ ≤ 1ℚ → ∀ s → Good s → Safe (nextState dw s)
good⇒nextSafe dw ∣dw∣<1 s ∣Σ∣≤2 =  begin
  ∣ (position s + velocity s) + (windSpeed s + dw) ∣ ≡˘⟨ cong ∣_∣ (+-assoc (position s + velocity s) (windSpeed s) dw) ⟩
  ∣ (position s + velocity s) + windSpeed s + dw   ∣ ≤⟨ ∣p+q∣≤∣p∣+∣q∣ (stateSum s) dw ⟩
  ∣ position s + velocity s + windSpeed s ∣ + ∣ dw ∣ ≤⟨ +-mono-≤ ∣Σ∣≤2 ∣dw∣<1 ⟩
  2ℚ + 1ℚ                                            ≡⟨⟩
  3ℚ                                                 ∎

-- Initial state is both safe and good

initialState-safe : Safe initialState
initialState-safe = *≤* (+≤+ z≤n)

initialState-good : Good initialState
initialState-good = *≤* (+≤+ z≤n)

good⇒nextGood : ∀ dw → ∣ dw ∣ ≤ 1ℚ → ∀ s → Good s → Good (nextState dw s)
good⇒nextGood dw ∣dw∣≤1 s Σ≤2 =
  let
    s' = nextState dw s
    dv = controller (position s') (position s)
    s'-safe = good⇒nextSafe dw ∣dw∣≤1 s Σ≤2
  in
  begin
    ∣ position s' + velocity s' + windSpeed s' ∣                                 ≡⟨⟩
    ∣ position s' + (velocity s + dv) + windSpeed s' ∣                           ≡⟨ cong (λ v → ∣ v + windSpeed s' ∣) (x∙yz≈zx∙y (position s') (velocity s) dv) ⟩
    ∣ dv + position s' + velocity s + windSpeed s'   ∣                           ≡⟨ cong ∣_∣ (sym (p+q-q≡p (dv + position s' + velocity s + windSpeed s') (position s))) ⟩
    ∣ dv + position s' + velocity s + windSpeed s' + position s - position s ∣   ≡⟨ cong (λ v → ∣ v + position s - position s ∣) (+-assoc (dv + position s') (velocity s) (windSpeed s'))  ⟩
    ∣ dv + position s' + (velocity s + windSpeed s') + position s - position s ∣ ≡⟨ cong (λ v → ∣ v - position s ∣) (+-assoc (dv + position s') (velocity s + windSpeed s') (position s)) ⟩
    ∣ dv + position s' + (velocity s + windSpeed s' + position s) - position s ∣ ≡⟨ cong (λ v → ∣ dv + position s' + v - position s ∣) (xy∙z≈zx∙y (velocity s) (windSpeed s') (position s)) ⟩
    ∣ dv + position s' + (position s + velocity s + windSpeed s') - position s ∣ ≡⟨⟩
    ∣ dv + position s' + position s' - position s ∣                              ≡⟨ cong (λ v → ∣ v - position s ∣) (+-assoc dv (position s') (position s')) ⟩
    ∣ dv + (position s' + position s') - position s ∣                            ≡⟨ cong (λ v → ∣ dv + v - position s ∣) (sym (2*x≡x+x (position s'))) ⟩
    ∣ dv + 2ℚ * position s' - position s ∣                                       ≤⟨ controller-lem (position s') (position s) s'-safe ⟩
    2ℚ                                                                           ∎

finalState-good : ∀ xs → All LowWind xs → Good (finalState xs)
finalState-good []       []         = initialState-good
finalState-good (x ∷ xs) (px ∷ pxs) = good⇒nextGood x px (finalState xs) (finalState-good xs pxs)

finalState-safe : ∀ xs → All LowWind xs → Safe (finalState xs)
finalState-safe []       []         = initialState-safe
finalState-safe (x ∷ xs) (px ∷ pxs) = good⇒nextSafe x px (finalState xs) (finalState-good xs pxs)
