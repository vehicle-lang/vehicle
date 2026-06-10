
module Vehicle.Data.Tensor where

open import Level using (Level; 0ℓ)
open import Data.Bool using (Bool; true; false; _∧_; _∨_)
open import Data.Empty.Polymorphic using (⊥)
open import Data.Nat.Base using (ℕ; zero; suc)
open import Data.List.Base using (List; []; _∷_)
open import Data.Integer using (+_)
open import Data.Nat.ListAction using (product)
open import Data.Vec.Functional using (Vector)
open import Function.Base using (id; _$_)
import Data.Vec.Functional as Vec
import Data.Vec.Functional.Relation.Binary.Pointwise as VecPointwise
import Data.Vec.Functional.Relation.Binary.Pointwise.Properties as VecPointwise
open import Data.Fin.Base using (Fin; zero; suc; combine)
open import Data.Rational as ℚ using (ℚ)
open import Function.Base using (flip)
open import Vehicle.Utils
open import Relation.Binary
open import Relation.Binary.PropositionalEquality using (_≡_)

Dimension : Set
Dimension = ℕ

Dimensions : Set
Dimensions = List Dimension

private
  variable
    a p ℓ : Level
    A B C : Set a
    d : Dimension
    ds : Dimensions
    R : Rel A ℓ

data Tensor (A : Set a) (dims : Dimensions) : Set a where
  tensor : Vector A (product dims) → Tensor A dims

-- Tensor : Set a → Dimensions → Set a
-- Tensor A []       = A
-- Tensor A (d ∷ ds) = Vector (Tensor A ds) d
unTensor : Tensor A ds → Vector A (product ds)
unTensor (tensor xs) = xs

scalar : A → Tensor A []
scalar x = tensor (Vec.replicate 1 x)

unScalar : Tensor A [] → A
unScalar (tensor xs) = xs zero

Pointwise : (A → B → Set p) → Tensor A ds → Tensor B ds → Set p
Pointwise P (tensor xs) (tensor ys) = VecPointwise.Pointwise P xs ys

unscalarPointwise : ∀ {R : A → B → Set p} {xs ys} → Pointwise R xs ys → R (unScalar xs) (unScalar ys)
unscalarPointwise {xs = tensor _} {ys = tensor _} Rxsys = Rxsys zero

refl : Reflexive R → ∀ {ds} → Reflexive (Pointwise {ds = ds} R)
refl {R = R} R-refl {x = tensor _} = VecPointwise.refl {R = R} R-refl

sym : Symmetric R → ∀ {ds} → Symmetric (Pointwise {ds = ds} R)
sym {R = R} R-sym {x = tensor _} {y = tensor _} = VecPointwise.sym {R = R} R-sym

trans : Transitive R → ∀ {ds} → Transitive (Pointwise {ds = ds} R)
trans {R = R} R-trans {i = tensor _} {j = tensor _} {k = tensor _} = VecPointwise.trans {R = R} R-trans

decidable : Decidable R → ∀ {ds} → Decidable (Pointwise {ds = ds} R)
decidable R? (tensor x) (tensor y) = VecPointwise.decidable R? x y

isEquivalence : IsEquivalence R → ∀ {ds} → IsEquivalence (Pointwise {ds = ds} R)
isEquivalence {R = R} isEq {ds} = record
  { refl = refl E.refl
  ; sym = sym E.sym
  ; trans = trans E.trans
  }
  where module E = IsEquivalence isEq

isDecEquivalence : IsDecEquivalence R → ∀ {ds} → IsDecEquivalence (Pointwise {ds = ds} R)
isDecEquivalence {R = R} isDecEq {ds} = record
  { isEquivalence = isEquivalence E.isEquivalence
  ; _≟_ = decidable E._≟_
  }
  where module E = IsDecEquivalence isDecEq

stack : Vector (Tensor A ds) d → Tensor A (d ∷ ds)
stack xs = tensor (Vec.concat (Vec.map unTensor xs))

foreach : (Fin d → Tensor A ds) → Tensor A (d ∷ ds)
foreach = stack

const : A → (ds : Dimensions) → Tensor A ds
const v ds = tensor (Vec.replicate (product ds) v)

map : (A → B) → Tensor A ds → Tensor B ds
map f (tensor xs) = tensor (Vec.map f xs)

zipWith : (A → B → C) → Tensor A ds → Tensor B ds → Tensor C ds
zipWith f (tensor xs) (tensor ys) = tensor (Vec.zipWith f xs ys)

toList : Tensor A ds → List A
toList (tensor xs) = Vec.toList xs

reduce : (A → B → B) → B → Tensor A ds → Tensor B []
reduce f e (tensor xs) = scalar (Vec.foldr f e xs)

infix 6 _!_

_!_ : Tensor A (d ∷ ds) → Fin d → Tensor A ds
tensor xs ! i = tensor (λ j → xs (combine i j))

--------------------------------------------------------------------------------
-- Rational specialisations

infix  8 -_
infixl 7 _*_ _⊓_
infixl 6 _-_ _+_ _⊔_

natScalar : ℕ → Tensor ℚ []
natScalar n = scalar (+ n ℚ./ 1)

_+_ : Tensor ℚ ds → Tensor ℚ ds → Tensor ℚ ds
_+_ = zipWith ℚ._+_

_-_ : Tensor ℚ ds → Tensor ℚ ds → Tensor ℚ ds
_-_ = zipWith ℚ._-_

_*_ : Tensor ℚ ds → Tensor ℚ ds → Tensor ℚ ds
_*_ = zipWith ℚ._*_

_÷_ : (p q : Tensor ℚ []) → .⦃ _ : ℚ.NonZero (unScalar q) ⦄ → Tensor ℚ []
_÷_ p q = scalar (unScalar p ℚ.÷ unScalar q)

-_ : Tensor ℚ ds → Tensor ℚ ds
-_ = map (ℚ.-_)

_⊔_ : Tensor ℚ ds → Tensor ℚ ds → Tensor ℚ ds
_⊔_ = zipWith ℚ._⊔_

_⊓_ : Tensor ℚ ds → Tensor ℚ ds → Tensor ℚ ds
_⊓_ = zipWith ℚ._⊓_

reduceAnd : Tensor Bool ds → Tensor Bool []
reduceAnd = reduce _∧_ true

reduceOr : Tensor Bool ds → Tensor Bool []
reduceOr = reduce _∨_ false

-- Type operations

infix 4 _≋_ _≤_ _<_ _≥_ _>_

_≋_ : Tensor ℚ ds → Tensor ℚ ds → Set 0ℓ
xs ≋ ys = Pointwise {A = ℚ} _≡_ xs ys

_≤_ : Tensor ℚ ds → Tensor ℚ ds → Set 0ℓ
xs ≤ ys = Pointwise ℚ._≤_ xs ys

_<_ : Tensor ℚ ds → Tensor ℚ ds → Set 0ℓ
xs < ys = Pointwise ℚ._<_ xs ys

_≥_ : Tensor ℚ ds → Tensor ℚ ds → Set 0ℓ
xs ≥ ys = Pointwise ℚ._≥_ xs ys

_>_ : Tensor ℚ ds → Tensor ℚ ds → Set 0ℓ
xs > ys = Pointwise ℚ._>_ xs ys

-- Boolean pointwise operations

_≤ᵇ∙_ : Tensor ℚ ds → Tensor ℚ ds → Tensor Bool ds
xs ≤ᵇ∙ ys = zipWith ℚ._≤ᵇ_ xs ys

_<ᵇ∙_ : Tensor ℚ ds → Tensor ℚ ds → Tensor Bool ds
xs <ᵇ∙ ys = zipWith _ℚ<ᵇ_ xs ys

_≥ᵇ∙_ : Tensor ℚ ds → Tensor ℚ ds → Tensor Bool ds
xs ≥ᵇ∙ ys = zipWith (flip ℚ._≤ᵇ_) xs ys

_>ᵇ∙_ : Tensor ℚ ds → Tensor ℚ ds → Tensor Bool ds
xs >ᵇ∙ ys = zipWith (flip _ℚ<ᵇ_) xs ys

-- Boolean whole tensor operations

_≤ᵇ_ : Tensor ℚ ds → Tensor ℚ ds → Tensor Bool []
xs ≤ᵇ ys = reduceAnd (xs ≤ᵇ∙ ys)

_<ᵇ_ : Tensor ℚ ds → Tensor ℚ ds → Tensor Bool []
xs <ᵇ ys = reduceAnd (xs <ᵇ∙ ys)

_≥ᵇ_ : Tensor ℚ ds → Tensor ℚ ds → Tensor Bool []
xs ≥ᵇ ys = reduceAnd (xs ≥ᵇ∙ ys)

_>ᵇ_ : Tensor ℚ ds → Tensor ℚ ds → Tensor Bool []
xs >ᵇ ys = reduceAnd (xs >ᵇ∙ ys)
