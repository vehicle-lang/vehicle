
module Vehicle.Data.Tensor where

open import Level using (Level; 0ℓ)
open import Data.Bool using (Bool; true; false; _∧_; _∨_)
open import Data.Empty.Polymorphic using (⊥)
open import Data.Nat.Base using (ℕ; zero; suc)
open import Data.List.Base using (List; []; _∷_; tabulate; concat; foldr)
open import Data.Vec.Functional using (Vector)
open import Function.Base using (id; _$_)
import Data.Vec.Functional as Vec
import Data.Vec.Functional.Relation.Binary.Pointwise as Vec
import Data.Vec.Functional.Relation.Binary.Pointwise.Properties as Vec
open import Data.Fin using (Fin)
import Data.Rational as ℚ
open import Data.Rational using (ℚ)
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

Tensor : Set a → Dimensions → Set a
Tensor A []       = A
Tensor A (d ∷ ds) = Vector (Tensor A ds) d

Pointwise : (A → B → Set p) → Tensor A ds → Tensor B ds → Set p
Pointwise {ds = []}      P xs ys = P xs ys
Pointwise {ds = d ∷ ds} P xs ys = Vec.Pointwise (Pointwise P) xs ys

refl : Reflexive R → ∀ ds → Reflexive (Pointwise {ds = ds} R)
refl R-refl []     = R-refl
refl {R = R} R-refl (d ∷ ds) = Vec.refl {R = Pointwise {ds = ds} R} (refl R-refl ds)

sym : Symmetric R → ∀ ds → Symmetric (Pointwise {ds = ds} R)
sym R-sym []     = R-sym
sym {R = R} R-sym (d ∷ ds) = Vec.sym {R = Pointwise {ds = ds} R} (sym R-sym ds)

trans : Transitive R → ∀ ds → Transitive (Pointwise {ds = ds} R)
trans R-trans [] = R-trans
trans {R = R} R-trans (d ∷ ds) = Vec.trans {R = Pointwise {ds = ds} R} (trans R-trans ds)

decidable : Decidable R → ∀ ds → Decidable (Pointwise {ds = ds} R)
decidable R? []        = R?
decidable R? (d ∷ ds) = Vec.decidable (decidable R? ds)

isEquivalence : IsEquivalence R → ∀ {ds} → IsEquivalence (Pointwise {ds = ds} R)
isEquivalence {R = R} isEq {ds} = record
  { refl = refl E.refl ds
  ; sym = sym E.sym ds
  ; trans = trans E.trans ds
  }
  where module E = IsEquivalence isEq

isDecEquivalence : IsDecEquivalence R → ∀ {ds} → IsDecEquivalence (Pointwise {ds = ds} R)
isDecEquivalence {R = R} isDecEq {ds} = record
  { isEquivalence = isEquivalence E.isEquivalence
  ; _≟_ = decidable E._≟_ ds
  }
  where module E = IsDecEquivalence isDecEq

stack : Vector (Tensor A ds) d → Tensor A (d ∷ ds)
stack = id

foreach : (Fin d → Tensor A ds) → Tensor A (d ∷ ds)
foreach f = f

const : A → (ds : Dimensions) → Tensor A ds
const v [] = v
const v (d ∷ ds) = λ i → const v ds

map : (A → B) → Tensor A ds → Tensor B ds
map {ds = []}      f xs = f xs
map {ds = d ∷ ds} f xs = λ i → map f (xs i)

zipWith : (A → B → C) → Tensor A ds → Tensor B ds → Tensor C ds
zipWith {ds = []}      f xs ys = f xs ys
zipWith {ds = d ∷ ds} f xs ys = λ i → zipWith f (xs i) (ys i)

toList : Tensor A ds → List A
toList {ds = []} x = x ∷ []
toList {ds = d ∷ ds} xs = concat (tabulate λ i → toList (xs i))

reduce : (A → B → B) → B → Tensor A ds → Tensor B []
reduce f e xs = foldr f e (toList xs)

infix 6 _!_

_!_ : Tensor A (d ∷ ds) → Fin d → Tensor A ds
_!_ = _$_

--------------------------------------------------------------------------------
-- Rational specialisations

infix  8 -_
infixl 7 _*_ _⊓_
infixl 6 _-_ _+_ _⊔_

_+_ : Tensor ℚ ds → Tensor ℚ ds → Tensor ℚ ds
_+_ = zipWith ℚ._+_

_-_ : Tensor ℚ ds → Tensor ℚ ds → Tensor ℚ ds
_-_ = zipWith ℚ._-_

_*_ : Tensor ℚ ds → Tensor ℚ ds → Tensor ℚ ds
_*_ = zipWith ℚ._*_

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

--------------------------------------------------------------------------------
-- Instances

instance
  subTensor : {{_ : HasSub A}} → HasSub (Tensor A ds)
  subTensor {{sub}} = hasSub (zipWith (_⊖_ {{sub}}))

  addTensor : ∀ {{_ : HasAdd A}} → HasAdd (Tensor A ds)
  addTensor {{add}} = hasAdd (zipWith (_⊕_ {{add}}))

  decEqTensor : ∀ {_≈_ : Rel A ℓ} {{_ : IsDecEquivalence _≈_}} → IsDecEquivalence (Pointwise {ds = ds} _≈_)
  decEqTensor {{isEq}} = isDecEquivalence isEq
