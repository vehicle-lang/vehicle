
module Vehicle.Data.Tensor where

open import Level using (Level)
open import Data.Empty.Polymorphic using (⊥)
open import Data.Nat.Base using (ℕ; zero; suc)
open import Data.List.Base using (List; []; _∷_)
open import Data.Vec.Functional using (Vector)

private
  variable
    a : Level
    A : Set a
    n : ℕ

Tensor : Set a → List ℕ → Set a
Tensor A []           = A
Tensor A (n ∷ [])     = Vector A n
Tensor A (m ∷ n ∷ ns) = Vector (Tensor A (n ∷ ns)) m