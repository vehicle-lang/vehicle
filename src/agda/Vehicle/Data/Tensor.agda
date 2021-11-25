
module Vehicle.Data.Tensor where

open import Level using (Level)
open import Data.Nat.Base using (ℕ; zero; suc)
open import Data.Vec.Base using (Vec; []; _∷_)
open import Data.Vec.Functional using (Vector)

private
  variable
    a : Level
    A : Set a
    n : ℕ

Tensor : Set a → Vec ℕ (suc n) → Set a
Tensor A (n ∷ [])     = Vector A n
Tensor A (m ∷ n ∷ ns) = Vector (Tensor A (n ∷ ns)) m