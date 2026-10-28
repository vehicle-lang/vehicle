module

public import Mathlib.Data.Holor

public section

namespace Holor

variable {α : Type} {ds : List ℕ}

instance instPartialOrder [PartialOrder α] : PartialOrder (Holor α ds) where
  le x y := ∀ t, x t ≤ y t
  le_refl x t := le_refl (x t)
  le_trans x y z hxy hyz t := le_trans (hxy t) (hyz t)
  le_antisymm x y hxy hyx := funext fun t ↦ le_antisymm (hxy t) (hyx t)

end Holor
