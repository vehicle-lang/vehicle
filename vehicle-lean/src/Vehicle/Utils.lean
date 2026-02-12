/-
Vehicle Lean 4 companion library - Utility functions
-/

namespace Vehicle

universe u

-- Quantification functions for indices
def forallIndex {n : Nat} (p : Fin n → Prop) : Prop :=
  ∀ i : Fin n, p i

def existsIndex {n : Nat} (p : Fin n → Prop) : Prop :=
  ∃ i : Fin n, p i

-- Quantification functions for lists
def forallInList {α : Type u} (p : α → Prop) : List α → Prop
  | [] => True
  | a :: as => p a ∧ forallInList p as

def existsInList {α : Type u} (p : α → Prop) : List α → Prop
  | [] => False
  | a :: as => p a ∨ existsInList p as

end Vehicle
