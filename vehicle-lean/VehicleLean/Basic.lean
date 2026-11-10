module

public import Mathlib.Analysis.SpecialFunctions.Log.Basic
public import Mathlib.Data.Holor
public import Mathlib.Order.CompleteLattice.Defs
public import Mathlib.Order.MinMax

public section

namespace HolorIndex

variable {ds : List ℕ} {d : ℕ}

instance : DecidableEq (HolorIndex ds) := Subtype.instDecidableEq

/-- Prepend a new dimension to a `HolorIndex`. -/
def cons (i : Fin d) (is : HolorIndex ds) : HolorIndex (d :: ds) :=
  ⟨i :: is.1, List.Forall₂.cons i.isLt is.2⟩

/-- The finite set of all `HolorIndex`s for a given list of dimensions. -/
def all_elems : (ds : List ℕ) → Finset (HolorIndex ds)
  | [] => {⟨[], .nil⟩}
  | d :: ds => Finset.image₂ cons (.univ : Finset (Fin d)) (all_elems ds)

instance : Fintype (HolorIndex ds) where
  elems := all_elems ds
  complete := by
    induction ds with
    | nil =>
      rintro ⟨is, ⟨h⟩⟩
      exact Finset.insert_eq_self.mp rfl
    | cons d ds ih =>
      rintro ⟨is, (h)⟩
      cases h with
      | @cons a _ l _ ha hl =>
        dsimp [all_elems]
        rw [Finset.image₂]
        let a' : Fin d := ⟨a, ha⟩
        have ha' : a' ∈ Finset.univ := Finset.mem_univ a'
        let l' : HolorIndex ds := ⟨l, hl⟩
        have hl' : l' ∈ all_elems ds := Finset.mem_coe.mp (ih l')
        exact Finset.mem_image.mpr ⟨⟨a', l'⟩, ⟨Finset.mk_mem_product ha' (ih l'), rfl⟩⟩

instance : Finite (HolorIndex ds) := by infer_instance

end HolorIndex

namespace Holor

variable {α β γ : Type} {ds : List ℕ}

section order

instance [LE α] : LE (Holor α ds) :=
  ⟨fun x y ↦ ∀ t, x t ≤ y t⟩

instance [LT α] : LT (Holor α ds) :=
  ⟨fun x y ↦ ∀ t, x t < y t⟩

instance [Preorder α] : Preorder (Holor α ds) :=
  inferInstanceAs <| Preorder (HolorIndex ds → α)

instance [PartialOrder α] : PartialOrder (Holor α ds) :=
  inferInstanceAs <| PartialOrder (HolorIndex ds → α)

@[to_dual]
instance [SemilatticeSup α] : SemilatticeSup (Holor α ds) where
  sup x y t := x t ⊔ y t
  le_sup_left _ _ _ := le_sup_left
  le_sup_right _ _ _ := le_sup_right
  sup_le _ _ _ hxy hyz t := sup_le (hxy t) (hyz t)

@[to_dual]
instance [SupSet α] : SupSet (Holor α ds) where
  sSup s t := SupSet.sSup (s.image fun x ↦ x t)

@[to_dual]
lemma isLUB_app_of_isLUB [LE α] {s : Set (Holor α ds)} {x : Holor α ds}
    (hsx : IsLUB s x) (t : HolorIndex ds) : IsLUB (s.image fun y ↦ y t) (x t) := by
  classical
  constructor
  · rintro _ ⟨y, hy, rfl⟩
    exact hsx.left hy t
  · intro a ha
    let z : Holor α ds := fun u ↦ if u = t then a else x u
    have hz : ∀ y ∈ s, y ≤ z := by
      intro y hy u
      by_cases h : u = t
      · subst u
        simp only [z, reduceIte]
        exact ha (s.mem_image_of_mem (fun w ↦ w t) hy)
      · have := hsx.left hy u
        simp only [h, reduceIte, ge_iff_le, z]
        exact hsx.left hy u
    simpa [z] using hsx.right hz t

@[to_dual]
instance [Preorder α] [OrderSupSet α] : OrderSupSet (Holor α ds) where
  isLUB_sSup_of_isLUB s x hsx := by
    have ht (t : HolorIndex ds) := OrderSupSet.isLUB_sSup_of_isLUB _ _ (isLUB_app_of_isLUB hsx t)
    constructor
    · intro y hy t
      refine (ht t).left (s.mem_image_of_mem _ hy)
    · intro y hy t
      apply (ht t).right
      rintro x ⟨z, hz, rfl⟩
      exact hy hz t

@[to_dual]
instance [CompleteSemilatticeSup α] : CompleteSemilatticeSup (Holor α ds) where
  isLUB_sSup s := by
    constructor
    · intro x hx t
      exact (isLUB_sSup (s.image fun y ↦ y t)).left ⟨x, hx, rfl⟩
    · intro x hx t
      apply (isLUB_sSup (s.image fun y ↦ y t)).right
      rintro y ⟨z, hz, rfl⟩
      exact hx hz t

instance [Lattice α] : Lattice (Holor α ds) :=
  inferInstanceAs <| Lattice (HolorIndex ds → α)

@[to_dual]
instance [Top α] : Top (Holor α ds) :=
  ⟨fun _ => ⊤⟩

@[to_dual]
instance [LE α] [OrderTop α] : OrderTop (Holor α ds) :=
  inferInstanceAs <| OrderTop (HolorIndex ds → α)

instance [LE α] [BoundedOrder α] : BoundedOrder (Holor α ds) :=
  inferInstanceAs <| BoundedOrder (HolorIndex ds → α)

instance [CompleteLattice α] : CompleteLattice (Holor α ds) where

end order

section HadamardMul

/-- The Hadamard product of two holors of the same type is the pointwise product. -/
def HadamardMul [Mul α] (x : Holor α ds) (y : Holor α ds) : Holor α ds := fun t ↦ x t * y t

@[inherit_doc] scoped infixl:70 " ⊙ " => HadamardMul

variable (x y z w : Holor α ds)

@[simp]
theorem hadamard_apply [Mul α] (t : HolorIndex ds) : (x ⊙ y) t = x t * y t := by rfl

theorem hadamard_comm [CommMagma α] : x ⊙ y = y ⊙ x :=
  funext fun _ ↦ mul_comm _ _

theorem hadamard_assoc [Semigroup α] : x ⊙ y ⊙ z = x ⊙ (y ⊙ z) :=
  funext fun _ ↦ Semigroup.mul_assoc _ _ _

theorem hadamard_add [Mul α] [Add α] [LeftDistribClass α] : x ⊙ (y + z) = x ⊙ y + x ⊙ z :=
  funext fun _ ↦ left_distrib _ _ _

theorem add_hadamard [Mul α] [Add α] [RightDistribClass α] : (y + z) ⊙ x = y ⊙ x + z ⊙ x :=
  funext fun _ ↦ right_distrib _ _ _

theorem smul_hadamard [Semigroup α] (k : α) : (k • x) ⊙ y = k • (x ⊙ y) :=
  funext fun _ ↦ Semigroup.mul_assoc _ _ _

theorem hadamard_smul [CommSemigroup α] (k : α) : x ⊙ (k • y) = k • (x ⊙ y) :=
  funext fun _ ↦ mul_left_comm _ _ _

@[simp]
theorem hadamard_zero [MulZeroClass α] : x ⊙ (0 : Holor α ds) = 0 :=
  funext fun _ ↦ MulZeroClass.mul_zero _

@[simp]
theorem zero_hadamard [MulZeroClass α] : (0 : Holor α ds) ⊙ x = 0 :=
  funext fun _ ↦ MulZeroClass.zero_mul _

theorem hadamard_mul_hadamard_comm [CommSemigroup α] :
    (mul x y) ⊙ (mul z w) = mul (x ⊙ z) (y ⊙ w) :=
  funext fun _ ↦ mul_mul_mul_comm _ _ _ _

end HadamardMul

section PointwiseOps

def pointwise (x : Holor α ds) (y : Holor β ds) (f : α → β → γ) : Holor γ ds :=
  fun t ↦ f (x t) (y t)

def pointwiseLE [LE α] [DecidableLE α] (x y : Holor α ds) : Holor Bool ds :=
  x.pointwise y (fun a b ↦ decide (a ≤ b))

def pointwiseLT [LT α] [DecidableLT α] (x y : Holor α ds) : Holor Bool ds :=
  x.pointwise y (fun a b ↦ decide (a < b))

def pointwiseEq [DecidableEq α] (x y : Holor α ds) : Holor Bool ds :=
  x.pointwise y BEq.beq

def pointwiseNe [DecidableEq α] (x y : Holor α ds) : Holor Bool ds :=
  x.pointwise y bne

def pointwiseAnd (x y : Holor Bool ds) : Holor Bool ds :=
  x.pointwise y and

def pointwiseOr (x y : Holor Bool ds) : Holor Bool ds :=
  x.pointwise y or

def pointwiseDiv [Div α] (x y : Holor α ds) : Holor α ds :=
  x.pointwise y Div.div

def pointwisePow [Pow α β] (x : Holor α ds) (y : Holor β ds) : Holor α ds :=
  x.pointwise y Pow.pow

noncomputable def pointwiseLog (x : Holor ℝ ds) : Holor ℝ ds :=
  fun t ↦ (x t).log

noncomputable def pointwiseExp (x : Holor ℝ ds) : Holor ℝ ds :=
  fun t ↦ (x t).exp

end PointwiseOps

section ReduceOps

def fold (op : α → α → α) [Std.Commutative op] [Std.Associative op] (init : α) {ds : List ℕ}
    (x : Holor α ds) : α :=
  Finset.univ.fold op init x

def reduceAnd (x : Holor Bool ds) : Bool := x.fold and true

def reduceOr (x : Holor Bool ds) : Bool := x.fold or false

def reduceAdd [AddCommMonoid α] (x : Holor α ds) : α :=
  ∑ t, x t

def reduceMin [LinearOrder α] [BoundedOrder α] (x : Holor α ds) : α :=
  x.fold min ⊤

def reduceMax [LinearOrder α] [BoundedOrder α] (x : Holor α ds) : α :=
  x.fold max ⊥

end ReduceOps

end Holor
