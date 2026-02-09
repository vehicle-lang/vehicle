/-
Vehicle standard library definitions for Lean 4
-/

import Mathlib.Data.List.Basic
import Mathlib.Data.Fintype.Basic
import Mathlib.Data.Real.Basic
import Mathlib.Tactic

namespace Vehicle

-- Universal quantification over a list
def forallInList {α : Type*} (f : α → Prop) (s : List α) : Prop :=
  s.all f

-- Existential quantification over a list
def existsInList {α : Type*} (f : α → Prop) (s : List α) : Prop :=
  s.any f

-- Universal quantification over an index
def forallIndex {n : ℕ} (f : Fin n → Prop) : Prop :=
  ∀ x : Fin n, f x

-- Existential quantification over an index
def existsIndex {n : ℕ} (f : Fin n → Prop) : Prop :=
  ∃ x : Fin n, f x

-- Create a tuple from a function
def foreachTuple {n : ℕ} {α : Type*} (f : Fin n → α) : Fin n → α := f

-- Reduce boolean tensor with conjunction
def reduceAnd {n : ℕ} (t : Fin n → Bool) : Bool :=
  (List.range n).all fun i => t ⟨i, by omega⟩

-- Reduce boolean tensor with disjunction
def reduceOr {n : ℕ} (t : Fin n → Bool) : Bool :=
  (List.range n).any fun i => t ⟨i, by omega⟩

-- Pointwise comparison with reduction
def pointwiseCompare (op : ℝ → ℝ → Prop) {n : ℕ} (xs ys : Fin n → ℝ) : Prop :=
  ∀ i : Fin n, op (xs i) (ys i)

-- Define comparison relations as reduced operations
def eqRatTensorReduced {n : ℕ} (xs ys : Fin n → ℝ) : Prop :=
  pointwiseCompare (· = ·) xs ys

def neRatTensorReduced {n : ℕ} (xs ys : Fin n → ℝ) : Prop :=
  pointwiseCompare (· ≠ ·) xs ys

def leRatTensorReduced {n : ℕ} (xs ys : Fin n → ℝ) : Prop :=
  pointwiseCompare (· ≤ ·) xs ys

def ltRatTensorReduced {n : ℕ} (xs ys : Fin n → ℝ) : Prop :=
  pointwiseCompare (· < ·) xs ys

def geRatTensorReduced {n : ℕ} (xs ys : Fin n → ℝ) : Prop :=
  pointwiseCompare (· ≥ ·) xs ys

def gtRatTensorReduced {n : ℕ} (xs ys : Fin n → ℝ) : Prop :=
  pointwiseCompare (· > ·) xs ys

end Vehicle
