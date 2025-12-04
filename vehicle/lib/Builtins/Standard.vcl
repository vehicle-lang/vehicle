-------------------------------------------
-- Builtins for the standard type-system --
-------------------------------------------
-- WARNING: as a builtin file, this has coercions disabled!

-----------
-- Types --
-----------

@builtin Unit : Type
@builtin Bool : Type
@builtin Nat : Type
@builtin Rat : Type
@builtin Index : Nat -> Type
@builtin List : Type -> Type
@builtin Vector : Type -> Nat -> Type
@builtin Tensor : Type -> List Nat -> Type

-- Implementation of the `:` syntax
typeAnn : forallT (t : Type) . t -> t
typeAnn t a = a

----------
-- List --
----------

-----------
-- Index --
-----------

-- Comparisons

@builtin eqIndex : Index d1 -> Index d2 -> Tensor Bool []
@builtin neIndex : Index d1 -> Index d2 -> Tensor Bool []
@builtin leIndex : Index d1 -> Index d2 -> Tensor Bool []
@builtin ltIndex : Index d1 -> Index d2 -> Tensor Bool []
@builtin geIndex : Index d1 -> Index d2 -> Tensor Bool []
@builtin gtIndex : Index d1 -> Index d2 -> Tensor Bool []

---------
-- Nat --
---------

-- Comparisons

@builtin eqNat : Nat -> Nat -> Tensor Bool []
@builtin neNat : Nat -> Nat -> Tensor Bool []
@builtin leNat : Nat -> Nat -> Tensor Bool []
@builtin ltNat : Nat -> Nat -> Tensor Bool []
@builtin geNat : Nat -> Nat -> Tensor Bool []
@builtin gtNat : Nat -> Nat -> Tensor Bool []

---------
-- Rat --
---------

-- Comparisons

@builtin eqRatTensorPointwise : Tensor Rat dims -> Tensor Rat dims -> Tensor Bool dims
@builtin neRatTensorPointwise : Tensor Rat dims -> Tensor Rat dims -> Tensor Bool dims
@builtin leRatTensorPointwise : Tensor Rat dims -> Tensor Rat dims -> Tensor Bool dims
@builtin ltRatTensorPointwise : Tensor Rat dims -> Tensor Rat dims -> Tensor Bool dims
@builtin geRatTensorPointwise : Tensor Rat dims -> Tensor Rat dims -> Tensor Bool dims
@builtin gtRatTensorPointwise : Tensor Rat dims -> Tensor Rat dims -> Tensor Bool dims

------------------
-- Type-classes --
------------------

-- HasCompare

record HasCompare t1 t2 t3 where
  { eqTC : t1 -> t2 -> t3
  , neTC : t1 -> t2 -> t3
  , leTC : t1 -> t2 -> t3
  , ltTC : t1 -> t2 -> t3
  , geTC : t1 -> t2 -> t3
  , gtTC : t1 -> t2 -> t3
  }

indexHasCompare : HasCompare (Index d1) (Index d2) (Tensor Bool [])
indexHasCompare = record
  { eqTC = eqIndex
  , neTC = neIndex
  , leTC = leIndex
  , ltTC = ltIndex
  , geTC = geIndex
  , gtTC = gtIndex
  }

natHasCompare : HasCompare Nat Nat (Tensor Bool [])
natHasCompare = record
  { eqTC = eqNat
  , neTC = neNat
  , leTC = leNat
  , ltTC = ltNat
  , geTC = geNat
  , gtTC = gtNat
  }

zeroDimRatTensorHasCompare : HasCompare (Tensor Rat []) (Tensor Rat []) (Tensor Bool [])
zeroDimRatTensorHasCompare = record
  { eqTC = eqRatTensorPointwise
  , neTC = neRatTensorPointwise
  , leTC = leRatTensorPointwise
  , ltTC = ltRatTensorPointwise
  , geTC = geRatTensorPointwise
  , gtTC = gtRatTensorPointwise
  }

nonZeroDimRatTensorHasCompare : HasCompare (Tensor Rat (d :: ds)) (Tensor Rat (d :: ds)) (Tensor Bool (d :: ds))
nonZeroDimRatTensorHasCompare = record
  { eqTC = eqRatTensorPointwise
  , neTC = neRatTensorPointwise
  , leTC = leRatTensorPointwise
  , ltTC = ltRatTensorPointwise
  , geTC = geRatTensorPointwise
  , gtTC = gtRatTensorPointwise
  }
