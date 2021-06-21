{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Vehicle.Core.AST.Info where

import Vehicle.Core.AST.Builtin
import Vehicle.Core.AST.Core
import Vehicle.Core.AST.DeBruijn
import Vehicle.Core.AST.Instance ()
import Vehicle.Core.AST.Recursive
import Vehicle.Core.AST.Utils (annotation)
import Vehicle.Prelude


-- |Abstract syntax trees, where the representation of names and builtins is fixed.
type ATree (ann :: Sort -> *) (sort :: Sort)
  = Tree DeBruijn ann sort

type AKind ann = ATree ann 'KIND
type AType ann = ATree ann 'TYPE
type ATArg ann = ATree ann 'TARG
type AExpr ann = ATree ann 'EXPR
type AEArg ann = ATree ann 'EARG
type ADecl ann = ATree ann 'DECL
type AProg ann = ATree ann 'PROG

-- |Abstract syntax trees, where the representation of names and builtins is fixed.
type ATreeF (ann :: Sort -> *) (sort :: Sort) (sorted :: Sort -> *)
  = TreeF DeBruijn ann sort sorted

type AKindF ann sorted = ATreeF ann 'KIND sorted
type ATypeF ann sorted = ATreeF ann 'TYPE sorted
type ATArgF ann sorted = ATreeF ann 'TARG sorted
type AExprF ann sorted = ATreeF ann 'EXPR sorted
type AEArgF ann sorted = ATreeF ann 'EARG sorted
type ADeclF ann sorted = ATreeF ann 'DECL sorted
type AProgF ann sorted = ATreeF ann 'PROG sorted

-- |Type information, based on sort.
newtype Info (sort :: Sort) = Info { unInfo :: INFO sort }

-- |Computes type information based on sort; kinds for types, types for expressions.
type family INFO (sort :: Sort) where
  INFO 'KIND = ()
  INFO 'TYPE = AKind (Info :*: K Provenance)
  INFO 'TARG = AKind (Info :*: K Provenance)
  INFO 'EXPR = AType (Info :*: K Provenance)
  INFO 'EARG = AType (Info :*: K Provenance)
  INFO 'DECL = ()
  INFO 'PROG = ()

instance KnownSort sort => Eq (Info sort) where
  Info info1 == Info info2 =
    case sortSing @sort of
      SKIND -> info1 == info2
      STYPE -> info1 == info2
      STARG -> info1 == info2
      SEXPR -> info1 == info2
      SEARG -> info1 == info2
      SDECL -> info1 == info2
      SPROG -> info1 == info2

instance Semigroup (Info 'KIND) where
  Info () <> Info () = Info ()

instance Monoid (Info 'KIND) where
  mempty = Info ()

instance Semigroup (Info 'DECL) where
  Info () <> Info () = Info ()

instance Monoid (Info 'DECL) where
  mempty = Info ()

instance Semigroup (Info 'PROG) where
  Info () <> Info () = Info ()

instance Monoid (Info 'PROG) where
  mempty = Info ()


-- * DSL for writing kinds as info annotations

class DSL (sort :: Sort) where
  type Underlying (sort :: Sort) :: Sort
  infixl 3 `app`
  infixr 4 ~>
  con  :: Builtin (Underlying sort) -> Info sort
  (~>) :: Info sort -> Info sort -> Info sort
  app  :: Info sort -> Info sort -> Info sort

instance DSL 'TYPE where
  type Underlying 'TYPE = 'KIND
  con op      = Info (KCon mempty op)
  k1 ~> k2    = kFun `app` k1 `app` k2
  k1 `app` k2 = Info $ KApp mempty (unInfo k1) (unInfo k2)

instance DSL 'TARG where
  type Underlying 'TARG = 'KIND
  con op      = Info (KCon mempty op)
  k1 ~> k2    = kFun `app` k1 `app` k2
  k1 `app` k2 = Info $ KApp mempty (unInfo k1) (unInfo k2)

-- TODO remove duplication of instances
-- TODO make tRes top-level function

instance DSL 'EXPR where
  type Underlying 'EXPR = 'TYPE
  con op      = Info (TCon (kindOf op :*: mempty) op)
  k1 ~> k2    = tFun `app` k1 `app` k2
  k1 `app` k2 = Info $ TApp (tRes :*: mempty) (unInfo k1) (unInfo k2)
    where
      tRes = resultOf (ifst (annotation (unInfo k1)))

instance DSL 'EARG where
  type Underlying 'EARG = 'TYPE
  con op      = Info (TCon (kindOf op :*: mempty) op)
  k1 ~> k2    = tFun `app` k1 `app` k2
  k1 `app` k2 = Info $ TApp (tRes :*: mempty) (unInfo k1) (unInfo k2)
    where
      tRes = resultOf (ifst (annotation (unInfo k1)))

resultOf :: Info 'TYPE -> Info 'TYPE
resultOf (Info (KApp _ (KApp _ (KCon _ KFun) _kArg) kRes)) = Info kRes
resultOf _ = error $ unlines
  [ "Incorrect kind annotation."
  , "Perhaps an error in kindOf?"
  , "Please report as a bug."
  ]

kFun, kType, kDim, kDimList :: (DSL sort, Underlying sort ~ 'KIND) => Info sort
kFun     = con KFun
kType    = con KType
kDim     = con KDim
kDimList = con KDimList

tFun, tBool, tProp, tInt, tReal, tList, tTensor, tAdd, tCons :: (DSL sort, Underlying sort ~ 'TYPE) => Info sort
tFun     = con TFun
tBool    = con TBool
tProp    = con TProp
tInt     = con TInt
tReal    = con TReal
tList    = con TList
tTensor  = con TTensor
tAdd     = con TAdd
tCons    = con TCons

-- |Return the kind for builtin types.
kindOf :: Builtin 'TYPE -> Info 'TYPE
kindOf = \case
  TFun    -> kType ~> kType ~> kType
  TBool   -> kType
  TProp   -> kType
  TInt    -> kType
  TReal   -> kType
  TList   -> kType ~> kType
  TTensor -> kDim ~> kType ~> kType
  TAdd    -> kDim ~> kDim ~> kDim
  TCons   -> kDim ~> kDimList ~> kDimList

-- |Return the kind for builtin exprs.
typeOf :: Builtin 'EXPR -> Info 'EXPR
typeOf = \case
  EIf      -> tBool ~> tInt ~> tInt -- TODO need HM to get rid of "tInt"s everywhere
  EImpl    -> tBool ~> tBool ~> tBool
  EAnd     -> tBool ~> tBool ~> tBool
  EOr      -> tBool ~> tBool ~> tBool
  ENot     -> tBool ~> tBool
  ETrue    -> tBool
  EFalse   -> tBool
  EEq      -> tInt ~> tInt ~> tBool
  ENeq     -> tInt ~> tInt ~> tBool
  ELe      -> tInt ~> tInt ~> tBool
  ELt      -> tInt ~> tInt ~> tBool
  EGe      -> tInt ~> tInt ~> tBool
  EGt      -> tInt ~> tInt ~> tBool
  EMul     -> tInt ~> tInt ~> tInt
  EDiv     -> tInt ~> tInt ~> tInt
  EAdd     -> tInt ~> tInt ~> tInt
  ESub     -> tInt ~> tInt ~> tInt
  ENeg     -> tInt ~> tInt
  ECons    -> tInt ~> tList `app` tInt ~> tList `app` tInt
  EAt      -> tList `app` tInt ~> tInt ~> tInt
  EAll     -> tList `app` tInt ~> (tInt ~> tBool) ~> tBool
  EAny     -> tList `app` tInt ~> (tInt ~> tBool) ~> tBool