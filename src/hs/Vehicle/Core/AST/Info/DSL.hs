{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Vehicle.Core.AST.Info.DSL where

import Vehicle.Core.AST.Builtin ( Builtin(..) )
import Vehicle.Core.AST.Core
import Vehicle.Core.AST.DeBruijn
import Vehicle.Core.AST.Instance ()
import Vehicle.Core.AST.Info.Core ( Info(..), InfoAnn)
import Vehicle.Core.AST.Utils (annotation)
import Vehicle.Prelude

-- * DSL for writing kinds as info annotations

class DSL (sort :: Sort) where
  type Underlying (sort :: Sort) :: Sort
  infixl 3 `app`
  infixr 4 ~>
  con  :: Builtin (Underlying sort) -> Info name sort
  (~>) :: Info name sort -> Info name sort -> Info name sort
  app  :: Info name sort -> Info name sort -> Info name sort

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

resultOf :: Info name 'TYPE -> Info name 'TYPE
resultOf (Info (KApp _ (KApp _ (KCon _ KFun) _kArg) kRes)) = Info kRes
resultOf _ = error $ unlines
  [ "Incorrect kind annotation."
  , "Perhaps an error in kindOf?"
  , "Please report as a bug."
  ]

kFun, kType, kDim, kDimList :: (DSL sort, Underlying sort ~ 'KIND) => Info name sort
kFun     = con KFun
kType    = con KType
kDim     = con KDim
kDimList = con KDimList

tFun, tBool, tProp, tInt, tReal, tAdd, tCons :: (DSL sort, Underlying sort ~ 'TYPE) => Info name sort
tFun     = con TFun
tBool    = con TBool
tProp    = con TProp
tInt     = con TInt
tReal    = con TReal
tAdd     = con TAdd
tCons    = con TCons

tLitDim :: Integer -> Info name 'EXPR
tLitDim d = Info $ TLitDim (kDim :*: mempty) d

tTensor :: (DSL sort, Underlying sort ~ 'TYPE) => Info name sort -> Info name sort -> Info name sort
tTensor tDim tElem = con TTensor `app` tDim `app` tElem

tList :: (DSL sort, Underlying sort ~ 'TYPE) => Info name sort -> Info name sort
tList tElem = con TList `app` tElem

-- TODO figure out how to do this without horrible -1 hacks
tForall :: Info DeBruijn 'TYPE -> (Info DeBruijn 'EXPR -> Info DeBruijn 'EXPR) -> Info DeBruijn 'EXPR
tForall k f = Info quantBody
  where
    badBody   = f (Info (TVar (kType :*: mempty) (TIndex (-1))))
    body      = liftDeBruijn @'TYPE (BindingDepth (-1) 0) (unInfo badBody)
    quantBody = TForall (kType :*: mempty) (Just $ unInfo k) (TArg (kType :*: mempty) (TSymbol Machine)) body

-- |Return the kind for builtin types.
kindOf :: Builtin 'TYPE -> Info name 'TYPE
kindOf = \case
  TFun    -> kType ~> kType ~> kType
  TBool   -> kType
  TProp   -> kType
  TInt    -> kType
  TReal   -> kType
  TList   -> kType ~> kType
  TTensor -> kType ~> kDim ~> kType
  TAdd    -> kDim ~> kDim ~> kDim
  TCons   -> kDim ~> kDimList ~> kDimList

-- |Return the kind for builtin exprs.
typeOf :: Builtin 'EXPR -> Info DeBruijn 'EXPR
typeOf = \case
  EIf      -> tForall kType $ \t -> tProp ~> t ~> t
  EImpl    -> tProp ~> tProp ~> tProp
  EAnd     -> tProp ~> tProp ~> tProp
  EOr      -> tProp ~> tProp ~> tProp
  ENot     -> tProp ~> tProp
  ETrue    -> tProp
  EFalse   -> tProp
  EEq      -> tForall kType $ \t -> t ~> t ~> tProp
  ENeq     -> tForall kType $ \t -> t ~> t ~> tProp
  ELe      -> tForall kType $ \t -> t ~> t ~> tProp
  ELt      -> tForall kType $ \t -> t ~> t ~> tProp
  EGe      -> tForall kType $ \t -> t ~> t ~> tProp
  EGt      -> tForall kType $ \t -> t ~> t ~> tProp

  -- TODO need some sort of bounded quantification over int/real?
  EMul     -> tReal ~> tReal ~> tReal
  EDiv     -> tReal ~> tReal ~> tReal
  EAdd     -> tReal ~> tReal ~> tReal
  ESub     -> tReal ~> tReal ~> tReal
  ENeg     -> tReal ~> tReal

  ECons    -> tForall kType $ \t -> t ~> tList t ~> tList t
  EAt      -> tForall kType $ \t -> tForall kDim $ \d -> tTensor t d ~> tInt ~> t
  EAll     -> tForall kType $ \t -> tForall kDim $ \d -> tTensor t d ~> (t ~> tProp) ~> tProp
  EAny     -> tForall kType $ \t -> tForall kDim $ \d -> tTensor t d ~> (t ~> tProp) ~> tProp