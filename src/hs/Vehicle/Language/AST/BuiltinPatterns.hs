{-# LANGUAGE OverloadedLists #-}

module Vehicle.Language.AST.BuiltinPatterns where

import Data.List.NonEmpty (NonEmpty(..))
import Vehicle.Language.AST.Builtin
import Vehicle.Language.AST.Core

-- Primed versions take `Arg` instead of `Expr`

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------
-- List

mkListType :: ann
           -> Expr binder var ann
           -> Expr binder var ann
mkListType ann tElem = App ann (BuiltinContainerType ann List) [ExplicitArg ann tElem]

--------------------------------------------------------------------------------
-- Tensor

mkTensor :: ann
         -> Expr binder var ann
         -> [Int]
         -> Expr binder var ann
mkTensor ann tElem dims =
  let listType = mkListType ann (Builtin ann (NumericType Nat)) in
  let dimExprs = fmap (Literal ann . LNat) dims in
  let dimList  = mkSeq ann (Builtin ann (NumericType Nat)) listType dimExprs in
  App ann (BuiltinContainerType ann Tensor) (fmap (ExplicitArg ann) [tElem, dimList])

--------------------------------------------------------------------------------
-- Numeric

pattern BuiltinNumericType :: ann -> NumericType -> Expr binder var ann
pattern BuiltinNumericType ann op = Builtin ann (NumericType op)

--------------------------------------------------------------------------------
-- Boolean

pattern BuiltinBooleanType :: ann -> BooleanType -> Expr binder var ann
pattern BuiltinBooleanType ann op = Builtin ann (BooleanType op)

--------------------------------------------------------------------------------
-- Container

pattern BuiltinContainerType :: ann -> ContainerType -> Expr binder var ann
pattern BuiltinContainerType ann op = Builtin ann (ContainerType op)

--------------------------------------------------------------------------------
-- Type classes
--------------------------------------------------------------------------------

pattern BuiltinTypeClass :: ann -> TypeClass -> Expr binder var ann
pattern BuiltinTypeClass ann tc = Builtin ann (TypeClass tc)

--------------------------------------------------------------------------------
-- IsTruth

mkIsTruth :: ann
          -> Expr binder var ann
          -> Expr binder var ann
mkIsTruth ann t = App ann (BuiltinTypeClass ann IsTruth)
  (fmap (ExplicitArg ann) [t])

--------------------------------------------------------------------------------
-- IsContainer

mkIsContainer :: ann
              -> Expr binder var ann
              -> Expr binder var ann
              -> Expr binder var ann
mkIsContainer ann tElem tCont = App ann (BuiltinTypeClass ann IsContainer)
  (fmap (ExplicitArg ann) [tElem, tCont])

--------------------------------------------------------------------------------
-- Literals
--------------------------------------------------------------------------------

mkLiteral' :: ann
           -> Literal
           -> Arg  binder var ann
           -> Arg  binder var ann
           -> Expr binder var ann
mkLiteral' ann lit t tc = App ann (Literal ann lit) [t, tc]

mkLiteral :: ann
          -> Literal
          -> Expr binder var ann
          -> Expr binder var ann
          -> Expr binder var ann
mkLiteral ann lit t tc = mkLiteral' ann lit
  (ImplicitArg ann t)
  (ImplicitArg ann (PrimDict tc))

--------------------------------------------------------------------------------
-- Bool

pattern LitBool :: ann -> Bool -> Expr binder var ann
pattern LitBool ann n = Literal ann (LBool n)

mkBoolLit :: ann -> Expr binder var ann -> Bool -> Expr binder var ann
mkBoolLit ann t b = mkLiteral ann (LBool b) t (PrimDict t)

--------------------------------------------------------------------------------
-- Nat

pattern LitNat :: ann -> Int -> Expr binder var ann
pattern LitNat ann n = Literal ann (LNat n)

mkNatLit :: ann -> Int -> Expr binder var ann
mkNatLit ann n = mkLiteral ann (LNat n) t (PrimDict t)
  where t = Builtin ann (NumericType Nat)

--------------------------------------------------------------------------------
-- Int

pattern LitInt :: ann -> Int -> Expr binder var ann
pattern LitInt ann n = Literal ann (LInt n)

mkIntLit :: ann -> Int -> Expr binder var ann
mkIntLit ann i = mkLiteral ann (LInt i) t (PrimDict t)
  where t = Builtin ann (NumericType Int)

--------------------------------------------------------------------------------
-- Rat

pattern LitRat :: ann -> Double -> Expr binder var ann
pattern LitRat ann n = Literal ann (LRat n)

mkRatLit :: ann -> Double -> Expr binder var ann
mkRatLit ann r = mkLiteral ann (LRat r) t (PrimDict t)
  where t = Builtin ann (NumericType Rat)

--------------------------------------------------------------------------------
-- Expressions
--------------------------------------------------------------------------------
-- Quantifier

pattern BuiltinQuantifier :: ann -> Quantifier -> Expr binder var ann
pattern BuiltinQuantifier ann q = Builtin ann (Quant q)

pattern QuantifierView :: Quantifier
                       -> ann
                       -> Binder binder var ann
                       -> Expr   binder var ann
                       -> Expr   binder var ann
pattern QuantifierView q ann b e <-
  App ann (BuiltinQuantifier _ q)
    [ ImplicitArg _ _
    , ExplicitArg _ (Lam _ b e)
    ]

mkQuantifier :: Quantifier
             -> ann
             -> Binder binder var ann
             -> Expr   binder var ann
             -> Expr   binder var ann
mkQuantifier q ann binder e =
  App ann (Builtin ann (Quant q))
    [ ImplicitArg ann (typeOf binder)
    , ExplicitArg ann (Lam ann binder e)
    ]

mkQuantifierSeq :: Quantifier
                -> ann
                -> [binder]
                -> Expr binder var ann
                -> Expr binder var ann
                -> Expr binder var ann
mkQuantifierSeq q ann names t body =
  foldl (\e name -> mkQuantifier q ann (ExplicitBinder ann name t) e) body names

--------------------------------------------------------------------------------
-- BooleanOp2

pattern BooleanOp2View :: BooleanOp2
                       -> ann
                       -> Expr  binder var ann
                       -> Expr  binder var ann
                       -> [Arg  binder var ann]
                       -> Expr  binder var ann
pattern BooleanOp2View op ann t tc explicitArgs <-
  App ann (Builtin _ (BooleanOp2 op))
    (  ImplicitArg _ t
    :| InstanceArg _ tc
    :  explicitArgs
    )

mkBoolOp2' :: BooleanOp2
           -> ann
           -> Arg binder var ann
           -> Arg binder var ann
           -> [Expr binder var ann]
           -> Expr binder var ann
mkBoolOp2' op ann t tc args = App ann (Builtin ann (BooleanOp2 op))
  (t :| tc : map (ExplicitArg ann) args)

mkBoolOp2 :: BooleanOp2
          -> ann
          -> Expr binder var ann
          -> [Expr binder var ann]
          -> Expr binder var ann
mkBoolOp2 op ann t = mkBoolOp2' op ann
  (ImplicitArg ann t)
  (InstanceArg ann (PrimDict (mkIsTruth ann t)))

--------------------------------------------------------------------------------
-- Not

mkNot' :: ann
       -> Arg binder var ann
       -> Arg binder var ann
       -> Expr binder var ann
       -> Expr binder var ann
mkNot' ann t tc e = App ann (Builtin ann Not) [t, tc, ExplicitArg ann e]

mkNot :: ann
      -> Expr binder var ann
      -> Expr binder var ann
      -> Expr binder var ann
mkNot ann t = mkNot' ann
  (ImplicitArg ann t)
  (InstanceArg ann (PrimDict (mkIsTruth ann t)))

--------------------------------------------------------------------------------
-- Equality

pattern BuiltinEquality :: ann -> Equality -> Expr binder var ann
pattern BuiltinEquality ann eq = Builtin ann (Equality eq)

mkEq' :: Equality
      -> ann
      -> Arg binder var ann
      -> Arg binder var ann
      -> Arg binder var ann
      -> [Expr binder var ann]
      -> Expr binder var ann
mkEq' eq ann tElem tRes tc args = App ann (BuiltinEquality ann eq)
  (tElem :| tRes : tc : map (ExplicitArg ann) args)

mkEq :: Equality
     -> ann
     -> Expr binder var ann
     -> Expr binder var ann
     -> [Expr binder var ann]
     -> Expr binder var ann
mkEq eq ann tElem tRes = mkEq' eq ann
  (ImplicitArg ann tElem)
  (ImplicitArg ann tRes)
  (InstanceArg ann (PrimDict (mkIsTruth ann tRes)))

--------------------------------------------------------------------------------
-- Order

pattern BuiltinOrder :: ann -> Order -> Expr binder var ann
pattern BuiltinOrder ann order = Builtin ann (Order order)

--------------------------------------------------------------------------------
-- Sequence

mkSeq' :: ann
       -> Arg binder var ann
       -> Arg binder var ann
       -> Arg binder var ann
       -> [Expr binder var ann]
       -> Expr binder var ann
mkSeq' ann tElem tCont tc xs = App ann (Seq ann xs) [tElem, tCont, tc]

mkSeq :: ann
      -> Expr binder var ann
      -> Expr binder var ann
      -> [Expr binder var ann]
      -> Expr binder var ann
mkSeq ann tElem tCont = mkSeq' ann
  (ImplicitArg ann tElem)
  (ImplicitArg ann tCont)
  (InstanceArg ann (PrimDict (mkIsContainer ann tElem tCont)))

--------------------------------------------------------------------------------
-- At

mkAt' :: ann
      -> Arg  binder var ann
      -> Arg  binder var ann
      -> [Expr binder var ann]
      -> Expr binder var ann
mkAt' ann tElem tDims args = App ann (Builtin ann At)
  (tElem :| tDims : map (ExplicitArg ann) args)

--------------------------------------------------------------------------------
-- Sequence

mkMap' :: ann
       -> Arg  binder var ann
       -> Arg  binder var ann
       -> [Expr binder var ann]
       -> Expr binder var ann
mkMap' ann tFrom tTo args = App ann (Builtin ann Map)
  (tFrom :| tTo : map (ExplicitArg ann) args)

--------------------------------------------------------------------------------
-- Sequence

mkFold' :: ann
        -> Arg  binder var ann
        -> Arg  binder var ann
        -> Arg  binder var ann
        -> Arg  binder var ann
        -> [Expr binder var ann]
        -> Expr binder var ann
mkFold' ann tElem tCont tRes tc args = App ann (Builtin ann Fold)
  (tElem :| tCont : tRes : tc : fmap (ExplicitArg ann) args)
