module Vehicle.Language.AST.BuiltinPatterns where

import Data.List.NonEmpty (NonEmpty(..))

import Vehicle.Language.AST.Builtin
import Vehicle.Language.AST.Core
import Vehicle.Language.AST.Provenance

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------
-- List

pattern ListType :: Provenance
                 -> Expr binder var
                 -> Expr binder var
pattern
  ListType ann tElem <- App ann (BuiltinContainerType _   List) [ExplicitArg _ tElem]
  where
  ListType ann tElem =  App ann (BuiltinContainerType ann List) [ExplicitArg ann tElem]

--------------------------------------------------------------------------------
-- Tensor

pattern TensorType :: Provenance
                   -> Expr binder var
                   -> Expr binder var
                   -> Expr binder var
pattern
  TensorType ann tElem tDims <-
    App ann (BuiltinContainerType _ Tensor)
      [ ExplicitArg _ tElem
      , ExplicitArg _ tDims ]
  where
  TensorType ann tElem tDims =
    App ann (BuiltinContainerType ann Tensor)
      [ ExplicitArg ann tElem
      , ExplicitArg ann tDims ]

--------------------------------------------------------------------------------
-- Tensor

pattern IndexType :: Provenance
                -> Expr binder var
                -> Expr binder var
pattern
  IndexType ann tSize <-
    App ann (Builtin _ Index)
      [ ExplicitArg _ tSize ]
  where
  IndexType ann tSize =
    App ann (Builtin ann Index)
      [ ExplicitArg ann tSize ]

--------------------------------------------------------------------------------
-- Numeric

pattern BuiltinNumericType :: Provenance -> NumericType -> Expr binder var
pattern BuiltinNumericType ann op = Builtin ann (NumericType op)

pattern NatType :: Provenance -> Expr binder var
pattern NatType ann = BuiltinNumericType ann Nat

pattern IntType :: Provenance -> Expr binder var
pattern IntType ann = BuiltinNumericType ann Int

pattern RatType :: Provenance -> Expr binder var
pattern RatType ann = BuiltinNumericType ann Rat

--------------------------------------------------------------------------------
-- Boolean

pattern BoolType :: Provenance -> Expr binder var
pattern BoolType ann = Builtin ann Bool

pattern AnnotatedBoolType :: Provenance -> Expr binder var -> Expr binder var
pattern AnnotatedBoolType ann t <-
  App ann (BoolType _)
    [ ImplicitArg _ t
    ]

--------------------------------------------------------------------------------
-- Container

pattern BuiltinContainerType :: Provenance -> ContainerType -> Expr binder var
pattern BuiltinContainerType ann op = Builtin ann (ContainerType op)

--------------------------------------------------------------------------------
-- Type classes
--------------------------------------------------------------------------------

pattern BuiltinTypeClass :: Provenance -> TypeClass -> Expr binder var
pattern BuiltinTypeClass ann tc = Builtin ann (TypeClass tc)

--------------------------------------------------------------------------------
-- Container type classes

pattern HasConLitsOfSizeExpr :: Provenance
                             -> Int
                             -> Expr binder var
                             -> Expr binder var
                             -> Expr binder var
pattern
  HasConLitsOfSizeExpr ann n tElem tCont <-
    App ann (BuiltinTypeClass _ (HasConLitsOfSize n))
      [ ExplicitArg _ tElem
      , ExplicitArg _ tCont
      ]
  where
  HasConLitsOfSizeExpr ann n tElem tCont =
    App ann (BuiltinTypeClass ann (HasConLitsOfSize n))
      [ ExplicitArg ann tElem
      , ExplicitArg ann tCont
      ]

pattern HasConOpsExpr :: Provenance
                      -> Expr binder var
                      -> Expr binder var
                      -> Expr binder var
pattern
  HasConOpsExpr ann tElem tCont <-
    App ann (BuiltinTypeClass _ HasConOps)
      [ ExplicitArg _ tElem
      , ExplicitArg _ tCont
      ]
  where
  HasConOpsExpr ann tElem tCont =
    App ann (BuiltinTypeClass ann HasConOps)
      [ ExplicitArg ann tElem
      , ExplicitArg ann tCont
      ]

--------------------------------------------------------------------------------
-- Natural type classes

pattern HasNatLitsUpToExpr :: Provenance
                           -> Int
                           -> Expr binder var
                           -> Expr binder var
pattern
  HasNatLitsUpToExpr ann n t <-
    App ann (BuiltinTypeClass _ (HasNatLitsUpTo n))
      [ ExplicitArg _ t
      ]
  where
  HasNatLitsUpToExpr ann n t =
    App ann (BuiltinTypeClass ann (HasNatLitsUpTo n))
      [ ExplicitArg ann t
      ]

--------------------------------------------------------------------------------
-- Integer type classes

pattern HasIntLitsExpr :: Provenance
                       -> Expr binder var
                       -> Expr binder var
pattern
  HasIntLitsExpr ann t <-
    App ann (BuiltinTypeClass _ HasIntLits)
      [ ExplicitArg _ t
      ]
  where
  HasIntLitsExpr ann t =
    App ann (BuiltinTypeClass ann HasIntLits)
      [ ExplicitArg ann t
      ]

--------------------------------------------------------------------------------
-- Rational type classes

pattern HasRatLitsExpr :: Provenance
                       -> Expr binder var
                       -> Expr binder var
pattern
  HasRatLitsExpr ann t <-
    App ann (BuiltinTypeClass _ HasRatLits)
      [ ExplicitArg _ t
      ]
  where
  HasRatLitsExpr ann t =
    App ann (BuiltinTypeClass ann HasRatLits)
      [ ExplicitArg ann t
      ]

--------------------------------------------------------------------------------
-- Arithmetic ops

pattern HasArithOpExpr :: Provenance
                       -> TypeClass
                       -> Expr binder var
                       -> Expr binder var
pattern
  HasArithOpExpr ann tc t <-
    App ann (BuiltinTypeClass _ tc)
      [ ExplicitArg _ t
      ]
  where
  HasArithOpExpr ann tc t =
    App ann (BuiltinTypeClass ann tc)
      [ ExplicitArg ann t
      ]

pattern HasAddExpr :: Provenance -> Expr binder var -> Expr binder var
pattern HasAddExpr ann t = HasArithOpExpr ann HasAdd t

pattern HasSubExpr :: Provenance -> Expr binder var -> Expr binder var
pattern HasSubExpr ann t = HasArithOpExpr ann HasSub t

pattern HasMulExpr :: Provenance -> Expr binder var -> Expr binder var
pattern HasMulExpr ann t = HasArithOpExpr ann HasMul t

pattern HasDivExpr :: Provenance -> Expr binder var -> Expr binder var
pattern HasDivExpr ann t = HasArithOpExpr ann HasDiv t

pattern HasNegExpr :: Provenance -> Expr binder var -> Expr binder var
pattern HasNegExpr ann t = HasArithOpExpr ann HasNeg t

--------------------------------------------------------------------------------
-- HasOrder

pattern HasOrdExpr :: Provenance
                   -> Expr binder var
                   -> Expr binder var
pattern
  HasOrdExpr ann tElem <-
    App ann (BuiltinTypeClass _ HasOrd)
      [ ExplicitArg _ tElem
      ]
  where
  HasOrdExpr ann tElem =
    App ann (BuiltinTypeClass ann HasOrd)
      [ ExplicitArg ann tElem
      ]

--------------------------------------------------------------------------------
-- HasEq

pattern HasEqExpr :: Provenance
                  -> Expr binder var
                  -> Expr binder var
pattern
  HasEqExpr ann tElem <-
    App ann (BuiltinTypeClass _ HasEq)
      [ ExplicitArg _ tElem
      ]
  where
  HasEqExpr ann tElem =
    App ann (BuiltinTypeClass ann HasEq)
      [ ExplicitArg ann tElem
      ]

--------------------------------------------------------------------------------
-- Literals
--------------------------------------------------------------------------------

pattern LiteralExpr :: Provenance
                    -> Expr binder var
                    -> Expr binder var
                    -> Literal
                    -> Expr binder var
pattern
  LiteralExpr ann litType litTC lit <-
    App ann (Literal _ lit)
      [ ImplicitArg _ litType
      , InstanceArg _ (PrimDict _ litTC)
      ]
  where
  LiteralExpr ann litType litTC lit =
    App ann (Literal ann lit)
      [ ImplicitArg ann litType
      , InstanceArg ann (PrimDict ann litTC)
      ]

--------------------------------------------------------------------------------
-- Bool

pattern LitBool :: Provenance -> Bool -> Expr binder var
pattern LitBool ann n = Literal ann (LBool n)

pattern BoolLiteralExpr :: Provenance
                        -> Bool
                        -> Expr binder var
pattern BoolLiteralExpr ann value = LitBool ann value

pattern TrueExpr :: Provenance -> Expr binder var
pattern TrueExpr ann = BoolLiteralExpr ann True

pattern FalseExpr :: Provenance -> Expr binder var
pattern FalseExpr ann = BoolLiteralExpr ann False

--------------------------------------------------------------------------------
-- Nat

pattern LitNat :: Provenance -> Int -> Expr binder var
pattern LitNat ann n = Literal ann (LNat n)

pattern NatLiteralExpr :: Provenance
                       -> Expr binder var
                       -> Int
                       -> Expr binder var
pattern
  NatLiteralExpr ann t n <-
    LiteralExpr ann t HasNatLitsUpToExpr{} (LNat n)
  where
  NatLiteralExpr ann t n =
    LiteralExpr ann t (HasNatLitsUpToExpr ann n t) (LNat n)

--------------------------------------------------------------------------------
-- Int

pattern LitInt :: Provenance -> Int -> Expr binder var
pattern LitInt ann n = Literal ann (LInt n)

pattern IntLiteralExpr :: Provenance
                       -> Expr binder var
                       -> Int
                       -> Expr binder var
pattern
  IntLiteralExpr ann t n <-
    LiteralExpr ann t (HasIntLitsExpr _ _) (LInt n)
  where
  IntLiteralExpr ann t n =
    LiteralExpr ann t (HasIntLitsExpr ann t) (LInt n)

--------------------------------------------------------------------------------
-- Rat

pattern LitRat :: Provenance -> Rational -> Expr binder var
pattern LitRat ann n = Literal ann (LRat n)

pattern RatLiteralExpr :: Provenance
                       -> Expr binder var
                       -> Rational
                       -> Expr binder var
pattern
  RatLiteralExpr ann t n <-
    LiteralExpr ann t (HasRatLitsExpr _ _) (LRat n)
  where
  RatLiteralExpr ann t n =
    LiteralExpr ann t (HasRatLitsExpr ann t) (LRat n)

--------------------------------------------------------------------------------
-- Expressions
--------------------------------------------------------------------------------
-- Quantifier

pattern BuiltinQuantifier :: Provenance -> Quantifier -> Expr binder var
pattern BuiltinQuantifier ann q = Builtin ann (Quant q)

-- | Matches on `forall` and `exists`, but not `foreach`
pattern QuantifierExpr :: Quantifier
                       -> Provenance
                       -> Binder binder var
                       -> Expr   binder var
                       -> Expr   binder var
pattern
  QuantifierExpr q ann binder body <-
    App ann (BuiltinQuantifier _ q)
      [ ImplicitArg _ _
      , ExplicitArg _ (Lam _ binder body)
      ]
  where
  QuantifierExpr q ann binder body =
    App ann (BuiltinQuantifier ann q)
      [ ImplicitArg ann (typeOf binder)
      , ExplicitArg ann (Lam ann binder body)
      ]

pattern ExistsExpr :: Provenance
                   -> Binder binder var
                   -> Expr   binder var
                   -> Expr   binder var
pattern ExistsExpr ann binder body = QuantifierExpr Exists ann binder body

pattern ForallExpr :: Provenance
                   -> Binder binder var
                   -> Expr   binder var
                   -> Expr   binder var
pattern ForallExpr ann binder body = QuantifierExpr Forall ann binder body

pattern ForeachExpr :: Provenance
                    -> Binder binder var
                    -> Expr   binder var
                    -> Expr   binder var
pattern
  ForeachExpr ann binder body <-
    App ann (Builtin _ Foreach)
      [ ImplicitArg _ _
      , ImplicitArg _ _
      , ExplicitArg _ (Lam _ binder body)
      ]

--------------------------------------------------------------------------------
-- QuantifierIn

pattern BuiltinQuantifierIn :: Provenance -> Quantifier -> Expr binder var
pattern BuiltinQuantifierIn ann q = Builtin ann (QuantIn q)

-- | Matches on forallIn and existsIn but not foreachIn
pattern QuantifierInExpr :: Quantifier
                                 -> Provenance
                                 -> Expr   binder var
                                 -> Binder binder var
                                 -> Expr   binder var
                                 -> Expr   binder var
                                 -> Expr   binder var
pattern
  QuantifierInExpr q ann tCont binder body container <-
    App ann (BuiltinQuantifierIn _ q)
      [ ImplicitArg _ _
      , ImplicitArg _ tCont
      , InstanceArg _ _
      , ExplicitArg _ (Lam _ binder body)
      , ExplicitArg _ container
      ]
  where
  QuantifierInExpr q ann tCont binder body container =
    App ann (BuiltinQuantifierIn ann q)
      [ ImplicitArg ann (typeOf binder)
      , ImplicitArg ann tCont
      , InstanceArg ann (HasConOpsExpr ann (typeOf binder) tCont)
      , ExplicitArg ann (Lam ann binder body)
      , ExplicitArg ann container
      ]

pattern ForallInExpr :: Provenance
                     -> Expr   binder var
                     -> Binder binder var
                     -> Expr   binder var
                     -> Expr   binder var
                     -> Expr   binder var
pattern ForallInExpr ann tCont binder body container =
  QuantifierInExpr Forall ann tCont binder body container

pattern ExistsInExpr :: Provenance
                     -> Expr   binder var
                     -> Binder binder var
                     -> Expr   binder var
                     -> Expr   binder var
                     -> Expr   binder var
pattern ExistsInExpr ann tCont binder body container
  = QuantifierInExpr Exists ann tCont binder body container

pattern ForeachInExpr :: Provenance
                      -> Expr   binder var
                      -> Expr   binder var
                      -> Binder binder var
                      -> Expr   binder var
                      -> Expr   binder var
                      -> Expr   binder var
pattern
  ForeachInExpr ann dim tCont binder body container <-
    App ann (Builtin _ ForeachIn)
      [ ImplicitArg _ dim
      , ImplicitArg _ tCont
      , ExplicitArg _ (Lam _ binder body)
      , ExplicitArg _ container
      ]
  where
  ForeachInExpr ann dim tCont binder body container =
    App ann (Builtin ann ForeachIn)
      [ ImplicitArg ann dim
      , ImplicitArg ann tCont
      , ExplicitArg ann (Lam ann binder body)
      , ExplicitArg ann container
      ]

--------------------------------------------------------------------------------
-- IfExpr

pattern IfExpr :: Provenance
               -> Expr binder var
               -> [Arg binder var]
               -> Expr binder var
pattern
  IfExpr ann tRes args <-
    App ann (Builtin _ If)
      (  ImplicitArg _ tRes
      :| args
      )
  where
  IfExpr ann tRes args =
    App ann (Builtin ann If)
      (  ImplicitArg ann tRes
      :| args
      )

--------------------------------------------------------------------------------
-- BooleanOp2

pattern BooleanOp2Expr :: BooleanOp2
                       -> Provenance
                       -> NonEmpty (Arg  binder var)
                       -> Expr  binder var
pattern
  BooleanOp2Expr op ann explicitArgs <-
    App ann (Builtin _ (BooleanOp2 op)) explicitArgs
  where
  BooleanOp2Expr op ann explicitArgs =
    App ann (Builtin ann (BooleanOp2 op)) explicitArgs

pattern AndExpr :: Provenance -> NonEmpty (Arg  binder var) -> Expr binder var
pattern AndExpr ann explicitArgs <- BooleanOp2Expr And ann explicitArgs
  where AndExpr ann explicitArgs = BooleanOp2Expr And ann explicitArgs

pattern OrExpr :: Provenance -> NonEmpty (Arg  binder var) -> Expr binder var
pattern OrExpr ann explicitArgs <- BooleanOp2Expr Or ann explicitArgs
  where OrExpr ann explicitArgs = BooleanOp2Expr Or ann explicitArgs

pattern ImplExpr :: Provenance -> NonEmpty (Arg  binder var) -> Expr binder var
pattern ImplExpr ann explicitArgs <- BooleanOp2Expr Impl ann explicitArgs
  where ImplExpr ann explicitArgs = BooleanOp2Expr Impl ann explicitArgs

--------------------------------------------------------------------------------
-- Not

pattern NotExpr :: Provenance
                -> NonEmpty (Arg  binder var)
                -> Expr  binder var
pattern
  NotExpr ann explicitArgs <-
    App ann (Builtin _ Not) explicitArgs
  where
  NotExpr ann explicitArgs =
    App ann (Builtin ann Not) explicitArgs

--------------------------------------------------------------------------------
-- NumericOp2

pattern NumericOp2Expr :: NumericOp2
                       -> Provenance
                       -> Expr  binder var
                       -> Expr  binder var
                       -> [Arg  binder var]
                       -> Expr  binder var
pattern
  NumericOp2Expr op ann t tc explicitArgs <-
    App ann (Builtin _ (NumericOp2 op))
      (  ImplicitArg _ t
      :| InstanceArg _ tc
      :  explicitArgs
      )
  where
  NumericOp2Expr op ann t tc explicitArgs =
    App ann (Builtin ann (NumericOp2 op))
      (  ImplicitArg ann t
      :| InstanceArg ann tc
      :  explicitArgs
      )

pattern AddExpr :: Provenance -> Expr binder var -> Expr binder var -> [Arg binder var] -> Expr binder var
pattern AddExpr ann t tc explicitArgs <- NumericOp2Expr Add ann t tc explicitArgs
  where AddExpr ann t tc explicitArgs =  NumericOp2Expr Add ann t tc explicitArgs

pattern SubExpr :: Provenance -> Expr binder var -> Expr binder var -> [Arg binder var] -> Expr binder var
pattern SubExpr ann t tc explicitArgs <- NumericOp2Expr Sub ann t tc explicitArgs
  where SubExpr ann t tc explicitArgs =  NumericOp2Expr Sub ann t tc explicitArgs

pattern MulExpr :: Provenance -> Expr binder var -> Expr binder var -> [Arg binder var] -> Expr binder var
pattern MulExpr ann t tc explicitArgs <- NumericOp2Expr Mul ann t tc explicitArgs
  where MulExpr ann t tc explicitArgs =  NumericOp2Expr Mul ann t tc explicitArgs

pattern DivExpr :: Provenance -> Expr binder var -> Expr binder var -> [Arg binder var] -> Expr binder var
pattern DivExpr ann t tc explicitArgs <- NumericOp2Expr Div ann t tc explicitArgs
  where DivExpr ann t tc explicitArgs =  NumericOp2Expr Div ann t tc explicitArgs

--------------------------------------------------------------------------------
-- Not

pattern NegExpr :: Provenance
                -> Expr  binder var
                -> [Arg  binder var]
                -> Expr  binder var
pattern
  NegExpr ann t explicitArgs <-
    App ann (Builtin _ Neg)
      (  ImplicitArg _ t
      :| InstanceArg _ _
      :  explicitArgs
      )
  where
  NegExpr ann t explicitArgs =
    App ann (Builtin ann Neg)
      (  ImplicitArg ann t
      :| InstanceArg ann (PrimDict ann (HasNegExpr ann t))
      :  explicitArgs
      )

--------------------------------------------------------------------------------
-- Equality

pattern BuiltinEquality :: Provenance -> Equality -> Expr binder var
pattern BuiltinEquality ann eq = Builtin ann (Equality eq)

pattern EqualityExpr :: Equality
                     -> Provenance
                     -> Expr  binder var
                     -> [Arg  binder var]
                     -> Expr  binder var
pattern
  EqualityExpr eq ann tElem explicitArgs <-
    App ann (BuiltinEquality _ eq)
      (  ImplicitArg _ tElem
      :| InstanceArg _ _
      :  explicitArgs
      )
  where
  EqualityExpr eq ann tElem explicitArgs =
    App ann (BuiltinEquality ann eq)
      (  ImplicitArg ann tElem
      :| InstanceArg ann (PrimDict ann (HasEqExpr ann tElem))
      :  explicitArgs
      )

--------------------------------------------------------------------------------
-- Order

pattern BuiltinOrder :: Provenance -> Order -> Expr binder var
pattern BuiltinOrder ann order = Builtin ann (Order order)

pattern OrderExpr :: Order
                  -> Provenance
                  -> Expr  binder var
                  -> [Arg  binder var]
                  -> Expr  binder var
pattern
  OrderExpr order ann tElem explicitArgs <-
    App ann (BuiltinOrder _ order)
      (  ImplicitArg _ tElem
      :| InstanceArg _ _
      :  explicitArgs
      )
  where
  OrderExpr order ann tElem explicitArgs =
    App ann (BuiltinOrder ann order)
      (  ImplicitArg ann tElem
      :| InstanceArg ann (PrimDict ann (HasOrdExpr ann tElem))
      :  explicitArgs
      )

--------------------------------------------------------------------------------
-- Sequence

pattern SeqExpr :: Provenance
                -> Expr  binder var
                -> Expr  binder var
                -> [Expr binder var]
                -> Expr  binder var
pattern
  SeqExpr ann tElem tCont xs <-
    LSeq ann (PrimDict _ (HasConLitsOfSizeExpr _ _ tElem tCont)) xs
  where
  SeqExpr ann tElem tCont xs =
    LSeq ann (PrimDict ann (HasConLitsOfSizeExpr ann (length xs) tElem tCont)) xs

--------------------------------------------------------------------------------
-- Nil and cons

pattern NilExpr :: Provenance
                -> Expr binder var
                -> Expr binder var
pattern
  NilExpr ann tElem <-
    LSeq ann tElem []
  where
  NilExpr ann tElem =
    LSeq ann tElem []

pattern ConsExpr :: Provenance
                 -> Expr  binder var
                 -> [Arg  binder var]
                 -> Expr  binder var
pattern
  ConsExpr ann tElem explicitArgs <-
    App ann (Builtin _ Cons)
      (  ImplicitArg _ tElem
      :| explicitArgs
      )
  where
  ConsExpr ann tElem explicitArgs =
    App ann (Builtin ann Cons)
      (  ImplicitArg ann tElem
      :| explicitArgs
      )

--------------------------------------------------------------------------------
-- At

pattern AtExpr :: Provenance
                -> Expr  binder var
                -> Expr  binder var
                -> Expr  binder var
                -> [Arg binder var]
                -> Expr  binder var
pattern
  AtExpr ann tElem tDim tDims explicitArgs <-
    App ann (Builtin _ At)
      (  ImplicitArg _ tElem
      :| ImplicitArg _ tDim
      :  ImplicitArg _ tDims
      :  explicitArgs
      )
  where
  AtExpr ann tElem tDim tDims explicitArgs =
    App ann (Builtin ann At)
      (  ImplicitArg ann tElem
      :| ImplicitArg ann tDim
      :  ImplicitArg ann tDims
      :  explicitArgs
      )

--------------------------------------------------------------------------------
-- Sequence

pattern MapExpr :: Provenance
                -> Expr  binder var
                -> Expr  binder var
                -> [Arg  binder var]
                -> Expr  binder var
pattern
  MapExpr ann tTo tFrom explicitArgs <-
    App ann (Builtin _ Map)
      (  ImplicitArg _ tTo
      :| ImplicitArg _ tFrom
      :  explicitArgs
      )
  where
  MapExpr ann tTo tFrom explicitArgs =
    App ann (Builtin ann Map)
      (  ImplicitArg ann tTo
      :| ImplicitArg ann tFrom
      :  explicitArgs
      )

--------------------------------------------------------------------------------
-- Sequence

pattern FoldExpr :: Provenance
                 -> Expr  binder var
                 -> Expr  binder var
                 -> Expr  binder var
                 -> [Arg  binder var]
                 -> Expr  binder var
pattern
  FoldExpr ann tElem tCont tRes explicitArgs <-
    App ann (Builtin _ Fold)
      (  ImplicitArg _ tElem
      :| ImplicitArg _ tCont
      :  ImplicitArg _ tRes
      :  InstanceArg _ _
      :  explicitArgs
      )
  where
  FoldExpr ann tElem tCont tRes explicitArgs =
    App ann (Builtin ann Fold)
      (  ImplicitArg ann tElem
      :| ImplicitArg ann tCont
      :  ImplicitArg ann tRes
      :  InstanceArg ann (PrimDict ann (HasConOpsExpr ann tElem tCont))
      :  explicitArgs
      )

