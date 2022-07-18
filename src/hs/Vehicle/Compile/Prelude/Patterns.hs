module Vehicle.Compile.Prelude.Patterns where

import Data.List.NonEmpty (NonEmpty(..))

import Vehicle.Language.AST

--------------------------------------------------------------------------------
-- Universes
--------------------------------------------------------------------------------

pattern TypeUniverse :: Provenance -> UniverseLevel -> Expr binder var
pattern TypeUniverse p l = Universe p (TypeUniv l)

pattern LinearityUniverse :: Provenance -> Expr binder var
pattern LinearityUniverse p = Universe p LinearityUniv

pattern PolarityUniverse :: Provenance -> Expr binder var
pattern PolarityUniverse p = Universe p PolarityUniv

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

pattern ConcreteIndexType :: Provenance
                          -> Int
                          -> Expr binder var
pattern
  ConcreteIndexType ann n <-
    IndexType ann (NatLiteralExpr _ _ n)
  where
  ConcreteIndexType ann n =
    IndexType ann (NatLiteralExpr ann (NatType ann) n)

--------------------------------------------------------------------------------
-- Numeric

pattern BuiltinNumericType :: Provenance -> NumericType -> Expr binder var
pattern BuiltinNumericType ann op = Builtin ann (NumericType op)

pattern NatType :: Provenance -> Expr binder var
pattern NatType ann = BuiltinNumericType ann Nat

pattern IntType :: Provenance -> Expr binder var
pattern IntType ann = BuiltinNumericType ann Int

--------------------------------------------------------------------------------
-- Rational

pattern RatType :: Provenance -> Expr binder var
pattern RatType ann = BuiltinNumericType ann Rat

-- | The annotated Bool type used only during type-checking
pattern AnnRatType :: Provenance
                   -> Expr binder var
                   -> Expr binder var
pattern
  AnnRatType ann lin <-
    App ann (RatType _)
      [ ImplicitArg _ lin
      ]
  where
  AnnRatType ann lin =
    App ann (RatType ann)
      [ ImplicitArg ann lin
      ]

--------------------------------------------------------------------------------
-- Boolean

pattern BoolType :: Provenance -> Expr binder var
pattern BoolType ann = Builtin ann Bool

-- | The annotated Bool type used only during type-checking
pattern AnnBoolType :: Provenance
                    -> Expr binder var
                    -> Expr binder var
                    -> Expr binder var
pattern
  AnnBoolType ann lin pol <-
    App ann (BoolType _)
      [ ImplicitArg _ lin
      , ImplicitArg _ pol
      ]
  where
  AnnBoolType ann lin pol =
    App ann (BoolType ann)
      [ ImplicitArg ann lin
      , ImplicitArg ann pol
      ]

--------------------------------------------------------------------------------
-- Container

pattern BuiltinContainerType :: Provenance -> ContainerType -> Expr binder var
pattern BuiltinContainerType ann op = Builtin ann (ContainerType op)

--------------------------------------------------------------------------------
-- Type classes
--------------------------------------------------------------------------------

pattern BuiltinTypeClass :: Provenance
                         -> TypeClass
                         -> NonEmpty (Arg binder var)
                         -> Expr binder var
pattern
  BuiltinTypeClass ann tc args <-
    App ann (Builtin _ (TypeClass tc)) args
  where
  BuiltinTypeClass ann tc args =
    App ann (Builtin ann (TypeClass tc)) args

--------------------------------------------------------------------------------
-- Container type classes

pattern HasConLitsOfSizeExpr :: Provenance
                             -> Int
                             -> Expr binder var
                             -> Expr binder var
                             -> Expr binder var
pattern
  HasConLitsOfSizeExpr ann n tElem tCont <-
    BuiltinTypeClass ann (HasConLitsOfSize n)
      [ ExplicitArg _ tElem
      , ExplicitArg _ tCont
      ]
  where
  HasConLitsOfSizeExpr ann n tElem tCont =
    BuiltinTypeClass ann (HasConLitsOfSize n)
      [ ExplicitArg ann tElem
      , ExplicitArg ann tCont
      ]

pattern HasFoldExpr :: Provenance
                      -> Expr binder var
                      -> Expr binder var
                      -> Expr binder var
pattern
  HasFoldExpr ann tElem tCont <-
    BuiltinTypeClass ann HasFold
      [ ExplicitArg _ tElem
      , ExplicitArg _ tCont
      ]
  where
  HasFoldExpr ann tElem tCont =
    BuiltinTypeClass ann HasFold
      [ ExplicitArg ann tElem
      , ExplicitArg ann tCont
      ]

pattern HasQuantifierInExpr :: Provenance
                            -> Quantifier
                            -> Expr binder var
                            -> Expr binder var
                            -> Expr binder var
pattern
  HasQuantifierInExpr ann q tElem tCont <-
    BuiltinTypeClass ann (HasQuantifierIn q)
      [ ExplicitArg _ tElem
      , ExplicitArg _ tCont
      ]
  where
  HasQuantifierInExpr ann q tElem tCont =
    BuiltinTypeClass ann (HasQuantifierIn q)
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
    BuiltinTypeClass ann (HasNatLitsUpTo n)
      [ ExplicitArg _ t
      ]
  where
  HasNatLitsUpToExpr ann n t =
    BuiltinTypeClass ann (HasNatLitsUpTo n)
      [ ExplicitArg ann t
      ]

--------------------------------------------------------------------------------
-- Integer type classes

pattern HasIntLitsExpr :: Provenance
                       -> Expr binder var
                       -> Expr binder var
pattern
  HasIntLitsExpr ann t <-
    BuiltinTypeClass ann HasIntLits
      [ ExplicitArg _ t
      ]
  where
  HasIntLitsExpr ann t =
    BuiltinTypeClass ann HasIntLits
      [ ExplicitArg ann t
      ]

--------------------------------------------------------------------------------
-- Rational type classes

pattern HasRatLitsExpr :: Provenance
                       -> Expr binder var
                       -> Expr binder var
pattern
  HasRatLitsExpr ann t <-
    BuiltinTypeClass ann HasRatLits
      [ ExplicitArg _ t
      ]
  where
  HasRatLitsExpr ann t =
    BuiltinTypeClass ann HasRatLits
      [ ExplicitArg ann t
      ]

--------------------------------------------------------------------------------
-- Arithmetic ops

pattern HasArithOp2Expr :: Provenance
                       -> TypeClass
                       -> Expr binder var
                       -> Expr binder var
                       -> Expr binder var
                       -> Expr binder var
pattern
  HasArithOp2Expr ann tc t1 t2 t3 <-
    BuiltinTypeClass ann tc
      [ ExplicitArg _ t1
      , ExplicitArg _ t2
      , ExplicitArg _ t3
      ]
  where
  HasArithOp2Expr ann tc t1 t2 t3 =
    BuiltinTypeClass ann tc
      [ ExplicitArg ann t1
      , ExplicitArg ann t2
      , ExplicitArg ann t3
      ]

pattern HasAddExpr :: Provenance
                   -> Expr binder var
                   -> Expr binder var
                   -> Expr binder var
                   -> Expr binder var
pattern HasAddExpr ann t1 t2 t3 = HasArithOp2Expr ann HasAdd t1 t2 t3

pattern HasSubExpr :: Provenance
                   -> Expr binder var
                   -> Expr binder var
                   -> Expr binder var
                   -> Expr binder var
pattern HasSubExpr ann t1 t2 t3 = HasArithOp2Expr ann HasSub t1 t2 t3

pattern HasMulExpr :: Provenance
                   -> Expr binder var
                   -> Expr binder var
                   -> Expr binder var
                   -> Expr binder var
pattern HasMulExpr ann t1 t2 t3 = HasArithOp2Expr ann HasMul t1 t2 t3

pattern HasDivExpr :: Provenance
                   -> Expr binder var
                   -> Expr binder var
                   -> Expr binder var
                   -> Expr binder var
pattern HasDivExpr ann t1 t2 t3 = HasArithOp2Expr ann HasDiv t1 t2 t3

pattern HasNegExpr :: Provenance
                   -> Expr binder var
                   -> Expr binder var
                   -> Expr binder var
pattern
  HasNegExpr ann argType resType <-
    BuiltinTypeClass ann HasNeg
      [ ExplicitArg _ argType
      , ExplicitArg _ resType
      ]
  where
  HasNegExpr ann argType resType =
    BuiltinTypeClass ann HasNeg
      [ ExplicitArg ann argType
      , ExplicitArg ann resType
      ]

--------------------------------------------------------------------------------
-- HasOrder

pattern HasOrdExpr :: Provenance
                   -> Order
                  -> Expr binder var
                  -> Expr binder var
                  -> Expr binder var
                   -> Expr binder var
pattern
  HasOrdExpr p ord arg1Type arg2Type resType <-
    BuiltinTypeClass p (HasOrd ord)
      [ ExplicitArg _ arg1Type
      , ExplicitArg _ arg2Type
      , ExplicitArg _ resType
      ]
  where
  HasOrdExpr p ord arg1Type arg2Type resType =
    BuiltinTypeClass p (HasOrd ord)
      [ ExplicitArg p arg1Type
      , ExplicitArg p arg2Type
      , ExplicitArg p resType
      ]

--------------------------------------------------------------------------------
-- HasEq

pattern HasEqExpr :: Provenance
                  -> Equality
                  -> Expr binder var
                  -> Expr binder var
                  -> Expr binder var
                  -> Expr binder var
pattern
  HasEqExpr p eq arg1Type arg2Type resType <-
    BuiltinTypeClass p (HasEq eq)
      [ ExplicitArg _ arg1Type
      , ExplicitArg _ arg2Type
      , ExplicitArg _ resType
      ]
  where
  HasEqExpr p eq arg1Type arg2Type resType =
    BuiltinTypeClass p (HasEq eq)
      [ ExplicitArg p arg1Type
      , ExplicitArg p arg2Type
      , ExplicitArg p resType
      ]

--------------------------------------------------------------------------------
-- HasQuantifier

pattern HasQuantifierExpr :: Quantifier
                          -> Provenance
                          -> Expr binder var
                          -> Expr binder var
pattern
  HasQuantifierExpr q p tDomain <-
    BuiltinTypeClass p (HasQuantifier q)
      [ ExplicitArg _ tDomain
      , ExplicitArg _ _
      , ExplicitArg _ _
      ]
  where
  HasQuantifierExpr q p tDomain =
    BuiltinTypeClass p (HasQuantifier q)
      [ ExplicitArg p tDomain
      , ExplicitArg p (BoolType p)
      , ExplicitArg p (BoolType p)
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
                       -> Expr binder var
                       -> Expr binder var
pattern
  QuantifierExpr q p binder body <-
    App p (BuiltinQuantifier _ q)
      [ ImplicitArg _ _
      , ImplicitArg _ _
      , ImplicitArg _ _
      , InstanceArg _ _
      , ExplicitArg _ (Lam _ binder body)
      ]
  where
  QuantifierExpr q p binder body =
    App p (BuiltinQuantifier p q)
      [ ImplicitArg p (typeOf binder)
      , ImplicitArg p (BoolType p)
      , ImplicitArg p (BoolType p)
      , InstanceArg p (PrimDict p (HasQuantifierExpr q p (typeOf binder)))
      , ExplicitArg p (Lam p binder body)
      ]

pattern ExistsExpr :: Provenance
                   -> Binder binder var
                   -> Expr binder var
                   -> Expr binder var
pattern ExistsExpr ann binder body = QuantifierExpr Exists ann binder body

pattern ForallExpr :: Provenance
                   -> Binder binder var
                   -> Expr binder var
                   -> Expr binder var
pattern ForallExpr ann binder body = QuantifierExpr Forall ann binder body

pattern ForeachExpr :: Provenance
                    -> Binder binder var
                    -> Expr binder var
                    -> Expr binder var
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

pattern QuantifierInExpr :: Quantifier
                                 -> Provenance
                                 -> Expr binder var
                                 -> Binder binder var
                                 -> Expr binder var
                                 -> Expr binder var
                                 -> Expr binder var
pattern
  QuantifierInExpr q p tCont binder body container <-
    App p (BuiltinQuantifierIn _ q)
      [ ImplicitArg _ _
      , ImplicitArg _ tCont
      , ImplicitArg _ _
      , InstanceArg _ _
      , ExplicitArg _ (Lam _ binder body)
      , ExplicitArg _ container
      ]
  where
  QuantifierInExpr q p tCont binder body container =
    App p (BuiltinQuantifierIn p q)
      [ ImplicitArg p (typeOf binder)
      , ImplicitArg p tCont
      , ImplicitArg p (BoolType p)
      , InstanceArg p (PrimDict p (HasQuantifierInExpr p q (typeOf binder) tCont))
      , ExplicitArg p (Lam p binder body)
      , ExplicitArg p container
      ]

pattern ForallInExpr :: Provenance
                     -> Expr binder var
                     -> Binder binder var
                     -> Expr binder var
                     -> Expr binder var
                     -> Expr binder var
pattern ForallInExpr ann tCont binder body container =
  QuantifierInExpr Forall ann tCont binder body container

pattern ExistsInExpr :: Provenance
                     -> Expr binder var
                     -> Binder binder var
                     -> Expr binder var
                     -> Expr binder var
                     -> Expr binder var
pattern ExistsInExpr ann tCont binder body container
  = QuantifierInExpr Exists ann tCont binder body container

pattern ForeachInExpr :: Provenance
                      -> Expr binder var
                      -> Expr binder var
                      -> Binder binder var
                      -> Expr binder var
                      -> Expr binder var
                      -> Expr binder var
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
                       -> TypeClass
                       -> Provenance
                       -> [Arg binder var]
                       -> Expr binder var
pattern
  BooleanOp2Expr op tc p explicitArgs <-
    App p (Builtin _ (BooleanOp2 op))
      (  ImplicitArg _ BoolType{}
      :| ImplicitArg _ BoolType{}
      :  ImplicitArg _ BoolType{}
      :  InstanceArg _ (PrimDict _ (BuiltinTypeClass _ tc _))
      :  explicitArgs
      )

  where
  BooleanOp2Expr op tc p explicitArgs =
    App p (Builtin p (BooleanOp2 op))
      (  ImplicitArg p (BoolType p)
      :| ImplicitArg p (BoolType p)
      :  ImplicitArg p (BoolType p)
      :  InstanceArg p (PrimDict p (BuiltinTypeClass p tc $
        ExplicitArg p <$> [BoolType p, BoolType p, BoolType p]))
      :  explicitArgs
      )

pattern AndExpr :: Provenance -> [Arg binder var] -> Expr binder var
pattern AndExpr ann explicitArgs <- BooleanOp2Expr And HasAnd ann explicitArgs
  where AndExpr ann explicitArgs = BooleanOp2Expr And HasAnd ann explicitArgs

pattern OrExpr :: Provenance -> [Arg binder var] -> Expr binder var
pattern OrExpr ann explicitArgs <- BooleanOp2Expr Or HasOr ann explicitArgs
  where OrExpr ann explicitArgs = BooleanOp2Expr Or HasOr ann explicitArgs

pattern ImplExpr :: Provenance -> [Arg binder var] -> Expr binder var
pattern ImplExpr ann explicitArgs <- BooleanOp2Expr Impl HasImpl ann explicitArgs
  where ImplExpr ann explicitArgs = BooleanOp2Expr Impl HasImpl ann explicitArgs

--------------------------------------------------------------------------------
-- Not

pattern NotExpr :: Provenance
                -> [Arg binder var]
                -> Expr binder var
pattern
  NotExpr p explicitArgs <-
    App p (Builtin _ Not)
      (  ImplicitArg _ BoolType{}
      :| ImplicitArg _ BoolType{}
      :  InstanceArg _ _
      :  explicitArgs
      )
  where
  NotExpr p explicitArgs =
    App p (Builtin p Not)
      (  ImplicitArg p (BoolType p)
      :| ImplicitArg p (BoolType p)
      :  InstanceArg p (PrimDict p (BuiltinTypeClass p HasNot (ExplicitArg p <$> [BoolType p, BoolType p])))
      :  explicitArgs
      )

--------------------------------------------------------------------------------
-- NumericOp2

pattern NumericOp2Expr :: NumericOp2
                       -> Provenance
                       -> Expr binder var
                       -> Expr binder var
                       -> Expr binder var
                       -> Expr binder var
                       -> [Arg binder var]
                       -> Expr binder var
pattern
  NumericOp2Expr op p t1 t2 t3 tc explicitArgs <-
    App p (Builtin _ (NumericOp2 op))
      (  ImplicitArg _ t1
      :| ImplicitArg _ t2
      :  ImplicitArg _ t3
      :  InstanceArg _ tc
      :  explicitArgs
      )
  where
  NumericOp2Expr op p t1 t2 t3 tc explicitArgs =
    App p (Builtin p (NumericOp2 op))
      (  ImplicitArg p t1
      :| ImplicitArg p t2
      :  ImplicitArg p t3
      :  InstanceArg p tc
      :  explicitArgs
      )

pattern AddExpr :: Provenance
                -> Expr binder var
                -> Expr binder var
                -> Expr binder var
                -> Expr binder var
                -> [Arg binder var]
                -> Expr binder var
pattern AddExpr ann t1 t2 t3 tc explicitArgs <- NumericOp2Expr Add ann t1 t2 t3 tc explicitArgs
  where AddExpr ann t1 t2 t3 tc explicitArgs =  NumericOp2Expr Add ann t1 t2 t3 tc explicitArgs

pattern SubExpr :: Provenance
                -> Expr binder var
                -> Expr binder var
                -> Expr binder var
                -> Expr binder var
                -> [Arg binder var]
                -> Expr binder var
pattern SubExpr ann t1 t2 t3 tc explicitArgs <- NumericOp2Expr Sub ann t1 t2 t3 tc explicitArgs
  where SubExpr ann t1 t2 t3 tc explicitArgs =  NumericOp2Expr Sub ann t1 t2 t3 tc explicitArgs

pattern MulExpr :: Provenance
                -> Expr binder var
                -> Expr binder var
                -> Expr binder var
                -> Expr binder var
                -> [Arg binder var]
                -> Expr binder var
pattern MulExpr ann t1 t2 t3 tc explicitArgs <- NumericOp2Expr Mul ann t1 t2 t3 tc explicitArgs
  where MulExpr ann t1 t2 t3 tc explicitArgs =  NumericOp2Expr Mul ann t1 t2 t3 tc explicitArgs

pattern DivExpr :: Provenance
                -> Expr binder var
                -> Expr binder var
                -> Expr binder var
                -> Expr binder var
                -> [Arg binder var]
                -> Expr binder var
pattern DivExpr ann t1 t2 t3 tc explicitArgs <- NumericOp2Expr Div ann t1 t2 t3 tc explicitArgs
  where DivExpr ann t1 t2 t3 tc explicitArgs =  NumericOp2Expr Div ann t1 t2 t3 tc explicitArgs

--------------------------------------------------------------------------------
-- Not

pattern NegExpr :: Provenance
                -> Expr binder var
                -> Expr binder var
                -> [Arg binder var]
                -> Expr binder var
pattern
  NegExpr ann t1 t2 explicitArgs <-
    App ann (Builtin _ Neg)
      (  ImplicitArg _ t1
      :| ImplicitArg _ t2
      :  InstanceArg _ _
      :  explicitArgs
      )
  where
  NegExpr ann t1 t2 explicitArgs =
    App ann (Builtin ann Neg)
      (  ImplicitArg ann t1
      :| ImplicitArg ann t2
      :  InstanceArg ann (PrimDict ann (HasNegExpr ann t1 t2))
      :  explicitArgs
      )

--------------------------------------------------------------------------------
-- Equality

pattern BuiltinEquality :: Provenance -> Equality -> Expr binder var
pattern BuiltinEquality ann eq = Builtin ann (Equality eq)

pattern EqualityExpr :: Provenance
                     -> Equality
                     -> Expr binder var
                     -> [Arg binder var]
                     -> Expr binder var
pattern
  EqualityExpr p eq tElem explicitArgs <-
    App p (BuiltinEquality _ eq)
      (  ImplicitArg _ tElem
      :| ImplicitArg _ _
      :  ImplicitArg _ _
      :  InstanceArg _ _
      :  explicitArgs
      )
  where
  EqualityExpr p eq tElem explicitArgs =
    App p (BuiltinEquality p eq)
      (  ImplicitArg p tElem
      :| ImplicitArg p tElem
      :  ImplicitArg p (BoolType p)
      :  InstanceArg p (PrimDict p (HasEqExpr p eq tElem tElem tElem))
      :  explicitArgs
      )

--------------------------------------------------------------------------------
-- Order

pattern BuiltinOrder :: Provenance -> Order -> Expr binder var
pattern BuiltinOrder ann order = Builtin ann (Order order)

pattern OrderExpr :: Provenance
                  -> Order
                  -> Expr binder var
                  -> [Arg binder var]
                  -> Expr binder var
pattern
  OrderExpr p order tElem explicitArgs <-
    App p (BuiltinOrder _ order)
      (  ImplicitArg _ tElem
      :| ImplicitArg _ _
      :  ImplicitArg _ _
      :  InstanceArg _ _
      :  explicitArgs
      )
  where
  OrderExpr p order tElem explicitArgs =
    App p (BuiltinOrder p order)
      (  ImplicitArg p tElem
      :| ImplicitArg p tElem
      :  ImplicitArg p (BoolType p)
      :  InstanceArg p (PrimDict p (HasOrdExpr p order tElem tElem tElem))
      :  explicitArgs
      )

--------------------------------------------------------------------------------
-- Sequence

pattern SeqExpr :: Provenance
                -> Expr binder var
                -> Expr binder var
                -> [Expr binder var]
                -> Expr binder var
pattern
  SeqExpr p tElem tCont xs <-
    App p (LSeq _ xs)
      [ ImplicitArg _ tElem
      , ImplicitArg _ tCont
      , InstanceArg _ _
      , InstanceArg _ _
      ]
  where
  SeqExpr p tElem tCont xs =
    App p (LSeq p xs)
      [ ImplicitArg p tElem
      , ImplicitArg p tCont
      , InstanceArg p (PrimDict p (HasConLitsOfSizeExpr p (length xs) tElem tCont))
      , InstanceArg p (TypeUniverse p 0)
      -- ^ TypeUniverse is a massive hack. The TypesEqualModuloAuxiliaryConstraints
      -- TC here has no computational content, so it should be okay though.
      ]

--------------------------------------------------------------------------------
-- Nil and cons

pattern NilExpr :: Provenance
                -> Expr binder var
                -> Expr binder var
                -> Expr binder var
pattern NilExpr ann tElem tCont <- SeqExpr ann tElem tCont []

pattern ConsExpr :: Provenance
                 -> Expr binder var
                 -> [Arg binder var]
                 -> Expr binder var
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
                -> Expr binder var
                -> Expr binder var
                -> Expr binder var
                -> [Arg binder var]
                -> Expr binder var
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
                -> Expr binder var
                -> Expr binder var
                -> [Arg binder var]
                -> Expr binder var
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
                 -> Expr binder var
                 -> Expr binder var
                 -> Expr binder var
                 -> [Arg binder var]
                 -> Expr binder var
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
      :  InstanceArg ann (PrimDict ann (HasFoldExpr ann tElem tCont))
      :  explicitArgs
      )

