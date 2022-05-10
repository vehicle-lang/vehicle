module Vehicle.Language.AST.BuiltinPatterns where

import Data.List.NonEmpty (NonEmpty(..))

import Vehicle.Language.AST.Builtin
import Vehicle.Language.AST.Core

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------
-- List

pattern ListType :: ann
                 -> Expr binder var ann
                 -> Expr binder var ann
pattern
  ListType ann tElem <- App ann (BuiltinContainerType _   List) [ExplicitArg _ tElem]
  where
  ListType ann tElem =  App ann (BuiltinContainerType ann List) [ExplicitArg ann tElem]

mkList :: ann
       -> Expr binder var ann
       -> [Expr binder var ann]
       -> Expr binder var ann
mkList ann tElem = foldr cons (NilExpr ann tElem)
  where cons x xs = ConsExpr ann tElem $ fmap (ExplicitArg ann) [x, xs]

--------------------------------------------------------------------------------
-- Tensor

pattern TensorType :: ann
                   -> Expr binder var ann
                   -> Expr binder var ann
                   -> Expr binder var ann
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

mkTensorDims :: ann
             -> [Int]
             -> Expr binder var ann
mkTensorDims ann dims =
  let listType = ListType ann (NatType ann) in
  let dimExprs = fmap (Literal ann . LNat) dims in
  let dimList  = SeqExpr ann (NatType ann) listType dimExprs in
  dimList

mkTensorType :: ann
             -> Expr binder var ann
             -> [Int]
             -> Expr binder var ann
mkTensorType ann tElem dims =
  let dimList = mkTensorDims ann dims in
  App ann (BuiltinContainerType ann Tensor) (fmap (ExplicitArg ann) [tElem, dimList])

--------------------------------------------------------------------------------
-- Tensor

pattern IndexType :: ann
                -> Expr binder var ann
                -> Expr binder var ann
pattern
  IndexType ann tSize <-
    App ann (Builtin _ Index)
      [ ExplicitArg _ tSize ]
  where
  IndexType ann tSize =
    App ann (Builtin ann Index)
      [ ExplicitArg ann tSize ]

mkIndexType :: ann -> Int -> Expr binder var ann
mkIndexType ann n = IndexType ann (NatLiteralExpr ann (NatType ann) n)

--------------------------------------------------------------------------------
-- Numeric

pattern BuiltinNumericType :: ann -> NumericType -> Expr binder var ann
pattern BuiltinNumericType ann op = Builtin ann (NumericType op)

pattern NatType :: ann -> Expr binder var ann
pattern NatType ann = BuiltinNumericType ann Nat

pattern IntType :: ann -> Expr binder var ann
pattern IntType ann = BuiltinNumericType ann Int

pattern RatType :: ann -> Expr binder var ann
pattern RatType ann = BuiltinNumericType ann Rat

pattern RealType :: ann -> Expr binder var ann
pattern RealType ann = BuiltinNumericType ann Real

--------------------------------------------------------------------------------
-- Boolean

pattern BoolType :: ann -> Expr binder var ann
pattern BoolType ann = Builtin ann Bool

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
-- Container type classes

pattern HasConLitsOfSizeExpr :: ann
                             -> Int
                             -> Expr binder var ann
                             -> Expr binder var ann
                             -> Expr binder var ann
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

pattern HasConOpsExpr :: ann
                      -> Expr binder var ann
                      -> Expr binder var ann
                      -> Expr binder var ann
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

pattern HasNatLitsUpToExpr :: ann
                           -> Int
                           -> Expr binder var ann
                           -> Expr binder var ann
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

pattern HasNatOpsExpr :: ann
                      -> Expr binder var ann
                      -> Expr binder var ann
pattern
  HasNatOpsExpr ann t <-
    App ann (BuiltinTypeClass _ HasNatOps)
      [ ExplicitArg _ t
      ]
  where
  HasNatOpsExpr ann t =
    App ann (BuiltinTypeClass ann HasNatOps)
      [ ExplicitArg ann t
      ]

--------------------------------------------------------------------------------
-- Integer type classes

pattern HasIntLitsExpr :: ann
                       -> Expr binder var ann
                       -> Expr binder var ann
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

pattern HasIntOpsExpr :: ann
                      -> Expr binder var ann
                      -> Expr binder var ann
pattern
  HasIntOpsExpr ann t <-
    App ann (BuiltinTypeClass _ HasIntOps)
      [ ExplicitArg _ t
      ]
  where
  HasIntOpsExpr ann t =
    App ann (BuiltinTypeClass ann HasIntOps)
      [ ExplicitArg ann t
      ]

--------------------------------------------------------------------------------
-- Rational type classes

pattern HasRatLitsExpr :: ann
                      -> Expr binder var ann
                      -> Expr binder var ann
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

pattern HasRatOpsExpr :: ann
                       -> Expr binder var ann
                       -> Expr binder var ann
pattern
  HasRatOpsExpr ann t <-
    App ann (BuiltinTypeClass _ HasRatOps)
      [ ExplicitArg _ t
      ]
  where
  HasRatOpsExpr ann t =
    App ann (BuiltinTypeClass ann HasRatOps)
      [ ExplicitArg ann t
      ]

--------------------------------------------------------------------------------
-- HasOrder

pattern HasOrdExpr :: ann
                   -> Expr binder var ann
                   -> Expr binder var ann
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

pattern HasEqExpr :: ann
                  -> Expr binder var ann
                  -> Expr binder var ann
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

pattern LiteralExpr :: ann
                    -> Expr binder var ann
                    -> Expr binder var ann
                    -> Literal
                    -> Expr binder var ann
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

pattern LitBool :: ann -> Bool -> Expr binder var ann
pattern LitBool ann n = Literal ann (LBool n)

pattern BoolLiteralExpr :: ann
                        -> Bool
                        -> Expr binder var ann
pattern BoolLiteralExpr ann value = LitBool ann value

pattern TrueExpr :: ann -> Expr binder var ann
pattern TrueExpr ann = BoolLiteralExpr ann True

pattern FalseExpr :: ann -> Expr binder var ann
pattern FalseExpr ann = BoolLiteralExpr ann False

--------------------------------------------------------------------------------
-- Nat

pattern LitNat :: ann -> Int -> Expr binder var ann
pattern LitNat ann n = Literal ann (LNat n)

pattern NatLiteralExpr :: ann
                       -> Expr binder var ann
                       -> Int
                       -> Expr binder var ann
pattern
  NatLiteralExpr ann t n <-
    LiteralExpr ann t HasNatLitsUpToExpr{} (LNat n)
  where
  NatLiteralExpr ann t n =
    LiteralExpr ann t (HasNatLitsUpToExpr ann n t) (LNat n)

--------------------------------------------------------------------------------
-- Int

pattern LitInt :: ann -> Int -> Expr binder var ann
pattern LitInt ann n = Literal ann (LInt n)

pattern IntLiteralExpr :: ann
                       -> NumericType
                       -> Int
                       -> Expr binder var ann
pattern
  IntLiteralExpr ann t n <-
    LiteralExpr ann (BuiltinNumericType _ t) (HasIntLitsExpr _ _) (LInt n)
  where
  IntLiteralExpr ann t n =
    LiteralExpr ann (BuiltinNumericType ann t) (HasIntLitsExpr ann (BuiltinNumericType ann t)) (LInt n)

--------------------------------------------------------------------------------
-- Rat

pattern LitRat :: ann -> Rational -> Expr binder var ann
pattern LitRat ann n = Literal ann (LRat n)

pattern RatLiteralExpr :: ann
                       -> NumericType
                       -> Rational
                       -> Expr binder var ann
pattern
  RatLiteralExpr ann t n <-
    LiteralExpr ann (BuiltinNumericType _ t) (HasRatLitsExpr _ _) (LRat n)
  where
  RatLiteralExpr ann t n =
    LiteralExpr ann (BuiltinNumericType ann t) (HasRatLitsExpr ann (BuiltinNumericType ann t)) (LRat n)

--------------------------------------------------------------------------------
-- Expressions
--------------------------------------------------------------------------------
-- Quantifier

pattern BuiltinQuantifier :: ann -> Quantifier -> Expr binder var ann
pattern BuiltinQuantifier ann q = Builtin ann (Quant q)

pattern QuantifierExpr :: Quantifier
                       -> ann
                       -> Binder binder var ann
                       -> Expr   binder var ann
                       -> Expr   binder var ann
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

mkQuantifierSeq :: Quantifier
                -> ann
                -> [binder]
                -> Expr binder var ann
                -> Expr binder var ann
                -> Expr binder var ann
mkQuantifierSeq q ann names t body =
  foldl (\e name -> QuantifierExpr q ann (ExplicitBinder ann name t) e) body names

--------------------------------------------------------------------------------
-- QuantifierIn

pattern BuiltinQuantifierIn :: ann -> Quantifier -> Expr binder var ann
pattern BuiltinQuantifierIn ann q = Builtin ann (QuantIn q)

pattern QuantifierInExpr :: Quantifier
                         -> ann
                         -> Expr   binder var ann
                         -> Binder binder var ann
                         -> Expr   binder var ann
                         -> Expr   binder var ann
                         -> Expr   binder var ann
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

--------------------------------------------------------------------------------
-- IfExpr

pattern IfExpr :: ann
               -> Expr binder var ann
               -> [Arg binder var ann]
               -> Expr binder var ann
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
                       -> ann
                       -> NonEmpty (Arg  binder var ann)
                       -> Expr  binder var ann
pattern
  BooleanOp2Expr op ann explicitArgs <-
    App ann (Builtin _ (BooleanOp2 op)) explicitArgs
  where
  BooleanOp2Expr op ann explicitArgs =
    App ann (Builtin ann (BooleanOp2 op)) explicitArgs

booleanBigOp :: forall binder var ann .
                BooleanOp2
             -> ann
             -> Expr binder var ann
             -> Expr binder var ann
             -> Expr binder var ann
booleanBigOp op ann containerType container =
  FoldExpr ann boolType containerType boolType $ fmap (ExplicitArg ann)
    [ Builtin ann (BooleanOp2 op)
    , BoolLiteralExpr ann unit
    , container
    ]
  where
    unit :: Bool
    unit = case op of
      And  -> True
      Or   -> False
      Impl -> True

    boolType :: Expr binder var ann
    boolType = BoolType ann

pattern AndExpr :: ann -> NonEmpty (Arg  binder var ann) -> Expr binder var ann
pattern AndExpr ann explicitArgs <- BooleanOp2Expr And ann explicitArgs
  where AndExpr ann explicitArgs = BooleanOp2Expr And ann explicitArgs

pattern OrExpr :: ann -> NonEmpty (Arg  binder var ann) -> Expr binder var ann
pattern OrExpr ann explicitArgs <- BooleanOp2Expr Or ann explicitArgs
  where OrExpr ann explicitArgs = BooleanOp2Expr Or ann explicitArgs

pattern ImplExpr :: ann -> NonEmpty (Arg  binder var ann) -> Expr binder var ann
pattern ImplExpr ann explicitArgs <- BooleanOp2Expr Impl ann explicitArgs
  where ImplExpr ann explicitArgs = BooleanOp2Expr Impl ann explicitArgs

--------------------------------------------------------------------------------
-- Not

pattern NotExpr :: ann
                -> NonEmpty (Arg  binder var ann)
                -> Expr  binder var ann
pattern
  NotExpr ann explicitArgs <-
    App ann (Builtin _ Not) explicitArgs
  where
  NotExpr ann explicitArgs =
    App ann (Builtin ann Not) explicitArgs

--------------------------------------------------------------------------------
-- NumericOp2

pattern NumericOp2Expr :: NumericOp2
                       -> ann
                       -> NumericType
                       -> Expr  binder var ann
                       -> [Arg  binder var ann]
                       -> Expr  binder var ann
pattern
  NumericOp2Expr op ann t tc explicitArgs <-
    App ann (Builtin _ (NumericOp2 op))
      (  ImplicitArg _ (BuiltinNumericType _ t)
      :| InstanceArg _ tc
      :  explicitArgs
      )
  where
  NumericOp2Expr op ann t tc explicitArgs =
    App ann (Builtin ann (NumericOp2 op))
      (  ImplicitArg ann (BuiltinNumericType ann t)
      :| InstanceArg ann tc
      :  explicitArgs
      )

pattern AddExpr :: ann -> NumericType -> Expr binder var ann -> [Arg binder var ann] -> Expr binder var ann
pattern AddExpr ann t tc explicitArgs <- NumericOp2Expr Add ann t tc explicitArgs
  where AddExpr ann t tc explicitArgs =  NumericOp2Expr Add ann t tc explicitArgs

pattern SubExpr :: ann -> NumericType -> Expr binder var ann -> [Arg binder var ann] -> Expr binder var ann
pattern SubExpr ann t tc explicitArgs <- NumericOp2Expr Sub ann t tc explicitArgs
  where SubExpr ann t tc explicitArgs =  NumericOp2Expr Sub ann t tc explicitArgs

pattern MulExpr :: ann -> NumericType -> Expr binder var ann -> [Arg binder var ann] -> Expr binder var ann
pattern MulExpr ann t tc explicitArgs <- NumericOp2Expr Mul ann t tc explicitArgs
  where MulExpr ann t tc explicitArgs =  NumericOp2Expr Mul ann t tc explicitArgs

pattern DivExpr :: ann -> NumericType -> Expr binder var ann -> [Arg binder var ann] -> Expr binder var ann
pattern DivExpr ann t tc explicitArgs <- NumericOp2Expr Div ann t tc explicitArgs
  where DivExpr ann t tc explicitArgs =  NumericOp2Expr Div ann t tc explicitArgs

--------------------------------------------------------------------------------
-- Not

pattern NegExpr :: ann
                -> NumericType
                -> [Arg  binder var ann]
                -> Expr  binder var ann
pattern
  NegExpr ann t explicitArgs <-
    App ann (Builtin _ Neg)
      (  ImplicitArg _ (BuiltinNumericType _ t)
      :| InstanceArg _ _
      :  explicitArgs
      )
  where
  NegExpr ann t explicitArgs =
    App ann (Builtin ann Neg)
      (  ImplicitArg ann (BuiltinNumericType ann t)
      :| InstanceArg ann (PrimDict ann (HasIntOpsExpr ann (BuiltinNumericType ann t)))
      :  explicitArgs
      )

--------------------------------------------------------------------------------
-- Equality

pattern BuiltinEquality :: ann -> Equality -> Expr binder var ann
pattern BuiltinEquality ann eq = Builtin ann (Equality eq)

pattern EqualityExpr :: Equality
                     -> ann
                     -> Expr  binder var ann
                     -> [Arg  binder var ann]
                     -> Expr  binder var ann
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

pattern BuiltinOrder :: ann -> Order -> Expr binder var ann
pattern BuiltinOrder ann order = Builtin ann (Order order)

pattern OrderExpr :: Order
                  -> ann
                  -> Expr  binder var ann
                  -> [Arg  binder var ann]
                  -> Expr  binder var ann
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

pattern SeqExpr :: ann
                -> Expr  binder var ann
                -> Expr  binder var ann
                -> [Expr binder var ann]
                -> Expr  binder var ann
pattern
  SeqExpr ann tElem tCont xs <-
    LSeq ann (PrimDict _ (HasConLitsOfSizeExpr _ _ tElem tCont)) xs
  where
  SeqExpr ann tElem tCont xs =
    LSeq ann (PrimDict ann (HasConLitsOfSizeExpr ann (length xs) tElem tCont)) xs

--------------------------------------------------------------------------------
-- Nil and cons

pattern NilExpr :: ann
                -> Expr binder var ann
                -> Expr binder var ann
pattern
  NilExpr ann tElem <-
    LSeq ann tElem []
  where
  NilExpr ann tElem =
    LSeq ann tElem []

pattern ConsExpr :: ann
                 -> Expr  binder var ann
                 -> [Arg  binder var ann]
                 -> Expr  binder var ann
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

pattern AtExpr :: ann
                -> Expr  binder var ann
                -> Expr  binder var ann
                -> Expr  binder var ann
                -> [Arg binder var ann]
                -> Expr  binder var ann
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

pattern MapExpr :: ann
                -> Expr  binder var ann
                -> Expr  binder var ann
                -> [Arg  binder var ann]
                -> Expr  binder var ann
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

pattern FoldExpr :: ann
                 -> Expr  binder var ann
                 -> Expr  binder var ann
                 -> Expr  binder var ann
                 -> [Arg  binder var ann]
                 -> Expr  binder var ann
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