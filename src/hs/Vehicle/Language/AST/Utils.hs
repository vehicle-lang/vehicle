{-# LANGUAGE OverloadedLists #-}

module Vehicle.Language.AST.Utils where

import Data.Functor.Foldable (Recursive(..))
import Data.List.NonEmpty qualified as NonEmpty (toList)
import Data.Text (pack)

import Vehicle.Prelude
import Vehicle.Language.AST.Core
import Vehicle.Language.AST.DeBruijn
import Vehicle.Language.AST.Builtin
import Vehicle.Language.AST.Visibility (Owner(..))

--------------------------------------------------------------------------------
-- Patterns

pattern Type0 :: Expr binder var ann
pattern Type0 = Type 0

pattern Type1 :: Expr binder var ann
pattern Type1 = Type 1

pattern LitNat :: ann -> Int -> Expr binder var ann
pattern LitNat ann n = Literal ann (LNat n)

pattern LitInt :: ann -> Int -> Expr binder var ann
pattern LitInt ann n = Literal ann (LInt n)

pattern LitRat :: ann -> Double -> Expr binder var ann
pattern LitRat ann n = Literal ann (LRat n)

pattern LitBool :: ann -> Bool -> Expr binder var ann
pattern LitBool ann n = Literal ann (LBool n)

pattern BuiltinNumericType :: ann -> NumericType -> Expr binder var ann
pattern BuiltinNumericType ann op = Builtin ann (NumericType op)

pattern BuiltinBooleanType :: ann -> BooleanType -> Expr binder var ann
pattern BuiltinBooleanType ann op = Builtin ann (BooleanType op)

pattern BuiltinContainerType :: ann -> ContainerType -> Expr binder var ann
pattern BuiltinContainerType ann op = Builtin ann (ContainerType op)

pattern BuiltinTypeClass :: ann -> TypeClass -> Expr binder var ann
pattern BuiltinTypeClass ann tc = Builtin ann (TypeClass tc)

pattern BuiltinOrder :: ann -> Order -> Expr binder var ann
pattern BuiltinOrder ann order = Builtin ann (Order order)

pattern BuiltinEquality :: ann -> Equality -> Expr binder var ann
pattern BuiltinEquality ann eq = Builtin ann (Equality eq)

pattern BuiltinQuantifier :: ann -> Quantifier -> Expr binder var ann
pattern BuiltinQuantifier ann q = Builtin ann (Quant q)

--------------------------------------------------------------------------------
-- Type synonyms

-- * Type of annotations attached to the Frontend AST after parsing
-- before being analysed by the compiler

type InputBinding = (Maybe Symbol)
type InputVar     = Symbol
type InputAnn     = (Provenance, Owner)

type InputArg       = Arg    InputBinding InputVar InputAnn
type InputBinder    = Binder InputBinding InputVar InputAnn
type InputExpr      = Expr   InputBinding InputVar InputAnn
type InputDecl      = Decl   InputBinding InputVar InputAnn
type InputProg      = Prog   InputBinding InputVar InputAnn

-- * Types pre type-checking

type UncheckedVar    = LocallyNamelessVar
type UncheckedAnn    = (Provenance, Owner)

type UncheckedBinder = DeBruijnBinder UncheckedAnn
type UncheckedArg    = DeBruijnArg    UncheckedAnn
type UncheckedExpr   = DeBruijnExpr   UncheckedAnn
type UncheckedDecl   = DeBruijnDecl   UncheckedAnn
type UncheckedProg   = DeBruijnProg   UncheckedAnn

-- * Types post type-checking

type CheckedVar    = LocallyNamelessVar
type CheckedAnn    = (Provenance, Owner)

type CheckedBinder = DeBruijnBinder  CheckedAnn
type CheckedArg    = DeBruijnArg     CheckedAnn
type CheckedExpr   = DeBruijnExpr    CheckedAnn
type CheckedDecl   = DeBruijnDecl    CheckedAnn
type CheckedProg   = DeBruijnProg    CheckedAnn

-- * Type of annotations attached to the Core AST that are output by the compiler

type OutputBinding = Symbol
type OutputVar     = Symbol
type OutputAnn     = (Provenance, Owner)

type OutputBinder = Binder OutputBinding OutputVar OutputAnn
type OutputArg    = Arg    OutputBinding OutputVar OutputAnn
type OutputExpr   = Expr   OutputBinding OutputVar OutputAnn
type OutputDecl   = Decl   OutputBinding OutputVar OutputAnn
type OutputProg   = Prog   OutputBinding OutputVar OutputAnn

emptyUserAnn :: InputAnn
emptyUserAnn = (mempty, TheUser)

emptyMachineAnn :: InputAnn
emptyMachineAnn = (mempty, TheMachine)

--------------------------------------------------------------------------------
-- Classes

class IsBoundCtx a where
  ctxNames :: a -> [Maybe Symbol]

instance IsBoundCtx [Maybe Symbol] where
  ctxNames = id

instance IsBoundCtx [Symbol] where
  ctxNames = map Just

--------------------------------------------------------------------------------
-- Utility functions

isHole :: Expr binder var ann -> Bool
isHole (Hole _ _ ) = True
isHole _           = False

isProperty :: Expr binder var ann -> Bool
isProperty (Builtin _ (BooleanType Prop)) = True
isProperty _                              = False

freeNames :: CheckedExpr -> [Identifier]
freeNames = cata $ \case
  TypeF     _                   -> []
  HoleF     _   _               -> []
  PrimDictF _                   -> []
  MetaF     _ _                 -> []
  LiteralF  _ _                 -> []
  BuiltinF  _ _                 -> []
  AnnF      _ e t               -> e <> t
  AppF      _ fun args          -> fun <> concatMap (freeNames . argExpr) args
  PiF       _ binder result     -> freeNames (typeOf binder) <> result
  VarF      _ (Free ident)      -> [ident]
  VarF      _ (Bound _)         -> []
  LetF      _ bound binder body -> bound <> freeNames (typeOf binder) <> body
  LamF      _ binder body       -> freeNames (typeOf binder) <> body
  SeqF      _ xs                -> concat xs

--------------------------------------------------------------------------------
-- Destruction functions

toHead :: Expr binder var ann -> (Expr binder var ann, [Arg binder var ann])
toHead (App _ann fun args ) = (fun, NonEmpty.toList args)
toHead e                    = (e, [])

exprHead :: Expr binder var ann -> Expr binder var ann
exprHead = fst . toHead

--------------------------------------------------------------------------------
-- Views

pattern QuantiferView :: ann
                      -> Quantifier
                      -> binder
                      -> Expr binder var ann
                      -> Expr binder var ann
                      -> Expr binder var ann
pattern QuantiferView ann q n t e <-
  App ann (BuiltinQuantifier _ q)
    [ ImplicitArg _ _
    , ExplicitArg _ (Lam _ (ExplicitBinder _  n t) e)
    ]

getQuantifierSymbol :: Maybe Symbol -> Symbol
getQuantifierSymbol (Just symbol) = symbol
getQuantifierSymbol Nothing       = developerError "Should not have quantifiers with machine names?"

--------------------------------------------------------------------------------
-- Construction functions

-- Primed versions take `Arg` instead of `Expr`

-- Types

mkListType :: CheckedAnn -> CheckedExpr -> CheckedExpr
mkListType ann tElem = App ann (BuiltinContainerType ann List) [ExplicitArg ann tElem]

mkTensorType :: CheckedAnn -> CheckedExpr -> [Int] -> CheckedExpr
mkTensorType ann tElem dims =
  let listType = mkListType ann (Builtin ann (NumericType Nat)) in
  let dimExprs = fmap (Literal ann . LNat) dims in
  let dimList  = mkSeq ann (Builtin ann (NumericType Nat)) listType dimExprs in
  App ann (BuiltinContainerType ann Tensor) [ExplicitArg ann tElem, ExplicitArg ann dimList]

mkIsTruth :: CheckedAnn -> CheckedExpr -> CheckedExpr
mkIsTruth ann t = App ann (Builtin ann (TypeClass IsTruth)) [ExplicitArg ann t]

mkIsContainer :: CheckedAnn -> CheckedExpr -> CheckedExpr -> CheckedExpr
mkIsContainer ann tElem tCont = App ann (Builtin ann (TypeClass IsContainer))
  [ExplicitArg ann tElem, ExplicitArg ann tCont]

-- Expressions

mkLiteral' :: CheckedAnn -> Literal -> CheckedArg -> CheckedArg -> CheckedExpr
mkLiteral' ann lit t tc = App ann (Literal ann lit) [t, tc]

mkLiteral :: CheckedAnn -> Literal -> CheckedExpr -> CheckedExpr -> CheckedExpr
mkLiteral ann lit t tc = mkLiteral' ann lit
  (ImplicitArg ann t)
  (ImplicitArg ann (PrimDict tc))

mkBool :: CheckedAnn -> CheckedExpr -> Bool -> CheckedExpr
mkBool ann t b = mkLiteral ann (LBool b) t (PrimDict t)

mkNat :: CheckedAnn -> Int -> CheckedExpr
mkNat ann n = let t = Builtin ann (NumericType Nat) in mkLiteral ann (LNat n) t (PrimDict t)

mkInt :: CheckedAnn -> Int -> CheckedExpr
mkInt ann i = let t = Builtin ann (NumericType Int) in mkLiteral ann (LInt i) t (PrimDict t)

mkRat :: CheckedAnn -> Double -> CheckedExpr
mkRat ann r = let t = Builtin ann (NumericType Rat) in mkLiteral ann (LRat r) t (PrimDict t)

mkSeq' :: CheckedAnn -> CheckedArg -> CheckedArg -> CheckedArg -> [CheckedExpr] -> CheckedExpr
mkSeq' ann tElem tCont tc xs = App ann (Seq ann xs) [tElem, tCont, tc]

mkSeq :: CheckedAnn -> CheckedExpr -> CheckedExpr -> [CheckedExpr] -> CheckedExpr
mkSeq ann tElem tCont = mkSeq' ann
  (ImplicitArg ann tElem)
  (ImplicitArg ann tCont)
  (InstanceArg ann (PrimDict (mkIsContainer ann tElem tCont)))

-- Expressions

mkEq' :: CheckedAnn -> CheckedArg -> CheckedArg -> CheckedArg -> CheckedExpr -> CheckedExpr -> CheckedExpr
mkEq' ann tElem tRes tc e1 e2 = App ann (Builtin ann (Equality Eq))
  [tElem, tRes, tc, ExplicitArg ann e1, ExplicitArg ann e2]

mkEq :: CheckedAnn -> CheckedExpr -> CheckedExpr -> CheckedExpr -> CheckedExpr -> CheckedExpr
mkEq ann tElem tRes = mkEq' ann
  (ImplicitArg ann tElem)
  (ImplicitArg ann tRes)
  (InstanceArg ann (PrimDict (mkIsTruth ann tRes)))

mkNot' :: CheckedAnn -> CheckedArg -> CheckedArg -> CheckedExpr -> CheckedExpr
mkNot' ann t tc e = App ann (Builtin ann Not) [t, tc, ExplicitArg ann e]

mkNot :: CheckedAnn -> CheckedExpr -> CheckedExpr  -> CheckedExpr
mkNot ann t = mkNot' ann
  (ImplicitArg ann t)
  (InstanceArg ann (PrimDict (mkIsTruth ann t)))

mkBoolOp2' :: BooleanOp2 -> CheckedAnn -> CheckedArg -> CheckedArg -> CheckedExpr -> CheckedExpr -> CheckedExpr
mkBoolOp2' op ann t tc e1 e2 = App ann (Builtin ann (BooleanOp2 op))
  [t, tc, ExplicitArg ann e1, ExplicitArg ann e2]

mkBoolOp2 :: BooleanOp2 -> CheckedAnn -> CheckedExpr -> CheckedExpr -> CheckedExpr -> CheckedExpr
mkBoolOp2 op ann t = mkBoolOp2' op ann
  (ImplicitArg ann t)
  (InstanceArg ann (PrimDict (mkIsTruth ann t)))

mkAt' :: CheckedAnn -> CheckedArg -> CheckedArg -> CheckedExpr -> CheckedExpr -> CheckedExpr
mkAt' ann tElem tDims xs i = App ann (Builtin ann At)
  [tElem, tDims, ExplicitArg ann xs , ExplicitArg ann i ]

mkMap' :: CheckedAnn -> CheckedArg -> CheckedArg -> CheckedArg -> CheckedArg -> CheckedExpr
mkMap' ann tFrom tTo f xs = App ann (Builtin ann Map)
  [tFrom, tTo, f, xs]

mkFold' :: CheckedAnn -> CheckedArg -> CheckedArg -> CheckedArg -> CheckedArg -> CheckedExpr -> CheckedExpr -> CheckedExpr -> CheckedExpr
mkFold' ann tElem tCont tRes tc bop unit xs = App ann (Builtin ann Fold)
  [tElem, tCont, tRes, tc, ExplicitArg ann bop, ExplicitArg ann unit, ExplicitArg ann xs]

mkQuantifier :: CheckedAnn -> Quantifier -> Symbol -> CheckedExpr -> CheckedExpr -> CheckedExpr
mkQuantifier ann q n t e =
  App ann (Builtin ann (Quant q))
    [ ImplicitArg ann t
    , ExplicitArg ann (Lam ann (ExplicitBinder ann (Just n) t) e)
    ]

-- | Generates a name for a variable based on the indices, e.g. x [1,2,3] -> x_1_2_3
mkNameWithIndices :: Symbol -> [Int] -> Symbol
mkNameWithIndices n indices = mconcat (n : ["_" <> pack (show index) | index <- indices])

substContainerType :: CheckedArg -> CheckedExpr -> CheckedExpr
substContainerType newTElem (App ann1 (BuiltinContainerType ann2 List)   [_tElem]) =
  App ann1 (BuiltinContainerType ann2 List) [newTElem]
substContainerType newTElem (App ann1 (BuiltinContainerType ann2 Tensor) [_tElem, tDims])  =
  App ann1 (BuiltinContainerType ann2 Tensor) [newTElem, tDims]
substContainerType _ _ = developerError "Provided an invalid container type"