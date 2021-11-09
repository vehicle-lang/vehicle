{-# LANGUAGE OverloadedLists #-}

module Vehicle.Language.AST.Utils where

import Data.Functor.Foldable (Recursive(..))
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NonEmpty (toList)
import Data.Text (pack)

import Vehicle.Prelude
import Vehicle.Language.AST.Core
import Vehicle.Language.AST.DeBruijn
import Vehicle.Language.AST.Builtin
import Vehicle.Language.AST.Visibility (Visibility(..))
import Vehicle.Language.AST.Name

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

pattern LitReal :: ann -> Double -> Expr binder var ann
pattern LitReal ann n = Literal ann (LRat n)

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

--------------------------------------------------------------------------------
-- Type synonyms

-- * Type of annotations attached to the Frontend AST after parsing
-- before being analysed by the compiler

type InputVar  = Name
type InputAnn  = Provenance

type InputArg       = NamedArg    InputAnn
type InputBinder    = NamedBinder InputAnn
type InputExpr      = NamedExpr   InputAnn
type InputDecl      = NamedDecl   InputAnn
type InputProg      = NamedProg   InputAnn

-- * Types pre type-checking

type UncheckedVar    = LocallyNamelessVar
type UncheckedAnn    = Provenance

type UncheckedBinder = DeBruijnBinder UncheckedAnn
type UncheckedArg    = DeBruijnArg    UncheckedAnn
type UncheckedExpr   = DeBruijnExpr   UncheckedAnn
type UncheckedDecl   = DeBruijnDecl   UncheckedAnn
type UncheckedProg   = DeBruijnProg   UncheckedAnn

-- * Types post type-checking

type CheckedVar    = LocallyNamelessVar
type CheckedAnn    = Provenance

type CheckedBinder = DeBruijnBinder  CheckedAnn
type CheckedArg    = DeBruijnArg     CheckedAnn
type CheckedExpr   = DeBruijnExpr    CheckedAnn
type CheckedDecl   = DeBruijnDecl    CheckedAnn
type CheckedProg   = DeBruijnProg    CheckedAnn

-- * Type of annotations attached to the Core AST that are output by the compiler

type OutputBinding = Name
type OutputVar     = Name
type OutputAnn     = Provenance

type OutputBinder = NamedBinder OutputAnn
type OutputArg    = NamedArg    OutputAnn
type OutputExpr   = NamedExpr   OutputAnn
type OutputDecl   = NamedDecl   OutputAnn
type OutputProg   = NamedProg   OutputAnn

--------------------------------------------------------------------------------
-- Classes

class IsBoundCtx a where
  ctxNames :: a -> [Name]

instance IsBoundCtx [Name] where
  ctxNames = id

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

data QuantView binder var ann =
  QuantView ann Quantifier binder (Expr binder var ann) (Expr binder var ann)

quantView :: Expr binder var ann -> Maybe (QuantView binder var ann)
quantView (App ann (Builtin _ (Quant q))
  (Arg _ Explicit (Lam _ (Binder _ _ Explicit n t) e) :| [])) = Just (QuantView ann q n t e)
quantView _ = Nothing

--------------------------------------------------------------------------------
-- Construction functions

-- Primed versions take `Arg` instead of `Expr`

-- Types

mkListType :: CheckedAnn -> CheckedExpr -> CheckedExpr
mkListType ann tElem = App ann (BuiltinContainerType ann List) [ExplicitArg tElem]

mkTensorType :: CheckedAnn -> CheckedExpr -> [Int] -> CheckedExpr
mkTensorType ann tElem dims =
  let listType = mkListType ann (Builtin ann (NumericType Nat)) in
  let dimExprs = fmap (Literal ann . LNat) dims in
  let dimList  = mkSeq ann (Builtin ann (NumericType Nat)) listType dimExprs in
  App ann (BuiltinContainerType ann Tensor) [ExplicitArg tElem, ExplicitArg dimList]

mkIsTruth :: CheckedAnn -> CheckedExpr -> CheckedExpr
mkIsTruth ann t = App ann (Builtin ann (TypeClass IsTruth)) [ExplicitArg t]

mkIsContainer :: CheckedAnn -> CheckedExpr -> CheckedExpr -> CheckedExpr
mkIsContainer ann tElem tCont = App ann (Builtin ann (TypeClass IsContainer))
  [ExplicitArg tElem, ExplicitArg tCont]

-- Expressions

mkLiteral' :: CheckedAnn -> Literal -> CheckedArg -> CheckedArg -> CheckedExpr
mkLiteral' ann lit t tc = App ann (Literal ann lit) [t, tc]

mkLiteral :: CheckedAnn -> Literal -> CheckedExpr -> CheckedExpr -> CheckedExpr
mkLiteral ann lit t tc = mkLiteral' ann lit
  (MachineImplicitArg t)
  (MachineImplicitArg (PrimDict tc))

mkBool :: CheckedAnn -> CheckedExpr -> Bool -> CheckedExpr
mkBool ann t b = mkLiteral ann (LBool b) t (PrimDict t)

mkNat :: CheckedAnn -> Int -> CheckedExpr
mkNat ann n  = let t = Builtin ann (NumericType Nat) in mkLiteral ann (LNat n) t (PrimDict t)

mkInt :: CheckedAnn -> Int -> CheckedExpr
mkInt ann i  = let t = Builtin ann (NumericType Int) in mkLiteral ann (LInt i) t (PrimDict t)

mkReal :: CheckedAnn -> Double -> CheckedExpr
mkReal ann r = let t = Builtin ann (NumericType Real) in mkLiteral ann (LRat r) t (PrimDict t)

mkSeq' :: CheckedAnn -> CheckedArg -> CheckedArg -> CheckedArg -> [CheckedExpr] -> CheckedExpr
mkSeq' ann tElem tCont tc xs = App ann (Seq ann xs) [tElem, tCont, tc]

mkSeq :: CheckedAnn -> CheckedExpr -> CheckedExpr -> [CheckedExpr] -> CheckedExpr
mkSeq ann tElem tCont = mkSeq' ann
  (MachineImplicitArg tElem)
  (MachineImplicitArg tCont)
  (MachineInstanceArg (PrimDict (mkIsContainer ann tElem tCont)))

-- Expressions

mkEq' :: CheckedAnn -> CheckedArg -> CheckedArg -> CheckedArg -> CheckedExpr -> CheckedExpr -> CheckedExpr
mkEq' ann tElem tRes tc e1 e2 = App ann (Builtin ann (Equality Eq))
  [tElem, tRes, tc, ExplicitArg e1, ExplicitArg e2]

mkEq :: CheckedAnn -> CheckedExpr -> CheckedExpr -> CheckedExpr -> CheckedExpr -> CheckedExpr
mkEq ann tElem tRes = mkEq' ann
  (MachineImplicitArg tElem)
  (MachineImplicitArg tRes)
  (MachineInstanceArg (PrimDict (mkIsTruth ann tRes)))

mkNot' :: CheckedAnn -> CheckedArg -> CheckedArg -> CheckedExpr -> CheckedExpr
mkNot' ann t tc e = App ann (Builtin ann Not) [t, tc, ExplicitArg e]

mkNot :: CheckedAnn -> CheckedExpr -> CheckedExpr  -> CheckedExpr
mkNot ann t = mkNot' ann
  (MachineImplicitArg t)
  (MachineInstanceArg (PrimDict (mkIsTruth ann t)))

mkBoolOp2' :: BooleanOp2 -> CheckedAnn -> CheckedArg -> CheckedArg -> CheckedExpr -> CheckedExpr -> CheckedExpr
mkBoolOp2' op ann t tc e1 e2 = App ann (Builtin ann (BooleanOp2 op))
  [t, tc, ExplicitArg e1, ExplicitArg e2]

mkBoolOp2 :: BooleanOp2 -> CheckedAnn -> CheckedExpr -> CheckedExpr -> CheckedExpr -> CheckedExpr
mkBoolOp2 op ann t = mkBoolOp2' op ann
  (MachineImplicitArg t)
  (MachineInstanceArg (PrimDict (mkIsTruth ann t)))

mkAt' :: CheckedAnn -> CheckedArg -> CheckedArg -> CheckedArg -> CheckedExpr -> CheckedExpr -> CheckedExpr
mkAt' ann tElem tCont tc xs i = App ann (Builtin ann At)
  [tElem, tCont, tc, ExplicitArg xs , ExplicitArg i ]

mkMap' :: CheckedAnn -> CheckedArg -> CheckedArg -> CheckedArg -> CheckedArg -> CheckedExpr
mkMap' ann tFrom tTo f xs = App ann (Builtin ann Map)
  [tFrom, tTo, f, xs]

mkFold' :: CheckedAnn -> CheckedArg -> CheckedArg -> CheckedArg -> CheckedArg -> CheckedExpr -> CheckedExpr -> CheckedExpr -> CheckedExpr
mkFold' ann tElem tCont tRes tc bop unit xs = App ann (Builtin ann Fold)
  [tElem, tCont, tRes, tc, ExplicitArg bop, ExplicitArg unit, ExplicitArg xs]

mkQuantifier :: CheckedAnn -> Quantifier -> Name -> CheckedExpr -> CheckedExpr -> CheckedExpr
mkQuantifier ann q n t e =
  App ann (Builtin ann (Quant q))
    (ExplicitArg (Lam ann (ExplicitBinder ann n t) e) :| [])

-- | Generates a name for a variable based on the indices, e.g. x [1,2,3] -> x_1_2_3
mkNameWithIndices :: Name -> [Int] -> Name
mkNameWithIndices Machine  _       = Machine
mkNameWithIndices (User n) indices = User $
  mconcat (n : ["_" <> pack (show index) | index <- indices])

substContainerType :: CheckedArg -> CheckedExpr -> CheckedExpr
substContainerType newTElem (App ann1 (BuiltinContainerType ann2 List)   [_tElem]) =
  App ann1 (BuiltinContainerType ann2 List) [newTElem]
substContainerType newTElem (App ann1 (BuiltinContainerType ann2 Tensor) [_tElem, tDims])  =
  App ann1 (BuiltinContainerType ann2 Tensor) [newTElem, tDims]
substContainerType _ _ = developerError "Provided an invalid container type"