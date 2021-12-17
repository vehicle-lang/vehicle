module Vehicle.Language.AST.Utils where

import Control.Monad (void)
import Data.Functor.Foldable (Recursive(..))
import Data.List.NonEmpty qualified as NonEmpty (toList)
import Data.Text (pack)

import Vehicle.Prelude
import Vehicle.Language.AST.Core
import Vehicle.Language.AST.DeBruijn
import Vehicle.Language.AST.Builtin
import Vehicle.Language.AST.Name
import Vehicle.Language.AST.Visibility (Owner(..))

--------------------------------------------------------------------------------
-- Patterns

pattern Type0 :: Expr binder var ann
pattern Type0 = Type 0

pattern Type1 :: Expr binder var ann
pattern Type1 = Type 1

--------------------------------------------------------------------------------
-- Type synonyms

-- * Type of annotations attached to the Frontend AST after parsing
-- before being analysed by the compiler

type InputBinding = Maybe NamedBinding
type InputVar     = NamedVar
type InputAnn     = (Provenance, Owner)

type InputArg       = Arg    InputBinding InputVar InputAnn
type InputBinder    = Binder InputBinding InputVar InputAnn
type InputExpr      = Expr   InputBinding InputVar InputAnn
type InputDecl      = Decl   InputBinding InputVar InputAnn
type InputProg      = Prog   InputBinding InputVar InputAnn

-- * Types pre type-checking

type UncheckedBinding = DBBinding
type UncheckedVar     = DBVar
type UncheckedAnn     = (Provenance, Owner)

type UncheckedBinder = DBBinder UncheckedAnn
type UncheckedArg    = DBArg    UncheckedAnn
type UncheckedExpr   = DBExpr   UncheckedAnn
type UncheckedDecl   = DBDecl   UncheckedAnn
type UncheckedProg   = DBProg   UncheckedAnn

-- * Types post type-checking

type CheckedBinding = DBBinding
type CheckedVar     = DBVar
type CheckedAnn     = (Provenance, Owner)

type CheckedBinder = DBBinder  CheckedAnn
type CheckedArg    = DBArg     CheckedAnn
type CheckedExpr   = DBExpr    CheckedAnn
type CheckedDecl   = DBDecl    CheckedAnn
type CheckedProg   = DBProg    CheckedAnn

-- * Type of annotations attached to the Core AST that are output by the compiler

type OutputBinding = NamedBinding
type OutputVar     = NamedVar
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
  ctxNames :: a -> [DBBinding]

instance IsBoundCtx [DBBinding] where
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
  PrimDictF _ _                 -> []
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
  LSeqF     _ _ xs              -> concat xs

--------------------------------------------------------------------------------
-- Destruction functions

toHead :: Expr binder var ann -> (Expr binder var ann, [Arg binder var ann])
toHead (App _ann fun args ) = (fun, NonEmpty.toList args)
toHead e                    = (e, [])

exprHead :: Expr binder var ann -> Expr binder var ann
exprHead = fst . toHead

--------------------------------------------------------------------------------
-- Views

getQuantifierSymbol :: Binder DBBinding var ann -> Symbol
getQuantifierSymbol binder = case nameOf binder of
  Just symbol -> symbol
  Nothing     -> developerError "Should not have quantifiers with machine names?"

--------------------------------------------------------------------------------
-- Construction functions

-- | Generates a name for a variable based on the indices, e.g. x [1,2,3] -> x_1_2_3
mkNameWithIndices :: Symbol -> [Int] -> Symbol
mkNameWithIndices n indices = mconcat (n : ["_" <> pack (show index) | index <- indices])

removeAnnotations :: Functor (t binder var) => t binder var ann -> t binder var ()
removeAnnotations = void