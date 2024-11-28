{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Compile.Print
  ( PrettyUsing (..),
    PrettyWith,
    PrettyFriendly,
    PrettyVerbose,
    PrettyExternal,
    PrintableBuiltin,
    Tags (..),
    prettyVerbose,
    prettyFriendly,
    prettyExternal,
    prettyFriendlyEmptyCtx,
  )
where

import Data.Bifunctor (Bifunctor (..))
import Data.Foldable qualified as NonEmpty
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap (assocs)
import Data.Text (Text)
import GHC.TypeLits (ErrorMessage (..), TypeError)
import Prettyprinter (fill, list)
import Vehicle.Compile.Descope
import Vehicle.Compile.Normalise.Quote (unnormalise)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print.Builtin (ConvertableBuiltin, PrintableBuiltin)
import Vehicle.Compile.Simplify
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Meta.Map (MetaMap (..))
import Vehicle.Data.Assertion (Assertion (..), Bounds (..), Equality, Inequality, equalityExpr, inequalityExpr, strictness)
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Code.BooleanExpr
import Vehicle.Data.Code.LinearExpr (Constant (..), LinearExpr, prettyLinearExpr)
import Vehicle.Data.Code.Value
import Vehicle.Data.Tensor (RationalTensor)
import Vehicle.Syntax.AST.Expr qualified as S
import Vehicle.Syntax.Print

--------------------------------------------------------------------------------
-- Public methods
--------------------------------------------------------------------------------

type In a b = (a, b)

type NoCtx = ()

type VerboseTags = 'Unnamed ('ShortVectors ('As 'Internal))

type ExternalTags = 'Named ('ShortVectors ('As 'External))

type FriendlyTags = 'Named ('Uninserted ('As 'External))

type PrettyVerbose a = PrettyWith VerboseTags a

type PrettyExternal a = PrettyWith ExternalTags a

type PrettyFriendly a = PrettyWith FriendlyTags a

-- | Prints to the internal language in all it's gory detail. Useful for debugging.
prettyVerbose :: (PrettyVerbose (a `In` NoCtx)) => a -> Doc b
prettyVerbose x = prettyWith @VerboseTags (x, ())

-- | Prints to the internal language in all it's gory detail. Useful for debugging.
prettyExternal :: (PrettyExternal a) => a -> Doc b
prettyExternal = prettyWith @ExternalTags

-- | Prints to the external language for things that need to be displayed to
-- the user. Must provide the context of the thing being printed.
prettyFriendly :: (PrettyFriendly a) => a -> Doc b
prettyFriendly = prettyWith @FriendlyTags

-- | Prints to the external language for things that need to be displayed to
--  the user. Should only be used when the bound context is guaranteed to
-- be empty.
prettyFriendlyEmptyCtx ::
  forall f builtin b.
  (PrettyFriendly (f builtin `In` NamedBoundCtx)) =>
  f builtin ->
  Doc b
prettyFriendlyEmptyCtx x = prettyFriendly (x, emptyNamedCtx)

--------------------------------------------------------------------------------
-- Printing strategies
--------------------------------------------------------------------------------

-- Tags denote at a high-level how you want the term to be printed.
data Tags
  = -- | The final tag denotes which output grammar should be used
    As VehicleLang
  | -- | The `Named` tag ensures that the term is converted back to using named binders
    Named Tags
  | -- | The `Unnamed` tag denotes that the term should not be converted back to using named binders
    Unnamed Tags
  | -- | The `Uninserted` tag ensures that automatically inserted annotations and binders are removed.
    Uninserted Tags
  | -- | The `ShortVectors` tag ensures that long vectors are printed out concisely.
    ShortVectors Tags

-- | A strategy is an abstract representation of the sequence of operations that
-- are needed in order to convert something into a printable form. It should not
-- be confused with the actual operations needed to do so.
data Strategy
  = SetupContext Strategy
  | DescopeNaively Strategy
  | DescopeWithNames Strategy
  | PrintAs VehicleLang
  | QuoteValue Strategy
  | UninsertArgsAndBinders Strategy
  | ShortenVectors Strategy
  | Pretty

-- | This type family computes the correct printing strategy given the tags
-- and the type of the expression.
type family StrategyFor (tags :: Tags) a :: Strategy where
  ----------
  -- Expr --
  ----------
  -- To convert any named representation to the target language, simply convert it.
  StrategyFor ('As lang) S.Expr = 'PrintAs lang
  StrategyFor ('Named tags) S.Expr = StrategyFor tags S.Expr
  StrategyFor ('Unnamed tags) S.Expr = StrategyFor tags S.Expr
  -------------------------
  -- Variable conversion --
  -------------------------
  -- Converting an `Expr` with DeBruijn indices to a named representation requires a named bound context to descope.
  -- Otherwise converting it to an unnamed representation we descope naively by just converting the variables directly
  StrategyFor ('Named tags) (Expr builtin `In` NamedBoundCtx) = 'DescopeWithNames (StrategyFor tags S.Expr)
  StrategyFor ('Unnamed tags) (Expr builtin `In` ctx) = 'DescopeNaively (StrategyFor tags S.Expr)
  -- To print a `Value` we need to quote it first. Note that we convert it to a `Builtin` representation immediately
  StrategyFor ('Named tags) (Value builtin `In` NamedBoundCtx) = 'QuoteValue (StrategyFor ('Named tags) (Expr Builtin `In` NamedBoundCtx))
  StrategyFor ('Unnamed tags) (Value builtin `In` ctx) = 'DescopeNaively (StrategyFor tags S.Expr)
  -- Converting an `Expr` with DeBruijn indices to a named representation requires a named bound context to descope.
  -- Otherwise converting it to an unnamed representation we descope naively by just converting the variables directly
  StrategyFor ('Named tags) (LinearExpr constant `In` NamedBoundCtx) = 'DescopeWithNames (StrategyFor tags (constant `In` NamedBoundCtx))
  StrategyFor ('Unnamed tags) (LinearExpr constant `In` ctx) = 'DescopeNaively (StrategyFor tags (constant `In` ctx))
  --------------------------------
  -- Distributing over functors --
  --------------------------------
  StrategyFor tags ([a] `In` ctx) = StrategyFor tags (a `In` ctx)
  StrategyFor tags (Maybe a `In` ctx) = StrategyFor tags (a `In` ctx)
  StrategyFor tags (ConjunctAll a `In` ctx) = StrategyFor tags (a `In` ctx)
  StrategyFor tags (DisjunctAll a `In` ctx) = StrategyFor tags (a `In` ctx)
  StrategyFor tags (BooleanExpr a `In` ctx) = StrategyFor tags (a `In` ctx)
  StrategyFor tags (MaybeTrivial a `In` ctx) = StrategyFor tags (a `In` ctx)
  StrategyFor tags (IntMap a `In` ctx) = StrategyFor tags (a `In` ctx)
  StrategyFor tags (MetaMap a `In` ctx) = StrategyFor tags (a `In` ctx)
  StrategyFor tags (Equality constant `In` ctx) = StrategyFor tags (LinearExpr constant `In` ctx)
  StrategyFor tags (Inequality constant `In` ctx) = StrategyFor tags (LinearExpr constant `In` ctx)
  StrategyFor tags (Bounds constant `In` ctx) = StrategyFor tags (Inequality constant `In` ctx)
  StrategyFor tags (Assertion `In` ctx) = StrategyFor tags (LinearExpr RationalTensor `In` ctx)
  StrategyFor tags (GenericProg expr `In` ctx) = (StrategyFor tags (expr `In` ctx))
  StrategyFor tags (GenericDecl expr `In` ctx) = (StrategyFor tags (expr `In` ctx))
  StrategyFor tags (GenericArg expr `In` ctx) = (StrategyFor tags (expr `In` ctx))
  StrategyFor tags (GenericBinder expr `In` ctx) = (StrategyFor tags (expr `In` ctx))
  -----------------
  -- Constraints --
  -----------------
  StrategyFor tags (ArgInsertionProblem builtin `In` NamedBoundCtx) = StrategyFor tags (Expr builtin `In` NamedBoundCtx)
  StrategyFor tags (InstanceConstraint builtin `In` ConstraintContext builtin) = StrategyFor tags (Value builtin `In` NamedBoundCtx)
  StrategyFor tags (UnificationConstraint builtin `In` ConstraintContext builtin) = StrategyFor tags (Value builtin `In` NamedBoundCtx)
  StrategyFor tags (ApplicationConstraint builtin `In` ConstraintContext builtin) = StrategyFor tags (Value builtin `In` NamedBoundCtx)
  StrategyFor tags (Constraint builtin `In` ConstraintContext builtin) = StrategyFor tags (Value builtin `In` NamedBoundCtx)
  ------------
  -- Pretty --
  ------------
  -- Things that we just pretty print.
  StrategyFor tags (Int `In` ctx) = 'Pretty
  StrategyFor tags (Text `In` ctx) = 'Pretty
  StrategyFor tags (RationalTensor `In` ctx) = 'Pretty
  --------------------
  -- Simplification --
  --------------------
  StrategyFor ('Uninserted tags) a = 'UninsertArgsAndBinders (StrategyFor tags a)
  StrategyFor ('ShortVectors tags) a = 'ShortenVectors (StrategyFor tags a)
  -------------------
  -- Context setup --
  -------------------
  StrategyFor tags (GenericProg expr) = 'SetupContext (StrategyFor tags (GenericProg expr `In` NamedBoundCtx))
  StrategyFor tags (GenericDecl expr) = 'SetupContext (StrategyFor tags (GenericDecl expr `In` NamedBoundCtx))
  StrategyFor tags (Contextualised object ctx) = 'SetupContext (StrategyFor tags (object `In` ctx))
  StrategyFor tags (Contextualised object ctx `In` NoCtx) = 'SetupContext (StrategyFor tags (object `In` ctx))
  -- StrategyFor tags (Contextualised object (ConstraintContext builtin)) = 'SetupContext (StrategyFor tags (object `In` NamedBoundCtx))
  ----------------
  -- Error case --
  ----------------
  -- Otherwise if we cannot compute an error then throw an informative error
  -- at type-checking time.
  StrategyFor tags a =
    TypeError
      ( 'Text "Cannot print value of type \""
          ':<>: 'ShowType a
          ':<>: 'Text "\" with tags \""
          ':<>: 'ShowType tags
          ':<>: 'Text "\"."
          ':$$: 'Text "Perhaps you could add support to Vehicle.Compile.Print.StrategyFor?"
      )

--------------------------------------------------------------------------------
-- Executing printing strategies
--------------------------------------------------------------------------------

-- | A type synonym that takes the tags and the type and computes the strategy
-- for the combination to guide type-class resolution.
type PrettyWith tags a = PrettyUsing (StrategyFor tags a) a

class PrettyUsing (strategy :: Strategy) a where
  prettyUsing :: a -> Doc b

prettyWith :: forall tags a b. (PrettyWith tags a) => a -> Doc b
prettyWith = prettyUsing @(StrategyFor tags a) @a @b

--------------------------------------------------------------------------------
-- SetupContext

instance (PrettyUsing rest (object `In` ctx)) => PrettyUsing ('SetupContext rest) (Contextualised object ctx) where
  prettyUsing (WithContext e ctx) = prettyUsing @rest (e, ctx)

instance (PrettyUsing rest (object `In` ctx)) => PrettyUsing ('SetupContext rest) (Contextualised object ctx `In` NoCtx) where
  prettyUsing (WithContext e ctx, _) = prettyUsing @rest (e, ctx)

instance (PrettyUsing rest (GenericProg expr `In` NamedBoundCtx)) => PrettyUsing ('SetupContext rest) (GenericProg expr) where
  prettyUsing prog = prettyUsing @rest (prog, emptyNamedCtx)

instance (PrettyUsing rest (GenericDecl expr `In` NamedBoundCtx)) => PrettyUsing ('SetupContext rest) (GenericDecl expr) where
  prettyUsing decl = prettyUsing @rest (decl, emptyNamedCtx)

--------------------------------------------------------------------------------
-- DescopeNaively

-- Convert closed terms from DeBruijn representation to named representation naively
-- (It would be nice if we could `Functor` instances going, but I can't get it to work
-- with the type-classes without getting ambiguties)

-- Expr

instance (PrettyUsing rest S.Expr, PrintableBuiltin builtin) => PrettyUsing ('DescopeNaively rest) (Expr builtin `In` ctx) where
  prettyUsing (e, _ctx) = prettyUsing @rest $ descopeExprNaively e

instance
  (PrettyUsing rest S.Arg, PrintableBuiltin builtin) =>
  PrettyUsing ('DescopeNaively rest) (Arg builtin `In` ctx)
  where
  prettyUsing (e, _ctx) = prettyUsing @rest $ fmap descopeExprNaively e

instance
  (PrettyUsing rest S.Binder, PrintableBuiltin builtin) =>
  PrettyUsing ('DescopeNaively rest) (Binder builtin `In` ctx)
  where
  prettyUsing (e, _ctx) = prettyUsing @rest $ fmap descopeExprNaively e

instance
  (PrettyUsing rest S.Decl, PrintableBuiltin builtin) =>
  PrettyUsing ('DescopeNaively rest) (Decl builtin `In` ctx)
  where
  prettyUsing (e, _ctx) = prettyUsing @rest $ fmap descopeExprNaively e

instance
  (PrettyUsing rest S.Prog, PrintableBuiltin builtin) =>
  PrettyUsing ('DescopeNaively rest) (Prog builtin `In` ctx)
  where
  prettyUsing (e, _ctx) = prettyUsing @rest $ fmap descopeExprNaively e

-- Value

instance (PrettyUsing rest S.Expr, PrintableBuiltin builtin) => PrettyUsing ('DescopeNaively rest) (Value builtin `In` ctx) where
  prettyUsing (e, _ctx) = prettyUsing @rest $ descopeValueNaively @builtin e

instance
  (PrettyUsing rest S.Arg, PrintableBuiltin builtin) =>
  PrettyUsing ('DescopeNaively rest) (VArg builtin `In` ctx)
  where
  prettyUsing (e, _ctx) = prettyUsing @rest $ fmap descopeValueNaively e

instance
  (PrettyUsing rest S.Binder, PrintableBuiltin builtin) =>
  PrettyUsing ('DescopeNaively rest) (VBinder builtin `In` ctx)
  where
  prettyUsing (e, _ctx) = prettyUsing @rest $ fmap descopeValueNaively e

instance
  (PrettyUsing rest S.Decl, PrintableBuiltin builtin) =>
  PrettyUsing ('DescopeNaively rest) (VDecl builtin `In` ctx)
  where
  prettyUsing (e, _ctx) = prettyUsing @rest $ fmap descopeValueNaively e

instance
  (PrettyUsing rest S.Prog, PrintableBuiltin builtin) =>
  PrettyUsing ('DescopeNaively rest) (VProg builtin `In` ctx)
  where
  prettyUsing (e, _ctx) = prettyUsing @rest $ fmap descopeValueNaively e

-- Linear expression

instance
  (Constant constant, PrettyUsing rest (constant `In` ctx)) =>
  PrettyUsing ('DescopeNaively rest) (LinearExpr constant `In` ctx)
  where
  prettyUsing (lexp, ctx) = prettyLinearExpr pretty (prettyUsing @rest . (,ctx)) lexp

--------------------------------------------------------------------------------
-- 'DescopeWithNames

-- Convert open terms from DeBruijn representation to named representation
-- (It would be nice if we could `Functor` instances going, but I can't get it to work
-- with the type-classes without getting ambiguties)

-- Expr

instance
  (PrettyUsing rest S.Expr, PrintableBuiltin builtin) =>
  PrettyUsing ('DescopeWithNames rest) (Expr builtin `In` NamedBoundCtx)
  where
  prettyUsing (e, ctx) = prettyUsing @rest $ descopeExpr e ctx

instance
  (PrettyUsing rest S.Arg, PrintableBuiltin builtin) =>
  PrettyUsing ('DescopeWithNames rest) (Arg builtin `In` NamedBoundCtx)
  where
  prettyUsing (e, ctx) = prettyUsing @rest $ fmap (`descopeExpr` ctx) e

instance
  (PrettyUsing rest S.Binder, PrintableBuiltin builtin) =>
  PrettyUsing ('DescopeWithNames rest) (Binder builtin `In` NamedBoundCtx)
  where
  prettyUsing (e, ctx) = prettyUsing @rest $ fmap (`descopeExpr` ctx) e

instance
  (PrettyUsing rest S.Decl, PrintableBuiltin builtin) =>
  PrettyUsing ('DescopeWithNames rest) (Decl builtin `In` NamedBoundCtx)
  where
  prettyUsing (e, ctx) = prettyUsing @rest $ fmap (`descopeExpr` ctx) e

instance
  (PrettyUsing rest S.Prog, PrintableBuiltin builtin) =>
  PrettyUsing ('DescopeWithNames rest) (Prog builtin `In` NamedBoundCtx)
  where
  prettyUsing (e, ctx) = prettyUsing @rest $ fmap (`descopeExpr` ctx) e

-- Value

-- LinearExpr

instance
  (Constant constant, PrettyUsing rest (constant `In` NamedBoundCtx)) =>
  PrettyUsing ('DescopeWithNames rest) (LinearExpr constant `In` NamedBoundCtx)
  where
  prettyUsing (lexp, ctx) = prettyLinearExpr prettyVar prettyConst lexp
    where
      prettyConst c = prettyUsing @rest (c, ctx)
      prettyVar lv = case lookupLvInBoundCtx lv ctx of
        Nothing -> developerError $ "Missing name for variable" <+> pretty lv
        Just n -> pretty n

--------------------------------------------------------------------------------
-- 'PrintAs

-- Internal

instance PrettyUsing ('PrintAs 'Internal) S.Prog where
  prettyUsing (Main decls) =
    -- BNFC doesn't add empty lines so add them manually here.
    vsep2 $ fmap (prettyUsing @('PrintAs 'Internal)) decls

instance PrettyUsing ('PrintAs 'Internal) S.Decl where
  prettyUsing = printInternal

instance PrettyUsing ('PrintAs 'Internal) S.Expr where
  prettyUsing = printInternal

instance PrettyUsing ('PrintAs 'Internal) S.Arg where
  prettyUsing = printInternal

instance PrettyUsing ('PrintAs 'Internal) S.Binder where
  prettyUsing = printInternal

-- External

instance PrettyUsing ('PrintAs 'External) S.Prog where
  prettyUsing (Main decls) =
    -- BNFC doesn't add empty lines so add them manually here.
    vsep2 $ fmap (prettyUsing @('PrintAs 'External)) decls

instance PrettyUsing ('PrintAs 'External) S.Decl where
  prettyUsing = printExternal

instance PrettyUsing ('PrintAs 'External) S.Expr where
  prettyUsing = printExternal

instance PrettyUsing ('PrintAs 'External) S.Arg where
  prettyUsing = printExternal

instance PrettyUsing ('PrintAs 'External) S.Binder where
  prettyUsing = printExternal

--------------------------------------------------------------------------------
-- Simplification

instance
  (Simplify a, PrettyUsing rest a) =>
  PrettyUsing ('UninsertArgsAndBinders rest) a
  where
  prettyUsing e = prettyUsing @rest (uninsert e)

instance
  (Simplify a, PrettyUsing rest a) =>
  PrettyUsing ('ShortenVectors rest) a
  where
  prettyUsing e = prettyUsing @rest (shortenVec e)

instance (Pretty a) => PrettyUsing 'Pretty (a `In` ctx) where
  prettyUsing (x, _) = pretty x

--------------------------------------------------------------------------------
-- Instances for normalised types

instance
  (PrettyUsing rest (Expr Builtin), ConvertableBuiltin builtin Builtin) =>
  PrettyUsing ('QuoteValue rest) (Value builtin)
  where
  prettyUsing e = prettyUsing @rest $ unnormalise @(Value builtin) @(Expr Builtin) 0 e

instance
  (PrettyUsing rest (Arg Builtin), ConvertableBuiltin builtin Builtin) =>
  PrettyUsing ('QuoteValue rest) (VArg builtin)
  where
  prettyUsing e = prettyUsing @rest $ fmap (unnormalise @(Value builtin) @(Expr Builtin) 0) e

instance
  (PrettyUsing rest (Binder Builtin), ConvertableBuiltin builtin Builtin) =>
  PrettyUsing ('QuoteValue rest) (VBinder builtin)
  where
  prettyUsing e = prettyUsing @rest $ fmap (unnormalise @(Value builtin) @(Expr Builtin) 0) e

instance
  (PrettyUsing rest (Decl Builtin), ConvertableBuiltin builtin Builtin) =>
  PrettyUsing ('QuoteValue rest) (VDecl builtin)
  where
  prettyUsing e = prettyUsing @rest $ fmap (unnormalise @(Value builtin) @(Expr Builtin) 0) e

instance
  (PrettyUsing rest (Expr Builtin `In` NamedBoundCtx), ConvertableBuiltin builtin Builtin) =>
  PrettyUsing ('QuoteValue rest) (Value builtin `In` NamedBoundCtx)
  where
  prettyUsing (e, ctx) = do
    let e' = unnormalise @(Value builtin) @(Expr Builtin) (Lv $ length ctx) e
    prettyUsing @rest (e', ctx)

instance
  (PrettyUsing rest (Arg builtin `In` NamedBoundCtx), ConvertableBuiltin builtin Builtin) =>
  PrettyUsing ('QuoteValue rest) (Arg builtin `In` NamedBoundCtx)
  where
  prettyUsing (e, ctx) = prettyUsing @rest (e, ctx)

instance
  (PrettyUsing rest (Expr builtin `In` NamedBoundCtx), ConvertableBuiltin builtin Builtin) =>
  PrettyUsing ('QuoteValue rest) (Expr builtin `In` NamedBoundCtx)
  where
  prettyUsing (e, ctx) = prettyUsing @rest (e, ctx)

instance PrettyUsing rest (GenericBinder ()) where
  prettyUsing b = maybe "_" pretty (nameOf b)

--------------------------------------------------------------------------------
-- Instances for constraints

instance
  ( PrettyUsing rest (Expr builtin `In` NamedBoundCtx),
    PrettyUsing rest (Arg builtin `In` NamedBoundCtx)
  ) =>
  PrettyUsing rest (ArgInsertionProblem builtin `In` NamedBoundCtx)
  where
  prettyUsing (problem, ctx) = do
    let checkedExpr = solutionSoFar problem
    let checkedExprDoc = prettyUsing @rest (checkedExpr, ctx)
    let expectedTypeDoc = prettyUsing @rest (currentExpectedType problem, ctx)
    let uncheckedArgsDoc = prettyUsing @rest (uncheckedArgs problem, ctx)
    parens (checkedExprDoc <+> ":" <+> expectedTypeDoc) <+> "@" <+> uncheckedArgsDoc

prettyConstraintContext :: ConstraintContext builtin -> Doc a
prettyConstraintContext ctx =
  "#" <> pretty (constraintID ctx) <> ". " -- <+> pretty ctx

instance
  (PrettyUsing rest (Value builtin `In` NamedBoundCtx)) =>
  PrettyUsing rest (UnificationConstraint builtin `In` ConstraintContext builtin)
  where
  prettyUsing (Unify _ e1 e2, ctx) = do
    let e1' = prettyUsing @rest (e1, namedBoundCtxOf ctx)
    let e2' = prettyUsing @rest (e2, namedBoundCtxOf ctx)
    prettyConstraintContext ctx <+> e1' <+> "~" <+> e2'

instance
  (PrettyUsing rest (Value builtin `In` NamedBoundCtx)) =>
  PrettyUsing rest (InstanceConstraint builtin `In` ConstraintContext builtin)
  where
  prettyUsing (Resolve _ m _ expr, ctx) = do
    let expr' = prettyUsing @rest (expr, namedBoundCtxOf ctx)
    prettyConstraintContext ctx <+> pretty m <+> "<=" <+> expr'

instance
  (PrettyUsing rest (ArgInsertionProblem builtin `In` NamedBoundCtx)) =>
  PrettyUsing rest (ApplicationConstraint builtin `In` ConstraintContext builtin)
  where
  prettyUsing (InferArgs {..}, ctx) = do
    let problemDoc = prettyUsing @rest (argInsertionProblem, namedBoundCtxOf ctx)
    prettyConstraintContext ctx <+> parens (pretty exprSolutionMeta <+> "=" <+> problemDoc) <+> ":" <+> pretty typeSolutionMeta

instance
  ( PrettyUsing rest (UnificationConstraint builtin `In` ctx),
    PrettyUsing rest (InstanceConstraint builtin `In` ctx),
    PrettyUsing rest (ApplicationConstraint builtin `In` ctx)
  ) =>
  PrettyUsing rest (Constraint builtin `In` ctx)
  where
  prettyUsing (c, ctx) = case c of
    UnificationConstraint uc -> prettyUsing @rest (uc, ctx)
    InstanceConstraint tc -> prettyUsing @rest (tc, ctx)
    ApplicationConstraint tc -> prettyUsing @rest (tc, ctx)

--------------------------------------------------------------------------------
-- Assertions

instance
  (Constant constant, PrettyUsing rest (LinearExpr constant `In` ctx)) =>
  PrettyUsing rest (Equality constant `In` ctx)
  where
  prettyUsing (e, ctx) = prettyUsing @rest (equalityExpr e, ctx) <+> "== 0"

instance
  (Constant constant, PrettyUsing rest (LinearExpr constant `In` ctx)) =>
  PrettyUsing rest (Inequality constant `In` ctx)
  where
  prettyUsing (e, ctx) =
    prettyUsing @rest (inequalityExpr e, ctx)
      <+> (if strictness e == Strict then "<" else "<=")
      <+> "0.0"

instance
  (Constant constant, PrettyUsing rest (Inequality constant `In` ctx)) =>
  PrettyUsing rest (Bounds constant `In` ctx)
  where
  prettyUsing (Bounds {..}, ctx) =
    "below by max"
      <+> vsep (fmap (prettyUsing @rest . (,ctx)) lowerBounds)
      <+> "and"
      <+> "above by min"
      <+> vsep (fmap (prettyUsing @rest . (,ctx)) upperBounds)

instance
  ( PrettyUsing rest (Inequality RationalTensor `In` ctx),
    PrettyUsing rest (Equality RationalTensor `In` ctx)
  ) =>
  PrettyUsing rest (Assertion `In` ctx)
  where
  prettyUsing (e, ctx) = case e of
    RationalEq eq -> prettyUsing @rest (eq, ctx)
    RationalIneq ineq -> prettyUsing @rest (ineq, ctx)
    TensorEq eq -> prettyUsing @rest (eq, ctx)

--------------------------------------------------------------------------------
-- Instances for functors types

instance
  (PrettyUsing rest (a `In` ctx)) =>
  PrettyUsing rest ([a] `In` ctx)
  where
  prettyUsing (es, ctx) = list (prettyUsing @rest . (,ctx) <$> es)

instance
  (PrettyUsing rest (a `In` ctx)) =>
  PrettyUsing rest (Maybe a `In` ctx)
  where
  prettyUsing (e, ctx) = case e of
    Nothing -> ""
    Just x -> prettyUsing @rest (x, ctx)

instance (PrettyUsing rest (a `In` ctx)) => PrettyUsing rest (MetaMap a `In` ctx) where
  prettyUsing (MetaMap m, ctx) = prettyMapEntries entries
    where
      entries = fmap (bimap (fill 3 . pretty . MetaID) (prettyUsing @rest . (,ctx))) (IntMap.assocs m)

instance (PrettyUsing rest (a `In` ctx)) => PrettyUsing rest (MaybeTrivial a `In` ctx) where
  prettyUsing (e, ctx) = case e of
    Trivial True -> "True"
    Trivial False -> "False"
    NonTrivial x -> prettyUsing @rest (x, ctx)

instance (PrettyUsing rest (a `In` ctx)) => PrettyUsing rest (ConjunctAll a `In` ctx) where
  prettyUsing (ConjunctAll cs, ctx) = "and" <> line <> indent 2 (vsep docs)
    where
      docs = NonEmpty.toList (fmap (prettyUsing @rest . (,ctx)) cs)

instance (PrettyUsing rest (a `In` ctx)) => PrettyUsing rest (DisjunctAll a `In` ctx) where
  prettyUsing (DisjunctAll cs, ctx) = "or" <> line <> indent 2 (vsep docs)
    where
      docs = NonEmpty.toList (fmap (prettyUsing @rest . (,ctx)) cs)

instance (PrettyUsing rest (a `In` ctx)) => PrettyUsing rest (BooleanExpr a `In` ctx) where
  prettyUsing (e, ctx) = case e of
    Query x -> prettyUsing @rest (x, ctx)
    Disjunct xs -> prettyUsing @rest (xs, ctx)
    Conjunct xs -> prettyUsing @rest (xs, ctx)
