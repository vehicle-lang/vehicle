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
    PrintableBuiltin (..),
    Tags (..),
    prettyVerbose,
    prettyFriendly,
    prettyExternal,
    prettyFriendlyEmptyCtx,
  )
where

import Data.Bifunctor (bimap)
import Data.Foldable qualified as NonEmpty
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap (assocs)
import Data.Text (Text)
import GHC.TypeLits (ErrorMessage (..), TypeError)
import Prettyprinter (list)
import Vehicle.Backend.Queries.LinearExpr (UnreducedAssertion (..), VectorEquality (..))
import Vehicle.Compile.Descope
import Vehicle.Compile.Normalise.Quote (unnormalise)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Simplify
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Meta.Map (MetaMap (..))
import Vehicle.Compile.Type.Subsystem.Standard.Core
import Vehicle.Data.BooleanExpr
import Vehicle.Data.BuiltinInterface
import Vehicle.Data.NormalisedExpr
import Vehicle.Syntax.Print

--------------------------------------------------------------------------------
-- Public methods
--------------------------------------------------------------------------------

type VerboseTags = 'Unnamed ('StandardiseBuiltin ('ShortVectors ('As 'Internal)))

type ExternalTags = 'Named ('StandardiseBuiltin ('ShortVectors ('As 'External)))

type FriendlyTags = 'Named ('StandardiseBuiltin ('Uninserted ('As 'External)))

type PrettyVerbose a = PrettyWith VerboseTags a

type PrettyExternal a = PrettyWith ExternalTags a

type PrettyFriendly a = PrettyWith FriendlyTags a

-- | Prints to the internal language in all it's gory detail. Does not convert
--  (Co)DeBruijn indices back to names. Useful for debugging.
prettyVerbose :: (PrettyVerbose a) => a -> Doc b
prettyVerbose = prettyWith @VerboseTags

-- | Prints to the internal language in all it's gory detail. Does not convert
--  (Co)DeBruijn indices back to names. Useful for debugging.
prettyExternal :: (PrettyExternal a) => a -> Doc b
prettyExternal = prettyWith @ExternalTags

-- | Prints to the external language for things that need to be displayed to
--  the user. Must provide the context of the thing being printed.
prettyFriendly :: (PrettyFriendly a) => a -> Doc b
prettyFriendly = prettyWith @FriendlyTags

-- | Prints to the external language for things that need to be displayed to
--  the user. Should only be used when the bound context is guaranteed to
-- be empty.
prettyFriendlyEmptyCtx ::
  forall f builtin b.
  (PrettyFriendly (Contextualised (f builtin) (BoundCtx builtin))) =>
  f builtin ->
  Doc b
prettyFriendlyEmptyCtx x = prettyFriendly (WithContext x (emptyBoundCtx @builtin))

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
  | -- | The `StandardiseBuiltin` tag ensures that the term is converted back to the standard set of builtins
    StandardiseBuiltin Tags

-- | A strategy is an abstract representation of the sequence of operations that
-- are needed in order to convert something into a printable form. It should not
-- be confused with the actual operations needed to do so.
data Strategy
  = PrintAs VehicleLang
  | ConvertBuiltins Strategy
  | DescopeNaively Strategy
  | DescopeWithNames Strategy
  | Denormalise Strategy
  | DiscardConstraintCtx Strategy
  | KeepConstraintCtx Strategy
  | UninsertArgsAndBinders Strategy
  | ShortenVectors Strategy
  | MapTuple2 Strategy Strategy
  | MapTuple3 Strategy Strategy Strategy
  | Opaque Strategy
  | Pretty

-- | This type family computes the correct printing strategy given the tags
-- and the type of the expression.
type family StrategyFor (tags :: Tags) a :: Strategy where
  -- To convert any named representation to the target language, simply convert it.
  StrategyFor ('As lang) (Prog Name Builtin) = 'PrintAs lang
  StrategyFor ('As lang) (Decl Name Builtin) = 'PrintAs lang
  StrategyFor ('As lang) (Expr Name Builtin) = 'PrintAs lang
  StrategyFor ('As lang) (Arg Name Builtin) = 'PrintAs lang
  StrategyFor ('As lang) (Binder Name Builtin) = 'PrintAs lang
  -- To print a DB expr in an unnamed representation, simply naively descope.
  StrategyFor ('Unnamed tags) (Prog Ix builtin) = 'DescopeNaively (StrategyFor tags (Prog Name builtin))
  StrategyFor ('Unnamed tags) (Decl Ix builtin) = 'DescopeNaively (StrategyFor tags (Decl Name builtin))
  StrategyFor ('Unnamed tags) (Expr Ix builtin) = 'DescopeNaively (StrategyFor tags (Expr Name builtin))
  StrategyFor ('Unnamed tags) (Arg Ix builtin) = 'DescopeNaively (StrategyFor tags (Arg Name builtin))
  StrategyFor ('Unnamed tags) (Binder Ix builtin) = 'DescopeNaively (StrategyFor tags (Binder Name builtin))
  -- To print a normalised expr in an unnamed representation, simply naively descope.
  StrategyFor ('Unnamed tags) (WHNFValue builtin) = 'DescopeNaively (StrategyFor tags (Expr Name builtin))
  StrategyFor ('Unnamed tags) (WHNFArg builtin) = 'DescopeNaively (StrategyFor tags (Arg Name builtin))
  StrategyFor ('Unnamed tags) (WHNFBinder builtin) = 'DescopeNaively (StrategyFor tags (Binder Name builtin))
  StrategyFor tags (EnvValue 'WHNF builtin) = StrategyFor tags (WHNFValue builtin)
  StrategyFor tags (GenericBinder ()) = 'Pretty
  -- To standardise builtins
  StrategyFor ('StandardiseBuiltin tags) (Prog Name builtin) = 'ConvertBuiltins (StrategyFor tags (Prog Name Builtin))
  StrategyFor ('StandardiseBuiltin tags) (Decl Name builtin) = 'ConvertBuiltins (StrategyFor tags (Decl Name Builtin))
  StrategyFor ('StandardiseBuiltin tags) (Expr Name builtin) = 'ConvertBuiltins (StrategyFor tags (Expr Name Builtin))
  StrategyFor ('StandardiseBuiltin tags) (Arg Name builtin) = 'ConvertBuiltins (StrategyFor tags (Arg Name Builtin))
  StrategyFor ('StandardiseBuiltin tags) (Binder Name builtin) = 'ConvertBuiltins (StrategyFor tags (Binder Name Builtin))
  -- To convert an expression using a named representation to a named representation is a no-op.
  StrategyFor ('Named tags) (Prog Name builtin) = StrategyFor tags (Prog Name builtin)
  StrategyFor ('Named tags) (Decl Name builtin) = StrategyFor tags (Decl Name builtin)
  StrategyFor ('Named tags) (Expr Name builtin) = StrategyFor tags (Expr Name builtin)
  StrategyFor ('Named tags) (Arg Name builtin) = StrategyFor tags (Arg Name builtin)
  StrategyFor ('Named tags) (Binder Name builtin) = StrategyFor tags (Binder Name builtin)
  -- To convert a closed expression using a DB representation but whose missing names have been supplied
  -- to a named representation, perform the Checked to named conversion. For expressions, args, binders
  -- we need to have the context in scope.
  StrategyFor ('Named tags) (Prog Ix builtin) = 'DescopeWithNames (StrategyFor tags (Prog Name Builtin))
  StrategyFor ('Named tags) (Decl Ix builtin) = 'DescopeWithNames (StrategyFor tags (Decl Name Builtin))
  StrategyFor ('Named tags) (Contextualised (Expr Ix builtin) (BoundCtx builtin)) = 'DescopeWithNames (StrategyFor tags (Expr Name Builtin))
  StrategyFor ('Named tags) (Contextualised (Arg Ix builtin) (BoundCtx builtin)) = 'DescopeWithNames (StrategyFor tags (Arg Name Builtin))
  StrategyFor ('Named tags) (Contextualised (Binder Ix builtin) (BoundCtx builtin)) = 'DescopeWithNames (StrategyFor tags (Binder Name Builtin))
  -- To convert a named normalised expr, first denormalise to a checked expr.
  StrategyFor ('Named tags) (Contextualised (WHNFValue builtin) (BoundCtx builtin)) = 'Denormalise (StrategyFor ('Named tags) (Contextualised (Expr Ix builtin) (BoundCtx builtin)))
  -- To convert an assertion simply defer to normalised expressions
  StrategyFor tags UnreducedAssertion = StrategyFor tags (WHNFValue Builtin)
  StrategyFor tags (Contextualised UnreducedAssertion (BoundCtx builtin)) = StrategyFor tags (Contextualised (WHNFValue Builtin) (BoundCtx builtin))
  -- Things that we just pretty print.
  StrategyFor tags Int = 'Pretty
  StrategyFor tags Text = 'Pretty
  StrategyFor tags (Contextualised Text ctx) = StrategyFor tags Text
  -- Objects for which we want to block the strategy computation on.
  StrategyFor ('Named tags) (Contextualised (Constraint builtin) (ConstraintContext builtin)) = 'KeepConstraintCtx (StrategyFor ('Named tags) (Contextualised (WHNFValue Builtin) (BoundCtx Builtin)))
  StrategyFor ('Named tags) (Contextualised (InstanceConstraint builtin) (ConstraintContext builtin)) = 'KeepConstraintCtx (StrategyFor ('Named tags) (Contextualised (WHNFValue Builtin) (BoundCtx Builtin)))
  StrategyFor ('Named tags) (Contextualised (UnificationConstraint builtin) (ConstraintContext builtin)) = 'KeepConstraintCtx (StrategyFor ('Named tags) (Contextualised (WHNFValue Builtin) (BoundCtx Builtin)))
  StrategyFor tags (Contextualised (Constraint builtin) (ConstraintContext builtin)) = 'DiscardConstraintCtx (StrategyFor tags (WHNFValue Builtin))
  StrategyFor tags (Contextualised (InstanceConstraint builtin) (ConstraintContext builtin)) = 'DiscardConstraintCtx (StrategyFor tags (WHNFValue Builtin))
  StrategyFor tags (Contextualised (UnificationConstraint builtin) (ConstraintContext builtin)) = 'DiscardConstraintCtx (StrategyFor tags (WHNFValue Builtin))
  StrategyFor tags (MetaMap a) = 'Opaque (StrategyFor tags a)
  -- Simplification
  StrategyFor ('Uninserted tags) a = 'UninsertArgsAndBinders (StrategyFor tags a)
  StrategyFor ('ShortVectors tags) a = 'ShortenVectors (StrategyFor tags a)
  -- Things were we just print the structure and recursively print through.
  StrategyFor tags (a, b) = 'MapTuple2 (StrategyFor tags a) (StrategyFor tags b)
  StrategyFor tags (a, b, c) = 'MapTuple3 (StrategyFor tags a) (StrategyFor tags b) (StrategyFor tags c)
  StrategyFor tags (IntMap a) = 'Opaque (StrategyFor tags a)
  StrategyFor tags [a] = 'Opaque (StrategyFor tags a)
  StrategyFor tags (Maybe a) = 'Opaque (StrategyFor tags a)
  StrategyFor tags (BooleanExpr a) = 'Opaque (StrategyFor tags a)
  StrategyFor tags (ConjunctAll a) = 'Opaque (StrategyFor tags a)
  StrategyFor tags (DisjunctAll a) = 'Opaque (StrategyFor tags a)
  StrategyFor tags (MaybeTrivial a) = 'Opaque (StrategyFor tags a)
  StrategyFor tags (Contextualised (a, b) ctx) =
    'MapTuple2 (StrategyFor tags (Contextualised a ctx)) (StrategyFor tags (Contextualised b ctx))
  StrategyFor tags (Contextualised [a] ctx) =
    StrategyFor tags [Contextualised a ctx]
  StrategyFor tags (Contextualised (BooleanExpr a) ctx) =
    StrategyFor tags (BooleanExpr (Contextualised a ctx))
  StrategyFor tags (Contextualised (Maybe a) ctx) =
    StrategyFor tags (Maybe (Contextualised a ctx))
  StrategyFor tags (Contextualised (ConjunctAll a) ctx) =
    StrategyFor tags (ConjunctAll (Contextualised a ctx))
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
-- Printing to internal language

instance PrettyUsing ('PrintAs 'Internal) (Prog Name Builtin) where
  prettyUsing (Main decls) =
    -- BNFC doesn't add empty lines so add them manually here.
    vsep2 $ fmap (prettyUsing @('PrintAs 'Internal)) decls

instance PrettyUsing ('PrintAs 'Internal) (Decl Name Builtin) where
  prettyUsing = printInternal

instance PrettyUsing ('PrintAs 'Internal) (Expr Name Builtin) where
  prettyUsing = printInternal

instance PrettyUsing ('PrintAs 'Internal) (Arg Name Builtin) where
  prettyUsing = printInternal

instance PrettyUsing ('PrintAs 'Internal) (Binder Name Builtin) where
  prettyUsing = printInternal

--------------------------------------------------------------------------------
-- Printing to external language

instance PrettyUsing ('PrintAs 'External) (Prog Name Builtin) where
  prettyUsing (Main decls) =
    -- BNFC doesn't add empty lines so add them manually here.
    vsep2 $ fmap (prettyUsing @('PrintAs 'External)) decls

instance PrettyUsing ('PrintAs 'External) (Decl Name Builtin) where
  prettyUsing = printExternal

instance PrettyUsing ('PrintAs 'External) (Expr Name Builtin) where
  prettyUsing = printExternal

instance PrettyUsing ('PrintAs 'External) (Arg Name Builtin) where
  prettyUsing = printExternal

instance PrettyUsing ('PrintAs 'External) (Binder Name Builtin) where
  prettyUsing = printExternal

--------------------------------------------------------------------------------
-- Converting builtins

instance
  (PrintableBuiltin builtin, PrettyUsing rest (Prog Name Builtin)) =>
  PrettyUsing ('ConvertBuiltins rest) (Prog Name builtin)
  where
  prettyUsing = prettyUsing @rest . fmap convertExprBuiltins

instance
  (PrintableBuiltin builtin, PrettyUsing rest (Decl Name Builtin)) =>
  PrettyUsing ('ConvertBuiltins rest) (Decl Name builtin)
  where
  prettyUsing = prettyUsing @rest . fmap convertExprBuiltins

instance
  (PrintableBuiltin builtin, PrettyUsing rest (Expr Name Builtin)) =>
  PrettyUsing ('ConvertBuiltins rest) (Expr Name builtin)
  where
  prettyUsing = prettyUsing @rest . convertExprBuiltins

instance
  (PrintableBuiltin builtin, PrettyUsing rest (Arg Name Builtin)) =>
  PrettyUsing ('ConvertBuiltins rest) (Arg Name builtin)
  where
  prettyUsing = prettyUsing @rest . fmap convertExprBuiltins

instance
  (PrintableBuiltin builtin, PrettyUsing rest (Binder Name Builtin)) =>
  PrettyUsing ('ConvertBuiltins rest) (Binder Name builtin)
  where
  prettyUsing = prettyUsing @rest . fmap convertExprBuiltins

convertExprBuiltins ::
  forall builtin var.
  (PrintableBuiltin builtin) =>
  Expr var builtin ->
  Expr var Builtin
convertExprBuiltins = mapBuiltins $ \p1 p2 b args ->
  normAppList p1 (convertBuiltin p2 b) args

--------------------------------------------------------------------------------
-- Convert closed terms from DeBruijn representation to named representation naively

instance (PrettyUsing rest (Prog Name builtin)) => PrettyUsing ('DescopeNaively rest) (Prog Ix builtin) where
  prettyUsing = prettyUsing @rest . descopeNaive

instance (PrettyUsing rest (Decl Name builtin)) => PrettyUsing ('DescopeNaively rest) (Decl Ix builtin) where
  prettyUsing = prettyUsing @rest . descopeNaive

instance (PrettyUsing rest (Expr Name builtin)) => PrettyUsing ('DescopeNaively rest) (Expr Ix builtin) where
  prettyUsing = prettyUsing @rest . descopeNaive

instance (PrettyUsing rest (Arg Name builtin)) => PrettyUsing ('DescopeNaively rest) (Arg Ix builtin) where
  prettyUsing = prettyUsing @rest . descopeNaive

instance (PrettyUsing rest (Binder Name builtin)) => PrettyUsing ('DescopeNaively rest) (Binder Ix builtin) where
  prettyUsing = prettyUsing @rest . descopeNaive

instance (PrettyUsing rest (Expr Name builtin)) => PrettyUsing ('DescopeNaively rest) (WHNFValue builtin) where
  prettyUsing = prettyUsing @rest . descopeNaive

instance (PrettyUsing rest (Arg Name builtin)) => PrettyUsing ('DescopeNaively rest) (WHNFArg builtin) where
  prettyUsing = prettyUsing @rest . descopeNaive

instance (PrettyUsing rest (Binder Name builtin)) => PrettyUsing ('DescopeNaively rest) (WHNFBinder builtin) where
  prettyUsing = prettyUsing @rest . descopeNaive

--------------------------------------------------------------------------------
-- Convert open terms from DeBruijn representation to named representation

instance (PrettyUsing rest (Prog Name builtin)) => PrettyUsing ('DescopeWithNames rest) (Prog Ix builtin) where
  prettyUsing = prettyUsing @rest . descopeNamed

instance (PrettyUsing rest (Decl Name builtin)) => PrettyUsing ('DescopeWithNames rest) (Decl Ix builtin) where
  prettyUsing = prettyUsing @rest . descopeNamed

instance
  (PrettyUsing rest (Expr Name builtin)) =>
  PrettyUsing ('DescopeWithNames rest) (Contextualised (Expr Ix builtin) (BoundCtx builtin))
  where
  prettyUsing (WithContext e ctx) = prettyUsing @rest $ descopeNamed (WithContext e (fmap nameOf ctx))

instance
  (PrettyUsing rest (Arg Name builtin)) =>
  PrettyUsing ('DescopeWithNames rest) (Contextualised (Arg Ix builtin) (BoundCtx builtin))
  where
  prettyUsing (WithContext e ctx) = prettyUsing @rest $ descopeNamed (WithContext e (fmap nameOf ctx))

instance
  (PrettyUsing rest (Binder Name builtin)) =>
  PrettyUsing ('DescopeWithNames rest) (Contextualised (Binder Ix builtin) (BoundCtx builtin))
  where
  prettyUsing (WithContext e ctx) = prettyUsing @rest $ descopeNamed (WithContext e (fmap nameOf ctx))

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

--------------------------------------------------------------------------------
-- Other

instance
  (PrettyUsing rest a) =>
  PrettyUsing ('Opaque rest) [a]
  where
  prettyUsing es = list (prettyUsing @rest <$> es)

instance
  (PrettyUsing rest a) =>
  PrettyUsing ('Opaque rest) (Maybe a)
  where
  prettyUsing = \case
    Nothing -> ""
    Just x -> prettyUsing @rest x

instance (PrettyUsing rest a) => PrettyUsing ('Opaque rest) (IntMap a) where
  prettyUsing es = prettyIntMap (prettyUsing @rest <$> es)

instance (PrettyUsing resta a, PrettyUsing restb b) => PrettyUsing ('MapTuple2 resta restb) (a, b) where
  prettyUsing (e1, e2) = "(" <> prettyUsing @resta e1 <> "," <+> prettyUsing @restb e2 <> ")"

instance
  (PrettyUsing resta (Contextualised a ctx), PrettyUsing restb (Contextualised b ctx)) =>
  PrettyUsing ('MapTuple2 resta restb) (Contextualised (a, b) ctx)
  where
  prettyUsing (WithContext (e1, e2) ctx) =
    parens (prettyUsing @resta (WithContext e1 ctx) <> "," <+> prettyUsing @restb (WithContext e2 ctx))

instance
  (PrettyUsing resta a, PrettyUsing restb b, PrettyUsing restc c) =>
  PrettyUsing ('MapTuple3 resta restb restc) (a, b, c)
  where
  prettyUsing (e1, e2, e3) =
    "("
      <> prettyUsing @resta e1
      <> ","
        <+> prettyUsing @restb e2
      <> ","
        <+> prettyUsing @restc e3
      <> ")"

--------------------------------------------------------------------------------
-- Instances which defer to primitive pretty instances

instance PrettyUsing 'Pretty Int where
  prettyUsing = pretty

instance PrettyUsing 'Pretty Text where
  prettyUsing = pretty

instance
  (PrettyUsing rest Text) =>
  (PrettyUsing rest (Contextualised Text ctx))
  where
  prettyUsing = prettyUsing @rest . objectIn

--------------------------------------------------------------------------------
-- Instances for normalised types

instance
  (PrettyUsing rest (Contextualised (Expr Ix builtin) (BoundCtx builtin))) =>
  PrettyUsing ('Denormalise rest) (Contextualised (WHNFValue builtin) (BoundCtx builtin))
  where
  prettyUsing (WithContext e ctx) = do
    let e' = unnormalise @(WHNFValue builtin) @(Expr Ix builtin) (Lv $ length ctx) e
    prettyUsing @rest (WithContext e' ctx)

instance (PrettyUsing rest (Expr Ix builtin)) => PrettyUsing ('Denormalise rest) (WHNFValue builtin) where
  prettyUsing e = prettyUsing @rest (unnormalise @(WHNFValue builtin) @(Expr Ix builtin) 0 e)

instance (PrettyUsing rest (Arg Ix builtin)) => PrettyUsing ('Denormalise rest) (WHNFArg builtin) where
  prettyUsing e = prettyUsing @rest (unnormalise @(WHNFArg builtin) @(Arg Ix builtin) 0 e)

instance (PrettyUsing rest (WHNFValue builtin)) => PrettyUsing rest (EnvValue 'WHNF builtin) where
  prettyUsing = \case
    Bound {} -> ""
    Defined v -> prettyUsing @rest v

instance PrettyUsing rest (GenericBinder ()) where
  prettyUsing b = maybe "_" pretty (nameOf b)

--------------------------------------------------------------------------------
-- Instances for unreduced assertions

instance
  (PrettyUsing rest (WHNFValue Builtin)) =>
  PrettyUsing rest UnreducedAssertion
  where
  prettyUsing = \case
    VectorEqualityAssertion VectorEquality {..} -> do
      let lhs = prettyUsing @rest assertionLHS
      let rhs = prettyUsing @rest assertionRHS
      prettyVectorEquality lhs rhs assertionDims
    NonVectorEqualityAssertion expr -> prettyUsing @rest expr

instance
  (PrettyUsing rest (Contextualised (WHNFValue Builtin) (BoundCtx builtin))) =>
  PrettyUsing rest (Contextualised UnreducedAssertion (BoundCtx builtin))
  where
  prettyUsing (WithContext assertion ctx) = case assertion of
    VectorEqualityAssertion VectorEquality {..} -> do
      let lhs = prettyUsing @rest (WithContext assertionLHS ctx)
      let rhs = prettyUsing @rest (WithContext assertionRHS ctx)
      prettyVectorEquality lhs rhs assertionDims
    NonVectorEqualityAssertion expr -> prettyUsing @rest (WithContext expr ctx)

prettyVectorEquality ::
  Doc a ->
  Doc a ->
  TensorDimensions ->
  Doc a
prettyVectorEquality lhs rhs _dims = do
  -- let dimsDoc = if null dims then "Rat" else "Tensor Rat" <+> pretty dims
  lhs <+> pretty Eq <+> rhs -- <> "     " <> parens dimsDoc

--------------------------------------------------------------------------------
-- Instances for constraints

prettyUnify :: Doc a -> Doc a -> Doc a
prettyUnify e1 e2 = e1 <+> "~" <+> e2

prettyTypeClass :: MetaID -> Doc a -> Doc a
prettyTypeClass m expr = pretty m <+> "<=" <+> expr

prettyConstraintContext :: Doc a -> ConstraintContext builtin -> Doc a
prettyConstraintContext constraint ctx =
  "#" <> pretty (constraintID ctx) <> ". " <+> constraint -- <+> pretty ctx

instance
  (PrettyUsing rest (WHNFValue builtin)) =>
  PrettyUsing ('DiscardConstraintCtx rest) (Contextualised (UnificationConstraint builtin) (ConstraintContext builtin))
  where
  prettyUsing (WithContext (Unify _ e1 e2) ctx) = do
    let e1' = prettyUsing @rest (e1 :: WHNFValue builtin)
    let e2' = prettyUsing @rest (e2 :: WHNFValue builtin)
    prettyConstraintContext (prettyUnify e1' e2') ctx

instance
  (PrettyUsing rest (WHNFValue builtin)) =>
  PrettyUsing ('DiscardConstraintCtx rest) (Contextualised (InstanceConstraint builtin) (ConstraintContext builtin))
  where
  prettyUsing (WithContext (Resolve _ m _ expr) ctx) = do
    let expr' = prettyUsing @rest (expr :: WHNFValue builtin)
    prettyConstraintContext (prettyTypeClass m expr') ctx

instance
  (PrettyUsing rest (Contextualised (WHNFValue builtin) (BoundCtx builtin))) =>
  PrettyUsing ('KeepConstraintCtx rest) (Contextualised (UnificationConstraint builtin) (ConstraintContext builtin))
  where
  prettyUsing (WithContext (Unify _ e1 e2) ctx) = do
    let e1' = prettyUsing @rest (WithContext e1 (boundContextOf ctx))
    let e2' = prettyUsing @rest (WithContext e2 (boundContextOf ctx))
    prettyConstraintContext (prettyUnify e1' e2') ctx

instance
  (PrettyUsing rest (Contextualised (WHNFValue builtin) (BoundCtx builtin))) =>
  PrettyUsing ('KeepConstraintCtx rest) (Contextualised (InstanceConstraint builtin) (ConstraintContext builtin))
  where
  prettyUsing (WithContext (Resolve _ m _ expr) ctx) = do
    let expr' = prettyUsing @rest (WithContext expr (boundContextOf ctx))
    prettyConstraintContext (prettyTypeClass m expr') ctx

instance
  ( PrettyUsing rest (Contextualised (UnificationConstraint builtin) (ConstraintContext builtin)),
    PrettyUsing rest (Contextualised (InstanceConstraint builtin) (ConstraintContext builtin))
  ) =>
  PrettyUsing rest (Contextualised (Constraint builtin) (ConstraintContext builtin))
  where
  prettyUsing (WithContext c ctx) = case c of
    UnificationConstraint uc -> prettyUsing @rest (WithContext uc ctx)
    InstanceConstraint tc -> prettyUsing @rest (WithContext tc ctx)

--------------------------------------------------------------------------------
-- Instances for opaque types

instance (PrettyUsing rest a) => PrettyUsing ('Opaque rest) (MetaMap a) where
  prettyUsing (MetaMap m) = prettyMapEntries entries
    where
      entries = fmap (bimap MetaID (prettyUsing @rest)) (IntMap.assocs m)

instance (PrettyUsing rest a) => PrettyUsing ('Opaque rest) (ConjunctAll a) where
  prettyUsing (ConjunctAll cs) = concatWith (\x y -> x <> line <> y) docs
    where
      docs = NonEmpty.toList (fmap (prettyUsing @rest) cs)

instance (PrettyUsing rest a) => PrettyUsing ('Opaque rest) (DisjunctAll a) where
  prettyUsing (DisjunctAll cs) = concatWith (\x y -> x <> line <> "or" <> y) docs
    where
      docs = NonEmpty.toList (fmap (prettyUsing @rest) cs)

instance (PrettyUsing rest a) => PrettyUsing ('Opaque rest) (MaybeTrivial a) where
  prettyUsing = \case
    Trivial True -> "True"
    Trivial False -> "False"
    NonTrivial x -> prettyUsing @rest x

instance (PrettyUsing rest a) => PrettyUsing ('Opaque rest) (BooleanExpr a) where
  prettyUsing = \case
    Query x -> prettyUsing @rest x
    Disjunct x y -> prettyUsing @('Opaque rest) x <+> "or" <+> prettyUsing @('Opaque rest) y
    Conjunct x y -> prettyUsing @('Opaque rest) x <+> "and" <+> prettyUsing @('Opaque rest) y

instance
  (Functor f, PrettyUsing ('Opaque rest) (f (Contextualised a ctx))) =>
  PrettyUsing ('Opaque rest) (Contextualised (f a) ctx)
  where
  prettyUsing (WithContext cs ctx) = prettyUsing @('Opaque rest) $ fmap (`WithContext` ctx) cs
