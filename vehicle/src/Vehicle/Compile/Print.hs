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

import Data.Bifunctor (bimap)
import Data.Foldable qualified as NonEmpty
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap (assocs)
import Data.Text (Text)
import GHC.TypeLits (ErrorMessage (..), TypeError)
import Prettyprinter (list)
import Vehicle.Compile.Descope
import Vehicle.Compile.Normalise.Quote (QuoteClosure, unnormalise)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Simplify
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Meta.Map (MetaMap (..))
import Vehicle.Data.Builtin.Standard.Core
import Vehicle.Data.Code.BooleanExpr
import Vehicle.Data.Code.Value
import Vehicle.Syntax.AST.Expr qualified as S
import Vehicle.Syntax.Print

--------------------------------------------------------------------------------
-- Public methods
--------------------------------------------------------------------------------

type VerboseTags = 'Unnamed ('ShortVectors ('As 'Internal))

type ExternalTags = 'Named ('ShortVectors ('As 'External))

type FriendlyTags = 'Named ('Uninserted ('As 'External))

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
  (PrettyFriendly (Contextualised (f builtin) NamedBoundCtx)) =>
  f builtin ->
  Doc b
prettyFriendlyEmptyCtx x = prettyFriendly (WithContext x emptyNamedCtx)

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
  = PrintAs VehicleLang
  | QuoteValue Strategy
  | DescopeNaively Strategy
  | DescopeWithNames Strategy
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
type family StrategyFor (tags :: Tags) a :: Strategy where --------------------
-- Generic syntax --
--------------------
-- The strategy for printing `Arg`, `Binder` is just the same strategy as to print the expressions they contain.
  StrategyFor tags (GenericArg expr) = StrategyFor tags expr
  StrategyFor tags (GenericBinder expr) = StrategyFor tags expr
  StrategyFor tags (Contextualised (GenericArg expr) NamedBoundCtx) = StrategyFor tags (Contextualised expr NamedBoundCtx)
  StrategyFor tags (Contextualised (GenericBinder expr) NamedBoundCtx) = StrategyFor tags (Contextualised expr NamedBoundCtx)
  ----------
  -- Expr --
  ----------
  -- To convert any named representation to the target language, simply convert it.
  StrategyFor ('As lang) S.Prog = 'PrintAs lang
  StrategyFor ('As lang) S.Decl = 'PrintAs lang
  StrategyFor ('As lang) S.Expr = 'PrintAs lang
  -- To convert an expression from a `Name` representation to a `Name` representation is a no-op.
  StrategyFor ('Named tags) S.Prog = StrategyFor tags S.Prog
  StrategyFor ('Named tags) S.Decl = StrategyFor tags S.Decl
  StrategyFor ('Named tags) S.Expr = StrategyFor tags S.Expr
  -- To convert an expression using a Ix representation but whose missing names have been supplied
  -- to a named representation.
  StrategyFor ('Named tags) (Prog builtin) = StrategyFor ('Named tags) (Contextualised (Expr builtin) NamedBoundCtx)
  StrategyFor ('Named tags) (Decl builtin) = StrategyFor ('Named tags) (Contextualised (Expr builtin) NamedBoundCtx)
  StrategyFor ('Named tags) (Contextualised (Expr builtin) NamedBoundCtx) = 'DescopeWithNames (StrategyFor tags S.Expr)
  -- To print a DB expr in an unnamed representation, simply naively descope.
  StrategyFor ('Unnamed tags) (Prog builtin) = 'DescopeNaively (StrategyFor tags S.Expr)
  StrategyFor ('Unnamed tags) (Decl builtin) = 'DescopeNaively (StrategyFor tags S.Expr)
  StrategyFor ('Unnamed tags) (Expr builtin) = 'DescopeNaively (StrategyFor tags S.Expr)
  -----------
  -- Value --
  -----------
  -- To print a `Value` we need to quote it first. Note that we convert it to a `Builtin` representation immediately
  StrategyFor ('Named tags) (VDecl closure builtin) = StrategyFor ('Named tags) (Contextualised (Value closure builtin) NamedBoundCtx)
  StrategyFor ('Named tags) (Contextualised (Value closure builtin) NamedBoundCtx) = 'QuoteValue (StrategyFor ('Named tags) (Contextualised (Expr Builtin) NamedBoundCtx))
  StrategyFor ('Unnamed tags) (Value closure builtin) = 'DescopeNaively (StrategyFor tags S.Expr)
  -----------
  -- State --
  -----------
  -- Objects for which we want to block the strategy computation on.
  StrategyFor ('Named tags) (Contextualised (Constraint builtin) (ConstraintContext builtin)) = 'KeepConstraintCtx (StrategyFor ('Named tags) (Contextualised (WHNFValue builtin) NamedBoundCtx))
  StrategyFor ('Named tags) (Contextualised (InstanceConstraint builtin) (ConstraintContext builtin)) = 'KeepConstraintCtx (StrategyFor ('Named tags) (Contextualised (WHNFValue builtin) NamedBoundCtx))
  StrategyFor ('Named tags) (Contextualised (UnificationConstraint builtin) (ConstraintContext builtin)) = 'KeepConstraintCtx (StrategyFor ('Named tags) (Contextualised (WHNFValue builtin) NamedBoundCtx))
  StrategyFor ('Unnamed tags) (Contextualised (Constraint builtin) (ConstraintContext builtin)) = 'DiscardConstraintCtx (StrategyFor ('Unnamed tags) (WHNFValue builtin))
  StrategyFor ('Unnamed tags) (Contextualised (InstanceConstraint builtin) (ConstraintContext builtin)) = 'DiscardConstraintCtx (StrategyFor ('Unnamed tags) (WHNFValue builtin))
  StrategyFor ('Unnamed tags) (Contextualised (UnificationConstraint builtin) (ConstraintContext builtin)) = 'DiscardConstraintCtx (StrategyFor ('Unnamed tags) (WHNFValue builtin))
  --------------------------------
  -- Distributing over functors --
  --------------------------------
  -- Contextualised functors
  StrategyFor tags (Contextualised (a, b) ctx) = 'MapTuple2 (StrategyFor tags (Contextualised a ctx)) (StrategyFor tags (Contextualised b ctx))
  StrategyFor tags (Contextualised [a] ctx) = StrategyFor tags [Contextualised a ctx]
  StrategyFor tags (Contextualised (Maybe a) ctx) = StrategyFor tags (Maybe (Contextualised a ctx))
  StrategyFor tags (Contextualised (ConjunctAll a) ctx) = StrategyFor tags (ConjunctAll (Contextualised a ctx))
  -- StrategyFor tags (Contextualised Text ctx) = StrategyFor tags Text
  -- Plain functors
  StrategyFor tags (a, b) = 'MapTuple2 (StrategyFor tags a) (StrategyFor tags b)
  StrategyFor tags (a, b, c) = 'MapTuple3 (StrategyFor tags a) (StrategyFor tags b) (StrategyFor tags c)
  StrategyFor tags [a] = 'Opaque (StrategyFor tags a)
  StrategyFor tags (Maybe a) = 'Opaque (StrategyFor tags a)
  StrategyFor tags (ConjunctAll a) = 'Opaque (StrategyFor tags a)
  StrategyFor tags (DisjunctAll a) = 'Opaque (StrategyFor tags a)
  StrategyFor tags (MaybeTrivial a) = 'Opaque (StrategyFor tags a)
  StrategyFor tags (IntMap a) = 'Opaque (StrategyFor tags a)
  StrategyFor tags (MetaMap a) = 'Opaque (StrategyFor tags a)
  --------------------
  -- Simplification --
  --------------------
  StrategyFor ('Uninserted tags) a = 'UninsertArgsAndBinders (StrategyFor tags a)
  StrategyFor ('ShortVectors tags) a = 'ShortenVectors (StrategyFor tags a)
  ------------
  -- Pretty --
  ------------
  -- Things that we just pretty print.
  StrategyFor tags () = 'Pretty
  StrategyFor tags Int = 'Pretty
  StrategyFor tags Text = 'Pretty
  -- StrategyFor tags (GenericBinder ()) = 'Pretty
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
-- Printing to internal language

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

--------------------------------------------------------------------------------
-- Printing to external language

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
-- Convert closed terms from DeBruijn representation to named representation naively

instance (PrettyUsing rest S.Prog, PrintableBuiltin builtin) => PrettyUsing ('DescopeNaively rest) (Prog builtin) where
  prettyUsing prog = prettyUsing @rest $ fmap descopeExprNaively prog

instance (PrettyUsing rest S.Decl, PrintableBuiltin builtin) => PrettyUsing ('DescopeNaively rest) (Decl builtin) where
  prettyUsing decl = prettyUsing @rest $ fmap descopeExprNaively decl

instance (PrettyUsing rest S.Expr, PrintableBuiltin builtin) => PrettyUsing ('DescopeNaively rest) (Expr builtin) where
  prettyUsing = prettyUsing @rest . descopeExprNaively

instance (PrettyUsing rest S.Arg, PrintableBuiltin builtin) => PrettyUsing ('DescopeNaively rest) (Arg builtin) where
  prettyUsing arg = prettyUsing @rest $ fmap descopeExprNaively arg

instance (PrettyUsing rest S.Binder, PrintableBuiltin builtin) => PrettyUsing ('DescopeNaively rest) (Binder builtin) where
  prettyUsing binder = prettyUsing @rest $ fmap descopeExprNaively binder

instance (PrettyUsing rest S.Expr, PrintableBuiltin builtin, DescopableClosure closure) => PrettyUsing ('DescopeNaively rest) (Value closure builtin) where
  prettyUsing = prettyUsing @rest . descopeValueNaively @builtin @closure

instance (PrettyUsing rest S.Arg, PrintableBuiltin builtin, DescopableClosure closure) => PrettyUsing ('DescopeNaively rest) (VArg closure builtin) where
  prettyUsing arg = prettyUsing @rest $ fmap (descopeValueNaively @builtin @closure) arg

instance (PrettyUsing rest S.Binder, PrintableBuiltin builtin, DescopableClosure closure) => PrettyUsing ('DescopeNaively rest) (VBinder closure builtin) where
  prettyUsing binder = prettyUsing @rest $ fmap (descopeValueNaively @builtin @closure) binder

--------------------------------------------------------------------------------
-- Convert open terms from DeBruijn representation to named representation

instance (PrettyUsing rest S.Prog, PrintableBuiltin builtin) => PrettyUsing ('DescopeWithNames rest) (Prog builtin) where
  prettyUsing prog = prettyUsing @rest $ fmap descopeExprInEmptyCtx prog

instance (PrettyUsing rest S.Decl, PrintableBuiltin builtin) => PrettyUsing ('DescopeWithNames rest) (Decl builtin) where
  prettyUsing decl = prettyUsing @rest $ fmap descopeExprInEmptyCtx decl

instance
  (PrettyUsing rest S.Expr, PrintableBuiltin builtin) =>
  PrettyUsing ('DescopeWithNames rest) (Contextualised (Expr builtin) NamedBoundCtx)
  where
  prettyUsing e = prettyUsing @rest $ descopeExpr e

instance
  (PrettyUsing rest S.Arg, PrintableBuiltin builtin) =>
  PrettyUsing ('DescopeWithNames rest) (Contextualised (Arg builtin) NamedBoundCtx)
  where
  prettyUsing (WithContext e ctx) = prettyUsing @rest $ fmap (\a -> descopeExpr (WithContext a ctx)) e

instance
  (PrettyUsing rest S.Binder, PrintableBuiltin builtin) =>
  PrettyUsing ('DescopeWithNames rest) (Contextualised (Binder builtin) NamedBoundCtx)
  where
  prettyUsing (WithContext e ctx) = prettyUsing @rest $ fmap (\a -> descopeExpr (WithContext a ctx)) e

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
  (PrettyUsing rest (Expr Builtin), ConvertableBuiltin builtin Builtin, QuoteClosure closure Builtin) =>
  PrettyUsing ('QuoteValue rest) (Value closure builtin)
  where
  prettyUsing e = prettyUsing @rest $ unnormalise @(Value closure builtin) @(Expr Builtin) 0 e

instance
  (PrettyUsing rest (Arg Builtin), ConvertableBuiltin builtin Builtin, QuoteClosure closure Builtin) =>
  PrettyUsing ('QuoteValue rest) (VArg closure builtin)
  where
  prettyUsing e = prettyUsing @rest $ fmap (unnormalise @(Value closure builtin) @(Expr Builtin) 0) e

instance
  (PrettyUsing rest (Binder Builtin), ConvertableBuiltin builtin Builtin, QuoteClosure closure Builtin) =>
  PrettyUsing ('QuoteValue rest) (VBinder closure builtin)
  where
  prettyUsing e = prettyUsing @rest $ fmap (unnormalise @(Value closure builtin) @(Expr Builtin) 0) e

instance
  (PrettyUsing rest (Decl Builtin), ConvertableBuiltin builtin Builtin, QuoteClosure closure Builtin) =>
  PrettyUsing ('QuoteValue rest) (VDecl closure builtin)
  where
  prettyUsing e = prettyUsing @rest $ fmap (unnormalise @(Value closure builtin) @(Expr Builtin) 0) e

instance
  (PrettyUsing rest (Contextualised (Expr Builtin) NamedBoundCtx), ConvertableBuiltin builtin Builtin, QuoteClosure closure Builtin) =>
  PrettyUsing ('QuoteValue rest) (Contextualised (Value closure builtin) NamedBoundCtx)
  where
  prettyUsing (WithContext e ctx) = do
    let e' = unnormalise @(Value closure builtin) @(Expr Builtin) (Lv $ length ctx) e
    prettyUsing @rest (WithContext e' ctx)

instance (ConvertableBuiltin builtin Builtin, PrettyUsing rest (Expr Builtin)) => PrettyUsing ('DescopeWithNames rest) (NFValue builtin) where
  prettyUsing e = prettyUsing @rest (unnormalise @(NFValue builtin) @(Expr Builtin) 0 e)

instance (ConvertableBuiltin builtin Builtin, PrettyUsing rest (Arg Builtin)) => PrettyUsing ('DescopeWithNames rest) (NFArg builtin) where
  prettyUsing e = prettyUsing @rest (unnormalise @(NFArg builtin) @(Arg Builtin) 0 e)

instance PrettyUsing rest (GenericBinder ()) where
  prettyUsing b = maybe "_" pretty (nameOf b)

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
  (PrettyUsing rest (Contextualised (WHNFValue builtin) NamedBoundCtx)) =>
  PrettyUsing ('KeepConstraintCtx rest) (Contextualised (UnificationConstraint builtin) (ConstraintContext builtin))
  where
  prettyUsing (WithContext (Unify _ e1 e2) ctx) = do
    let e1' = prettyUsing @rest (WithContext e1 (namedBoundCtxOf ctx))
    let e2' = prettyUsing @rest (WithContext e2 (namedBoundCtxOf ctx))
    prettyConstraintContext (prettyUnify e1' e2') ctx

instance
  (PrettyUsing rest (Contextualised (WHNFValue builtin) NamedBoundCtx)) =>
  PrettyUsing ('KeepConstraintCtx rest) (Contextualised (InstanceConstraint builtin) (ConstraintContext builtin))
  where
  prettyUsing (WithContext (Resolve _ m _ expr) ctx) = do
    let expr' = prettyUsing @rest (WithContext expr (namedBoundCtxOf ctx))
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

instance
  (Functor f, PrettyUsing ('Opaque rest) (f (Contextualised a ctx))) =>
  PrettyUsing ('Opaque rest) (Contextualised (f a) ctx)
  where
  prettyUsing (WithContext cs ctx) = prettyUsing @('Opaque rest) $ fmap (`WithContext` ctx) cs
