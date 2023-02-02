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
  )
where

import Control.Exception (assert)
import Data.Bifunctor (bimap)
import Data.Foldable qualified as NonEmpty
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap (assocs)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.TypeLits (ErrorMessage (..), TypeError)
import Prettyprinter (list)
import Vehicle.Compile.Descope
import Vehicle.Compile.Normalise.Quote (unnormalise)
import Vehicle.Compile.Prelude hiding (MapList)
import Vehicle.Compile.Simplify
import Vehicle.Compile.Type.Constraint
import Vehicle.Compile.Type.Meta.Map (MetaMap (..))
import Vehicle.Compile.Type.Subsystem.Standard.Core
import Vehicle.Expr.Boolean
import Vehicle.Expr.CoDeBruijn
import Vehicle.Expr.CoDeBruijn.Conversion
import Vehicle.Expr.CoDeBruijn.PositionTree
import Vehicle.Expr.DeBruijn
import Vehicle.Expr.Normalised
import Vehicle.Syntax.Print

--------------------------------------------------------------------------------
-- Public methods
--------------------------------------------------------------------------------

type VerboseTags = 'Unnamed ('StandardiseBuiltin ('As 'Internal))

type ExternalTags = 'Named ('StandardiseBuiltin ('As 'External))

type FriendlyTags = 'Named ('StandardiseBuiltin ('Simple ('As 'External)))

type PrettyVerbose a = PrettyWith VerboseTags a

type PrettyExternal a = PrettyWith ExternalTags a

type PrettyFriendly a = PrettyWith FriendlyTags a

-- | Prints to the internal language in all it's gory detail. Does not convert
--  (Co)DeBruijn indices back to names. Useful for debugging.
prettyVerbose :: PrettyVerbose a => a -> Doc b
prettyVerbose = prettyWith @VerboseTags

-- | Prints to the internal language in all it's gory detail. Does not convert
--  (Co)DeBruijn indices back to names. Useful for debugging.
prettyExternal :: PrettyExternal a => a -> Doc b
prettyExternal = prettyWith @ExternalTags

-- | Prints to the external language for things that need to be displayed to
--  the user. Must provide the context of the thing being printed.
prettyFriendly :: PrettyFriendly a => a -> Doc b
prettyFriendly = prettyWith @FriendlyTags

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
  | -- | The `Simple` tag ensures that superfluous information is erased
    Simple Tags
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
  | CoDBToDB Strategy
  | Denormalise Strategy
  | DiscardConstraintCtx Strategy
  | KeepConstraintCtx Strategy
  | Simplify Strategy
  | MapList Strategy
  | MapMaybe Strategy
  | MapIntMap Strategy
  | MapTuple2 Strategy Strategy
  | MapTuple3 Strategy Strategy Strategy
  | Opaque Strategy
  | Pretty

-- | This type family computes the correct printing strategy given the tags
-- and the type of the expression.
type family StrategyFor (tags :: Tags) a :: Strategy where
  -- To convert any named representation to the target language, simply convert it.
  StrategyFor ('As lang) InputProg = 'PrintAs lang
  StrategyFor ('As lang) InputDecl = 'PrintAs lang
  StrategyFor ('As lang) InputExpr = 'PrintAs lang
  StrategyFor ('As lang) InputArg = 'PrintAs lang
  StrategyFor ('As lang) InputBinder = 'PrintAs lang
  -- To print a DB expr in an unnamed representation, simply naively descope.
  StrategyFor ('Unnamed tags) (DBProg builtin) = 'DescopeNaively (StrategyFor tags (NamedProg builtin))
  StrategyFor ('Unnamed tags) (DBDecl builtin) = 'DescopeNaively (StrategyFor tags (NamedDecl builtin))
  StrategyFor ('Unnamed tags) (DBExpr builtin) = 'DescopeNaively (StrategyFor tags (NamedExpr builtin))
  StrategyFor ('Unnamed tags) (DBArg builtin) = 'DescopeNaively (StrategyFor tags (NamedArg builtin))
  StrategyFor ('Unnamed tags) (DBBinder builtin) = 'DescopeNaively (StrategyFor tags (NamedBinder builtin))
  --  To print a normalised expr in an unnamed representation, simply naively descope.
  StrategyFor ('Unnamed tags) (NormExpr builtin) = 'DescopeNaively (StrategyFor tags (NamedExpr builtin))
  StrategyFor ('Unnamed tags) (NormArg builtin) = 'DescopeNaively (StrategyFor tags (NamedArg builtin))
  StrategyFor ('Unnamed tags) (NormBinder builtin) = 'DescopeNaively (StrategyFor tags (NamedBinder builtin))
  -- To convert builtins
  StrategyFor ('StandardiseBuiltin tags) (NamedProg builtin) = 'ConvertBuiltins (StrategyFor tags InputProg)
  StrategyFor ('StandardiseBuiltin tags) (NamedDecl builtin) = 'ConvertBuiltins (StrategyFor tags InputDecl)
  StrategyFor ('StandardiseBuiltin tags) (NamedExpr builtin) = 'ConvertBuiltins (StrategyFor tags InputExpr)
  StrategyFor ('StandardiseBuiltin tags) (NamedArg builtin) = 'ConvertBuiltins (StrategyFor tags InputArg)
  StrategyFor ('StandardiseBuiltin tags) (NamedBinder builtin) = 'ConvertBuiltins (StrategyFor tags InputBinder)
  -- To convert an expression using a named representation to a named representation is a no-op.
  StrategyFor ('Named tags) InputProg = StrategyFor tags InputProg
  StrategyFor ('Named tags) InputDecl = StrategyFor tags InputDecl
  StrategyFor ('Named tags) InputExpr = StrategyFor tags InputExpr
  StrategyFor ('Named tags) InputArg = StrategyFor tags InputArg
  StrategyFor ('Named tags) InputBinder = StrategyFor tags InputBinder
  -- To convert a closed expression using a DB representation but whose missing names have been supplied
  -- to a named representation, perform the Checked to named conversion. For expressions, args, binders
  -- we need to have the context in scope.
  StrategyFor ('Named tags) (DBProg builtin) = 'DescopeWithNames (StrategyFor tags InputProg)
  StrategyFor ('Named tags) (DBDecl builtin) = 'DescopeWithNames (StrategyFor tags InputDecl)
  StrategyFor ('Named tags) (Contextualised (DBExpr builtin) BoundDBCtx) = 'DescopeWithNames (StrategyFor tags InputExpr)
  StrategyFor ('Named tags) (Contextualised (DBArg builtin) BoundDBCtx) = 'DescopeWithNames (StrategyFor tags InputArg)
  StrategyFor ('Named tags) (Contextualised (DBBinder builtin) BoundDBCtx) = 'DescopeWithNames (StrategyFor tags InputBinder)
  -- To convert a named normalised expr, first denormalise to a checked expr.
  StrategyFor ('Named tags) (Contextualised (NormExpr builtin) BoundDBCtx) = 'Denormalise (StrategyFor ('Named tags) (Contextualised (DBExpr builtin) BoundDBCtx))
  -- To convert an open expression using a Checked representation but whose missing names have been supplied
  -- to a named representation, perform the Checked to named conversion.
  StrategyFor tags CoDBExpr = 'CoDBToDB (StrategyFor tags TypeCheckedExpr)
  StrategyFor tags CoDBArg = 'CoDBToDB (StrategyFor tags TypeCheckedArg)
  StrategyFor tags CoDBBinder = 'CoDBToDB (StrategyFor tags TypeCheckedBinder)
  StrategyFor ('Named tags) (Contextualised CoDBExpr BoundDBCtx) = 'CoDBToDB (StrategyFor ('Named tags) (Contextualised TypeCheckedExpr BoundDBCtx))
  StrategyFor ('Named tags) (Contextualised CoDBArg BoundDBCtx) = 'CoDBToDB (StrategyFor ('Named tags) (Contextualised TypeCheckedArg BoundDBCtx))
  StrategyFor ('Named tags) (Contextualised CoDBBinder BoundDBCtx) = 'CoDBToDB (StrategyFor ('Named tags) (Contextualised TypeCheckedBinder BoundDBCtx))
  -- Things that we just pretty print.
  StrategyFor tags PositionTree = 'Pretty
  StrategyFor tags Int = 'Pretty
  StrategyFor tags Text = 'Pretty
  -- Objects for which we want to block the strategy computation on.
  StrategyFor ('Named tags) (Contextualised (Constraint builtin) (ConstraintContext builtin)) = 'KeepConstraintCtx (StrategyFor ('Named tags) (Contextualised StandardNormExpr BoundDBCtx))
  StrategyFor ('Named tags) (Contextualised (TypeClassConstraint builtin) (ConstraintContext builtin)) = 'KeepConstraintCtx (StrategyFor ('Named tags) (Contextualised StandardNormExpr BoundDBCtx))
  StrategyFor ('Named tags) (Contextualised (UnificationConstraint builtin) (ConstraintContext builtin)) = 'KeepConstraintCtx (StrategyFor ('Named tags) (Contextualised StandardNormExpr BoundDBCtx))
  StrategyFor tags (Contextualised (Constraint builtin) (ConstraintContext builtin)) = 'DiscardConstraintCtx (StrategyFor tags StandardNormExpr)
  StrategyFor tags (Contextualised (TypeClassConstraint builtin) (ConstraintContext builtin)) = 'DiscardConstraintCtx (StrategyFor tags StandardNormExpr)
  StrategyFor tags (Contextualised (UnificationConstraint builtin) (ConstraintContext builtin)) = 'DiscardConstraintCtx (StrategyFor tags StandardNormExpr)
  StrategyFor tags (MetaMap a) = 'Opaque (StrategyFor tags a)
  StrategyFor tags PositionsInExpr = 'Opaque (StrategyFor tags TypeCheckedExpr)
  -- Simplification
  StrategyFor ('Simple tags) a = 'Simplify (StrategyFor tags a)
  -- Things were we just print the structure and recursively print through.
  StrategyFor tags (Maybe a) = 'MapMaybe (StrategyFor tags a)
  StrategyFor tags (IntMap a) = 'MapIntMap (StrategyFor tags a)
  StrategyFor tags [a] = 'MapList (StrategyFor tags a)
  StrategyFor tags (a, b) = 'MapTuple2 (StrategyFor tags a) (StrategyFor tags b)
  StrategyFor tags (a, b, c) = 'MapTuple3 (StrategyFor tags a) (StrategyFor tags b) (StrategyFor tags c)
  StrategyFor tags (BooleanExpr a) = 'Opaque (StrategyFor tags a)
  StrategyFor tags (ConjunctAll a) = 'Opaque (StrategyFor tags a)
  StrategyFor tags (DisjunctAll a) = 'Opaque (StrategyFor tags a)
  StrategyFor tags (MaybeTrivial a) = 'Opaque (StrategyFor tags a)
  -- StrategyFor tags (Contextualised (ConjunctAll a) BoundDBCtx) = StrategyFor tags (ConjunctAll (Contextualised a BoundDBCtx))
  -- StrategyFor tags (Contextualised (DisjunctAll a) BoundDBCtx) = StrategyFor tags (DisjunctAll (Contextualised a BoundDBCtx))
  -- StrategyFor tags (Contextualised (MaybeTrivial a) BoundDBCtx) = StrategyFor tags (MaybeTrivial (Contextualised a BoundDBCtx))
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
-- Class for printable builtins

class PrintableBuiltin builtin where
  -- | Convert expressions with the builtin back to expressions with the standard
  -- builtin type. Used for printing.
  convertBuiltin ::
    Expr binder var builtin -> Expr binder var Builtin

instance PrintableBuiltin Builtin where
  convertBuiltin = id

--------------------------------------------------------------------------------
-- Executing printing strategies
--------------------------------------------------------------------------------

-- | A type synonym that takes the tags and the type and computes the strategy
-- for the combination to guide type-class resolution.
type PrettyWith tags a = PrettyUsing (StrategyFor tags a) a

class PrettyUsing (strategy :: Strategy) a where
  prettyUsing :: a -> Doc b

prettyWith :: forall tags a b. PrettyWith tags a => a -> Doc b
prettyWith = prettyUsing @(StrategyFor tags a) @a @b

--------------------------------------------------------------------------------
-- Printing to internal language

instance PrettyUsing ('PrintAs 'Internal) InputProg where
  prettyUsing = printInternal

instance PrettyUsing ('PrintAs 'Internal) InputDecl where
  prettyUsing = printInternal

instance PrettyUsing ('PrintAs 'Internal) InputExpr where
  prettyUsing = printInternal

instance PrettyUsing ('PrintAs 'Internal) InputArg where
  prettyUsing = printInternal

instance PrettyUsing ('PrintAs 'Internal) InputBinder where
  prettyUsing = printInternal

--------------------------------------------------------------------------------
-- Printing to external language

instance PrettyUsing ('PrintAs 'External) InputProg where
  prettyUsing = printExternal

instance PrettyUsing ('PrintAs 'External) InputDecl where
  prettyUsing = printExternal

instance PrettyUsing ('PrintAs 'External) InputExpr where
  prettyUsing = printExternal

instance PrettyUsing ('PrintAs 'External) InputArg where
  prettyUsing = printExternal

instance PrettyUsing ('PrintAs 'External) InputBinder where
  prettyUsing = printExternal

--------------------------------------------------------------------------------
-- Converting builtins

instance
  (PrintableBuiltin builtin, PrettyUsing rest InputProg) =>
  PrettyUsing ('ConvertBuiltins rest) (NamedProg builtin)
  where
  prettyUsing = prettyUsing @rest . fmap convertBuiltin

instance
  (PrintableBuiltin builtin, PrettyUsing rest InputDecl) =>
  PrettyUsing ('ConvertBuiltins rest) (NamedDecl builtin)
  where
  prettyUsing = prettyUsing @rest . fmap convertBuiltin

instance
  (PrintableBuiltin builtin, PrettyUsing rest InputExpr) =>
  PrettyUsing ('ConvertBuiltins rest) (NamedExpr builtin)
  where
  prettyUsing = prettyUsing @rest . convertBuiltin

instance
  (PrintableBuiltin builtin, PrettyUsing rest InputArg) =>
  PrettyUsing ('ConvertBuiltins rest) (NamedArg builtin)
  where
  prettyUsing = prettyUsing @rest . fmap convertBuiltin

instance
  (PrintableBuiltin builtin, PrettyUsing rest InputBinder) =>
  PrettyUsing ('ConvertBuiltins rest) (NamedBinder builtin)
  where
  prettyUsing = prettyUsing @rest . fmap convertBuiltin

--------------------------------------------------------------------------------
-- Convert closed terms from DeBruijn representation to named representation naively

instance PrettyUsing rest (NamedProg builtin) => PrettyUsing ('DescopeNaively rest) (DBProg builtin) where
  prettyUsing = prettyUsing @rest . descopeNaive

instance PrettyUsing rest (NamedDecl builtin) => PrettyUsing ('DescopeNaively rest) (DBDecl builtin) where
  prettyUsing = prettyUsing @rest . descopeNaive

instance PrettyUsing rest (NamedExpr builtin) => PrettyUsing ('DescopeNaively rest) (DBExpr builtin) where
  prettyUsing = prettyUsing @rest . descopeNaive

instance PrettyUsing rest (NamedArg builtin) => PrettyUsing ('DescopeNaively rest) (DBArg builtin) where
  prettyUsing = prettyUsing @rest . descopeNaive

instance PrettyUsing rest (NamedBinder builtin) => PrettyUsing ('DescopeNaively rest) (DBBinder builtin) where
  prettyUsing = prettyUsing @rest . descopeNaive

instance PrettyUsing rest (NamedExpr builtin) => PrettyUsing ('DescopeNaively rest) (NormExpr builtin) where
  prettyUsing = prettyUsing @rest . descopeNaive

instance PrettyUsing rest (NamedArg builtin) => PrettyUsing ('DescopeNaively rest) (NormArg builtin) where
  prettyUsing = prettyUsing @rest . descopeNaive

instance PrettyUsing rest (NamedBinder builtin) => PrettyUsing ('DescopeNaively rest) (NormBinder builtin) where
  prettyUsing = prettyUsing @rest . descopeNaive

--------------------------------------------------------------------------------
-- Convert open terms from DeBruijn representation to named representation

instance PrettyUsing rest (NamedProg builtin) => PrettyUsing ('DescopeWithNames rest) (DBProg builtin) where
  prettyUsing = prettyUsing @rest . descopeNamed

instance PrettyUsing rest (NamedDecl builtin) => PrettyUsing ('DescopeWithNames rest) (DBDecl builtin) where
  prettyUsing = prettyUsing @rest . descopeNamed

instance
  PrettyUsing rest (NamedExpr builtin) =>
  PrettyUsing ('DescopeWithNames rest) (Contextualised (DBExpr builtin) BoundDBCtx)
  where
  prettyUsing = prettyUsing @rest . descopeNamed

instance
  PrettyUsing rest (NamedArg builtin) =>
  PrettyUsing ('DescopeWithNames rest) (Contextualised (DBArg builtin) BoundDBCtx)
  where
  prettyUsing = prettyUsing @rest . descopeNamed

instance
  PrettyUsing rest (NamedBinder builtin) =>
  PrettyUsing ('DescopeWithNames rest) (Contextualised (DBBinder builtin) BoundDBCtx)
  where
  prettyUsing = prettyUsing @rest . descopeNamed

--------------------------------------------------------------------------------
-- Convert from CoDeBruijn representation to named representation naively
{-
instance
  PrettyUsing rest NamedExpr =>
  PrettyUsing ('CoDBToNamedNaive rest) (Expr (CoDBBinding Name) CoDBVar)
  where
  prettyUsing e = _

    let (e', pts) = runNaiveCoDBDescope e
     in prettyUsing @rest e' <+> prettyMap pts
  -}
--------------------------------------------------------------------------------
-- Convert open term from CoDeBruijn representation to DeBruijn representation

instance
  PrettyUsing rest (Contextualised TypeCheckedExpr BoundDBCtx) =>
  PrettyUsing rest (Contextualised CoDBExpr BoundDBCtx)
  where
  prettyUsing (WithContext e ctx) =
    prettyUsing @rest (WithContext (fromCoDB e) ctx)

{-
instance (PrettyUsing rest (BoundDBCtx, CheckedArg))
      => PrettyUsing ('CoDBToDBOpen rest) (BoundDBCtx, CoDBArg) where
  prettyUsing (ctx, (e, bvm)) = prettyUsing @rest (ctx , fromCoDBArg (e, bvm))

instance (PrettyUsing rest (BoundDBCtx, CheckedBinder))
      => PrettyUsing ('CoDBToDBOpen rest) (BoundDBCtx, CoDBBinder) where
  prettyUsing (ctx, (e, bvm)) = prettyUsing @rest (ctx , fromCoDBBinder (e, bvm))
-}

--------------------------------------------------------------------------------
-- Convert closed term from CoDeBruijn representation to DeBruijn representation

instance
  PrettyUsing rest TypeCheckedExpr =>
  PrettyUsing ('CoDBToDB rest) CoDBExpr
  where
  prettyUsing (e, bvm) = assert (null bvm) $ prettyUsing @rest (fromCoDB (e, bvm))

{-
instance PrettyUsing rest CheckedArg
      => PrettyUsing ('CoDBToDBClosed rest) CoDBArg where
  prettyUsing (e, bvm) = assert (null bvm) $ prettyUsing @rest (fromCoDB (e, bvm))

instance PrettyUsing rest CheckedBinder
      => PrettyUsing ('CoDBToDBClosed rest) CoDBBinder where
  prettyUsing (e, bvm) = assert (null bvm) $ prettyUsing @rest (fromCoDB (e, bvm))
-}

--------------------------------------------------------------------------------
-- Simplification

instance
  (Simplify a, PrettyUsing rest a) =>
  PrettyUsing ('Simplify rest) a
  where
  prettyUsing e = prettyUsing @rest (simplify e)

instance
  PrettyUsing rest a =>
  PrettyUsing ('MapList rest) [a]
  where
  prettyUsing es = list (prettyUsing @rest <$> es)

instance
  PrettyUsing rest a =>
  PrettyUsing ('MapMaybe rest) (Maybe a)
  where
  prettyUsing = \case
    Nothing -> ""
    Just x -> prettyUsing @rest x

instance PrettyUsing rest a => PrettyUsing ('MapIntMap rest) (IntMap a) where
  prettyUsing es = prettyIntMap (prettyUsing @rest <$> es)

instance (PrettyUsing resta a, PrettyUsing restb b) => PrettyUsing ('MapTuple2 resta restb) (a, b) where
  prettyUsing (e1, e2) = "(" <> prettyUsing @resta e1 <> "," <+> prettyUsing @restb e2 <> ")"

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

instance Pretty a => PrettyUsing 'Pretty a where
  prettyUsing = pretty

--------------------------------------------------------------------------------
-- Instances for normalised types

instance
  PrettyUsing rest (Contextualised (DBExpr builtin) BoundDBCtx) =>
  PrettyUsing ('Denormalise rest) (Contextualised (NormExpr builtin) BoundDBCtx)
  where
  prettyUsing (WithContext e ctx) = do
    let e' = unnormalise @(NormExpr builtin) @(DBExpr builtin) (DBLevel $ length ctx) e
    prettyUsing @rest (WithContext e' ctx)

instance PrettyUsing rest (DBExpr builtin) => PrettyUsing ('Denormalise rest) (NormExpr builtin) where
  prettyUsing e = prettyUsing @rest (unnormalise @(NormExpr builtin) @(DBExpr builtin) 0 e)

instance PrettyUsing rest (DBArg builtin) => PrettyUsing ('Denormalise rest) (NormArg builtin) where
  prettyUsing e = prettyUsing @rest (unnormalise @(NormArg builtin) @(DBArg builtin) 0 e)

--------------------------------------------------------------------------------
-- Instances for constraints

prettyUnify :: Doc a -> Doc a -> Doc a
prettyUnify e1 e2 = e1 <+> "~" <+> e2

prettyTypeClass :: MetaID -> Doc a -> Doc a
prettyTypeClass m expr = pretty m <+> "<=" <+> expr

instance
  PrettyUsing rest (NormExpr builtin) =>
  PrettyUsing ('DiscardConstraintCtx rest) (Contextualised (UnificationConstraint builtin) (ConstraintContext builtin))
  where
  prettyUsing (WithContext (Unify e1 e2) _) = do
    let e1' = prettyUsing @rest (e1 :: NormExpr builtin)
    let e2' = prettyUsing @rest (e2 :: NormExpr builtin)
    prettyUnify e1' e2'

instance
  PrettyUsing rest (NormExpr builtin) =>
  PrettyUsing ('DiscardConstraintCtx rest) (Contextualised (TypeClassConstraint builtin) (ConstraintContext builtin))
  where
  prettyUsing (WithContext (Has m tc args) _) = do
    let expr = VBuiltin tc args
    let expr' = prettyUsing @rest (expr :: NormExpr builtin)
    prettyTypeClass m expr'

instance
  PrettyUsing rest (Contextualised (NormExpr builtin) BoundDBCtx) =>
  PrettyUsing ('KeepConstraintCtx rest) (Contextualised (UnificationConstraint builtin) (ConstraintContext builtin))
  where
  prettyUsing (WithContext (Unify e1 e2) ctx) = do
    let e1' = prettyUsing @rest (WithContext e1 (boundContextOf ctx))
    let e2' = prettyUsing @rest (WithContext e2 (boundContextOf ctx))
    prettyUnify e1' e2'

instance
  PrettyUsing rest (Contextualised (NormExpr builtin) BoundDBCtx) =>
  PrettyUsing ('KeepConstraintCtx rest) (Contextualised (TypeClassConstraint builtin) (ConstraintContext builtin))
  where
  prettyUsing (WithContext (Has m tc args) ctx) = do
    let expr = VBuiltin tc args
    let expr' = prettyUsing @rest (WithContext expr (boundContextOf ctx))
    prettyTypeClass m expr' <+> pretty (originalProvenance ctx)

instance
  ( PrettyUsing rest (Contextualised (UnificationConstraint builtin) (ConstraintContext builtin)),
    PrettyUsing rest (Contextualised (TypeClassConstraint builtin) (ConstraintContext builtin))
  ) =>
  PrettyUsing rest (Contextualised (Constraint builtin) (ConstraintContext builtin))
  where
  prettyUsing (WithContext c ctx) = case c of
    UnificationConstraint uc -> prettyUsing @rest (WithContext uc ctx)
    TypeClassConstraint tc -> prettyUsing @rest (WithContext tc ctx)

--------------------------------------------------------------------------------
-- Instances for opaque types

instance PrettyUsing rest a => PrettyUsing ('Opaque rest) (MetaMap a) where
  prettyUsing (MetaMap m) = prettyMapEntries entries
    where
      entries = fmap (bimap MetaID (prettyUsing @rest)) (IntMap.assocs m)

instance (PrettyUsing rest TypeCheckedExpr) => PrettyUsing ('Opaque rest) PositionsInExpr where
  prettyUsing (PositionsInExpr e p) = prettyUsing @rest (fromCoDB (substPos hole (Just p) e))
    where
      hole = (Hole mempty $ Text.pack "@", mempty)

instance PrettyUsing rest a => PrettyUsing ('Opaque rest) (ConjunctAll a) where
  prettyUsing (ConjunctAll cs) = concatWith (\x y -> x <> line <> "and" <> y) docs
    where
      docs = NonEmpty.toList (fmap (prettyUsing @rest) cs)

instance PrettyUsing rest a => PrettyUsing ('Opaque rest) (DisjunctAll a) where
  prettyUsing (DisjunctAll cs) = concatWith (\x y -> x <> line <> "or" <> y) docs
    where
      docs = NonEmpty.toList (fmap (prettyUsing @rest) cs)

instance PrettyUsing rest a => PrettyUsing ('Opaque rest) (MaybeTrivial a) where
  prettyUsing = \case
    Trivial True -> "True"
    Trivial False -> "False"
    NonTrivial x -> prettyUsing @rest x

instance PrettyUsing rest a => PrettyUsing ('Opaque rest) (BooleanExpr a) where
  prettyUsing = \case
    Query x -> prettyUsing @rest x
    Disjunct x y -> prettyUsing @('Opaque rest) x <+> "or" <+> prettyUsing @('Opaque rest) y
    Conjunct x y -> prettyUsing @('Opaque rest) x <+> "and" <+> prettyUsing @('Opaque rest) y
