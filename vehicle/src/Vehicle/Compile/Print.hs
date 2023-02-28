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
import Vehicle.Compile.Prelude
import Vehicle.Compile.Simplify
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Meta.Map (MetaMap (..))
import Vehicle.Compile.Type.Subsystem.Standard.Core
import Vehicle.Expr.Boolean
import Vehicle.Expr.CoDeBruijn
import Vehicle.Expr.CoDeBruijn.Conversion
import Vehicle.Expr.CoDeBruijn.PositionTree
import Vehicle.Expr.DeBruijn
import Vehicle.Expr.Normalisable
import Vehicle.Expr.Normalised
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
  | CoDBToDB Strategy
  | Denormalise Strategy
  | DiscardConstraintCtx Strategy
  | KeepConstraintCtx Strategy
  | UninsertArgsAndBinders Strategy
  | ShortenVectors Strategy
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
  -- To print a normalised expr in an unnamed representation, simply naively descope.
  StrategyFor ('Unnamed tags) (NormExpr types) = 'DescopeNaively (StrategyFor tags (NamedExpr (NormalisableBuiltin types)))
  StrategyFor ('Unnamed tags) (NormArg types) = 'DescopeNaively (StrategyFor tags (NamedArg (NormalisableBuiltin types)))
  StrategyFor ('Unnamed tags) (NormBinder types) = 'DescopeNaively (StrategyFor tags (NamedBinder (NormalisableBuiltin types)))
  -- To standardise builtins
  StrategyFor ('StandardiseBuiltin tags) (NamedProg builtin) = 'ConvertBuiltins (StrategyFor tags InputProg)
  StrategyFor ('StandardiseBuiltin tags) (NamedDecl builtin) = 'ConvertBuiltins (StrategyFor tags InputDecl)
  StrategyFor ('StandardiseBuiltin tags) (NamedExpr builtin) = 'ConvertBuiltins (StrategyFor tags InputExpr)
  StrategyFor ('StandardiseBuiltin tags) (NamedArg builtin) = 'ConvertBuiltins (StrategyFor tags InputArg)
  StrategyFor ('StandardiseBuiltin tags) (NamedBinder builtin) = 'ConvertBuiltins (StrategyFor tags InputBinder)
  -- To convert an expression using a named representation to a named representation is a no-op.
  StrategyFor ('Named tags) (NamedProg builtin) = StrategyFor tags (NamedProg builtin)
  StrategyFor ('Named tags) (NamedDecl builtin) = StrategyFor tags (NamedDecl builtin)
  StrategyFor ('Named tags) (NamedExpr builtin) = StrategyFor tags (NamedExpr builtin)
  StrategyFor ('Named tags) (NamedArg builtin) = StrategyFor tags (NamedArg builtin)
  StrategyFor ('Named tags) (NamedBinder builtin) = StrategyFor tags (NamedBinder builtin)
  -- To convert a closed expression using a DB representation but whose missing names have been supplied
  -- to a named representation, perform the Checked to named conversion. For expressions, args, binders
  -- we need to have the context in scope.
  StrategyFor ('Named tags) (DBProg builtin) = 'DescopeWithNames (StrategyFor tags InputProg)
  StrategyFor ('Named tags) (DBDecl builtin) = 'DescopeWithNames (StrategyFor tags InputDecl)
  StrategyFor ('Named tags) (Contextualised (DBExpr builtin) BoundDBCtx) = 'DescopeWithNames (StrategyFor tags InputExpr)
  StrategyFor ('Named tags) (Contextualised (DBArg builtin) BoundDBCtx) = 'DescopeWithNames (StrategyFor tags InputArg)
  StrategyFor ('Named tags) (Contextualised (DBBinder builtin) BoundDBCtx) = 'DescopeWithNames (StrategyFor tags InputBinder)
  -- To convert a named normalised expr, first denormalise to a checked expr.
  StrategyFor ('Named tags) (Contextualised (NormExpr types) BoundDBCtx) = 'Denormalise (StrategyFor ('Named tags) (Contextualised (DBExpr (NormalisableBuiltin types)) BoundDBCtx))
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
  StrategyFor ('Named tags) (Contextualised (Constraint types) (ConstraintContext types)) = 'KeepConstraintCtx (StrategyFor ('Named tags) (Contextualised StandardNormExpr BoundDBCtx))
  StrategyFor ('Named tags) (Contextualised (TypeClassConstraint types) (ConstraintContext types)) = 'KeepConstraintCtx (StrategyFor ('Named tags) (Contextualised StandardNormExpr BoundDBCtx))
  StrategyFor ('Named tags) (Contextualised (UnificationConstraint types) (ConstraintContext types)) = 'KeepConstraintCtx (StrategyFor ('Named tags) (Contextualised StandardNormExpr BoundDBCtx))
  StrategyFor tags (Contextualised (Constraint types) (ConstraintContext types)) = 'DiscardConstraintCtx (StrategyFor tags StandardNormExpr)
  StrategyFor tags (Contextualised (TypeClassConstraint types) (ConstraintContext types)) = 'DiscardConstraintCtx (StrategyFor tags StandardNormExpr)
  StrategyFor tags (Contextualised (UnificationConstraint types) (ConstraintContext types)) = 'DiscardConstraintCtx (StrategyFor tags StandardNormExpr)
  StrategyFor tags (MetaMap a) = 'Opaque (StrategyFor tags a)
  StrategyFor tags PositionsInExpr = 'Opaque (StrategyFor tags TypeCheckedExpr)
  -- Simplification
  StrategyFor ('Uninserted tags) a = 'UninsertArgsAndBinders (StrategyFor tags a)
  StrategyFor ('ShortVectors tags) a = 'ShortenVectors (StrategyFor tags a)
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
-- Converting the basic builtins

instance PrettyUsing rest InputProg => PrettyUsing ('ConvertBuiltins rest) InputProg where
  prettyUsing = prettyUsing @rest

instance PrettyUsing rest InputDecl => PrettyUsing ('ConvertBuiltins rest) InputDecl where
  prettyUsing = prettyUsing @rest

instance PrettyUsing rest InputExpr => PrettyUsing ('ConvertBuiltins rest) InputExpr where
  prettyUsing = prettyUsing @rest

instance PrettyUsing rest InputArg => PrettyUsing ('ConvertBuiltins rest) InputArg where
  prettyUsing = prettyUsing @rest

instance PrettyUsing rest InputBinder => PrettyUsing ('ConvertBuiltins rest) InputBinder where
  prettyUsing = prettyUsing @rest

--------------------------------------------------------------------------------
-- Converting builtins

instance
  (PrintableBuiltin types, PrettyUsing rest InputProg) =>
  PrettyUsing ('ConvertBuiltins rest) (NamedProg (NormalisableBuiltin types))
  where
  prettyUsing = prettyUsing @rest . fmap convertExprBuiltins

instance
  (PrintableBuiltin types, PrettyUsing rest InputDecl) =>
  PrettyUsing ('ConvertBuiltins rest) (NamedDecl (NormalisableBuiltin types))
  where
  prettyUsing = prettyUsing @rest . fmap convertExprBuiltins

instance
  (PrintableBuiltin types, PrettyUsing rest InputExpr) =>
  PrettyUsing ('ConvertBuiltins rest) (NamedExpr (NormalisableBuiltin types))
  where
  prettyUsing = prettyUsing @rest . convertExprBuiltins

instance
  (PrintableBuiltin types, PrettyUsing rest InputArg) =>
  PrettyUsing ('ConvertBuiltins rest) (NamedArg (NormalisableBuiltin types))
  where
  prettyUsing = prettyUsing @rest . fmap convertExprBuiltins

instance
  (PrintableBuiltin types, PrettyUsing rest InputBinder) =>
  PrettyUsing ('ConvertBuiltins rest) (NamedBinder (NormalisableBuiltin types))
  where
  prettyUsing = prettyUsing @rest . fmap convertExprBuiltins

convertExprBuiltins ::
  forall types binder var.
  PrintableBuiltin types =>
  Expr binder var (NormalisableBuiltin types) ->
  Expr binder var Builtin
convertExprBuiltins = traverseBuiltins $ \p1 p2 b args -> do
  let fn = case b of
        CConstructor c -> Builtin p2 $ Constructor c
        CFunction f -> Builtin p2 $ BuiltinFunction f
        CType t -> convertBuiltin p2 t

  normAppList p1 fn args

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

instance PrettyUsing rest (NamedExpr (NormalisableBuiltin types)) => PrettyUsing ('DescopeNaively rest) (NormExpr types) where
  prettyUsing = prettyUsing @rest . descopeNaive

instance PrettyUsing rest (NamedArg (NormalisableBuiltin types)) => PrettyUsing ('DescopeNaively rest) (NormArg types) where
  prettyUsing = prettyUsing @rest . descopeNaive

instance PrettyUsing rest (NamedBinder (NormalisableBuiltin types)) => PrettyUsing ('DescopeNaively rest) (NormBinder types) where
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
  PrettyUsing rest (Contextualised (DBExpr (NormalisableBuiltin types)) BoundDBCtx) =>
  PrettyUsing ('Denormalise rest) (Contextualised (NormExpr types) BoundDBCtx)
  where
  prettyUsing (WithContext e ctx) = do
    let e' = unnormalise @(NormExpr types) @(DBExpr (NormalisableBuiltin types)) (DBLevel $ length ctx) e
    prettyUsing @rest (WithContext e' ctx)

instance PrettyUsing rest (DBExpr (NormalisableBuiltin types)) => PrettyUsing ('Denormalise rest) (NormExpr types) where
  prettyUsing e = prettyUsing @rest (unnormalise @(NormExpr types) @(DBExpr (NormalisableBuiltin types)) 0 e)

instance PrettyUsing rest (DBArg (NormalisableBuiltin types)) => PrettyUsing ('Denormalise rest) (NormArg types) where
  prettyUsing e = prettyUsing @rest (unnormalise @(NormArg types) @(DBArg (NormalisableBuiltin types)) 0 e)

--------------------------------------------------------------------------------
-- Instances for constraints

prettyUnify :: Doc a -> Doc a -> Doc a
prettyUnify e1 e2 = e1 <+> "~" <+> e2

prettyTypeClass :: MetaID -> Doc a -> Doc a
prettyTypeClass m expr = pretty m <+> "<=" <+> expr

prettyConstraintContext :: Doc a -> ConstraintContext types -> Doc a
prettyConstraintContext constraint ctx =
  "#" <> pretty (constraintID ctx) <> ". " <+> constraint -- <+> pretty (originalProvenance ctx)

instance
  PrettyUsing rest (NormExpr types) =>
  PrettyUsing ('DiscardConstraintCtx rest) (Contextualised (UnificationConstraint types) (ConstraintContext types))
  where
  prettyUsing (WithContext (Unify e1 e2) ctx) = do
    let e1' = prettyUsing @rest (e1 :: NormExpr types)
    let e2' = prettyUsing @rest (e2 :: NormExpr types)
    prettyConstraintContext (prettyUnify e1' e2') ctx

instance
  PrettyUsing rest (NormExpr types) =>
  PrettyUsing ('DiscardConstraintCtx rest) (Contextualised (TypeClassConstraint types) (ConstraintContext types))
  where
  prettyUsing (WithContext (Has m tc args) ctx) = do
    let expr = VBuiltin (CType tc) args
    let expr' = prettyUsing @rest (expr :: NormExpr types)
    prettyConstraintContext (prettyTypeClass m expr') ctx

instance
  PrettyUsing rest (Contextualised (NormExpr types) BoundDBCtx) =>
  PrettyUsing ('KeepConstraintCtx rest) (Contextualised (UnificationConstraint types) (ConstraintContext types))
  where
  prettyUsing (WithContext (Unify e1 e2) ctx) = do
    let e1' = prettyUsing @rest (WithContext e1 (boundContextOf ctx))
    let e2' = prettyUsing @rest (WithContext e2 (boundContextOf ctx))
    prettyConstraintContext (prettyUnify e1' e2') ctx

instance
  PrettyUsing rest (Contextualised (NormExpr types) BoundDBCtx) =>
  PrettyUsing ('KeepConstraintCtx rest) (Contextualised (TypeClassConstraint types) (ConstraintContext types))
  where
  prettyUsing (WithContext (Has m tc args) ctx) = do
    let expr = VBuiltin (CType tc) args
    let expr' = prettyUsing @rest (WithContext expr (boundContextOf ctx))
    prettyConstraintContext (prettyTypeClass m expr') ctx

instance
  ( PrettyUsing rest (Contextualised (UnificationConstraint types) (ConstraintContext types)),
    PrettyUsing rest (Contextualised (TypeClassConstraint types) (ConstraintContext types))
  ) =>
  PrettyUsing rest (Contextualised (Constraint types) (ConstraintContext types))
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
