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
import Vehicle.Compile.Prelude hiding (MapList)
import Vehicle.Compile.Simplify
import Vehicle.Compile.Type.Constraint
import Vehicle.Compile.Type.Meta.Map (MetaMap (..))
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

type PrettyVerbose a = PrettyWith ('As 'Internal) a

type PrettyExternal a = PrettyWith ('Named ('As 'External)) a

type PrettyFriendly a = PrettyWith ('Named ('Simple ('As 'External))) a

-- | Prints to the internal language in all it's gory detail. Does not convert
--  (Co)DeBruijn indices back to names. Useful for debugging.
prettyVerbose :: PrettyVerbose a => a -> Doc b
prettyVerbose = prettyWith @('As 'Internal)

-- | Prints to the internal language in all it's gory detail. Does not convert
--  (Co)DeBruijn indices back to names. Useful for debugging.
prettyExternal :: PrettyExternal a => a -> Doc b
prettyExternal = prettyWith @('Named ('As 'External))

-- | Prints to the external language for things that need to be displayed to
--  the user. Must provide the context of the thing being printed.
prettyFriendly :: PrettyFriendly a => a -> Doc b
prettyFriendly = prettyWith @('Named ('Simple ('As 'External)))

--------------------------------------------------------------------------------
-- Printing strategies
--------------------------------------------------------------------------------

-- Tags denote at a high-level how you want the term to be printed.
data Tags
  = -- | The final tag denotes which output grammar should be used
    As VehicleLang
  | -- | The named tag ensures that the term is converted back to using named binders
    Named Tags
  | -- | The simple tag ensures that superfluous information is erased
    Simple Tags

-- | A strategy is an abstract representation of the sequence of operations that
-- are needed in order to convert something into a printable form. It should not
-- be confused with the actual operations needed to do so.
data Strategy
  = PrintAs VehicleLang
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
  -- To convert a Checked representation with the names supplied directly to the target language
  -- (instead of first converting the Checked indices to names) then convert the Checked indices naively to a
  -- string representing them, e.g. the index `0` gets converted to `i0`
  StrategyFor ('As lang) CheckedProg = 'DescopeNaively (StrategyFor ('As lang) InputProg)
  StrategyFor ('As lang) CheckedDecl = 'DescopeNaively (StrategyFor ('As lang) InputDecl)
  StrategyFor ('As lang) CheckedExpr = 'DescopeNaively (StrategyFor ('As lang) InputExpr)
  StrategyFor ('As lang) CheckedArg = 'DescopeNaively (StrategyFor ('As lang) InputArg)
  StrategyFor ('As lang) CheckedBinder = 'DescopeNaively (StrategyFor ('As lang) InputBinder)
  -- To convert a BasicNormExpr convert the `DBLevel`s naively.
  StrategyFor ('As lang) BasicNormExpr = 'DescopeNaively (StrategyFor ('As lang) InputExpr)
  StrategyFor ('As lang) BasicNormArg = 'DescopeNaively (StrategyFor ('As lang) InputArg)
  StrategyFor ('As lang) BasicNormBinder = 'DescopeNaively (StrategyFor ('As lang) InputBinder)
  -- To convert an expression using a named representation to a named representation is a no-op.
  StrategyFor ('Named tags) InputProg = StrategyFor tags InputProg
  StrategyFor ('Named tags) InputDecl = StrategyFor tags InputDecl
  StrategyFor ('Named tags) InputExpr = StrategyFor tags InputExpr
  StrategyFor ('Named tags) InputArg = StrategyFor tags InputArg
  StrategyFor ('Named tags) InputBinder = StrategyFor tags InputBinder
  -- To convert a closed expression using a Checked representation but whose missing names have been supplied
  -- to a named representation, perform the Checked to named conversion. For expressions, args, binders
  -- we need to have the context in scope.
  StrategyFor ('Named tags) CheckedProg = 'DescopeWithNames (StrategyFor tags InputProg)
  StrategyFor ('Named tags) CheckedDecl = 'DescopeWithNames (StrategyFor tags InputDecl)
  StrategyFor ('Named tags) (Contextualised CheckedExpr BoundDBCtx) = 'DescopeWithNames (StrategyFor tags InputExpr)
  StrategyFor ('Named tags) (Contextualised CheckedArg BoundDBCtx) = 'DescopeWithNames (StrategyFor tags InputArg)
  StrategyFor ('Named tags) (Contextualised CheckedBinder BoundDBCtx) = 'DescopeWithNames (StrategyFor tags InputBinder)
  -- To convert a named Basicnormalised expr, first denormalise to a checked expr.
  StrategyFor ('Named tags) (Contextualised BasicNormExpr BoundDBCtx) = 'Denormalise (StrategyFor ('Named tags) (Contextualised CheckedExpr BoundDBCtx))
  -- To convert an open expression using a Checked representation but whose missing names have been supplied
  -- to a named representation, perform the Checked to named conversion.
  StrategyFor tags CoDBExpr = 'CoDBToDB (StrategyFor tags CheckedExpr)
  StrategyFor tags CoDBArg = 'CoDBToDB (StrategyFor tags CheckedArg)
  StrategyFor tags CoDBBinder = 'CoDBToDB (StrategyFor tags CheckedBinder)
  StrategyFor ('Named tags) (Contextualised CoDBExpr BoundDBCtx) = 'CoDBToDB (StrategyFor ('Named tags) (Contextualised CheckedExpr BoundDBCtx))
  StrategyFor ('Named tags) (Contextualised CoDBArg BoundDBCtx) = 'CoDBToDB (StrategyFor ('Named tags) (Contextualised CheckedArg BoundDBCtx))
  StrategyFor ('Named tags) (Contextualised CoDBBinder BoundDBCtx) = 'CoDBToDB (StrategyFor ('Named tags) (Contextualised CheckedBinder BoundDBCtx))
  -- Things that we just pretty print.
  StrategyFor tags PositionTree = 'Pretty
  StrategyFor tags Int = 'Pretty
  StrategyFor tags Text = 'Pretty
  -- Objects for which we want to block the strategy computation on.
  StrategyFor ('Named tags) (Contextualised Constraint ConstraintContext) = 'KeepConstraintCtx (StrategyFor ('Named tags) (Contextualised BasicNormExpr BoundDBCtx))
  StrategyFor ('Named tags) (Contextualised TypeClassConstraint ConstraintContext) = 'KeepConstraintCtx (StrategyFor ('Named tags) (Contextualised BasicNormExpr BoundDBCtx))
  StrategyFor ('Named tags) (Contextualised UnificationConstraint ConstraintContext) = 'KeepConstraintCtx (StrategyFor ('Named tags) (Contextualised BasicNormExpr BoundDBCtx))
  StrategyFor tags (Contextualised Constraint ConstraintContext) = 'DiscardConstraintCtx (StrategyFor tags BasicNormExpr)
  StrategyFor tags (Contextualised TypeClassConstraint ConstraintContext) = 'DiscardConstraintCtx (StrategyFor tags BasicNormExpr)
  StrategyFor tags (Contextualised UnificationConstraint ConstraintContext) = 'DiscardConstraintCtx (StrategyFor tags BasicNormExpr)
  StrategyFor tags (MetaMap a) = 'Opaque (StrategyFor tags a)
  StrategyFor tags PositionsInExpr = 'Opaque (StrategyFor tags CheckedExpr)
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

{-
  -- To convert a CoDBExpr with the names supplied directly to the target language
  -- (instead of first converting to CoDB indices to names) then convert the CoDB indices naively to
  -- a string representing them.
  StrategyFor ('As lang) (t (NamedBinding, Maybe PositionTree) CoDBVar) =
    'CoDBToNamedNaive ('PrintAs lang)
-}
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
-- Convert closed terms from DeBruijn representation to named representation naively

instance PrettyUsing rest InputProg => PrettyUsing ('DescopeNaively rest) CheckedProg where
  prettyUsing = prettyUsing @rest . descopeNaive

instance PrettyUsing rest InputDecl => PrettyUsing ('DescopeNaively rest) CheckedDecl where
  prettyUsing = prettyUsing @rest . descopeNaive

instance PrettyUsing rest InputExpr => PrettyUsing ('DescopeNaively rest) CheckedExpr where
  prettyUsing = prettyUsing @rest . descopeNaive

instance PrettyUsing rest InputArg => PrettyUsing ('DescopeNaively rest) CheckedArg where
  prettyUsing = prettyUsing @rest . descopeNaive

instance PrettyUsing rest InputBinder => PrettyUsing ('DescopeNaively rest) CheckedBinder where
  prettyUsing = prettyUsing @rest . descopeNaive

instance PrettyUsing rest InputExpr => PrettyUsing ('DescopeNaively rest) BasicNormExpr where
  prettyUsing = prettyUsing @rest . descopeNaive

instance PrettyUsing rest InputArg => PrettyUsing ('DescopeNaively rest) BasicNormArg where
  prettyUsing = prettyUsing @rest . descopeNaive

instance PrettyUsing rest InputBinder => PrettyUsing ('DescopeNaively rest) BasicNormBinder where
  prettyUsing = prettyUsing @rest . descopeNaive

--------------------------------------------------------------------------------
-- Convert open terms from DeBruijn representation to named representation

instance PrettyUsing rest InputProg => PrettyUsing ('DescopeWithNames rest) CheckedProg where
  prettyUsing = prettyUsing @rest . descopeNamed

instance PrettyUsing rest InputDecl => PrettyUsing ('DescopeWithNames rest) CheckedDecl where
  prettyUsing = prettyUsing @rest . descopeNamed

instance
  PrettyUsing rest InputExpr =>
  PrettyUsing ('DescopeWithNames rest) (Contextualised CheckedExpr BoundDBCtx)
  where
  prettyUsing = prettyUsing @rest . descopeNamed

instance
  PrettyUsing rest InputArg =>
  PrettyUsing ('DescopeWithNames rest) (Contextualised CheckedArg BoundDBCtx)
  where
  prettyUsing = prettyUsing @rest . descopeNamed

instance
  PrettyUsing rest InputBinder =>
  PrettyUsing ('DescopeWithNames rest) (Contextualised CheckedBinder BoundDBCtx)
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
  PrettyUsing rest (Contextualised CheckedExpr BoundDBCtx) =>
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
  PrettyUsing rest CheckedExpr =>
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
-- Instances for Basicnormalised types

instance
  PrettyUsing rest (Contextualised CheckedExpr BoundDBCtx) =>
  PrettyUsing ('Denormalise rest) (Contextualised BasicNormExpr BoundDBCtx)
  where
  prettyUsing (WithContext e ctx) = do
    let e' = unnormalise @BasicNormExpr @CheckedExpr (DBLevel $ length ctx) e
    prettyUsing @rest (WithContext e' ctx)

instance PrettyUsing rest CheckedExpr => PrettyUsing ('Denormalise rest) BasicNormExpr where
  prettyUsing e = prettyUsing @rest (unnormalise @BasicNormExpr @CheckedExpr 0 e)

instance PrettyUsing rest CheckedArg => PrettyUsing ('Denormalise rest) BasicNormArg where
  prettyUsing e = prettyUsing @rest (unnormalise @BasicNormArg @CheckedArg 0 e)

--------------------------------------------------------------------------------
-- Instances for constraints

prettyUnify :: Doc a -> Doc a -> Doc a
prettyUnify e1 e2 = e1 <+> "~" <+> e2

prettyTypeClass :: MetaID -> Doc a -> Doc a
prettyTypeClass m expr = pretty m <+> "<=" <+> expr

instance
  PrettyUsing rest BasicNormExpr =>
  PrettyUsing ('DiscardConstraintCtx rest) (Contextualised UnificationConstraint ConstraintContext)
  where
  prettyUsing (WithContext (Unify e1 e2) _) = do
    let e1' = prettyUsing @rest (e1 :: BasicNormExpr)
    let e2' = prettyUsing @rest (e2 :: BasicNormExpr)
    prettyUnify e1' e2'

instance
  PrettyUsing rest BasicNormExpr =>
  PrettyUsing ('DiscardConstraintCtx rest) (Contextualised TypeClassConstraint ConstraintContext)
  where
  prettyUsing (WithContext (Has m tc args) _) = do
    let expr = VBuiltin (Constructor $ TypeClass tc) args
    let expr' = prettyUsing @rest (expr :: BasicNormExpr)
    prettyTypeClass m expr'

instance
  PrettyUsing rest (Contextualised BasicNormExpr BoundDBCtx) =>
  PrettyUsing ('KeepConstraintCtx rest) (Contextualised UnificationConstraint ConstraintContext)
  where
  prettyUsing (WithContext (Unify e1 e2) ctx) = do
    let e1' = prettyUsing @rest (WithContext e1 (boundContextOf ctx))
    let e2' = prettyUsing @rest (WithContext e2 (boundContextOf ctx))
    prettyUnify e1' e2'

instance
  PrettyUsing rest (Contextualised BasicNormExpr BoundDBCtx) =>
  PrettyUsing ('KeepConstraintCtx rest) (Contextualised TypeClassConstraint ConstraintContext)
  where
  prettyUsing (WithContext (Has m tc args) ctx) = do
    let expr = VBuiltin (Constructor $ TypeClass tc) args
    let expr' = prettyUsing @rest (WithContext expr (boundContextOf ctx))
    prettyTypeClass m expr' <+> pretty (originalProvenance ctx)

instance
  ( PrettyUsing rest (Contextualised UnificationConstraint ConstraintContext),
    PrettyUsing rest (Contextualised TypeClassConstraint ConstraintContext)
  ) =>
  PrettyUsing rest (Contextualised Constraint ConstraintContext)
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

instance (PrettyUsing rest CheckedExpr) => PrettyUsing ('Opaque rest) PositionsInExpr where
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
