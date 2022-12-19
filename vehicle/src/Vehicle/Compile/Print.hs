{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Compile.Print
  ( PrettyUsing (..),
    PrettyWith,
    PrettyFriendly,
    PrettyVerbose,
    PrettySimple,
    Tags (..),
    prettySimple,
    prettyVerbose,
    prettyFriendly,
  )
where

import Control.Exception (assert)
import Data.Bifunctor (bimap)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap (assocs)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.TypeLits (ErrorMessage (..), TypeError)
import Prettyprinter (list)
import Vehicle.Compile.Descope
import Vehicle.Compile.Normalise.Quote (unnormalise)
import Vehicle.Compile.Prelude hiding (MapList)
import Vehicle.Compile.Simplify
import Vehicle.Compile.SupplyNames
import Vehicle.Compile.Type.Constraint
import Vehicle.Compile.Type.Meta.Map (MetaMap (..))
import Vehicle.Expr.CoDeBruijn
import Vehicle.Expr.CoDeBruijn.Conversion
import Vehicle.Expr.CoDeBruijn.PositionTree
import Vehicle.Expr.DeBruijn
import Vehicle.Expr.Normalised
import Vehicle.Syntax.Print

--------------------------------------------------------------------------------
-- Public methods
--------------------------------------------------------------------------------

type PrettySimple a = PrettyWith ('Simple ('As 'Internal)) a

type PrettyVerbose a = PrettyWith ('As 'Internal) a

type PrettyFriendly a = PrettyWith ('Named ('As 'External)) a

-- | Prints to the internal language removing all implicit/instance arguments and
--  automatically inserted code. Does not convert (Co)DeBruijn indices back to
--  names.
prettySimple :: PrettySimple a => a -> Doc b
prettySimple = prettyWith @('Simple ('As 'Internal))

-- | Prints to the internal language in all it's gory detail. Does not convert
--  (Co)DeBruijn indices back to names. Useful for debugging.
prettyVerbose :: PrettyVerbose a => a -> Doc b
prettyVerbose = prettyWith @('As 'Internal)

-- | Prints to the external language for things that need to be displayed to
--  the user. Must provide the context of the thing being printed.
prettyFriendly :: PrettyFriendly a => a -> Doc b
prettyFriendly = prettyWith @('Named ('As 'External))

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
  | SupplyNames Strategy
  | Denormalise Strategy
  | SimplifyWithOptions Strategy
  | SimplifyDefault Strategy
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
  StrategyFor ('As lang) NamedProg = 'PrintAs lang
  StrategyFor ('As lang) NamedDecl = 'PrintAs lang
  StrategyFor ('As lang) NamedExpr = 'PrintAs lang
  StrategyFor ('As lang) NamedArg = 'PrintAs lang
  StrategyFor ('As lang) NamedBinder = 'PrintAs lang
  -- To convert a DB representation with the names supplied directly to the target language
  -- (instead of first converting the DB indices to names) then convert the DB indices naively to a
  -- string representing them, e.g. the index `0` gets converted to `i0`
  StrategyFor ('As lang) SuppliedDBProg = 'DescopeNaively (StrategyFor ('As lang) NamedProg)
  StrategyFor ('As lang) SuppliedDBDecl = 'DescopeNaively (StrategyFor ('As lang) NamedDecl)
  StrategyFor ('As lang) SuppliedDBExpr = 'DescopeNaively (StrategyFor ('As lang) NamedExpr)
  StrategyFor ('As lang) SuppliedDBArg = 'DescopeNaively (StrategyFor ('As lang) NamedArg)
  StrategyFor ('As lang) SuppliedDBBinder = 'DescopeNaively (StrategyFor ('As lang) NamedBinder)
  {-
    -- To convert a CoDBExpr with the names supplied directly to the target language
    -- (instead of first converting to CoDB indices to names) then convert the CoDB indices naively to
    -- a string representing them.
    StrategyFor ('As lang) (t (NamedBinding, Maybe PositionTree) CoDBVar) =
      'CoDBToNamedNaive ('PrintAs lang)
  -}
  -- To convert an expression using a named representation to a named representation is a no-op.
  StrategyFor ('Named tags) NamedProg = StrategyFor tags NamedProg
  StrategyFor ('Named tags) NamedDecl = StrategyFor tags NamedDecl
  StrategyFor ('Named tags) NamedExpr = StrategyFor tags NamedExpr
  StrategyFor ('Named tags) NamedArg = StrategyFor tags NamedArg
  StrategyFor ('Named tags) NamedBinder = StrategyFor tags NamedBinder
  -- To convert a closed expression using a DB representation but whose missing names have been supplied
  -- to a named representation, perform the DB to named conversion. For expressions, args, binders
  -- we need to have the context in scope.
  StrategyFor ('Named tags) SuppliedDBProg = 'DescopeWithNames (StrategyFor tags NamedProg)
  StrategyFor ('Named tags) SuppliedDBDecl = 'DescopeWithNames (StrategyFor tags NamedDecl)
  StrategyFor ('Named tags) (Contextualised SuppliedDBExpr NamedBoundCtx) = 'DescopeWithNames (StrategyFor tags NamedExpr)
  StrategyFor ('Named tags) (Contextualised SuppliedDBArg NamedBoundCtx) = 'DescopeWithNames (StrategyFor tags NamedArg)
  StrategyFor ('Named tags) (Contextualised SuppliedDBBinder NamedBoundCtx) = 'DescopeWithNames (StrategyFor tags NamedBinder)
  -- To convert an open expression using a DB representation but whose missing names have been supplied
  -- to a named representation, perform the DB to named conversion.
  StrategyFor tags CoDBExpr = 'CoDBToDB (StrategyFor tags DBExpr)
  StrategyFor tags CoDBArg = 'CoDBToDB (StrategyFor tags DBArg)
  StrategyFor tags CoDBBinder = 'CoDBToDB (StrategyFor tags DBBinder)
  StrategyFor ('Named tags) (Contextualised CoDBExpr BoundDBCtx) = 'CoDBToDB (StrategyFor ('Named tags) (Contextualised DBExpr BoundDBCtx))
  StrategyFor ('Named tags) (Contextualised CoDBArg BoundDBCtx) = 'CoDBToDB (StrategyFor ('Named tags) (Contextualised DBArg BoundDBCtx))
  StrategyFor ('Named tags) (Contextualised CoDBBinder BoundDBCtx) = 'CoDBToDB (StrategyFor ('Named tags) (Contextualised DBBinder BoundDBCtx))
  -- If we have a closed term with DB indices, then first supply the missing names.
  StrategyFor tags (Prog DBBinding var) = 'SupplyNames (StrategyFor tags (Prog NamedBinding var))
  StrategyFor tags (Decl DBBinding var) = 'SupplyNames (StrategyFor tags (Decl NamedBinding var))
  StrategyFor tags (Expr DBBinding var) = 'SupplyNames (StrategyFor tags (Expr NamedBinding var))
  StrategyFor tags (Arg DBBinding var) = 'SupplyNames (StrategyFor tags (Arg NamedBinding var))
  StrategyFor tags (Binder DBBinding var) = 'SupplyNames (StrategyFor tags (Binder NamedBinding var))
  -- If we have an open term with DB indices, then first supply the missing names.
  StrategyFor tags (Contextualised (Expr DBBinding var) BoundDBCtx) = 'SupplyNames (StrategyFor tags (Contextualised (Expr NamedBinding var) NamedBoundCtx))
  StrategyFor tags (Contextualised (Arg DBBinding var) BoundDBCtx) = 'SupplyNames (StrategyFor tags (Contextualised (Arg NamedBinding var) NamedBoundCtx))
  StrategyFor tags (Contextualised (Binder DBBinding var) BoundDBCtx) = 'SupplyNames (StrategyFor tags (Contextualised (Binder NamedBinding var) NamedBoundCtx))
  -- Normalised expressions
  StrategyFor tags NormExpr = 'Denormalise (StrategyFor tags CheckedExpr)
  StrategyFor tags NormArg = 'Denormalise (StrategyFor tags CheckedArg)
  StrategyFor tags (Contextualised NormExpr BoundDBCtx) =
    'Denormalise (StrategyFor tags (Contextualised CheckedExpr BoundDBCtx))
  -- Constraints
  StrategyFor tags Constraint = StrategyFor tags NormExpr
  StrategyFor tags TypeClassConstraint = StrategyFor tags NormExpr
  StrategyFor tags UnificationConstraint = StrategyFor tags NormExpr
  StrategyFor ('Named tags) (Contextualised Constraint ConstraintContext) =
    StrategyFor ('Named tags) (Contextualised NormExpr BoundDBCtx)
  StrategyFor tags (Contextualised Constraint ConstraintContext) =
    StrategyFor tags Constraint
  StrategyFor ('Named tags) (Contextualised TypeClassConstraint ConstraintContext) =
    StrategyFor ('Named tags) (Contextualised NormExpr BoundDBCtx)
  StrategyFor tags (Contextualised TypeClassConstraint ConstraintContext) =
    StrategyFor tags TypeClassConstraint
  -- Things that we just pretty print.
  StrategyFor tags DBBinding = 'Pretty
  StrategyFor tags PositionTree = 'Pretty
  StrategyFor tags Int = 'Pretty
  StrategyFor tags Text = 'Pretty
  -- Objects for which we want to block the strategy computation on.
  StrategyFor tags (MetaMap a) = 'Opaque (StrategyFor tags a)
  StrategyFor tags PositionsInExpr = 'Opaque (StrategyFor tags CheckedExpr)
  -- Simplification
  StrategyFor ('Simple tags) (SimplifyOptions, a) = 'SimplifyWithOptions (StrategyFor tags a)
  StrategyFor ('Simple tags) a = 'SimplifyDefault (StrategyFor tags a)
  -- Things were we just print the structure and recursively print through.
  StrategyFor tags (Maybe a) = 'MapMaybe (StrategyFor tags a)
  StrategyFor tags (IntMap a) = 'MapIntMap (StrategyFor tags a)
  StrategyFor tags [a] = 'MapList (StrategyFor tags a)
  StrategyFor tags (a, b) = 'MapTuple2 (StrategyFor tags a) (StrategyFor tags b)
  StrategyFor tags (a, b, c) = 'MapTuple3 (StrategyFor tags a) (StrategyFor tags b) (StrategyFor tags c)
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

instance PrettyUsing ('PrintAs 'Internal) NamedProg where
  prettyUsing = printInternal

instance PrettyUsing ('PrintAs 'Internal) NamedDecl where
  prettyUsing = printInternal

instance PrettyUsing ('PrintAs 'Internal) NamedExpr where
  prettyUsing = printInternal

instance PrettyUsing ('PrintAs 'Internal) NamedArg where
  prettyUsing = printInternal

instance PrettyUsing ('PrintAs 'Internal) NamedBinder where
  prettyUsing = printInternal

--------------------------------------------------------------------------------
-- Printing to external language

instance PrettyUsing ('PrintAs 'External) NamedProg where
  prettyUsing = printExternal

instance PrettyUsing ('PrintAs 'External) NamedDecl where
  prettyUsing = printExternal

instance PrettyUsing ('PrintAs 'External) NamedExpr where
  prettyUsing = printExternal

instance PrettyUsing ('PrintAs 'External) NamedArg where
  prettyUsing = printExternal

instance PrettyUsing ('PrintAs 'External) NamedBinder where
  prettyUsing = printExternal

--------------------------------------------------------------------------------
-- Convert closed terms from DeBruijn representation to named representation naively

instance PrettyUsing rest NamedProg => PrettyUsing ('DescopeNaively rest) SuppliedDBProg where
  prettyUsing = prettyUsing @rest . descopeNaive

instance PrettyUsing rest NamedDecl => PrettyUsing ('DescopeNaively rest) SuppliedDBDecl where
  prettyUsing = prettyUsing @rest . descopeNaive

instance PrettyUsing rest NamedExpr => PrettyUsing ('DescopeNaively rest) SuppliedDBExpr where
  prettyUsing = prettyUsing @rest . descopeNaive

instance PrettyUsing rest NamedArg => PrettyUsing ('DescopeNaively rest) SuppliedDBArg where
  prettyUsing = prettyUsing @rest . descopeNaive

instance PrettyUsing rest NamedBinder => PrettyUsing ('DescopeNaively rest) SuppliedDBBinder where
  prettyUsing = prettyUsing @rest . descopeNaive

--------------------------------------------------------------------------------
-- Convert open terms from DeBruijn representation to named representation

instance
  PrettyUsing rest NamedProg =>
  PrettyUsing ('DescopeWithNames rest) SuppliedDBProg
  where
  prettyUsing = prettyUsing @rest . descopeNamed

instance
  PrettyUsing rest NamedDecl =>
  PrettyUsing ('DescopeWithNames rest) (Decl Name (LocallyNamelessVar DBIndex))
  where
  prettyUsing = prettyUsing @rest . descopeNamed

instance
  PrettyUsing rest NamedExpr =>
  PrettyUsing ('DescopeWithNames rest) (Contextualised SuppliedDBExpr NamedBoundCtx)
  where
  prettyUsing = prettyUsing @rest . descopeNamed

instance
  PrettyUsing rest NamedArg =>
  PrettyUsing ('DescopeWithNames rest) (Contextualised SuppliedDBArg NamedBoundCtx)
  where
  prettyUsing = prettyUsing @rest . descopeNamed

instance
  PrettyUsing rest NamedBinder =>
  PrettyUsing ('DescopeWithNames rest) (Contextualised SuppliedDBBinder NamedBoundCtx)
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
  PrettyUsing rest (Contextualised DBExpr BoundDBCtx) =>
  PrettyUsing rest (Contextualised CoDBExpr BoundDBCtx)
  where
  prettyUsing (WithContext e ctx) =
    prettyUsing @rest (WithContext (fromCoDB e) ctx)

{-
instance (PrettyUsing rest (BoundDBCtx, DBArg))
      => PrettyUsing ('CoDBToDBOpen rest) (BoundDBCtx, CoDBArg) where
  prettyUsing (ctx, (e, bvm)) = prettyUsing @rest (ctx , fromCoDBArg (e, bvm))

instance (PrettyUsing rest (BoundDBCtx, DBBinder))
      => PrettyUsing ('CoDBToDBOpen rest) (BoundDBCtx, CoDBBinder) where
  prettyUsing (ctx, (e, bvm)) = prettyUsing @rest (ctx , fromCoDBBinder (e, bvm))
-}

--------------------------------------------------------------------------------
-- Convert closed term from CoDeBruijn representation to DeBruijn representation

instance
  PrettyUsing rest DBExpr =>
  PrettyUsing ('CoDBToDB rest) CoDBExpr
  where
  prettyUsing (e, bvm) = assert (null bvm) $ prettyUsing @rest (fromCoDB (e, bvm))

{-
instance PrettyUsing rest DBArg
      => PrettyUsing ('CoDBToDBClosed rest) CoDBArg where
  prettyUsing (e, bvm) = assert (null bvm) $ prettyUsing @rest (fromCoDB (e, bvm))

instance PrettyUsing rest DBBinder
      => PrettyUsing ('CoDBToDBClosed rest) CoDBBinder where
  prettyUsing (e, bvm) = assert (null bvm) $ prettyUsing @rest (fromCoDB (e, bvm))
-}

--------------------------------------------------------------------------------
-- Supply names for open DB terms

instance
  PrettyUsing rest (Contextualised (Expr Name var) NamedBoundCtx) =>
  PrettyUsing ('SupplyNames rest) (Contextualised (Expr DBBinding var) BoundDBCtx)
  where
  prettyUsing = prettyUsing @rest . supplyNames

{-
instance
  PrettyUsing rest (Contextualised (Arg Name var) NamedBoundCtx) =>
  PrettyUsing ('SupplyNames rest) (Contextualised (Arg DBBinding var) BoundDBCtx)
  where
  prettyUsing e = prettyUsing @rest (supplyNames e :: Contextualised (Arg Name var) NamedBoundCtx)

instance
  PrettyUsing rest (Contextualised (Binder Name var) NamedBoundCtx) =>
  PrettyUsing ('SupplyNames rest) (Contextualised (Binder DBBinding var) BoundDBCtx)
  where
  prettyUsing e = prettyUsing @rest (supplyNames e :: Contextualised (Binder Name var) NamedBoundCtx)
-}
--------------------------------------------------------------------------------
-- Supply names for closed DB terms

instance
  PrettyUsing rest (Prog Name var) =>
  PrettyUsing ('SupplyNames rest) (Prog DBBinding var)
  where
  prettyUsing = prettyUsing @rest . supplyNames

instance
  PrettyUsing rest (Decl Name var) =>
  PrettyUsing ('SupplyNames rest) (Decl DBBinding var)
  where
  prettyUsing = prettyUsing @rest . supplyNames

instance
  PrettyUsing rest (Expr Name var) =>
  PrettyUsing ('SupplyNames rest) (Expr DBBinding var)
  where
  prettyUsing = prettyUsing @rest . supplyNames

instance
  PrettyUsing rest (Arg Name var) =>
  PrettyUsing ('SupplyNames rest) (Arg DBBinding var)
  where
  prettyUsing = prettyUsing @rest . supplyNames

instance
  PrettyUsing rest (Binder Name var) =>
  PrettyUsing ('SupplyNames rest) (Binder DBBinding var)
  where
  prettyUsing = prettyUsing @rest . supplyNames

{-
instance
  (PrettyUsing rest ([Name], t (CoDBBinding Name) var)) =>
  PrettyUsing ('SupplyNames rest) ([DBBinding], t (CoDBBinding DBBinding) var)
  where
  prettyUsing p = prettyUsing @rest (supplyCoDBNamesWithCtx p)

instance
  (SupplyNames t, PrettyUsing rest (t (CoDBBinding Name) var)) =>
  PrettyUsing ('SupplyNames rest) (t (CoDBBinding DBBinding) var)
  where
  prettyUsing e = prettyUsing @rest (supplyCoDBNames e)
-}

--------------------------------------------------------------------------------
-- Simplification

instance
  (Simplify a, PrettyUsing rest a) =>
  PrettyUsing ('SimplifyWithOptions rest) (SimplifyOptions, a)
  where
  prettyUsing (options, e) = prettyUsing @rest (simplifyWith options e)

instance
  (Simplify a, PrettyUsing rest a) =>
  PrettyUsing ('SimplifyDefault rest) a
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

instance PrettyUsing rest CheckedExpr => PrettyUsing ('Denormalise rest) NormExpr where
  prettyUsing e = prettyUsing @rest (unnormalise @NormExpr @CheckedExpr e)

instance
  PrettyUsing rest (Contextualised CheckedExpr BoundDBCtx) =>
  PrettyUsing ('Denormalise rest) (Contextualised NormExpr BoundDBCtx)
  where
  prettyUsing (WithContext e ctx) =
    prettyUsing @rest (WithContext (unnormalise @NormExpr @CheckedExpr e) ctx)

instance PrettyUsing rest CheckedArg => PrettyUsing ('Denormalise rest) NormArg where
  prettyUsing e = prettyUsing @rest (unnormalise @NormArg @CheckedArg e)

--------------------------------------------------------------------------------
-- Instances for constraints

instance PrettyUsing rest NormExpr => PrettyUsing rest UnificationConstraint where
  prettyUsing (Unify e1 e2) = prettyUsing @rest e1 <+> "~" <+> prettyUsing @rest e2

instance PrettyUsing rest NormExpr => PrettyUsing rest TypeClassConstraint where
  prettyUsing (Has m tc args) =
    pretty m
      <+> "<="
      <+> prettyUsing @rest (VBuiltin mempty (Constructor $ TypeClass tc) (NonEmpty.toList args))

instance
  (PrettyUsing rest UnificationConstraint, PrettyUsing rest TypeClassConstraint) =>
  PrettyUsing rest Constraint
  where
  prettyUsing = \case
    UnificationConstraint c -> prettyUsing @rest c
    TypeClassConstraint c -> prettyUsing @rest c

instance
  PrettyUsing rest Constraint =>
  PrettyUsing rest (Contextualised Constraint ConstraintContext)
  where
  prettyUsing (WithContext c ctx) = prettyUsing @rest c <+> parens (pretty ctx)

instance
  PrettyUsing rest TypeClassConstraint =>
  PrettyUsing rest (Contextualised TypeClassConstraint ConstraintContext)
  where
  prettyUsing (WithContext c ctx) = prettyUsing @rest c <+> parens (pretty ctx)

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
