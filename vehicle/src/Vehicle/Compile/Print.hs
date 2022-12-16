{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Compile.Print
  ( PrettyUsing (..),
    PrettyWith,
    Tags (..),
    prettySimple,
    prettyVerbose,
    prettyFriendly,
    prettyFriendlyDB,
    prettyFriendlyDBClosed,
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

-- | Prints to the internal language removing all implicit/instance arguments and
--  automatically inserted code. Does not convert (Co)DeBruijn indices back to
--  names.
prettySimple :: (PrettyWith ('Simple ('As 'Internal)) a) => a -> Doc b
prettySimple = prettyWith @('Simple ('As 'Internal))

-- | Prints to the internal language in all it's gory detail. Does not convert
--  (Co)DeBruijn indices back to names. Useful for debugging.
prettyVerbose :: (PrettyWith ('As 'Internal) a) => a -> Doc b
prettyVerbose = prettyWith @('As 'Internal)

-- | Prints to the external language for things that need to be displayed to
--  the user.
prettyFriendly :: (PrettyWith ('Named ('As 'External)) a) => a -> Doc b
prettyFriendly = prettyWith @('Named ('As 'External))

-- | Prints to the external language for things that need to be displayed to
--  the user. Use this when the expression is using DeBruijn indices and is
--  not closed.
prettyFriendlyDB ::
  (PrettyWith ('Named ('As 'External)) ([DBBinding], a)) =>
  [DBBinding] ->
  a ->
  Doc b
prettyFriendlyDB ctx e = prettyWith @('Named ('As 'External)) (ctx, e)

-- | This is identical to |prettyFriendly|, but exists for historical reasons.
prettyFriendlyDBClosed ::
  (PrettyWith ('Simple ('Named ('As 'External))) a) =>
  a ->
  Doc b
prettyFriendlyDBClosed = prettyWith @('Simple ('Named ('As 'External)))

prettyWith :: forall tags a b. PrettyWith tags a => a -> Doc b
prettyWith = prettyUsing @(StrategyFor tags a) @a @b

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
  | DBToNamedNaive Strategy
  | DBToNamedOpen Strategy
  | DBToNamedClosed Strategy
  | CoDBToNamedNaive Strategy
  | CoDBToDBOpen Strategy
  | CoDBToDBClosed Strategy
  | SupplyNamesOpen Strategy
  | SupplyNamesClosed Strategy
  | Denormalise Strategy
  | SimplifyWithOptions Strategy
  | SimplifyDefault Strategy
  | MapList Strategy
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
  StrategyFor ('As lang) SuppliedDBProg = 'DBToNamedNaive (StrategyFor ('As lang) NamedProg)
  StrategyFor ('As lang) SuppliedDBDecl = 'DBToNamedNaive (StrategyFor ('As lang) NamedDecl)
  StrategyFor ('As lang) SuppliedDBExpr = 'DBToNamedNaive (StrategyFor ('As lang) NamedExpr)
  StrategyFor ('As lang) SuppliedDBArg = 'DBToNamedNaive (StrategyFor ('As lang) NamedArg)
  StrategyFor ('As lang) SuppliedDBBinder = 'DBToNamedNaive (StrategyFor ('As lang) NamedBinder)
  -- To convert a CoDBExpr with the names supplied directly to the target language
  -- (instead of first converting to CoDB indices to names) then convert the CoDB indices naively to
  -- a string representing them.
  StrategyFor ('As lang) (t (NamedBinding, Maybe PositionTree) CoDBVar) =
    'CoDBToNamedNaive ('PrintAs lang)
  -- To convert an expression using a named representation to a named representation is a no-op.
  StrategyFor ('Named tags) NamedProg = StrategyFor tags NamedProg
  StrategyFor ('Named tags) NamedDecl = StrategyFor tags NamedDecl
  StrategyFor ('Named tags) NamedExpr = StrategyFor tags NamedExpr
  StrategyFor ('Named tags) NamedArg = StrategyFor tags NamedArg
  StrategyFor ('Named tags) NamedBinder = StrategyFor tags NamedBinder
  -- To convert a closed expression using a DB representation but whose missing names have been supplied
  -- to a named representation, perform the DB to named conversion.
  StrategyFor ('Named tags) SuppliedDBProg = 'DBToNamedClosed (StrategyFor tags NamedProg)
  StrategyFor ('Named tags) SuppliedDBDecl = 'DBToNamedClosed (StrategyFor tags NamedDecl)
  StrategyFor ('Named tags) SuppliedDBExpr = 'DBToNamedClosed (StrategyFor tags NamedExpr)
  StrategyFor ('Named tags) SuppliedDBArg = 'DBToNamedClosed (StrategyFor tags NamedArg)
  StrategyFor ('Named tags) SuppliedDBBinder = 'DBToNamedClosed (StrategyFor tags NamedBinder)
  -- To convert an open expression using a DB representation but whose missing names have been supplied
  -- to a named representation, perform the DB to named conversion.
  StrategyFor ('Named tags) (NamedBoundCtx, SuppliedDBExpr) = 'DBToNamedOpen (StrategyFor tags NamedExpr)
  StrategyFor ('Named tags) (NamedBoundCtx, SuppliedDBArg) = 'DBToNamedOpen (StrategyFor tags NamedArg)
  StrategyFor ('Named tags) (NamedBoundCtx, SuppliedDBBinder) = 'DBToNamedOpen (StrategyFor tags NamedBinder)
  -- To convert a closed expression in the co-deBruijn representation to a named representation
  -- first convert to a deBruijn representation.
  StrategyFor ('Named tags) CoDBExpr = 'CoDBToDBClosed (StrategyFor tags DBExpr)
  StrategyFor ('Named tags) CoDBArg = 'CoDBToDBClosed (StrategyFor tags DBArg)
  StrategyFor ('Named tags) CoDBBinder = 'CoDBToDBClosed (StrategyFor tags DBBinder)
  -- To convert an open expression in the co-deBruijn representation to a named representation
  -- first convert to a deBruijn representation.
  StrategyFor ('Named tags) (BoundDBCtx, CoDBExpr) = 'CoDBToDBOpen (StrategyFor ('Named tags) (BoundDBCtx, DBExpr))
  StrategyFor ('Named tags) (BoundDBCtx, CoDBArg) = 'CoDBToDBOpen (StrategyFor ('Named tags) (BoundDBCtx, DBArg))
  StrategyFor ('Named tags) (BoundDBCtx, CoDBBinder) = 'CoDBToDBOpen (StrategyFor ('Named tags) (BoundDBCtx, DBBinder))
  -- If we have a closed term with DB indices, then first supply the missing names.
  StrategyFor tags (Prog DBBinding var) = 'SupplyNamesClosed (StrategyFor tags (Prog NamedBinding var))
  StrategyFor tags (Decl DBBinding var) = 'SupplyNamesClosed (StrategyFor tags (Decl NamedBinding var))
  StrategyFor tags (Expr DBBinding var) = 'SupplyNamesClosed (StrategyFor tags (Expr NamedBinding var))
  StrategyFor tags (Arg DBBinding var) = 'SupplyNamesClosed (StrategyFor tags (Arg NamedBinding var))
  StrategyFor tags (Binder DBBinding var) = 'SupplyNamesClosed (StrategyFor tags (Binder NamedBinding var))
  -- If we have an open term with DB indices, then first supply the missing names.
  StrategyFor tags (BoundDBCtx, Expr DBBinding var) = 'SupplyNamesOpen (StrategyFor tags (NamedBoundCtx, Expr NamedBinding var))
  StrategyFor tags (BoundDBCtx, Arg DBBinding var) = 'SupplyNamesOpen (StrategyFor tags (NamedBoundCtx, Arg NamedBinding var))
  StrategyFor tags (BoundDBCtx, Binder DBBinding var) = 'SupplyNamesOpen (StrategyFor tags (NamedBoundCtx, Binder NamedBinding var))
  StrategyFor tags (BoundDBCtx, t (CoDBBinding DBBinding) var) =
    'SupplyNamesOpen (StrategyFor tags (BoundDBCtx, t (Name, Maybe PositionTree) var))
  StrategyFor tags (t (CoDBBinding DBBinding) var) =
    'SupplyNamesClosed (StrategyFor tags (t (NamedBinding, Maybe PositionTree) var))
  -- Closed normalised expressions
  StrategyFor tags NormExpr = 'Denormalise (StrategyFor tags CheckedExpr)
  StrategyFor tags NormArg = 'Denormalise (StrategyFor tags CheckedArg)
  -- Open normalised expressions
  StrategyFor tags (BoundDBCtx, NormExpr) = 'Denormalise (StrategyFor tags (BoundDBCtx, CheckedExpr))
  -- Constraints
  StrategyFor tags Constraint = StrategyFor tags NormExpr
  StrategyFor tags TypeClassConstraint = StrategyFor tags NormExpr
  StrategyFor tags UnificationConstraint = StrategyFor tags NormExpr
  -- Things that we just pretty print.
  StrategyFor tags DBBinding = 'Pretty
  StrategyFor tags PositionTree = 'Pretty
  StrategyFor tags Int = 'Pretty
  StrategyFor tags Text = 'Pretty
  -- Objects for which we want to block the strategy computation on.
  StrategyFor tags (MetaMap a) = 'Opaque (StrategyFor tags a)
  StrategyFor tags PositionsInExpr = 'Opaque (StrategyFor tags CheckedExpr)
  StrategyFor tags (Contextualised a b) = StrategyFor tags a
  -- Simplification
  StrategyFor ('Simple tags) (SimplifyOptions, a) = 'SimplifyWithOptions (StrategyFor tags a)
  StrategyFor ('Simple tags) a = 'SimplifyDefault (StrategyFor tags a)
  -- Things were we just print the structure and recursively print through.
  StrategyFor tags (IntMap a) = 'MapIntMap (StrategyFor tags a)
  StrategyFor tags [a] = 'MapList (StrategyFor tags a)
  StrategyFor tags (a, b) = 'MapTuple2 (StrategyFor tags a) (StrategyFor tags b)
  StrategyFor tags (a, b, c) = 'MapTuple3 (StrategyFor tags a) (StrategyFor tags b) (StrategyFor tags c)
  -- Otherwise if we cannot compute an error then throw an informative error at type-checking time.
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

instance (PrettyUsing rest NamedProg) => PrettyUsing ('DBToNamedNaive rest) SuppliedDBProg where
  prettyUsing e = prettyUsing @rest (unwrapProg (runNaiveDBDescope (WrapProg e)))

instance (PrettyUsing rest NamedDecl) => PrettyUsing ('DBToNamedNaive rest) SuppliedDBDecl where
  prettyUsing e = prettyUsing @rest (unwrapDecl (runNaiveDBDescope (WrapDecl e)))

instance (PrettyUsing rest NamedExpr) => PrettyUsing ('DBToNamedNaive rest) SuppliedDBExpr where
  prettyUsing e = prettyUsing @rest (runNaiveDBDescope e)

instance (PrettyUsing rest NamedArg) => PrettyUsing ('DBToNamedNaive rest) SuppliedDBArg where
  prettyUsing e = prettyUsing @rest (unwrapArg (runNaiveDBDescope (WrapArg e)))

instance (PrettyUsing rest NamedBinder) => PrettyUsing ('DBToNamedNaive rest) SuppliedDBBinder where
  prettyUsing e = prettyUsing @rest (unwrapBinder (runNaiveDBDescope (WrapBinder e)))

--------------------------------------------------------------------------------
-- Convert open terms from DeBruijn representation to named representation

instance
  PrettyUsing rest NamedExpr =>
  PrettyUsing ('DBToNamedOpen rest) (NamedBoundCtx, SuppliedDBExpr)
  where
  prettyUsing (ctx, e) = prettyUsing @rest (runDescope ctx e)

instance
  PrettyUsing rest NamedArg =>
  PrettyUsing ('DBToNamedOpen rest) (NamedBoundCtx, SuppliedDBArg)
  where
  prettyUsing (ctx, e) = prettyUsing @rest (unwrapArg $ runDescope ctx $ WrapArg e)

instance
  PrettyUsing rest NamedBinder =>
  PrettyUsing ('DBToNamedOpen rest) (NamedBoundCtx, SuppliedDBBinder)
  where
  prettyUsing (ctx, e) = prettyUsing @rest (unwrapBinder $ runDescope ctx $ WrapBinder e)

--------------------------------------------------------------------------------
-- Convert from CoDeBruijn representation to named representation naively

instance
  PrettyUsing rest NamedExpr =>
  PrettyUsing ('CoDBToNamedNaive rest) (Expr (CoDBBinding Name) CoDBVar)
  where
  prettyUsing e =
    let (e', pts) = runNaiveCoDBDescope e
     in prettyUsing @rest e' <+> prettyMap pts

--------------------------------------------------------------------------------
-- Convert closed term from DeBruijn representation to named representation

instance PrettyUsing rest NamedProg => PrettyUsing ('DBToNamedClosed rest) SuppliedDBProg where
  prettyUsing e = prettyUsing @rest (unwrapProg $ runDescope mempty $ WrapProg e)

instance PrettyUsing rest NamedDecl => PrettyUsing ('DBToNamedClosed rest) SuppliedDBDecl where
  prettyUsing e = prettyUsing @rest (unwrapDecl $ runDescope mempty $ WrapDecl e)

instance PrettyUsing rest NamedExpr => PrettyUsing ('DBToNamedClosed rest) SuppliedDBExpr where
  prettyUsing e = prettyUsing @rest (runDescope mempty e)

instance PrettyUsing rest NamedArg => PrettyUsing ('DBToNamedClosed rest) SuppliedDBArg where
  prettyUsing e = prettyUsing @rest (unwrapArg $ runDescope mempty $ WrapArg e)

instance PrettyUsing rest NamedBinder => PrettyUsing ('DBToNamedClosed rest) SuppliedDBBinder where
  prettyUsing e = prettyUsing @rest (unwrapBinder $ runDescope mempty $ WrapBinder e)

--------------------------------------------------------------------------------
-- Convert open term from CoDeBruijn representation to DeBruijn representation

instance
  (PrettyUsing rest (BoundDBCtx, DBExpr)) =>
  PrettyUsing ('CoDBToDBOpen rest) (BoundDBCtx, CoDBExpr)
  where
  prettyUsing (ctx, (e, bvm)) = prettyUsing @rest (ctx, fromCoDB (e, bvm))

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
  PrettyUsing ('CoDBToDBClosed rest) CoDBExpr
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
  PrettyUsing rest (NamedBoundCtx, Prog Name var) =>
  PrettyUsing ('SupplyNamesOpen rest) (BoundDBCtx, Prog DBBinding var)
  where
  prettyUsing (ctx, a) =
    let (ctx', a') = supplyDBNamesWithCtx (ctx, WrapProg a)
     in prettyUsing @rest (ctx', unwrapProg a')

instance
  PrettyUsing rest (NamedBoundCtx, Decl Name var) =>
  PrettyUsing ('SupplyNamesOpen rest) (BoundDBCtx, Decl DBBinding var)
  where
  prettyUsing (ctx, a) =
    let (ctx', a') = supplyDBNamesWithCtx (ctx, WrapDecl a)
     in prettyUsing @rest (ctx', unwrapDecl a')

instance
  PrettyUsing rest (NamedBoundCtx, Expr Name var) =>
  PrettyUsing ('SupplyNamesOpen rest) (BoundDBCtx, Expr DBBinding var)
  where
  prettyUsing p = prettyUsing @rest (supplyDBNamesWithCtx p)

instance
  PrettyUsing rest (NamedBoundCtx, Arg Name var) =>
  PrettyUsing ('SupplyNamesOpen rest) (BoundDBCtx, Arg DBBinding var)
  where
  prettyUsing (ctx, a) =
    let (ctx', a') = supplyDBNamesWithCtx (ctx, WrapArg a)
     in prettyUsing @rest (ctx', unwrapArg a')

instance
  PrettyUsing rest (NamedBoundCtx, Binder Name var) =>
  PrettyUsing ('SupplyNamesOpen rest) (BoundDBCtx, Binder DBBinding var)
  where
  prettyUsing (ctx, a) =
    let (ctx', a') = supplyDBNamesWithCtx (ctx, WrapBinder a)
     in prettyUsing @rest (ctx', unwrapBinder a')

--------------------------------------------------------------------------------
-- Supply names for closed DB terms

instance
  PrettyUsing rest (Prog Name var) =>
  PrettyUsing ('SupplyNamesClosed rest) (Prog DBBinding var)
  where
  prettyUsing e = prettyUsing @rest (unwrapProg $ supplyDBNames $ WrapProg e)

instance
  PrettyUsing rest (Decl Name var) =>
  PrettyUsing ('SupplyNamesClosed rest) (Decl DBBinding var)
  where
  prettyUsing e = prettyUsing @rest (unwrapDecl $ supplyDBNames $ WrapDecl e)

instance
  PrettyUsing rest (Expr Name var) =>
  PrettyUsing ('SupplyNamesClosed rest) (Expr DBBinding var)
  where
  prettyUsing e = prettyUsing @rest (supplyDBNames e)

instance
  PrettyUsing rest (Arg Name var) =>
  PrettyUsing ('SupplyNamesClosed rest) (Arg DBBinding var)
  where
  prettyUsing e = prettyUsing @rest (unwrapArg (supplyDBNames (WrapArg e)))

instance
  PrettyUsing rest (Binder Name var) =>
  PrettyUsing ('SupplyNamesClosed rest) (Binder DBBinding var)
  where
  prettyUsing e = prettyUsing @rest (unwrapBinder (supplyDBNames (WrapBinder e)))

instance
  (SupplyNames t, PrettyUsing rest ([Name], t (CoDBBinding Name) var)) =>
  PrettyUsing ('SupplyNamesOpen rest) ([DBBinding], t (CoDBBinding DBBinding) var)
  where
  prettyUsing p = prettyUsing @rest (supplyCoDBNamesWithCtx p)

instance
  (SupplyNames t, PrettyUsing rest (t (CoDBBinding Name) var)) =>
  PrettyUsing ('SupplyNamesClosed rest) (t (CoDBBinding DBBinding) var)
  where
  prettyUsing e = prettyUsing @rest (supplyCoDBNames e)

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
  PrettyUsing rest (BoundDBCtx, CheckedExpr) =>
  PrettyUsing ('Denormalise rest) (BoundDBCtx, NormExpr)
  where
  prettyUsing (ctx, e) = prettyUsing @rest (ctx, unnormalise @NormExpr @CheckedExpr e)

instance PrettyUsing rest CheckedArg => PrettyUsing ('Denormalise rest) NormArg where
  prettyUsing e = prettyUsing @rest (unnormalise @NormArg @CheckedArg e)

-- This is a hack that should eventually be removed when `TypeClassConstraints`
-- use norm expressions.
instance PrettyUsing rest CheckedExpr => PrettyUsing ('Denormalise rest) CheckedExpr where
  prettyUsing = prettyUsing @rest

--------------------------------------------------------------------------------
-- Instances for opaque types

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

instance (PrettyUsing rest a, Pretty b) => PrettyUsing rest (Contextualised a b) where
  prettyUsing (WithContext a b) = prettyUsing @rest a <> "     " <> pretty b

instance PrettyUsing rest Constraint => PrettyUsing ('Opaque rest) (Contextualised Constraint ConstraintContext) where
  prettyUsing (WithContext c ctx) = prettyUsing @rest c <+> parens (pretty ctx)

instance PrettyUsing rest a => PrettyUsing ('Opaque rest) (MetaMap a) where
  prettyUsing (MetaMap m) = prettyMapEntries entries
    where
      entries = fmap (bimap MetaID (prettyUsing @rest)) (IntMap.assocs m)

instance (PrettyUsing rest CheckedExpr) => PrettyUsing ('Opaque rest) PositionsInExpr where
  prettyUsing (PositionsInExpr e p) = prettyUsing @rest (fromCoDB (substPos hole (Just p) e))
    where
      hole = (Hole mempty $ Text.pack "@", mempty)
