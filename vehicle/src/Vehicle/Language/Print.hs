{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}

module Vehicle.Language.Print
  ( PrettyUsing(..)
  , PrettyWith
  , Tags(..)
  , prettySimple
  , prettyVerbose
  , prettyFriendly
  , prettyFriendlyDB
  , prettyFriendlyDBClosed
  ) where

import Control.Exception (assert)
import Data.Bifunctor (bimap)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap (assocs)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.TypeLits (ErrorMessage (..), TypeError)
import Prettyprinter (list)
import Vehicle.Compile.CoDeBruijnify
import Vehicle.Compile.Delaborate.External as External (Delaborate (delab))
import Vehicle.Compile.Delaborate.Internal as Internal
import Vehicle.Compile.Descope
import Vehicle.Compile.Prelude hiding (MapList)
import Vehicle.Compile.Simplify
import Vehicle.Compile.SupplyNames
import Vehicle.Compile.Type.Constraint
import Vehicle.Compile.Type.MetaMap (MetaMap (..))
import Vehicle.Compile.Type.MetaSet qualified as MetaSet
import Vehicle.Syntax.External.Abs qualified as BF
import Vehicle.Syntax.External.Print as External (Print, printTree)
import Vehicle.Syntax.Internal.Abs qualified as BC
import Vehicle.Syntax.Internal.Print as Internal (Print, printTree)


--------------------------------------------------------------------------------
-- Public methods
--------------------------------------------------------------------------------

-- |Prints to the internal language removing all implicit/instance arguments and
-- automatically inserted code. Does not convert (Co)DeBruijn indices back to
-- names.
prettySimple :: (PrettyWith ('Simple ('As 'Internal)) a) => a -> Doc b
prettySimple = prettyWith @('Simple ('As 'Internal))

-- |Prints to the internal language in all it's gory detail. Does not convert
-- (Co)DeBruijn indices back to names. Useful for debugging.
prettyVerbose :: (PrettyWith ('As 'Internal) a) => a -> Doc b
prettyVerbose = prettyWith @('As 'Internal)

-- |Prints to the external language for things that need to be displayed to
-- the user.
prettyFriendly :: (PrettyWith ('Named ('As 'External)) a) => a -> Doc b
prettyFriendly = prettyWith @('Named ('As 'External))

-- |Prints to the external language for things that need to be displayed to
-- the user. Use this when the expression is using DeBruijn indices and is
-- not closed.
prettyFriendlyDB :: (PrettyWith ('Named ('As 'External)) ([DBBinding], a))
                 => [DBBinding] -> a -> Doc b
prettyFriendlyDB ctx e = prettyWith @('Named ('As 'External)) (ctx, e)

-- | This is identical to |prettyFriendly|, but exists for historical reasons.
prettyFriendlyDBClosed :: (PrettyWith ('Simple ('Named ('As 'External))) a)
                       => a -> Doc b
prettyFriendlyDBClosed = prettyWith @('Simple ('Named ('As 'External)))

prettyWith :: forall tags a b. PrettyWith tags a => a -> Doc b
prettyWith = prettyUsing @(StrategyFor tags a) @a @b

--------------------------------------------------------------------------------
-- Printing strategies
--------------------------------------------------------------------------------

-- Tags denote at a high-level how you want the term to be printed.
data Tags
  = As VehicleLang
  -- ^ The final tag denotes which output grammar should be used
  | Named Tags
  -- ^ The named tag ensures that the term is converted back to using named binders
  | Simple Tags
  -- ^ The simple tag ensures that superfluous information is erased

-- | A strategy is an abstract representation of the sequence of operations that
-- are needed in order to convert something into a printable form. It should not
-- be confused with the actual operations needed to do so.
data Strategy
  = ConvertTo            VehicleLang
  | DBToNamedNaive       Strategy
  | DBToNamedOpen        Strategy
  | DBToNamedClosed      Strategy
  | CoDBToNamedNaive     Strategy
  | CoDBToDBOpen         Strategy
  | CoDBToDBClosed       Strategy
  | SupplyNamesOpen      Strategy
  | SupplyNamesClosed    Strategy
  | SimplifyWithOptions  Strategy
  | SimplifyDefault      Strategy
  | MapList              Strategy
  | MapIntMap            Strategy
  | MapTuple2            Strategy Strategy
  | MapTuple3            Strategy Strategy Strategy
  | Opaque               Strategy
  | Pretty

-- | This type family computes the correct printing strategy given the tags
-- and the type of the expression.
type family StrategyFor (tags :: Tags) a :: Strategy where

  -- To convert any named representation to the target language, simply convert it.
  StrategyFor ('As lang) NamedProg   = 'ConvertTo lang
  StrategyFor ('As lang) NamedDecl   = 'ConvertTo lang
  StrategyFor ('As lang) NamedExpr   = 'ConvertTo lang
  StrategyFor ('As lang) NamedArg    = 'ConvertTo lang
  StrategyFor ('As lang) NamedBinder = 'ConvertTo lang

  -- To convert a DB representation with the names supplied directly to the target language
  -- (instead of first converting the DB indices to names) then convert the DB indices naively to a
  -- string representing them, e.g. the index `0` gets converted to `i0`
  StrategyFor ('As lang) SuppliedDBProg   = 'DBToNamedNaive (StrategyFor ('As lang) NamedProg)
  StrategyFor ('As lang) SuppliedDBDecl   = 'DBToNamedNaive (StrategyFor ('As lang) NamedDecl)
  StrategyFor ('As lang) SuppliedDBExpr   = 'DBToNamedNaive (StrategyFor ('As lang) NamedExpr)
  StrategyFor ('As lang) SuppliedDBArg    = 'DBToNamedNaive (StrategyFor ('As lang) NamedArg)
  StrategyFor ('As lang) SuppliedDBBinder = 'DBToNamedNaive (StrategyFor ('As lang) NamedBinder)

  -- To convert a CoDBExpr with the names supplied directly to the target language
  -- (instead of first converting to CoDB indices to names) then convert the CoDB indices naively to
  -- a string representing them.
  StrategyFor ('As lang) (t (NamedBinding, Maybe PositionTree) CoDBVar)
    = 'CoDBToNamedNaive ('ConvertTo lang)

  -- To convert an expression using a named representation to a named representation is a no-op.
  StrategyFor ('Named tags) NamedProg   = StrategyFor tags NamedProg
  StrategyFor ('Named tags) NamedDecl   = StrategyFor tags NamedDecl
  StrategyFor ('Named tags) NamedExpr   = StrategyFor tags NamedExpr
  StrategyFor ('Named tags) NamedArg    = StrategyFor tags NamedArg
  StrategyFor ('Named tags) NamedBinder = StrategyFor tags NamedBinder

  -- To convert a closed expression using a DB representation but whose missing names have been supplied
  -- to a named representation, perform the DB to named conversion.
  StrategyFor ('Named tags) SuppliedDBProg   = 'DBToNamedClosed (StrategyFor tags NamedProg)
  StrategyFor ('Named tags) SuppliedDBDecl   = 'DBToNamedClosed (StrategyFor tags NamedDecl)
  StrategyFor ('Named tags) SuppliedDBExpr   = 'DBToNamedClosed (StrategyFor tags NamedExpr)
  StrategyFor ('Named tags) SuppliedDBArg    = 'DBToNamedClosed (StrategyFor tags NamedArg)
  StrategyFor ('Named tags) SuppliedDBBinder = 'DBToNamedClosed (StrategyFor tags NamedBinder)

  -- To convert an open expression using a DB representation but whose missing names have been supplied
  -- to a named representation, perform the DB to named conversion.
  StrategyFor ('Named tags) (NamedBoundCtx, SuppliedDBExpr)   = 'DBToNamedOpen (StrategyFor tags NamedExpr)
  StrategyFor ('Named tags) (NamedBoundCtx, SuppliedDBArg)    = 'DBToNamedOpen (StrategyFor tags NamedArg)
  StrategyFor ('Named tags) (NamedBoundCtx, SuppliedDBBinder) = 'DBToNamedOpen (StrategyFor tags NamedBinder)

  -- To convert a closed expression in the co-deBruijn representation to a named representation
  -- first convert to a deBruijn representation.
  StrategyFor ('Named tags) CoDBExpr   = 'CoDBToDBClosed (StrategyFor tags DBExpr)
  StrategyFor ('Named tags) CoDBArg    = 'CoDBToDBClosed (StrategyFor tags DBArg)
  StrategyFor ('Named tags) CoDBBinder = 'CoDBToDBClosed (StrategyFor tags DBBinder)

  -- To convert an open expression in the co-deBruijn representation to a named representation
  -- first convert to a deBruijn representation.
  StrategyFor ('Named tags) (BoundDBCtx, CoDBExpr)   = 'CoDBToDBOpen (StrategyFor ('Named tags) (BoundDBCtx, DBExpr))
  StrategyFor ('Named tags) (BoundDBCtx, CoDBArg)    = 'CoDBToDBOpen (StrategyFor ('Named tags) (BoundDBCtx, DBArg))
  StrategyFor ('Named tags) (BoundDBCtx, CoDBBinder) = 'CoDBToDBOpen (StrategyFor ('Named tags) (BoundDBCtx, DBBinder))

  -- If we have a closed term with DB indices, then first supply the missing names.
  StrategyFor tags (Prog   DBBinding var) = 'SupplyNamesClosed (StrategyFor tags (Prog   NamedBinding var))
  StrategyFor tags (Decl   DBBinding var) = 'SupplyNamesClosed (StrategyFor tags (Decl   NamedBinding var))
  StrategyFor tags (Expr   DBBinding var) = 'SupplyNamesClosed (StrategyFor tags (Expr   NamedBinding var))
  StrategyFor tags (Arg    DBBinding var) = 'SupplyNamesClosed (StrategyFor tags (Arg    NamedBinding var))
  StrategyFor tags (Binder DBBinding var) = 'SupplyNamesClosed (StrategyFor tags (Binder NamedBinding var))

  -- If we have an open term with DB indices, then first supply the missing names.
  StrategyFor tags (BoundDBCtx, Expr   DBBinding var) = 'SupplyNamesOpen (StrategyFor tags (NamedBoundCtx, Expr   NamedBinding var))
  StrategyFor tags (BoundDBCtx, Arg    DBBinding var) = 'SupplyNamesOpen (StrategyFor tags (NamedBoundCtx, Arg    NamedBinding var))
  StrategyFor tags (BoundDBCtx, Binder DBBinding var) = 'SupplyNamesOpen (StrategyFor tags (NamedBoundCtx, Binder NamedBinding var))

  StrategyFor tags (BoundDBCtx, t (CoDBBinding DBBinding) var)
    = 'SupplyNamesOpen (StrategyFor tags (BoundDBCtx, t (Name, Maybe PositionTree) var))

  StrategyFor tags (t (CoDBBinding DBBinding) var)
    = 'SupplyNamesClosed (StrategyFor tags (t (NamedBinding, Maybe PositionTree) var))

  -- Simplification
  StrategyFor ('Simple tags) (SimplifyOptions, a) = 'SimplifyWithOptions (StrategyFor tags a)
  StrategyFor ('Simple tags) a                    = 'SimplifyDefault (StrategyFor tags a)

  -- Things that we just pretty print.
  StrategyFor tags DBBinding    = 'Pretty
  StrategyFor tags PositionTree = 'Pretty
  StrategyFor tags Int          = 'Pretty
  StrategyFor tags Text         = 'Pretty

  -- Things were we just print the structure and recursively print through.
  StrategyFor tags (IntMap a) = 'MapIntMap (StrategyFor tags a)
  StrategyFor tags [a]        = 'MapList   (StrategyFor tags a)
  StrategyFor tags (a, b)     = 'MapTuple2 (StrategyFor tags a) (StrategyFor tags b)
  StrategyFor tags (a, b, c)  = 'MapTuple3 (StrategyFor tags a) (StrategyFor tags b) (StrategyFor tags c)

  -- Objects for which we want to block the strategy computation on.
  StrategyFor tags Constraint      = 'Opaque (StrategyFor tags CheckedExpr)
  StrategyFor tags (MetaMap a)     = 'Opaque (StrategyFor tags a)
  StrategyFor tags PositionsInExpr = 'Opaque (StrategyFor tags CheckedExpr)

  -- Otherwise if we cannot compute an error then throw an informative error at type-checking time.
  StrategyFor tags a
    = TypeError ('Text "Cannot print value of type \"" ':<>: 'ShowType a ':<>: 'Text "\" with tags \"" ':<>:
                 'ShowType tags ':<>: 'Text "\"."
           ':$$: 'Text "Perhaps you could add support to Vehicle.Language.Print.StrategyFor?")

--------------------------------------------------------------------------------
-- Executing printing strategies
--------------------------------------------------------------------------------

-- | A type synonym that takes the tags and the type and computes the strategy
-- for the combination to guide type-class resolution.
type PrettyWith tags a = PrettyUsing (StrategyFor tags a) a

class PrettyUsing (strategy :: Strategy) a where
  prettyUsing :: a -> Doc b

--------------------------------------------------------------------------------
-- BNFC ASTs
--
-- Once we're down at the BNFC AST we can just pretty print it.

instance Internal.Print a => Pretty (ViaBnfcInternal a) where
  pretty (ViaBnfcInternal e) = pretty $ bnfcPrintHack $ Internal.printTree e

newtype ViaBnfcInternal a = ViaBnfcInternal a

deriving via (ViaBnfcInternal BC.Prog) instance Pretty BC.Prog
deriving via (ViaBnfcInternal BC.Decl) instance Pretty BC.Decl
deriving via (ViaBnfcInternal BC.Expr) instance Pretty BC.Expr
deriving via (ViaBnfcInternal BC.Arg)  instance Pretty BC.Arg

instance External.Print a => Pretty (ViaBnfcExternal a) where
  pretty (ViaBnfcExternal e) = pretty $ bnfcPrintHack $ External.printTree e

newtype ViaBnfcExternal a = ViaBnfcExternal a

deriving via (ViaBnfcExternal BF.Prog)   instance Pretty BF.Prog
deriving via (ViaBnfcExternal BF.Decl)   instance Pretty BF.Decl
deriving via (ViaBnfcExternal BF.Expr)   instance Pretty BF.Expr
deriving via (ViaBnfcExternal BF.Binder) instance Pretty BF.Binder
deriving via (ViaBnfcExternal BF.Arg)    instance Pretty BF.Arg

--------------------------------------------------------------------------------
-- Conversion to internal BNFC representation

instance (Internal.Delaborate t bnfc, Pretty bnfc) => PrettyUsing ('ConvertTo 'Internal) t where
  prettyUsing e = pretty (Internal.delab @t @bnfc e)

--------------------------------------------------------------------------------
-- Conversion to external BNFC representation

instance (External.Delaborate t bnfc, Pretty bnfc) => PrettyUsing ('ConvertTo 'External) t where
  prettyUsing e = pretty (External.delab @t @bnfc e)

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

instance PrettyUsing rest NamedExpr
      => PrettyUsing ('DBToNamedOpen rest) (NamedBoundCtx, SuppliedDBExpr) where
  prettyUsing (ctx, e) = prettyUsing @rest (runDescope ctx e)

instance PrettyUsing rest NamedArg
      => PrettyUsing ('DBToNamedOpen rest) (NamedBoundCtx, SuppliedDBArg) where
  prettyUsing (ctx, e) = prettyUsing @rest (unwrapArg $ runDescope ctx $ WrapArg e)

instance PrettyUsing rest NamedBinder
      => PrettyUsing ('DBToNamedOpen rest) (NamedBoundCtx, SuppliedDBBinder) where
  prettyUsing (ctx, e) = prettyUsing @rest (unwrapBinder $ runDescope ctx $ WrapBinder e)

--------------------------------------------------------------------------------
-- Convert from CoDeBruijn representation to named representation naively

instance PrettyUsing rest NamedExpr
      => PrettyUsing ('CoDBToNamedNaive rest) (Expr (CoDBBinding Name) CoDBVar) where
  prettyUsing e = let (e', pts) = runNaiveCoDBDescope e in
    prettyUsing @rest e' <+> prettyMap pts

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

instance (PrettyUsing rest (BoundDBCtx, DBExpr))
      => PrettyUsing ('CoDBToDBOpen rest) (BoundDBCtx, CoDBExpr) where
  prettyUsing (ctx, (e, bvm)) = prettyUsing @rest (ctx , fromCoDB (e, bvm))
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

instance PrettyUsing rest DBExpr
      => PrettyUsing ('CoDBToDBClosed rest) CoDBExpr where
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

instance PrettyUsing rest (NamedBoundCtx, Prog Name var)
      => PrettyUsing ('SupplyNamesOpen rest) (BoundDBCtx, Prog DBBinding var) where
  prettyUsing (ctx, a) =
    let (ctx', a') = supplyDBNamesWithCtx (ctx, WrapProg a) in
    prettyUsing @rest (ctx', unwrapProg a')

instance PrettyUsing rest (NamedBoundCtx, Decl Name var)
      => PrettyUsing ('SupplyNamesOpen rest) (BoundDBCtx, Decl DBBinding var) where
  prettyUsing (ctx, a) =
    let (ctx', a') = supplyDBNamesWithCtx (ctx, WrapDecl a) in
    prettyUsing @rest (ctx', unwrapDecl a')

instance PrettyUsing rest (NamedBoundCtx, Expr Name var)
      => PrettyUsing ('SupplyNamesOpen rest) (BoundDBCtx, Expr DBBinding var) where
  prettyUsing p = prettyUsing @rest (supplyDBNamesWithCtx p)

instance PrettyUsing rest (NamedBoundCtx, Arg Name var)
      => PrettyUsing ('SupplyNamesOpen rest) (BoundDBCtx, Arg DBBinding var) where
  prettyUsing (ctx, a) =
    let (ctx', a') = supplyDBNamesWithCtx (ctx, WrapArg a) in
    prettyUsing @rest (ctx', unwrapArg a')

instance PrettyUsing rest (NamedBoundCtx, Binder Name var)
      => PrettyUsing ('SupplyNamesOpen rest) (BoundDBCtx, Binder DBBinding var) where
  prettyUsing (ctx, a) =
    let (ctx', a') = supplyDBNamesWithCtx (ctx, WrapBinder a) in
    prettyUsing @rest (ctx', unwrapBinder a')


--------------------------------------------------------------------------------
-- Supply names for closed DB terms

instance PrettyUsing rest (Prog Name var)
      => PrettyUsing ('SupplyNamesClosed rest) (Prog DBBinding var) where
  prettyUsing e = prettyUsing @rest (unwrapProg $ supplyDBNames $ WrapProg e)

instance PrettyUsing rest (Decl Name var)
      => PrettyUsing ('SupplyNamesClosed rest) (Decl DBBinding var) where
  prettyUsing e = prettyUsing @rest (unwrapDecl $ supplyDBNames $ WrapDecl e)

instance PrettyUsing rest (Expr Name var)
      => PrettyUsing ('SupplyNamesClosed rest) (Expr DBBinding var) where
  prettyUsing e = prettyUsing @rest (supplyDBNames e)

instance PrettyUsing rest (Arg Name var)
      => PrettyUsing ('SupplyNamesClosed rest) (Arg DBBinding var) where
  prettyUsing e = prettyUsing @rest (unwrapArg (supplyDBNames (WrapArg e)))

instance PrettyUsing rest (Binder Name var)
      => PrettyUsing ('SupplyNamesClosed rest) (Binder DBBinding var) where
  prettyUsing e = prettyUsing @rest (unwrapBinder (supplyDBNames (WrapBinder e)))




instance (SupplyNames t, PrettyUsing rest ([Name], t (CoDBBinding Name) var))
      => PrettyUsing ('SupplyNamesOpen rest) ([DBBinding], t (CoDBBinding DBBinding) var) where
  prettyUsing p = prettyUsing @rest (supplyCoDBNamesWithCtx p)

instance (SupplyNames t, PrettyUsing rest (t (CoDBBinding Name) var))
      => PrettyUsing ('SupplyNamesClosed rest) (t (CoDBBinding DBBinding) var) where
  prettyUsing e = prettyUsing @rest (supplyCoDBNames e)

instance (Simplify a, PrettyUsing rest a)
      => PrettyUsing ('SimplifyWithOptions rest) (SimplifyOptions, a) where
  prettyUsing (options, e) = prettyUsing @rest (simplifyWith options e)

instance (Simplify a, PrettyUsing rest a)
      => PrettyUsing ('SimplifyDefault rest) a where
  prettyUsing e = prettyUsing @rest (simplify e)

instance PrettyUsing rest a
      => PrettyUsing ('MapList rest) [a] where
  prettyUsing es = list (prettyUsing @rest <$> es)

instance PrettyUsing rest a => PrettyUsing ('MapIntMap rest) (IntMap a) where
  prettyUsing es = prettyIntMap (prettyUsing @rest <$> es)

instance (PrettyUsing resta a, PrettyUsing restb b) => PrettyUsing ('MapTuple2 resta restb) (a, b) where
  prettyUsing (e1, e2) = "(" <> prettyUsing @resta e1 <> "," <+> prettyUsing @restb e2 <> ")"

instance (PrettyUsing resta a, PrettyUsing restb b, PrettyUsing restc c)
      => PrettyUsing ('MapTuple3 resta restb restc) (a, b, c) where
  prettyUsing (e1, e2, e3) =
    "(" <>  prettyUsing @resta e1 <>
    "," <+> prettyUsing @restb e2 <>
    "," <+> prettyUsing @restc e3 <>
    ")"

--------------------------------------------------------------------------------
-- Instances which defer to primitive pretty instances

instance Pretty a => PrettyUsing 'Pretty a where
  prettyUsing = pretty

--------------------------------------------------------------------------------
-- Instances for opaque types

instance (PrettyUsing rest CheckedExpr) => PrettyUsing ('Opaque rest) Constraint where
  prettyUsing = \case
    UC ctx (Unify (e1, e2)) -> prettyUsing @rest e1 <+> "~" <+> prettyUsing @rest e2 <> prettyCtx ctx
    TC ctx (Has m tc e)     -> pretty m <+> "<=" <+> prettyUsing @rest (BuiltinTypeClass mempty tc e) <> prettyCtx ctx
    where
      prettyCtx :: ConstraintContext -> Doc a
      prettyCtx ctx = do
        let blockingMetas = blockedBy ctx
        if MetaSet.null blockingMetas
          then ""
          else "     " <> parens ("blockedBy:" <+> pretty (blockedBy ctx))
    -- <+> "<boundCtx=" <> pretty (ctxNames (boundContext c)) <> ">"
    -- <+> parens (pretty (provenanceOf c))

instance PrettyUsing rest a => PrettyUsing ('Opaque rest) (MetaMap a) where
  prettyUsing (MetaMap m) = prettyMapEntries entries
    where entries = fmap (bimap MetaVar (prettyUsing @rest)) (IntMap.assocs m)

instance (PrettyUsing rest CheckedExpr) => PrettyUsing ('Opaque rest) PositionsInExpr where
  prettyUsing (PositionsInExpr e p) = prettyUsing @rest (fromCoDB (substPos hole (Just p) e))
    where hole = (Hole mempty $ Text.pack "@", mempty)



--------------------------------------------------------------------------------
-- Other
--------------------------------------------------------------------------------

-- BNFC printer treats the braces for implicit arguments as layout braces and
-- therefore adds a ton of newlines everywhere. This hack attempts to undo this.
bnfcPrintHack :: String -> Text
bnfcPrintHack =
  Text.replace "{{ " "{{" .
  Text.replace "{  " "{" .
  Text.replace "\n{" " {" .
  Text.replace "{\n" "{" .
  Text.replace "\n}" "}" .
  Text.replace "}\n" "} " .
  Text.pack
