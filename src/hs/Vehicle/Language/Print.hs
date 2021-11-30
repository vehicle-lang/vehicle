{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE  #-}

module Vehicle.Language.Print
  ( prettySimple
  , prettyVerbose
  , prettyFriendly
  , prettyFriendlyDB
  , prettyFriendlyDBClosed
  ) where

import Prettyprinter (list, tupled)

import Data.Text

import Vehicle.Core.Print as Core (printTree, Print)
import Vehicle.Frontend.Print as Frontend (printTree)
import Vehicle.Core.Abs qualified as BC
import Vehicle.Frontend.Abs qualified as BF

import Vehicle.Prelude
import Vehicle.Language.AST
import Vehicle.Compile.Simplify
import Vehicle.Compile.Delaborate.Core as Core
import Vehicle.Compile.Delaborate.Frontend as Frontend
import Vehicle.Compile.Descope
import Vehicle.Compile.SupplyNames (SupplyNames, runSupplyNamesWithCtx, runSupplyNames)
import Vehicle.Compile.Type.MetaSubstitution
import Vehicle.Compile.Type.Constraint


-- |Prints to the core language removing all implicit/instance arguments and
-- automatically inserted code. Does not convert DeBruijn indices back to names.
prettySimple :: PrettyWith ('Simple 'Core) a => a -> Doc b
prettySimple = prettyWith

-- |Prints to the core language in all it's gory detail. Does not convert DeBruijn
-- indices back to names. Useful for debugging.
prettyVerbose :: PrettyWith 'Core a => a -> Doc b
prettyVerbose = prettyWith

-- |Prints to the frontend language for things that need to be displayed to
-- the user.
prettyFriendly :: PrettyWith ('Simple ('Named 'Frontend)) a => a -> Doc b
prettyFriendly = prettyWith

-- |Prints to the frontend language for things that need to be displayed to
-- the user. Use this when the expression is using DeBruijn indices and is
-- not closed.
prettyFriendlyDB :: PrettyWith ('Simple ('Named ('With [Maybe Symbol] 'Frontend))) ([Maybe Symbol], t (Maybe Symbol) LocallyNamelessVar ann)
                 => [Maybe Symbol]
                 -> t (Maybe Symbol) LocallyNamelessVar ann
                 -> Doc b
prettyFriendlyDB ctx e = prettyWith (ctx, e)

-- | This is identical to |prettyFriendly|, but exists for historical reasons.
prettyFriendlyDBClosed :: (PrettyWith ('Simple ('Named 'Frontend))) => a -> Doc b
prettyFriendlyDBClosed = prettyWith

data Tags
  = Core
  | Frontend
  | Named Tags
  | Simple Tags
  | With (arg :: *) Tags

class PrettyWith (tags :: Tags) a where
  prettyWith :: forall tags. a -> Doc b


-- Core printing functions, via the BNFC grammars

newtype ViaBnfc a = ViaBnfc a

instance (Core.Delaborate a b, Core.Print b) => PrettyWith 'Core (ViaBnfc a) where
  prettyWith (ViaBnfc e) = pretty (Core.printTree (Core.delab e))

instance (Frontend.Delaborate a b, Frontend.Print b) => PrettyWith 'Front (ViaBnfc a) where
  prettyWith (ViaBnfc e) = pretty (Frontend.printTree (Frontend.delab e))

deriving via (ViaBnfc (NamedProg ann)) instance PrettyWith 'Core (NamedProg ann)
deriving via (ViaBnfc (NamedDecl ann)) instance PrettyWith 'Core (NamedDecl ann)
deriving via (ViaBnfc (NamedExpr ann)) instance PrettyWith 'Core (NamedExpr ann)

deriving via (ViaBnfc (NamedProg ann)) instance PrettyWith 'Frontend (NamedProg ann)
deriving via (ViaBnfc (NamedDecl ann)) instance PrettyWith 'Frontend (NamedDecl ann)
deriving via (ViaBnfc (NamedExpr ann)) instance PrettyWith 'Frontend (NamedExpr ann)


-- Converting from deBruijn indices back to names

-- | An instance for named where the argument is already named.
instance (PrettyWith tags (t Symbol Symbol ann))
      => PrettyWith ('Named tags) (t Symbol Symbol ann) where
  prettyWith e = e

-- | An instance for named where the argument needs to be descoped, but is assumed to be a closed term.
instance (SupplyNames t, Descope t, PrettyWith tags (t Symbol Symbol ann))
      => PrettyWith ('Named tags) (t Symbol LocallyNamelessVar ann) where
  prettyWith e = prettyWith (runDescope mempty (runSupplyNames e))

instance (SupplyNames t, Descope t, PrettyWith tags (t Symbol Symbol ann))
      => PrettyWith ('Named ('With [Maybe Symbol] tags)) ([Maybe Symbol], t Symbol LocallyNamelessVar ann) where
  prettyWith p = prettyWith (uncurry runDescope (runSupplyNamesWithCtx p))


-- Printing deBruijn indices

newtype ViaNaiveDescope a = ViaNaiveDescope a

instance (Descope t, PrettyWith tags (t Symbol Symbol ann))
      => PrettyWith tags (ViaNaiveDescope (t (Maybe Symbol) LocallyNamelessVar ann)) where
  prettyWith e = prettyWith (runNaiveDescope e)

deriving via (ViaNaiveDescope (DeBruijnDecl ann)) instance PrettyWith 'Core (DeBruijnDecl ann)
deriving via (ViaNaiveDescope (DeBruijnProg ann)) instance PrettyWith 'Core (DeBruijnProg ann)
deriving via (ViaNaiveDescope (DeBruijnExpr ann)) instance PrettyWith 'Core (DeBruijnExpr ann)

deriving via (ViaNaiveDescope (DeBruijnProg ann)) instance PrettyWith 'Frontend (DeBruijnProg ann)
deriving via (ViaNaiveDescope (DeBruijnDecl ann)) instance PrettyWith 'Frontend (DeBruijnDecl ann)
deriving via (ViaNaiveDescope (DeBruijnExpr ann)) instance PrettyWith 'Frontend (DeBruijnExpr ann)


-- Optional simplification

instance (Simplify a, PrettyWith tags a)
      => PrettyWith ('Simple tags) a where
  prettyWith e = prettyWith (simplify e)

instance (Simplify a, PrettyWith tags a)
      => PrettyWith ('Simple ('With SimplifyOptions tags)) (SimplifyOptions, a) where
  prettyWith p = prettyWith (uncurry simplifyWith p)

-- etc...

{-
--------------------------------------------------------------------------------
-- Top-level interface for printing Vehicle expressions/programs

-- |Prints to the core language removing all implicit/instance arguments and
-- automatically inserted code. Does not convert DeBruijn indices back to names.
prettySimple :: (PrettyLang a, Simplify a) => a -> Doc b
prettySimple = prettyLang Core . runSimplify options
  where
    options = SimplifyOptions
      { removeImplicits   = True
      , removeInstances   = True
      , removeNonUserCode = True
      }

-- |Prints to the core language in all it's gory detail. Does not convert DeBruijn
-- indices back to names. Useful for debugging.
prettyVerbose :: (PrettyLang a, Simplify a) => a -> Doc b
prettyVerbose = prettyLang Core . runSimplify options
  where
    options = SimplifyOptions
      { removeImplicits   = False
      , removeInstances   = False
      , removeNonUserCode = False
      }

-- |Prints to the frontend language for things that need to be displayed to
-- the user. Use this when not using DeBruijn indices.
prettyFriendly :: (PrettyLang a, Simplify a) => a -> Doc b
prettyFriendly = prettyLang Frontend . runSimplify options
  where
    options = SimplifyOptions
      { removeImplicits   = False
      , removeInstances   = False
      , removeNonUserCode = False
      }

-- |Prints to the frontend language for things that need to be displayed to
-- the user. Use this when the expression is using DeBruijn indices
prettyFriendlyDB :: ( PrettyDescopedLang t binder var ann
                  , Simplify (t binder var ann))
                 => [binder]
                 -> t binder var ann
                 -> Doc b
prettyFriendlyDB ctx = prettyDescopeLang Frontend ctx . runSimplify options
  where
    options = SimplifyOptions
      { removeImplicits   = True
      , removeInstances   = True
      , removeNonUserCode = False
      }

-- |Use for closed terms using DeBruijn indices
prettyFriendlyDBClosed :: (PrettyDescopedLang t binder var ann
                        , Simplify (t binder var ann))
                     => t binder var ann
                     -> Doc b
prettyFriendlyDBClosed = prettyFriendlyDB mempty

--------------------------------------------------------------------------------
-- Printing when using names rather than DeBruijn indices

class PrettyLang a where
  prettyLang :: VehicleLang -> a -> Doc b

instance PrettyLang a => PrettyLang [a] where
  prettyLang target xs = list (fmap (prettyLang target) xs)

instance (Pretty a, PrettyLang b) => PrettyLang (a, b) where
  prettyLang target (x, y) = tupled [pretty x, prettyLang target y]

instance PrettyNamedLang t binder var ann => PrettyLang (t binder var ann) where
  prettyLang = prettyNamedLang

--------------------------------------------------------------------------------
-- Printing when using names

class PrettyNamedLang t binder var ann where
  prettyNamedLang :: VehicleLang -> t binder var ann -> Doc b

instance PrettyNamedLang Arg  Symbol Symbol ann where
  prettyNamedLang Core     e = pretty $ bnfcPrintHack $ Core.printTree (Core.runDelabWithoutLogging e :: BC.Arg)
  prettyNamedLang Frontend e = pretty $ bnfcPrintHack $ Frontend.printTree (Frontend.runDelabWithoutLogging e :: BF.Arg)

instance PrettyNamedLang Expr Symbol Symbol ann where
  prettyNamedLang Core     e = pretty $ bnfcPrintHack $ Core.printTree (Core.runDelabWithoutLogging e :: BC.Expr)
  prettyNamedLang Frontend e = pretty $ bnfcPrintHack $ Frontend.printTree (Frontend.runDelabWithoutLogging e :: BF.Expr)

instance PrettyNamedLang Decl Symbol Symbol ann where
  prettyNamedLang Core     e = pretty $ bnfcPrintHack $ Core.printTree (Core.runDelabWithoutLogging e :: BC.Decl)
  prettyNamedLang Frontend e = pretty $ bnfcPrintHack $ Frontend.printTree (Frontend.runDelabWithoutLogging e :: [BF.Decl])

instance PrettyNamedLang Prog Symbol Symbol ann where
  prettyNamedLang Core     e = pretty $ bnfcPrintHack $ Core.printTree (Core.runDelabWithoutLogging e :: BC.Prog)
  prettyNamedLang Frontend e = pretty $ bnfcPrintHack $ Frontend.printTree (Frontend.runDelabWithoutLogging e :: BF.Prog)

instance ( SupplyNames t
         , PrettyNamedLang t Symbol var ann
         ) => PrettyNamedLang t (Maybe Symbol) var ann where
  prettyNamedLang target e = prettyNamedLang target (runSupplyNames e)

-- A hack to that simply converts DeBruijn indices into their corresponding
-- counterpart, i.e. 0 -> "i0"
instance ( Descope t
         , PrettyNamedLang t Symbol Symbol ann
         ) => PrettyNamedLang t Symbol LocallyNamelessVar ann where
  prettyNamedLang target e = prettyLang target (runNaiveDescope e)

--------------------------------------------------------------------------------
-- Printing when using DeBruijn indices

class PrettyDescopedLang t binder var ann where
  prettyDescopeLang :: VehicleLang -> [binder] -> t binder var ann -> Doc b

-- DeBruijn indices with machine names
instance ( SupplyNames t
         , Descope t
         , PrettyNamedLang t Symbol Symbol ann )
         => PrettyDescopedLang t (Maybe Symbol) LocallyNamelessVar ann where
  prettyDescopeLang target ctx e = prettyDescopeLang target ctx' e'
    where (ctx', e') = runSupplyNamesWithCtx (ctx, e)

instance PrettyNamedLang t Symbol Symbol ann
      => PrettyDescopedLang t Symbol Symbol ann where
  prettyDescopeLang target _ctx = prettyNamedLang target

-- DeBruijn indices without machine names
instance (PrettyNamedLang t Symbol Symbol ann, Descope t)
      => PrettyDescopedLang t Symbol LocallyNamelessVar ann where
  prettyDescopeLang target ctx e = prettyDescopeLang target ctx (runDescope ctx e)

--------------------------------------------------------------------------------
-- Other

-- BNFC printer treats the braces for implicit arguments as layout braces and
-- therefore adds a ton of newlines everywhere. This hack attempts to undo this.
bnfcPrintHack :: String -> Text
bnfcPrintHack = pack
  -- replaceAll "\\{\\s*" "{" $
  -- replaceAll "\\s*\\}\\s*" "} " $

instance PrettyLang MetaSubstitution where
  prettyLang target (MetaSubstitution m) = pretty $ fmap (prettyLang target) m

instance PrettyLang BaseConstraint where
  prettyLang target (Unify (e1, e2)) = prettyLang target e1 <+> "~" <+> prettyLang target e2
  prettyLang target (m `Has` e)      = pretty m <+> "~" <+> prettyLang target e

instance PrettyLang Constraint where
  prettyLang target (Constraint _ c) = prettyLang target c

-- -}