{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Language.Print
  ( PrettyLang(..)
  , PrettyDescopedLang(..)
  , prettySimple
  , prettyVerbose
  , prettyFriendly
  , prettyFriendlyDB
  , prettyFriendlyDBClosed
  ) where

import Prettyprinter (list, tupled)

import Vehicle.Core.Print as Core (printTree)
import Vehicle.Frontend.Print as Frontend (printTree)
import Vehicle.Core.Abs qualified as BC
import Vehicle.Frontend.Abs qualified as BF

import Vehicle.Prelude
import Vehicle.Language.Simplify
import Vehicle.Language.Delaborate.Core as Core
import Vehicle.Language.Delaborate.Frontend as Frontend
import Vehicle.Language.Descope
import Vehicle.Language.SupplyNames (SupplyNames, runSupplyNamesWithCtx, runSupplyNames)
import Vehicle.Language.AST

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
      { removeImplicits   = True
      , removeInstances   = True
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
  prettyNamedLang Core     e = pretty $ Core.printTree (Core.runDelabWithoutLogging e :: BC.Arg)
  prettyNamedLang Frontend e = pretty $ Frontend.printTree (Frontend.runDelabWithoutLogging e :: BF.Arg)

instance PrettyNamedLang Expr Symbol Symbol ann where
  prettyNamedLang Core     e = pretty $ Core.printTree (Core.runDelabWithoutLogging e :: BC.Expr)
  prettyNamedLang Frontend e = pretty $ Frontend.printTree (Frontend.runDelabWithoutLogging e :: BF.Expr)

instance PrettyNamedLang Decl Symbol Symbol ann where
  prettyNamedLang Core     e = pretty $ Core.printTree (Core.runDelabWithoutLogging e :: BC.Decl)
  prettyNamedLang Frontend e = pretty $ Frontend.printTree (Frontend.runDelabWithoutLogging e :: [BF.Decl])

instance PrettyNamedLang Prog Symbol Symbol ann where
  prettyNamedLang Core     e = pretty $ Core.printTree (Core.runDelabWithoutLogging e :: BC.Prog)
  prettyNamedLang Frontend e = pretty $ Frontend.printTree (Frontend.runDelabWithoutLogging e :: BF.Prog)

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