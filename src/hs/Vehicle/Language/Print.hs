{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Language.Print
  ( PrettyLang(..)
  , PrettyDescopedLang(..)
  , prettySimple
  , prettyVerbose
  , prettyFriendly
  , prettyFriendlyDescope
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
import Vehicle.Language.AST

--------------------------------------------------------------------------------
-- Top-level interface for printing Vehicle expressions/programs

-- |Prints to the core language removing all implicit/instance arguments and
-- automatically inserted code. Does not convert DeBruijn indices back to names.
prettySimple :: (PrettyLang a, Simplify a) => a -> Doc b
prettySimple = prettyLang Core . runSimplify (SimplifyOptions
  { removeImplicits   = True
  , removeInstances   = True
  , removeNonUserCode = True
  })

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
      , removeNonUserCode = True
      }

-- |Prints to the frontend language for things that need to be displayed to
-- the user. Use this when the expression is using DeBruijn indices
prettyFriendlyDescope :: (PrettyDescopedLang a, Simplify a, IsBoundCtx ctx) => ctx -> a -> Doc b
prettyFriendlyDescope ctx = prettyDescopedLang Frontend (ctxNames ctx) . runSimplify options
  where
    options = SimplifyOptions
      { removeImplicits   = False
      , removeInstances   = False
      , removeNonUserCode = True
      }

--------------------------------------------------------------------------------
-- Printing when using names rather than DeBruijn indices

class PrettyLang a where
  prettyLang :: VehicleLang -> a -> Doc b

instance PrettyLang (Arg Name ann) where
  prettyLang Core     e = pretty $ Core.printTree (Core.runDelabWithoutLogging e :: BC.Arg)
  prettyLang Frontend e = pretty $ Frontend.printTree (Frontend.runDelabWithoutLogging e :: BF.Arg)

instance PrettyLang (Expr Name ann) where
  prettyLang Core     e = pretty $ Core.printTree (Core.runDelabWithoutLogging e :: BC.Expr)
  prettyLang Frontend e = pretty $ Frontend.printTree (Frontend.runDelabWithoutLogging e :: BF.Expr)

instance PrettyLang (Decl Name ann) where
  prettyLang Core     e = pretty $ Core.printTree (Core.runDelabWithoutLogging e :: BC.Decl)
  prettyLang Frontend e = pretty $ Frontend.printTree (Frontend.runDelabWithoutLogging e :: [BF.Decl])

instance PrettyLang (Prog Name ann) where
  prettyLang Core     e = pretty $ Core.printTree (Core.runDelabWithoutLogging e :: BC.Prog)
  prettyLang Frontend e = pretty $ Frontend.printTree (Frontend.runDelabWithoutLogging e :: BF.Prog)

instance PrettyLang a => PrettyLang [a] where
  prettyLang target xs = list (fmap (prettyLang target) xs)

instance (Pretty a, PrettyLang b) => PrettyLang (a, b) where
  prettyLang target (x, y) = tupled [pretty x, prettyLang target y]

-- These just naively translate DeBruijn indices to the corresponding strings, i.e. 0 -> "i0"
instance PrettyLang (Arg Var ann) where
  prettyLang target e = prettyLang target (runNaiveDescope e :: Arg Name ann)

instance PrettyLang (Expr Var ann) where
  prettyLang target e = prettyLang target (runNaiveDescope e :: Expr Name ann)

instance PrettyLang (Decl Var ann) where
  prettyLang target e = prettyLang target (runNaiveDescope e :: Decl Name ann)

instance PrettyLang (Prog Var ann) where
  prettyLang target e = prettyLang target (runNaiveDescope e :: Prog Name ann)

--------------------------------------------------------------------------------
-- Printing when using DeBruijn indices

type Ctx = [Name]

class PrettyDescopedLang a where
  prettyDescopedLang :: VehicleLang -> Ctx -> a -> Doc b

instance PrettyDescopedLang (Arg Var ann) where
  prettyDescopedLang target ctx e = prettyLang target (runDescope ctx e :: Arg Name ann)

instance PrettyDescopedLang (Expr Var ann) where
  prettyDescopedLang target ctx e = prettyLang target (runDescope ctx e :: Expr Name ann)

instance PrettyDescopedLang (Decl Var ann) where
  prettyDescopedLang target ctx e = prettyLang target (runDescope ctx e :: Decl Name ann)

instance PrettyDescopedLang (Prog Var ann) where
  prettyDescopedLang target ctx e = prettyLang target (runDescope ctx e :: Prog Name ann)