{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Language.Print
  ( PrettyLang(..)
  , prettySimple
  , prettyVerbose
  , prettyFriendly
  , prettyFriendlyTopLevel
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
import Vehicle.Language.Compile.Descope
import Vehicle.Language.AST

--------------------------------------------------------------------------------
-- Top-level interface for printing Vehicle expressions/programs

type Ctx = [Name]

class PrettyLang a where
  prettyCore :: a -> Doc b
  prettyFrontend :: Ctx -> a -> Doc b

-- |Prints to the core language removing all implicit/instance arguments and
-- automatically inserted code. Does not convert DeBruijn indices back to names.
prettySimple :: (PrettyLang a, Simplify a) => a -> Doc b
prettySimple x = prettyCore $ runSimplify x (SimplifyOptions
  { removeImplicits   = True
  , removeInstances   = True
  , removeNonUserCode = True
  })

-- |Prints to the core language in all it's gory detail. Does not convert DeBruijn
-- indices back to names. Useful for debugging.
prettyVerbose :: (PrettyLang a, Simplify a) => a -> Doc b
prettyVerbose x = prettyCore $ runSimplify x (SimplifyOptions
  { removeImplicits   = False
  , removeInstances   = False
  , removeNonUserCode = False
  })

-- |Prints to the frontend language for things that need to be displayed to
-- the user.
prettyFriendly :: (PrettyLang a, Simplify a, IsBoundCtx ctx) => ctx -> a -> Doc b
prettyFriendly ctx x = prettyFrontend (ctxNames ctx) $ runSimplify x (SimplifyOptions
  { removeImplicits   = False
  , removeInstances   = False
  , removeNonUserCode = True
  })


-- |Prints to the frontend language for things that need to be displayed to
-- the user. Only for use at the top-level!
prettyFriendlyTopLevel :: (PrettyLang a, Simplify a) => a -> Doc b
prettyFriendlyTopLevel = prettyFriendly (mempty :: [Name])

--------------------------------------------------------------------------------
-- Printing when using names rather than DeBruijn indices

instance PrettyLang (Arg Name ann) where
  prettyCore          e = pretty $ Core.printTree (Core.runDelabWithoutLogging e :: BC.Arg)
  prettyFrontend _ctx e = pretty $ Frontend.printTree (Frontend.runDelabWithoutLogging e :: BF.Arg)

instance PrettyLang (Expr Name ann) where
  prettyCore          e = pretty $ Core.printTree (Core.runDelabWithoutLogging e :: BC.Expr)
  prettyFrontend _ctx e = pretty $ Frontend.printTree (Frontend.runDelabWithoutLogging e :: BF.Expr)

instance PrettyLang (Decl Name ann) where
  prettyCore          e = pretty $ Core.printTree (Core.runDelabWithoutLogging e :: BC.Decl)
  prettyFrontend _ctx e = pretty $ Frontend.printTree (Frontend.runDelabWithoutLogging e :: [BF.Decl])

instance PrettyLang (Prog Name ann) where
  prettyCore          e = pretty $ Core.printTree (Core.runDelabWithoutLogging e :: BC.Prog)
  prettyFrontend _ctx e = pretty $ Frontend.printTree (Frontend.runDelabWithoutLogging e :: BF.Prog)

--------------------------------------------------------------------------------
-- Printing when using DeBruijn indices rather than names

instance PrettyLang (Arg Var ann) where
  prettyCore         e = prettyCore         (runNaiveDescope e :: Arg Name ann)
  prettyFrontend ctx e = prettyFrontend ctx (runDescope ctx e  :: Arg Name ann)

instance PrettyLang (Expr Var ann) where
  prettyCore         e = prettyCore         (runNaiveDescope e :: Expr Name ann)
  prettyFrontend ctx e = prettyFrontend ctx (runDescope ctx e  :: Expr Name ann)

instance PrettyLang (Decl Var ann) where
  prettyCore         e = prettyCore         (runNaiveDescope e :: Decl Name ann)
  prettyFrontend ctx e = prettyFrontend ctx (runDescope ctx e  :: Decl Name ann)

instance PrettyLang (Prog Var ann) where
  prettyCore         e = prettyCore         (runNaiveDescope e :: Prog Name ann)
  prettyFrontend ctx e = prettyFrontend ctx (runDescope ctx e  :: Prog Name ann)

--------------------------------------------------------------------------------
-- Other

instance PrettyLang a => PrettyLang [a] where
  prettyCore xs = list (fmap prettyCore xs)
  prettyFrontend ctx xs = list (map (prettyFrontend ctx) xs)

instance (Pretty a, PrettyLang b) => PrettyLang (a, b) where
  prettyCore         (x, y) = tupled [pretty x, prettyCore y]
  prettyFrontend ctx (x, y) = tupled [pretty x, prettyFrontend ctx y]