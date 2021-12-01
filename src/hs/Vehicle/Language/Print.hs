{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Vehicle.Language.Print
  ( prettySimple
  , prettyVerbose
  , prettyFriendly
  , prettyFriendlyDB
  , prettyFriendlyDBClosed
  ) where

import GHC.TypeLits (TypeError, ErrorMessage(..))

import Vehicle.Core.Print as Core (printTree, Print)
import Vehicle.Frontend.Print as Frontend (printTree, Print)
import Vehicle.Core.Abs qualified as BC
import Vehicle.Frontend.Abs qualified as BF

import Vehicle.Prelude
import Vehicle.Language.AST
import Vehicle.Compile.Simplify
import Vehicle.Compile.Delaborate.Core as Core
import Vehicle.Compile.Delaborate.Frontend as Frontend
import Vehicle.Compile.Descope
import Vehicle.Compile.SupplyNames (SupplyNames, runSupplyNamesWithCtx, runSupplyNames)
import Prettyprinter (list, tupled)
import Vehicle.Compile.Type.Constraint (Constraint (..), BaseConstraint (..))
import Vehicle.Compile.Type.MetaSubstitution (MetaSubstitution(MetaSubstitution))


-- The old methods for compatibility:

-- |Prints to the core language removing all implicit/instance arguments and
-- automatically inserted code. Does not convert DeBruijn indices back to names.
prettySimple :: (PrettyWith ('Simple ('As 'Core)) a) => a -> Doc b
prettySimple = prettyWith @('Simple ('As 'Core))

-- |Prints to the core language in all it's gory detail. Does not convert DeBruijn
-- indices back to names. Useful for debugging.
prettyVerbose :: (PrettyWith ('As 'Core) a) => a -> Doc b
prettyVerbose = prettyWith @('As 'Core)

-- |Prints to the frontend language for things that need to be displayed to
-- the user.
prettyFriendly :: (PrettyWith ('Simple ('Named ('As 'Frontend))) a) => a -> Doc b
prettyFriendly = prettyWith @('Simple ('Named ('As 'Frontend)))

-- |Prints to the frontend language for things that need to be displayed to
-- the user. Use this when the expression is using DeBruijn indices and is
-- not closed.
prettyFriendlyDB
    :: (PrettyWith ('Named ('As 'Frontend)) ([Symbol], t Symbol var ann), Simplify (t Symbol var ann), SupplyNames t)
    => [Maybe Symbol] -> t (Maybe Symbol) var ann -> Doc b
prettyFriendlyDB ctx e = prettyWith @('Simple ('Named ('As 'Frontend))) (ctx, e)

-- | This is identical to |prettyFriendly|, but exists for historical reasons.
prettyFriendlyDBClosed :: (PrettyWith ('Simple ('Named ('As 'Frontend))) a) => a -> Doc b
prettyFriendlyDBClosed = prettyWith @('Simple ('Named ('As 'Frontend)))



-- The new methods are defined in terms of tags:

type PrettyWith (tags :: Tags) a = PrettyUsing (StrategyFor tags a) a

data Tags
  = As VehicleLang -- ^ The final tag denotes which output grammar should be used
  | Named Tags     -- ^ The named tag ensures that the term is converted back to using named binders
  | Simple Tags    -- ^ The simple tag ensures that superfluous information is erased

prettyWith :: forall tags a b. PrettyWith tags a => a -> Doc b
prettyWith = prettyUsing @(StrategyFor tags a) @a @b


-- Tags are used to compute a printing strategy:
data Strategy
  = ConvertTo VehicleLang
  | DescopeNaive Strategy
  | DescopeOpen Strategy
  | DescopeClosed Strategy
  | SupplyNamesOpen Strategy
  | SupplyNamesClosed Strategy
  | SimplifyWithOptions Strategy
  | SimplifyDefault Strategy
  | MapList Strategy
  | MapTuple Strategy Strategy
  | Opaque Strategy
  | Pretty

-- | Compute the printing strategy given the tags and the type of the expression.
type family StrategyFor (tags :: Tags) a :: Strategy where
  StrategyFor ('As lang)     (t Symbol Symbol ann)                       = 'ConvertTo lang
  StrategyFor ('As lang)     (t Symbol LocallyNamelessVar ann)           = 'DescopeNaive ('ConvertTo lang)
  StrategyFor ('Named tags)  ([Symbol], t Symbol LocallyNamelessVar ann) = 'DescopeOpen (StrategyFor tags (t Symbol Symbol ann))
  StrategyFor ('Named tags)  (t Symbol LocallyNamelessVar ann)           = 'DescopeClosed (StrategyFor tags (t Symbol Symbol ann))
  StrategyFor ('Named tags)  (t Symbol Symbol ann)                       = StrategyFor tags (t Symbol Symbol ann)
  StrategyFor tags           ([Maybe Symbol], t (Maybe Symbol) var ann)  = 'SupplyNamesOpen (StrategyFor tags ([Symbol], t Symbol var ann))
  StrategyFor tags           (t (Maybe Symbol) var ann)                  = 'SupplyNamesClosed (StrategyFor tags (t Symbol var ann))
  StrategyFor ('Simple tags) (SimplifyOptions, a)                        = 'SimplifyWithOptions (StrategyFor tags a)
  StrategyFor ('Simple tags) a                                           = 'SimplifyDefault (StrategyFor tags a)
  StrategyFor tags           Constraint                                  = 'Opaque (StrategyFor tags BaseConstraint)
  StrategyFor tags           BaseConstraint                              = 'Opaque (StrategyFor tags CheckedExpr)
  StrategyFor tags           MetaSubstitution                            = 'Opaque (StrategyFor tags CheckedExpr)
  StrategyFor tags           (Maybe Symbol)                              = 'Pretty
  StrategyFor tags           [a]                                         = 'MapList (StrategyFor tags a)
  StrategyFor tags           (a, b)                                      = 'MapTuple (StrategyFor tags a) (StrategyFor tags b)
  StrategyFor tags           a                                           = TypeError ('Text "Cannot print value of type " ':<>: 'ShowType a ':<>: 'Text "."
                                                                                ':$$: 'Text "Perhaps you could add support to Vehicle.Language.Print.StrategyFor?")


-- The printing strategy guides the type class resolution:

class PrettyUsing (strategy :: Strategy) a where
  prettyUsing :: a -> Doc b


-- The use of deriving via is necessary to inform the type class resolution of the intermediate BNFC type:

newtype ViaDelaborate a b = ViaDelaborate a

instance (Core.Delaborate a b, Pretty b)
      => PrettyUsing ('ConvertTo 'Core) (ViaDelaborate a b) where
  prettyUsing (ViaDelaborate e) = pretty (Core.runDelabWithoutLogging @a @b e)

instance (Frontend.Delaborate a b, Pretty b)
      => PrettyUsing ('ConvertTo 'Frontend) (ViaDelaborate a b) where
  prettyUsing (ViaDelaborate e) = pretty (Frontend.runDelabWithoutLogging @a @b e)

deriving via (ViaDelaborate (NamedProg ann) BC.Prog) instance PrettyUsing ('ConvertTo 'Core) (NamedProg ann)
deriving via (ViaDelaborate (NamedDecl ann) BC.Decl) instance PrettyUsing ('ConvertTo 'Core) (NamedDecl ann)
deriving via (ViaDelaborate (NamedExpr ann) BC.Expr) instance PrettyUsing ('ConvertTo 'Core) (NamedExpr ann)

deriving via (ViaDelaborate (NamedProg ann)  BF.Prog ) instance PrettyUsing ('ConvertTo 'Frontend) (NamedProg ann)
deriving via (ViaDelaborate (NamedDecl ann) [BF.Decl]) instance PrettyUsing ('ConvertTo 'Frontend) (NamedDecl ann)
deriving via (ViaDelaborate (NamedExpr ann)  BF.Expr ) instance PrettyUsing ('ConvertTo 'Frontend) (NamedExpr ann)

instance (Descope t, PrettyUsing rest (t Symbol Symbol ann))
      => PrettyUsing ('DescopeNaive rest) (t Symbol LocallyNamelessVar ann) where
  prettyUsing e = prettyUsing @rest (runNaiveDescope e)

instance (Descope t, PrettyUsing rest (t Symbol Symbol ann))
      => PrettyUsing ('DescopeOpen rest) ([Symbol], t Symbol LocallyNamelessVar ann) where
  prettyUsing (ctx, e) = prettyUsing @rest (runDescope ctx e)

instance (Descope t, PrettyUsing rest (t Symbol Symbol ann))
      => PrettyUsing ('DescopeClosed rest) (t Symbol LocallyNamelessVar ann) where
  prettyUsing e = prettyUsing @rest (runDescope mempty e)

instance (SupplyNames t, PrettyUsing rest ([Symbol], t Symbol var ann))
      => PrettyUsing ('SupplyNamesOpen rest) ([Maybe Symbol], t (Maybe Symbol) var ann) where
  prettyUsing p = prettyUsing @rest (runSupplyNamesWithCtx p)

instance (SupplyNames t, PrettyUsing rest (t Symbol var ann))
      => PrettyUsing ('SupplyNamesClosed rest) (t (Maybe Symbol) var ann) where
  prettyUsing e = prettyUsing @rest (runSupplyNames e)

instance (Simplify a, PrettyUsing rest a)
      => PrettyUsing ('SimplifyWithOptions rest) (SimplifyOptions, a) where
  prettyUsing (options, e) = prettyUsing @rest (simplifyWith options e)

instance (Simplify a, PrettyUsing rest a)
      => PrettyUsing ('SimplifyDefault rest) a where
  prettyUsing e = prettyUsing @rest (simplify e)

instance PrettyUsing rest a
      => PrettyUsing ('MapList rest) [a] where
  prettyUsing es = list (prettyUsing @rest <$> es)

instance (PrettyUsing resta a, PrettyUsing restb b)
      => PrettyUsing ('MapTuple resta restb) (a, b) where
  prettyUsing (e1, e2) = tupled [prettyUsing @resta e1, prettyUsing @restb e2]

-- instances which defer to primitive pretty instances

instance Pretty a => PrettyUsing 'Pretty a where
  prettyUsing = pretty

-- instances for opaque types BaseConstraint, Constraint, and MetaSubstitution

instance PrettyUsing rest CheckedExpr
      => PrettyUsing ('Opaque rest) BaseConstraint where
  prettyUsing (Unify (e1, e2)) = prettyUsing @rest e1 <+> "~" <+> prettyUsing @rest e2
  prettyUsing (m `Has` e)      = pretty m <+> "~" <+> prettyUsing @rest e

instance PrettyUsing rest BaseConstraint
      => PrettyUsing ('Opaque rest) Constraint where
  prettyUsing (Constraint _ c) = prettyUsing @rest c

instance PrettyUsing rest CheckedExpr
      => PrettyUsing ('Opaque rest) MetaSubstitution where
  prettyUsing (MetaSubstitution m) = pretty (prettyUsing @rest <$> m)

-- Pretty instances for the BNFC data types

newtype ViaBnfcCore a = ViaBnfcCore a

instance Core.Print a => Pretty (ViaBnfcCore a) where
  pretty (ViaBnfcCore e) = pretty (Core.printTree e)

deriving via (ViaBnfcCore BC.Prog) instance Pretty BC.Prog
deriving via (ViaBnfcCore BC.Decl) instance Pretty BC.Decl
deriving via (ViaBnfcCore BC.Expr) instance Pretty BC.Expr

newtype ViaBnfcFrontend a = ViaBnfcFrontend a

instance Frontend.Print a => Pretty (ViaBnfcFrontend a) where
  pretty (ViaBnfcFrontend e) = pretty (Frontend.printTree e)

deriving via (ViaBnfcFrontend BF.Prog) instance Pretty BF.Prog
deriving via (ViaBnfcFrontend BF.Decl) instance Pretty BF.Decl
deriving via (ViaBnfcFrontend BF.Expr) instance Pretty BF.Expr

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
-- -}