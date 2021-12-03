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
import Data.Text (Text)
import Data.Text qualified as Text
import Prettyprinter (list, tupled)

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
prettyFriendly :: (PrettyWith ('Named ('As 'Frontend)) a) => a -> Doc b
prettyFriendly = prettyWith @('Named ('As 'Frontend))

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
  pretty (ViaBnfcCore e) = pretty $ bnfcPrintHack (Core.printTree e)

deriving via (ViaBnfcCore BC.Prog) instance Pretty BC.Prog
deriving via (ViaBnfcCore BC.Decl) instance Pretty BC.Decl
deriving via (ViaBnfcCore BC.Expr) instance Pretty BC.Expr

newtype ViaBnfcFrontend a = ViaBnfcFrontend a

instance Frontend.Print a => Pretty (ViaBnfcFrontend a) where
  pretty (ViaBnfcFrontend e) = pretty $ bnfcPrintHack (Frontend.printTree e)

deriving via (ViaBnfcFrontend BF.Prog) instance Pretty BF.Prog
deriving via (ViaBnfcFrontend BF.Decl) instance Pretty BF.Decl
deriving via (ViaBnfcFrontend BF.Expr) instance Pretty BF.Expr

-- BNFC printer treats the braces for implicit arguments as layout braces and
-- therefore adds a ton of newlines everywhere. This hack attempts to undo this.
bnfcPrintHack :: String -> Text
bnfcPrintHack =
  Text.replace "{\n" "{" .
  Text.replace "\n}\n" "} " .
  Text.pack