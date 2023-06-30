module Vehicle.Backend.JSON
  ( compileProgToJSON,
  )
where

import Data.Aeson (Options (..), ToJSON (..), defaultOptions, genericToJSON)
import Data.Aeson.Encode.Pretty (encodePretty')
import Data.ByteString.Lazy.Char8 (unpack)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (mapMaybe)
import GHC.Generics (Generic)
import Vehicle.Compile.Error (MonadCompile, compilerDeveloperError, resolutionError)
import Vehicle.Compile.FunctionaliseResources (functionaliseResources)
import Vehicle.Compile.Prelude (DefAbstractSort (..), Doc, getExplicitArg, pretty, prettyJSONConfig, quotePretty, squotes, (<+>))
import Vehicle.Syntax.AST (BinderDisplayForm, Identifier, Name, Provenance, UniverseLevel, isExplicit)
import Vehicle.Syntax.AST qualified as V (Binder, Decl, Expr (..), GenericBinder (..), GenericDecl (..), GenericProg (..), Prog)

--------------------------------------------------------------------------------
-- Public method

compileProgToJSON ::
  (MonadCompile m, ToJSON builtin) =>
  V.Prog Name builtin ->
  m (Doc a)
compileProgToJSON prog = do
  functionalisedProg <- functionaliseResources prog
  jProg <- toJSON <$> toJProg functionalisedProg
  return $ pretty $ unpack $ encodePretty' prettyJSONConfig jProg

--------------------------------------------------------------------------------
-- Datatype

-- We have a separate datatype for JSON expressions so we
-- can maintain backwards compatability, even when the core
-- Vehicle AST changes.

newtype JProg builtin
  = Main [JDecl builtin]
  deriving (Generic)

data JDecl builtin
  = DefPostulate Provenance Identifier (JExpr builtin)
  | DefFunction Provenance Identifier (JExpr builtin) (JExpr builtin)
  deriving (Generic)

data JExpr builtin
  = Universe Provenance UniverseLevel
  | UnsafeApp Provenance (JExpr builtin) (NonEmpty (JExpr builtin))
  | Pi Provenance (JBinder builtin) (JExpr builtin)
  | Builtin Provenance builtin
  | BoundVar Provenance Name
  | FreeVar Provenance Identifier
  | Let Provenance (JExpr builtin) (JBinder builtin) (JExpr builtin)
  | Lam Provenance (JBinder builtin) (JExpr builtin)
  deriving (Generic)

data JBinder builtin = JBinder
  { binderProvenance :: Provenance,
    binderDisplayForm :: BinderDisplayForm,
    binderType :: JExpr builtin
  }
  deriving (Generic)

--------------------------------------------------------------------------------
-- Conversion of JExpr to JSON

jsonOptions :: Options
jsonOptions =
  defaultOptions
    { tagSingleConstructors = True
    }

instance (ToJSON builtin) => ToJSON (JProg builtin) where
  toJSON = genericToJSON jsonOptions

instance (ToJSON builtin) => ToJSON (JDecl builtin) where
  toJSON = genericToJSON jsonOptions

instance (ToJSON builtin) => ToJSON (JExpr builtin) where
  toJSON = genericToJSON jsonOptions

instance (ToJSON builtin) => ToJSON (JBinder builtin) where
  toJSON = genericToJSON jsonOptions

--------------------------------------------------------------------------------
-- Conversion Expr to JExpr

currentPass :: Doc a
currentPass = "conversion to JSON"

toJProg :: (MonadCompile m) => V.Prog Name builtin -> m (JProg builtin)
toJProg (V.Main ds) = Main <$> traverse toJDecl ds

toJDecl :: (MonadCompile m) => V.Decl Name builtin -> m (JDecl builtin)
toJDecl d = case d of
  V.DefAbstract p i s t -> do
    case s of
      NetworkDef -> resourceError s
      DatasetDef -> resourceError s
      ParameterDef {} -> resourceError s
      PostulateDef -> DefPostulate p i <$> toJExpr t
  V.DefFunction p i _anns t e -> DefFunction p i <$> toJExpr t <*> toJExpr e

toJExpr :: (MonadCompile m) => V.Expr Name builtin -> m (JExpr builtin)
toJExpr = \case
  V.Hole {} -> resolutionError currentPass "Hole"
  V.Meta {} -> resolutionError currentPass "Meta"
  V.Ann {} -> resolutionError currentPass "Ann"
  V.Universe p l -> return $ Universe p l
  V.Builtin p b -> return $ Builtin p b
  V.BoundVar p v -> return $ BoundVar p v
  V.FreeVar p v -> return $ FreeVar p v
  V.App p fun args -> do
    fun' <- toJExpr fun
    args' <- traverse toJExpr (mapMaybe getExplicitArg (NonEmpty.toList args))
    return $ case args' of
      [] -> fun'
      (a : as) -> UnsafeApp p fun' (a :| as)
  V.Pi p binder body
    | isExplicit binder -> Pi p <$> toJBinder binder <*> toJExpr body
    | otherwise -> toJExpr body
  V.Lam p binder body
    | isExplicit binder -> Lam p <$> toJBinder binder <*> toJExpr body
    | otherwise -> toJExpr body
  V.Let p bound binder body ->
    Let p <$> toJExpr bound <*> toJBinder binder <*> toJExpr body

toJBinder :: (MonadCompile m) => V.Binder Name builtin -> m (JBinder builtin)
toJBinder binder = do
  type' <- toJExpr $ V.binderType binder
  return $
    JBinder
      { binderProvenance = V.binderProvenance binder,
        binderDisplayForm = V.binderDisplayForm binder,
        binderType = type'
      }

resourceError :: (MonadCompile m) => DefAbstractSort -> m a
resourceError resourceType =
  compilerDeveloperError $
    "All"
      <+> quotePretty resourceType
      <+> "declarations should have been removed before"
      <+> squotes currentPass
