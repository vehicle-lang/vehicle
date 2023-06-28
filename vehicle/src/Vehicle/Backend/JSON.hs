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
import Vehicle.Compile.Error (MonadCompile, resolutionError)
import Vehicle.Compile.Prelude (DefAbstractSort, Doc, GenericProg (..), getExplicitArg, pretty, prettyJSONConfig)
import Vehicle.Syntax.AST (BinderDisplayForm, Identifier, Name, Provenance, UniverseLevel, isExplicit)
import Vehicle.Syntax.AST qualified as V (Binder, Decl, Expr (..), GenericBinder (..), GenericDecl (..), Prog)

--------------------------------------------------------------------------------
-- Public method

compileProgToJSON ::
  (MonadCompile m, ToJSON builtin) =>
  V.Prog Name builtin ->
  m (Doc a)
compileProgToJSON (Main prog) = do
  decls <- toJSON <$> traverse toJDecl prog
  return $ pretty $ unpack $ encodePretty' prettyJSONConfig decls

--------------------------------------------------------------------------------
-- Datatype

-- We have a separate datatype for JSON expressions so we
-- can maintain backwards compatability, even when the core
-- Vehicle AST changes.

data JDecl builtin
  = DefAbstract Provenance Identifier DefAbstractSort (JExpr builtin)
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

toJDecl :: (MonadCompile m) => V.Decl Name builtin -> m (JDecl builtin)
toJDecl d = case d of
  V.DefAbstract p i s t -> DefAbstract p i s <$> toJExpr t
  V.DefFunction p i _anns t e -> DefFunction p i <$> toJExpr t <*> toJExpr e

toJExpr :: (MonadCompile m) => V.Expr Name builtin -> m (JExpr builtin)
toJExpr = \case
  V.Hole {} -> resolutionError currentPass "Hole"
  V.Meta {} -> resolutionError currentPass "Meta"
  V.Ann {} -> resolutionError currentPass "Ann"
  V.Universe p l -> return $ Universe p l
  V.Builtin p b -> return $ Builtin p b
  V.BoundVar p v -> return $ BoundVar p v
  V.FreeVar p b -> return $ FreeVar p b
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
