
module Vehicle.Backend.Verifier.SMTLib where

import Control.Monad.Writer (MonadWriter(..))
import Control.Monad.Except (MonadError(..))
import Control.Monad.Reader (MonadReader(..))
import Data.List.NonEmpty (NonEmpty(..))

import Vehicle.Prelude
import Vehicle.Core.AST

unexpectedExprError :: Doc a -> Doc a
unexpectedExprError name =
  "encountered unexpected expression" <+> squotes name <+> "during compilation."

typeError :: Doc a -> b
typeError name = developerError $
  unexpectedExprError name <+> "We should not be compiling types."

visibilityError :: Doc a -> b
visibilityError name = developerError $
  unexpectedExprError name <+> "Should not be present as explicit arguments"

resolutionError :: Doc a -> b
resolutionError name = developerError $
  unexpectedExprError name <+> "We should have resolved this during type-checking."

normalisationError :: Doc a -> b
normalisationError name = developerError $
  unexpectedExprError name <+> "We should have normalised this out."

data SMTLibError

type DeclCtx = [(DeclType, WithProvenance Identifier)]
type BoundCtx = [Name]

type MonadSMTLib m =
  ( MonadLogger m
  , MonadError SMTLibError m
  , MonadReader (BoundCtx, DeclCtx)  m -- The current context
  , MonadWriter [Symbol] m             -- The list of top-level constants (from `Exists` statements)
  )

class CompileSMTLib a where
  compile :: MonadSMTLib m => a -> m (Doc b)

instance CompileSMTLib CheckedProg where
  compile (Main ds) = compile ds

instance CompileSMTLib [CheckedDecl] where
  compile []       = return ""
  compile (d : ds) = do
    dDoc  <- case d of
      DefFun _ _ (Builtin _ Prop) e -> do eDoc <- compile e; return $ eDoc <> line <> line
      _                             -> return ""
    dsDoc <- local (\ctx -> getDeclCtxElement d : ctx) (compile ds)

    return $ dDoc <> dsDoc

getDeclCtxElement :: CheckedDecl -> (DeclType, WithProvenance Identifier)
getDeclCtxElement = \case
  DeclNetw _ ident _   -> (Network,  ident)
  DeclData _ ident _   -> (Dataset,  ident)
  DefFun   _ ident _ _ -> (Function, ident)

instance CompileSMTLib CheckedExpr where
  compile = \case
    Type _         -> typeError "Type"
    Pi   _ann _ _  -> typeError "Pi"
    Hole _p _      -> resolutionError "Hole"
    Meta _p _      -> resolutionError "Meta"
    Ann _ann _ _   -> normalisationError "Ann"
    Let _ann _ _ _ -> normalisationError "Let"
    Lam _ann _ _   -> normalisationError "Lam"
    Seq _ann _     -> normalisationError "Seq"
    PrimDict _tc   -> visibilityError "PrimDict"

    Builtin _ann op -> compile op
    Literal _ann l  -> return $ pretty l
    Var     _ann v  -> return $ pretty v

    App _ann fun args -> do
      funDoc  <- compile fun
      argDocs <- compile args
      return $ parens (funDoc <+> hsep argDocs)

instance CompileSMTLib (NonEmpty CheckedArg) where
  compile args = _


instance CompileSMTLib CheckedVar where
  compile (Bound _) = _
  compile (Free  _) = _

{-
--instance PrettyWithConfig a => Pretty a where
--  pretty x = runReader (compile x) False

instance CompileSMTLib Name where
  compile Machine       = developerError "Should not be translating machine names when compiling to SMTLib"
  compile (User symbol) = pretty symbol

instance Pretty (WithProvenance Identifier) where
  pretty (WithProvenance _ann n) = pretty n

instance Pretty Literal where
  pretty = \case
    LNat  x -> pretty x
    LInt  x -> pretty x
    LRat  x -> pretty x
    LBool x -> pretty x

instance Pretty Meta where
  pretty (MetaVar v) = "?" <> pretty v

instance Pretty Builtin where
  pretty b = pretty $ fromMaybe "" (symbolFromBuiltin b)

instance Pretty var => PrettyWithConfig (Arg var ann) where
  compile (Arg _p v expr) = brackets v <$> compile expr

instance Pretty var => PrettyWithConfig (Binder var ann) where
  compile (Binder _ann v n t) = do
    tDoc <- compile t
    return $ brackets v (pretty n <+> ":type" <+> tDoc)

prettyArgs :: (Pretty var, MonadPrettyConfig m)
           => NonEmpty (Arg var ann)
           -> m [Doc a]
prettyArgs args = catMaybes <$> traverse prettyArg (NonEmpty.toList args)
  where
    prettyArg :: (Pretty var, MonadPrettyConfig m)
              => Arg var ann
              -> m (Maybe (Doc a))
    prettyArg arg = do
      PrettyOptions{..} <- ask
      argDoc <- compile arg
      let v = vis arg
      let showArg = v == Explicit
                 || v == Implicit && showImplicitArgs
                 || v == Instance && showInstanceArgs
      return $ if showArg
        then Just argDoc
        else Nothing

instance Pretty var => PrettyWithConfig (Expr var ann) where


instance PrettyWithConfig MetaSubstitution where
  compile m = prettyMapList (MetaSubst.toAscList m)

instance PrettyWithConfig MetaSet where
  compile m = return $
    encloseSep lbrace rbrace comma (fmap pretty (MetaSet.toList m))

--------------------------------------------------------------------------------
-- Derived instances

instance (Pretty a, PrettyWithConfig b) => PrettyWithConfig (a, b) where
  compile (a, b) = do
    let a' = pretty a
    b' <- compile b
    return $ tupled [a', b']

instance PrettyWithConfig a => PrettyWithConfig [a] where
  compile xs = list <$> traverse compile xs


prettyMapList :: (MonadPrettyConfig m, Pretty a, PrettyWithConfig b) => [(a,b)] -> m (Doc c)
prettyMapList entries = do
  let (keys, values) = unzip entries
  let keys' = fmap pretty keys
  values' <- traverse compile values
  let entries' = zipWith (\k v -> k <+> ":=" <+> v) keys' values'

  return $ "{" <+> align (group
    (concatWith (\x y -> x <> ";" <> line <> y) entries')
    <> softline <> "}")

instance (Pretty a, PrettyWithConfig b) => PrettyWithConfig (Map a b) where
  compile m = prettyMapList (Map.toAscList m)
-}