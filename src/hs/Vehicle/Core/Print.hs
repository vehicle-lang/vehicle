{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Core.Print
  ( PrettyWithConfig(..)
  , prettySimple
  , prettyVerbose
  ) where

import Control.Monad.Reader (MonadReader(..), runReader)
import Data.Maybe (fromMaybe, catMaybes)
import Data.Map (Map)
import Data.Map qualified as Map (toAscList)
import Data.Default.Class (Default(..))
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty (toList)
import Prettyprinter (list, lbrace, rbrace, comma, group, softline, tupled, encloseSep)

import Vehicle.Core.AST
import Vehicle.Prelude
import Vehicle.Core.MetaSet (MetaSet)
import Vehicle.Core.MetaSet qualified as MetaSet
import Vehicle.Core.MetaSubstitution (MetaSubstitution)
import Vehicle.Core.MetaSubstitution qualified as MetaSubst

--------------------------------------------------------------------------------
-- Printing to Core language

data PrettyOptions = PrettyOptions
  { showImplicitArgs :: Bool
  , showInstanceArgs :: Bool
  }

instance Default PrettyOptions where
  def = PrettyOptions
    { showImplicitArgs = True
    , showInstanceArgs = True
    }

prettySimple :: PrettyWithConfig a => a -> Doc b
prettySimple x = runReader (pretty' x) (def
  { showImplicitArgs = False
  , showInstanceArgs = False
  })

prettyVerbose :: PrettyWithConfig a => a -> Doc b
prettyVerbose x = runReader (pretty' x) def

brackets :: Visibility -> (Doc a -> Doc a)
brackets Explicit = parens
brackets Implicit = braces
brackets Instance = braces . braces

type MonadPrettyConfig m = (MonadReader PrettyOptions m)

class PrettyWithConfig a where
  pretty' :: MonadPrettyConfig m => a -> m (Doc b)

--instance PrettyWithConfig a => Pretty a where
--  pretty x = runReader (pretty' x) False

instance Pretty Name where
  pretty Machine       = "_"
  pretty (User symbol) = pretty symbol

instance Pretty Var where
  pretty (Free  ident) = pretty ident
  pretty (Bound index)
    | index >= 0 = "i" <> pretty index
    | otherwise  = "i[" <> pretty index <> "]"

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
  pretty' (Arg v expr) = brackets v <$> pretty' expr

instance Pretty var => PrettyWithConfig (Binder var ann) where
  pretty' (Binder _ann v n t) = do
    tDoc <- pretty' t
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
      argDoc <- pretty' arg
      let v = vis arg
      let showArg = v == Explicit
                 || v == Implicit && showImplicitArgs
                 || v == Instance && showInstanceArgs
      return $ if showArg
        then Just argDoc
        else Nothing

instance Pretty var => PrettyWithConfig (Expr var ann) where
  pretty' = \case
    Type l                      -> return $ "Type" <> pretty l
    Hole    _p   name           -> return $ "?"    <> pretty name
    Meta    _p   m              -> return $ pretty m
    Builtin _ann op             -> return $ pretty op
    Literal _ann l              -> return $ pretty l
    Var     _ann v              -> return $ pretty v

    App _ann fun args -> do
      funDoc <- pretty' fun
      argDocs <- prettyArgs args
      return $ if null argDocs then funDoc else parens (funDoc <+> hsep argDocs)

    Seq _ann es -> do
      es' <- traverse pretty' es
      return $ list es'

    Ann _ann term typ -> do
      term' <- pretty' term
      typ'  <- pretty' typ
      return $ parens (term' <+> ":type" <+> typ')

    Pi      _ann binder res     -> do
      binder' <- pretty' binder
      res' <- pretty' res
      return $ parens ("pi" <+> binder' <+> res')

    Let     _ann bound binder body   -> do
      bound' <- pretty' bound
      binder' <- pretty' binder
      body' <- pretty' body
      return $ parens ("let" <+> binder' <+> bound' <+> body')

    Lam     _ann binder body -> do
      binder' <- pretty' binder
      body' <- pretty' body
      return $ parens ("lambda" <+> binder' <+> body')

    PrimDict tc -> do
      tc' <- pretty' tc
      return $ parens $ "PrimDict" <+> tc'

instance Pretty var => PrettyWithConfig (Decl var ann) where
  pretty' = \case
    DeclNetw _ann n t -> do
      t' <- pretty' t
      return $ parens ("declare-network" <+> pretty n <+> t') <+> line

    DeclData _ann n t -> do
      t' <- pretty' t
      return $ parens ("declare-dataset" <+> pretty n <+> t') <+> line

    DefFun _ann n t e -> do
      t' <- pretty' t
      e' <- pretty' e
      return $ parens ("define-fun" <+> pretty n <+> t' <+> e') <+> line

instance Pretty var => PrettyWithConfig (Prog var ann) where
  pretty' (Main ds) = do ds' <- traverse pretty' ds; return $ vsep ds'

instance PrettyWithConfig MetaSubstitution where
  pretty' m = prettyMapList (MetaSubst.toAscList m)

instance PrettyWithConfig MetaSet where
  pretty' m = return $
    encloseSep lbrace rbrace comma (fmap pretty (MetaSet.toList m))

instance PrettyWithConfig Name where
  pretty' n = return $ pretty n

--------------------------------------------------------------------------------
-- Derived instances

instance (PrettyWithConfig a, PrettyWithConfig b) => PrettyWithConfig (a, b) where
  pretty' (a, b) = do
    a' <- pretty' a
    b' <- pretty' b
    return $ tupled [a', b']

instance PrettyWithConfig a => PrettyWithConfig [a] where
  pretty' xs = list <$> traverse pretty' xs


prettyMapList :: (MonadPrettyConfig m, Pretty a, PrettyWithConfig b) => [(a,b)] -> m (Doc c)
prettyMapList entries = do
  let (keys, values) = unzip entries
  let keys' = fmap pretty keys
  values' <- traverse pretty' values
  let entries' = zipWith (\k v -> k <+> ":=" <+> v) keys' values'

  return $ "{" <+> align (group
    (concatWith (\x y -> x <> ";" <> line <> y) entries')
    <> softline <> "}")

instance (Pretty a, PrettyWithConfig b) => PrettyWithConfig (Map a b) where
  pretty' m = prettyMapList (Map.toAscList m)