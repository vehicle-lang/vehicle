
module Vehicle.Language.Simplify
  ( runSimplify
  , Simplify(..)
  , SimplifyOptions(..)
  ) where

import Control.Monad.Reader (MonadReader(..), runReader)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty (toList)
import Data.Maybe (catMaybes)

import Vehicle.Prelude
import Vehicle.Language.AST

runSimplify :: Simplify a => SimplifyOptions -> a -> a
runSimplify options x = runReader (simplify x) options

data SimplifyOptions = SimplifyOptions
  { removeImplicits   :: Bool
  , removeInstances   :: Bool
  , removeNonUserCode :: Bool
  }

type MonadSimplify m = MonadReader SimplifyOptions m

class Simplify a where
  simplify :: MonadSimplify m => a -> m a

instance Semigroup ann => Simplify (Prog var ann) where
  simplify (Main ds) = Main <$> traverse simplify ds

instance Semigroup ann => Simplify (Decl var ann) where
  simplify = \case
    DeclNetw ann n t -> DeclNetw ann n <$> simplify t
    DeclData ann n t -> DeclData ann n <$> simplify t
    DefFun ann n t e -> DefFun ann n <$> simplify t <*> simplify e

instance Semigroup ann => Simplify (Expr var ann) where
  simplify expr = case expr of
    Type{}    -> return expr
    Hole{}    -> return expr
    Meta{}    -> return expr
    Builtin{} -> return expr
    Literal{} -> return expr
    Var{}     -> return expr

    App ann fun args          -> normAppList ann <$> simplify fun <*> simplifyArgs args
    Seq ann xs                -> Seq ann <$> traverse simplify xs
    Ann ann e t               -> Ann ann <$> simplify e <*> simplify t
    Pi ann binder result      -> Pi  ann <$> simplify binder <*> simplify result
    Let ann bound binder body -> Let ann <$> simplify bound  <*> simplify binder <*> simplify body
    Lam ann binder body       -> Lam ann <$> simplify binder <*> simplify body
    PrimDict tc               -> PrimDict <$> simplify tc

instance Semigroup ann => Simplify (Binder var ann) where
  simplify (Binder ann v n t) = Binder ann v n <$> simplify t

instance Semigroup ann => Simplify (Arg var ann) where
  simplify (Arg v t) = Arg v <$> simplify t

simplifyArgs :: (Semigroup ann, MonadSimplify m)
           => NonEmpty (Arg var ann)
           -> m [Arg var ann]
simplifyArgs args = catMaybes <$> traverse prettyArg (NonEmpty.toList args)
  where
    prettyArg :: (Semigroup ann, MonadSimplify m)
              => Arg var ann
              -> m (Maybe (Arg var ann))
    prettyArg arg = do
      SimplifyOptions{..} <- ask
      let v = vis arg
      let keepArg = v == Explicit
                 || v == Implicit && not removeImplicits
                 || v == Instance && not removeInstances
      if keepArg
        then Just <$> simplify arg
        else return Nothing

{-
instance Simplify MetaSubstitution where
  simplify m = _

instance PrettyWithConfig MetaSet where
  simplify m = return $
    encloseSep lbrace rbrace comma (fmap pretty (MetaSet.toList m))

instance PrettyWithConfig Name where
  simplify n = return $ pretty n


instance (PrettyWithConfig a, PrettyWithConfig b) => PrettyWithConfig (a, b) where
  simplify (a, b) = do
    a' <- simplify a
    b' <- simplify b
    return $ tupled [a', b']



instance (Pretty a, PrettyWithConfig b) => PrettyWithConfig (Map a b) where
  simplify m = prettyMapList (Map.toAscList m)
  -}

--------------------------------------------------------------------------------
-- Other instances

instance Simplify a => Simplify [a] where
  simplify = traverse simplify

instance Simplify b => Simplify (a, b) where
  simplify (x, y) = do y' <- simplify y; return (x, y')