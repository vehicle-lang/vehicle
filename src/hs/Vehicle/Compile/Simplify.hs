
module Vehicle.Compile.Simplify
  ( Simplify(..)
  , SimplifyOptions(..)
  ) where

import Control.Monad.Reader (MonadReader(..), runReader)
import Data.Default (Default (..))
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty (toList)
import Data.Maybe (catMaybes)
import Data.IntMap ( IntMap )
import Data.Text ( Text )

import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Constraint
import Vehicle.Compile.Type.MetaMap


data SimplifyOptions = SimplifyOptions
  { removeImplicits   :: Bool
  , removeInstances   :: Bool
  , removeNonUserCode :: Bool
  }

instance Default SimplifyOptions where
  def = SimplifyOptions
      { removeImplicits   = True
      , removeInstances   = True
      , removeNonUserCode = False
      }

type MonadSimplify m = MonadReader SimplifyOptions m

class Simplify a where

  -- | Simplifies the code with the default options.
  simplify :: Simplify a => a -> a
  simplify = simplifyWith def

  -- | Simplifies the code according to the options provided.
  --   Note that this can be seen as undoing parts of the type-checking,
  --   and therefore the resulting code is not guaranteed to be well-typed.
  simplifyWith :: Simplify a => SimplifyOptions -> a -> a
  simplifyWith options x = runReader (simplifyReader x) options

  simplifyReader :: MonadSimplify m => a -> m a

instance Simplify (Prog binder var) where
  simplifyReader (Main ds) = Main <$> traverse simplifyReader ds

instance Simplify (Decl binder var) where
  simplifyReader = traverseDeclExprs simplifyReader

instance Simplify (Expr binder var) where
  simplifyReader expr = case expr of
    Universe{} -> return expr
    Hole{}     -> return expr
    Meta{}     -> return expr
    Builtin{}  -> return expr
    Literal{}  -> return expr
    Var{}      -> return expr

    App ann fun args          -> normAppList ann <$> simplifyReader fun <*> simplifyReaderArgs args
    LVec ann xs               -> LVec ann <$> traverse simplifyReader xs
    Ann ann e t               -> Ann ann <$> simplifyReader e <*> simplifyReader t
    Pi ann binder result      -> Pi  ann <$> simplifyReader binder <*> simplifyReader result
    Let ann bound binder body -> Let ann <$> simplifyReader bound  <*> simplifyReader binder <*> simplifyReader body
    Lam ann binder body       -> Lam ann <$> simplifyReader binder <*> simplifyReader body

instance Simplify (Binder binder var) where
  simplifyReader = traverseBinderType simplifyReader

instance Simplify (Arg binder var) where
  simplifyReader = traverseArgExpr simplifyReader

simplifyReaderArgs :: MonadSimplify m
                   => NonEmpty (Arg binder var)
                   -> m [Arg binder var]
simplifyReaderArgs args = catMaybes <$> traverse prettyArg (NonEmpty.toList args)
  where
    prettyArg :: MonadSimplify m
              => Arg binder var
              -> m (Maybe (Arg binder var))
    prettyArg arg = do
      SimplifyOptions{..} <- ask

      let removeArg =
            (removeNonUserCode && wasInsertedByCompiler arg) ||
            (visibilityOf arg == Implicit && removeImplicits) ||
            (visibilityOf arg == Instance && removeInstances)

      if removeArg
        then return Nothing
        else Just <$> simplifyReader arg

--------------------------------------------------------------------------------
-- Other instances

instance Simplify a => Simplify [a] where
  simplifyReader = traverse simplifyReader

instance (Simplify a, Simplify b) => Simplify (a, b) where
  simplifyReader (x, y) = do
    x' <- simplifyReader x;
    y' <- simplifyReader y;
    return (x', y')

instance (Simplify a, Simplify b, Simplify c) => Simplify (a, b, c) where
  simplifyReader (x, y, z) = do
    x' <- simplifyReader x;
    y' <- simplifyReader y;
    z' <- simplifyReader z;
    return (x', y', z')

instance Simplify a => Simplify (IntMap a) where
  simplifyReader = traverse simplifyReader

instance Simplify PositionTree where
  simplifyReader = return

instance Simplify PositionsInExpr where
  simplifyReader (PositionsInExpr e t) = return $ PositionsInExpr e t

instance Simplify Text where
  simplifyReader = return

instance Simplify Int where
  simplifyReader = return

instance Simplify UnificationConstraint where
  simplifyReader (Unify (e1, e2)) = do
    e1' <- simplifyReader e1
    e2' <- simplifyReader e2
    return $ Unify (e1', e2')

instance Simplify TypeClassConstraint where
  simplifyReader (m `Has` e) = do
    e' <- simplifyReader e
    return $ m `Has` e'

instance Simplify Constraint where
  simplifyReader (TC ctx c) = TC ctx <$> simplifyReader c
  simplifyReader (UC ctx c) = UC ctx <$> simplifyReader c

instance Simplify a => Simplify (MetaMap a) where
  simplifyReader (MetaMap m) = MetaMap <$> traverse simplifyReader m
