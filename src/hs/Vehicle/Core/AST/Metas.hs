
module Vehicle.Core.AST.Metas
  ( MetaSet
  , MetaSubstitution
  , MetaSubstitutable(..)
  ) where

import Control.Monad.Reader (Reader, runReader, ask, local)

import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.IntSet (IntSet)

import Vehicle.Core.AST.Core
import Vehicle.Core.AST.Utils
import Vehicle.Core.AST.DeBruijn

type MetaSet = IntSet

type MetaSubstitution = IntMap CheckedExpr

class MetaSubstitutable a where
  -- TODO change name away from M
  substM :: a -> Reader MetaSubstitution a

  substMetas :: MetaSubstitution -> a -> a
  substMetas s e = runReader (substM e) s

  substMeta :: Meta -> CheckedExpr -> a -> a
  substMeta m e = substMetas (IntMap.singleton m e)

  substMetasLiftLocal :: a -> Reader MetaSubstitution a
  substMetasLiftLocal e = local (IntMap.map (liftDBIndices 1)) (substM e)

instance MetaSubstitutable a => MetaSubstitutable (a, a) where
  substM (e1, e2) = do
    e1' <- substM e1
    e2' <- substM e2
    return (e1', e2')

instance MetaSubstitutable a => MetaSubstitutable [a] where
  substM = traverse substM

instance MetaSubstitutable CheckedArg where
  substM (Arg p v e) = Arg p v <$> substM e

instance MetaSubstitutable CheckedBinder where
  substM (Binder p v n t) = Binder p v n <$> substM t

instance MetaSubstitutable CheckedExpr where
  substM = \case
    Type l                   -> return (Type l)
    Hole p name              -> return (Hole p name)
    Builtin ann op           -> return (Builtin ann op)
    Literal ann l            -> return (Literal ann l)
    Var     ann v            -> return (Var     ann v)
    Seq     ann es           -> Seq     ann <$> traverse substM es
    Ann     ann term typ     -> Ann     ann <$> substM term   <*> substM typ
    App     ann fun arg      -> App     ann <$> substM fun    <*> substM arg
    Pi      ann binder res   -> Pi      ann <$> substM binder <*> substMetasLiftLocal res
    Let     ann e1 binder e2 -> Let     ann <$> substM e1     <*> substM binder <*> substMetasLiftLocal e2
    Lam     ann binder e     -> Lam     ann <$> substM binder <*> substMetasLiftLocal e

    Meta    ann m -> do
      subst <- ask
      return $ case IntMap.lookup m subst of
        Nothing -> Meta ann m
        Just e  -> e

instance MetaSubstitutable CheckedDecl where
  substM = \case
    DeclNetw p ident t   -> DeclNetw p ident <$> substM t
    DeclData p ident t   -> DeclData p ident <$> substM t
    DefFun   p ident t e -> DefFun   p ident <$> substM t <*> substM e

instance MetaSubstitutable CheckedProg where
  substM (Main ds) = Main <$> traverse substM ds