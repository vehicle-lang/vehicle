
module Vehicle.Core.Compile.Descope where

import Control.Monad.Except
import Control.Monad.Supply (MonadSupply, Supply, demand, runSupplyT, withSupplyT)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.State (MonadState, StateT(..), evalStateT, runStateT, modify, lift, get)
import Control.Monad.Reader (ReaderT, MonadReader(..), runReaderT)
import Data.Functor.Foldable (Recursive(..))
import Data.List (elemIndex)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text (pack)
import Prettyprinter


import Vehicle.Core.AST hiding (lift)
import Vehicle.Prelude

-- |Throw an |IndexOutOfBounds| error using an arbitrary index.
indexOutOfBounds :: Index -> Int -> Provenance -> a
indexOutOfBounds index ctxSize p = developerError $
  "DeBruijn index" <+> pretty index <+>
  "greater than current context size" <+> pretty ctxSize


{-
-- |Find the name for a given index of a given sort.
getName ::
  (KnownSort sort, sort `In` ['TYPE, 'EXPR]) =>
  Provenance ->
  DeBruijn sort ->
  DataflowT sort (Ctx Name) (Except ScopeError) (K Name sort)
getName p (db :: DeBruijn sort) = do
  subctx <- getSubCtxFor @sort <$> askData
  let index = toIndex db
  let maybeSymbol = subctx Seq.!? index
  let symbolOrError = maybe (indexOutOfBounds index (length subctx) p) return maybeSymbol
  K <$> symbolOrError

-- * Scope checking.

-- |Check if a tree is well-scoped, replacing name tokens with deBruijn indices.
symbolToDeBruijn ::
  forall sort.
  (KnownSort sort) =>
  Tree (K Symbol) (K Provenance) sort ->
  Except ScopeError (Tree DeBruijn (K Provenance) sort)
symbolToDeBruijn = evalDataflowT mempty . unSDF . foldTree (SDF . symbolToDeBruijnF)

-- |Check if a single layer is well-scoped in the appropriate data-flow context.
symbolToDeBruijnF ::
  forall sort.
  (KnownSort sort) =>
  TreeF (K Symbol) (K Provenance) sort (SortedDataflowT (Ctx Symbol) (Except ScopeError) (Tree DeBruijn (K Provenance))) ->
  DataflowT sort (Ctx Symbol) (Except ScopeError) (Tree DeBruijn (K Provenance) sort)
-}

data Ctx = Ctx

type MonadDescope m = (MonadSupply Int Identity m, MonadState Ctx m)

class Descope a b where
  descope :: MonadDescope m => a -> m b

instance Descope CheckedAnn OutputAnn where
  descope ctx (RecAnn e p) = RecAnn <$> descope ctx e <*> pure p

-- |Check if a single layer is well-scoped in the appropriate data-flow context.
instance Descope CheckedExpr OutputExpr where
  descope c = \case
    Type     l                     -> return (Type l)
    Constraint                     -> return Constraint
    Meta     p i                   -> return (Meta p i)
    Hole     ann name              -> Hole    (descope c ann) name
    Ann      ann e t               -> Ann     (descope c ann) (descope c e) (descope c t)
    App      ann fun arg           -> App     (descope c ann) _ _
    Pi       ann binder body       -> Pi      (descope c ann) _ _
    Builtin  ann op                -> Builtin (descope c ann) _
    Var      ann v                 -> Var     (descope c ann) _
    Let      ann binder bound body -> Let     (descope c ann) _ _ _
    Lam      ann binder body       -> Lam     (descope c ann) _ _
    Literal  ann l                 -> Literal (descope c ann) l
    Seq      ann es                -> Seq     (descope c ann) (fmap (descope c) es)

instance Descope CheckedDecl OutputDecl where
  descope c = \case
    DeclNetw ann n t -> _ {-do ann' <- convertAnn ann
                               t'   <- sflow t
                               n'   <- sflow n
                               return $ DeclNetw ann' n' t'
                               -}
    DeclData ann n t -> _ {-do ann' <- convertAnn ann
                               t'   <- sflow t
                               n'   <- sflow n
                               return $ DeclData ann' n' t'-}
    DefFun ann n t e -> _ {-do ann' <- convertAnn ann
                               t'   <- sflow t
                               e'   <- sflow e
                               n'   <- sflow n
                               return $ DefFun ann' n' t' e'-}

instance Descope CheckedProg OutputProg where
  descope c (Main ds) = Main (fmap descope c ds)
{-
  -- Declarations
  SDECL ->

  -- Programs
  SPROG -> \case
    MainF ann ds -> Main <$> convertAnn ann <*> flow (traverse unSDF ds)

convertAnn ::
  forall sort.
  KnownSort sort =>
  (Info DeBruijn :*: K Provenance) sort ->
  DataflowT sort (Ctx Name) (Except ScopeError) ((Info (K Name) :*: K Provenance) sort)
convertAnn (Info v :*: p) = case sortSing @sort of
  SKIND -> return $ Info v :*: p
  STYPE -> do v' <- flow $ deBruijnToName' v; return $ Info v' :*: p
  STARG -> do v' <- flow $ deBruijnToName' v; return $ Info v' :*: p
  SEXPR -> do v' <- flow $ deBruijnToName' v; return $ Info v' :*: p
  SEARG -> do v' <- flow $ deBruijnToName' v; return $ Info v' :*: p
  SDECL -> return $ Info v :*: p
  SPROG -> return $ Info v :*: p

nameToSymbol ::
  forall sort.
  KnownSort sort =>
  Tree (K Name) (Info (K Name) :*: K Provenance) sort ->
  Supply Symbol (Tree (K Symbol) (Info (K Symbol) :*: K Provenance) sort)
nameToSymbol = traverseTreeFields convName convAnn
  where
    convName :: forall sort2. KnownSort sort2 => K Name sort2 -> Supply Symbol (K Symbol sort2)
    convName (K (User symbol)) = return (K symbol)
    convName (K Machine)       = K <$> demand

    convAnn :: forall sort2. KnownSort sort2 =>
      (Info (K Name) :*: K Provenance) sort2 ->
      Supply Symbol ((Info (K Symbol) :*: K Provenance) sort2)
    convAnn (Info v :*: p) = case sortSing @sort2 of
      SKIND -> return $ Info v :*: p
      STYPE -> do v' <- nameToSymbol v; return $ Info v' :*: p
      STARG -> do v' <- nameToSymbol v; return $ Info v' :*: p
      SEXPR -> do v' <- nameToSymbol v; return $ Info v' :*: p
      SEARG -> do v' <- nameToSymbol v; return $ Info v' :*: p
      SDECL -> return $ Info v :*: p
      SPROG -> return $ Info v :*: p
-}
{-
deBruijnToSymbol ::
  Tree DeBruijn (Info DeBruijn :*: K Provenance) sort ->
  Except ScopeError (Tree (K Symbol) (Info (K Symbol) :*: K Provenance) sort)
deBruijnToSymbol dbTree = _

do
  nmTree <- deBruijnToName dbTree
  return $ runIdentity $ runSupplyT (withSupplyT (\i -> "v" <> pack (show i)) (nameToSymbol nmTree)) (+1) (0 :: Integer)
-}