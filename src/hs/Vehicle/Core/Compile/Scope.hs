
module Vehicle.Core.Compile.Scope
  ( ScopeError(..)
  , runScopeCheck
  ) where

import Control.Monad.Except
import Control.Monad.State (StateT(..), evalStateT, modify, get)
import Control.Monad.Reader (ReaderT, MonadReader(..), runReaderT)
import Data.List (elemIndex)
import Data.Set (Set,)
import Data.Set qualified as Set (member, insert)
import Prettyprinter (Pretty(..), (<+>), squotes)

import Vehicle.Core.AST
import Vehicle.Prelude


runScopeCheck :: InputProg -> ExceptT ScopeError Logger UncheckedProg
runScopeCheck p = liftExceptWithLogging $ scopeProg p

-- * Errors.

-- |Type of errors thrown by scope checking.
data ScopeError
  = UnboundName Symbol Provenance
  deriving Show

instance MeaningfulError ScopeError where
  details  (UnboundName name p) = UError $ UserError
    { problem    = "The name" <+> squotes (pretty name) <+> "is not in scope"
    , provenance = p
    -- TODO can use Levenschtein distance to search contexts/builtins
    , fix        = pretty ("Unknown" :: String)
    }

-- |Throw an |UnboundName| error using an arbitrary token.
unboundNameError :: MonadError ScopeError m => Symbol -> Provenance -> m a
unboundNameError n p = throwError $ UnboundName n p

-- * Scope checking contexts.

type SCM = Except ScopeError
type ExprSCM a = ReaderT Ctx SCM a
type DeclSCM a = StateT  Ctx SCM a

-- |Type of scope checking contexts.
data Ctx = Ctx
  { declCtx :: Set Identifier
  , exprCtx :: [Symbol]
  }

emptyCtx :: Ctx
emptyCtx = Ctx mempty mempty

addDeclToCtx :: WithProvenance Identifier -> Ctx -> Ctx
addDeclToCtx (WithProvenance _ ident) Ctx {..} = Ctx (Set.insert ident declCtx) exprCtx

addBinderToCtx :: Name -> Ctx -> Ctx
addBinderToCtx Machine       ctx     = ctx
addBinderToCtx (User symbol) Ctx{..} = Ctx declCtx (symbol : exprCtx)

-- |Find the index for a given name of a given sort.
getVar :: InputAnn -> Name -> ExprSCM Var
getVar ann Machine       = developerError $ "Machine names should not be in use " <+> pretty ann
getVar ann (User symbol) = do
  Ctx declCtx exprCtx <- ask
  case elemIndex symbol exprCtx of
    Just i -> return $ Bound i
    Nothing ->
      if Set.member (Identifier symbol) declCtx
        then return $ Free (Identifier symbol)
        else unboundNameError symbol (prov ann)

bindVar :: InputBinder -> (UncheckedBinder -> ExprSCM UncheckedExpr) -> ExprSCM UncheckedExpr
bindVar (Binder p v n t) update = do
  t' <- scope t
  let binder' = Binder p v n t'
  local (addBinderToCtx n) (update binder')

flow :: ExprSCM a -> DeclSCM a
flow r = do
  ctx <- get
  let s = local (const ctx) r
  lift (runReaderT s ctx)

class ScopeCheck a b where
  scope :: a -> ReaderT Ctx SCM b

instance ScopeCheck InputArg UncheckedArg where
  scope (Arg p v e) = Arg p v <$> scope e

instance ScopeCheck InputExpr UncheckedExpr where
  scope = \case
    Type l                         -> return $ Type l
    Meta p i                       -> return $ Meta p i
    Hole     ann n                 -> return $ Hole ann n
    Ann      ann e t               -> Ann ann <$> scope e <*> scope t
    App      ann fun arg           -> App ann <$> scope fun <*> scope arg
    Builtin  ann op                -> return $ Builtin ann op
    Var      ann v                 -> Var ann <$> getVar ann v
    Literal  ann l                 -> return $ Literal ann l
    Seq      ann es                -> Seq ann <$> traverse scope es

    Pi  ann binder res -> do
      bindVar binder $ \binder' -> Pi ann binder' <$> scope res

    Lam ann binder body -> do
      bindVar binder $ \binder' -> Lam ann binder' <$> scope body

    Let ann bound binder body -> do
      bound' <- scope bound
      bindVar binder $ \binder' -> Let ann bound' binder' <$> scope body


scopeDecl :: InputDecl -> StateT Ctx SCM UncheckedDecl
scopeDecl = \case

  DeclNetw ann n t -> do
    t' <- flow $ scope t
    modify (addDeclToCtx n)
    return $ DeclNetw ann n t'

  DeclData ann n t -> do
    t' <- flow $ scope t
    modify (addDeclToCtx n)
    return $ DeclNetw ann n t'

  DefFun ann n t e -> do
    t' <- flow $ scope t
    e' <- flow $ scope e
    modify (addDeclToCtx n)
    return $ DefFun ann n t' e'

scopeProg :: InputProg -> Except ScopeError UncheckedProg
scopeProg (Main ds) = Main <$> evalStateT (traverse scopeDecl ds) emptyCtx