
module Vehicle.Core.Compile.Scope
  ( ScopeError(..)
  , checkProg
  ) where

import Control.Monad.Except
import Control.Monad.State (StateT(..), evalStateT, modify, get)
import Control.Monad.Reader (ReaderT, MonadReader(..), runReaderT)
import Data.List (elemIndex)
import Data.Set (Set,)
import Data.Set qualified as Set (member, insert)
import Prettyprinter (pretty, (<+>))

import Vehicle.Core.AST hiding (lift)
import Vehicle.Prelude


-- * Errors thrown during scope checking.

-- |Type of errors thrown by scpe checking.
data ScopeError
  = UnboundName Symbol Provenance
  deriving Show

instance MeaningfulError ScopeError where
  details  (UnboundName name p) = UError $ UserError
    { problem    = "The name" <+> squotes name <+> "is not in scope"
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

addDeclToCtx :: Identifier -> Ctx -> Ctx
addDeclToCtx ident Ctx {..} = Ctx (Set.insert ident declCtx) exprCtx

addBinderToCtx :: Symbol -> Ctx -> Ctx
addBinderToCtx name Ctx {..} = Ctx declCtx (name : exprCtx)

-- |Find the index for a given name of a given sort.
getVar :: InputAnn -> Symbol -> ExprSCM Var
getVar ann name = do
  Ctx declCtx exprCtx <- ask
  case elemIndex name exprCtx of
    Just i -> return $ Bound i
    Nothing ->
      if Set.member (Identifier name) declCtx
        then return $ Free (Identifier name)
        else unboundNameError name (prov ann)

bindVar :: InputBinder -> (UncheckedBinder -> ExprSCM UncheckedExpr) -> ExprSCM UncheckedExpr
bindVar (Binder p v name t) update = do
  t' <- check t
  let binder' = Binder p v (User name) t'
  local (addBinderToCtx name) (update binder')

bindDecl :: WithProvenance Identifier -> ExprSCM UncheckedDecl -> DeclSCM UncheckedDecl
bindDecl (WithProvenance _ ident) decl = do
  -- TODO get Wen to cast an eye over this
  modify (addDeclToCtx ident)
  ctx <- get;
  let r = local (const ctx) decl
  lift (runReaderT r ctx)

class ScopeCheck a b where
  check :: a -> ReaderT Ctx SCM b

instance ScopeCheck InputArg UncheckedArg where
  check (Arg p v e) = Arg p v <$> check e

instance ScopeCheck InputExpr UncheckedExpr where
  check = \case
    Type l                         -> return $ Type l
    Constraint                     -> return Constraint
    Meta i                         -> return $ Meta i
    Hole p n                       -> return $ Hole p n
    Ann      ann e t               -> Ann ann <$> check e <*> check t
    App      ann fun arg           -> App ann <$> check fun <*> check arg
    Pi       ann binder res        -> bindVar binder $ \binder' -> Pi ann binder' <$> check res
    Builtin  ann op                -> return $ Builtin ann op
    Var      ann v                 -> Var ann <$> getVar ann v
    Let      ann binder bound body -> bindVar binder $ \binder' -> Let ann binder' <$> check bound <*> check body
    Lam      ann binder       body -> bindVar binder $ \binder' -> Lam ann binder' <$> check body
    Literal  ann l                 -> return $ Literal ann l
    Seq      ann es                -> Seq ann <$> traverse check es

checkDecl :: InputDecl -> StateT Ctx SCM UncheckedDecl
checkDecl = \case
  DeclNetw ann n t    -> bindDecl n (DeclNetw ann n <$> check t)
  DeclData ann n t    -> bindDecl n (DeclData ann n <$> check t)
  DefFun   ann n t e  -> bindDecl n (DefFun ann n <$> check t <*> check e)

checkProg :: InputProg -> Except ScopeError UncheckedProg
checkProg (Main ds) = Main <$> evalStateT (traverse checkDecl ds) emptyCtx