module Vehicle.Compile.Type.WeakHeadNormalForm
  ( WHNFable(..)
  , whnfExprWithMetas
  , whnfConstraintWithMetas
  , whnf
  ) where

import Control.Monad ((<=<))
import Control.Monad.Reader (MonadReader (..), runReaderT)

import Vehicle.Compile.Error
import Vehicle.Compile.Normalise
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Constraint as Constraint
import Vehicle.Compile.Type.Monad
import Vehicle.Compile.Type.VariableContext

--------------------------------------------------------------------------------
-- Weak head options

whnf :: MonadCompile m => TypingDeclCtx -> CheckedExpr -> m CheckedExpr
whnf declCtx e = do
  discardLoggerT $ normalise e Options
    { implicationsToDisjunctions  = False
    , expandOutPolynomials        = False
    , declContext                 = toNormalisationDeclContext declCtx
    , boundContext                = mempty -- see issue #129
    , normaliseDeclApplications   = True
    , normaliseLambdaApplications = True
    , normaliseStdLibApplications = True
    , normaliseBuiltin            = const True
    , normaliseWeakly             = False
    }

--------------------------------------------------------------------------------
-- WHNF combined with meta variable subsitution

whnfExprWithMetas :: TCM m
                  => CheckedExpr
                  -> m CheckedExpr
whnfExprWithMetas e = do
  e' <- substMetas e
  declCtx <- getDeclContext
  whnf declCtx e'

whnfConstraintWithMetas :: TCM m => Constraint -> m Constraint
whnfConstraintWithMetas c = case c of
  UC ctx (Unify (e1, e2)) -> do
    e1' <- whnfExprWithMetas e1
    e2' <- whnfExprWithMetas e2
    return $ UC ctx (Unify (e1', e2'))

  TC ctx (Has m tc args) -> do
    args' <- traverse (traverse whnfExprWithMetas) args
    return $ TC ctx (Has m tc args')

--------------------------------------------------------------------------------
-- Recursively go through a check program and convert all implicit argument
-- types to whnf. This is needed because we pattern match on implicit args to
-- do type-directed compilation.

type WHNFMonad m =
  ( MonadCompile m
  , MonadReader TypingDeclCtx m
  )

class WHNFable a where
  convertImplicitArgsToWHNF :: MonadCompile m => a -> m a

instance WHNFable CheckedProg where
  convertImplicitArgsToWHNF p = runReaderT (whnfProg p) mempty

instance WHNFable CheckedExpr where
  convertImplicitArgsToWHNF e = runReaderT (whnfExpr e) mempty

whnfProg :: WHNFMonad m => CheckedProg -> m CheckedProg
whnfProg (Main ds) = Main <$> whnfDecls ds

whnfDecls :: WHNFMonad m => [CheckedDecl] -> m [CheckedDecl]
whnfDecls [] = return []
whnfDecls (d : ds) = do
  decl <- whnfDecl d
  decls <- local (addToDeclCtx d) (whnfDecls ds)
  return $ decl : decls

whnfDecl :: WHNFMonad m => CheckedDecl -> m CheckedDecl
whnfDecl = traverseDeclExprs whnfExpr

whnfExpr :: WHNFMonad m => CheckedExpr -> m CheckedExpr
whnfExpr expr = do
  res <- case expr of
    Hole{} -> resolutionError currentPhase "Hole"

    Meta{}     -> return expr
    Universe{} -> return expr
    Literal{}  -> return expr
    Builtin{}  -> return expr
    Ann{}      -> return expr
    Var{}      -> return expr

    Pi ann binder resultType -> Pi ann binder <$> whnfExpr resultType
    LVec ann es              -> LVec ann <$> traverse whnfExpr es
    App p fun args           -> App p <$> whnfExpr fun <*> traverse whnfArg args

    Let ann e1 binder e2 -> do
      -- Check the type of the bound expression against the provided type
      e1' <- whnfExpr e1
      e2' <- whnfExpr e2 -- addToBoundCtx (nameOf binder, typeOf binder, Just e1') $ whnfExpr e2
      return (Let ann e1' binder e2')

    Lam ann binder body -> do
      body' <- whnfExpr body -- addToBoundCtx (nameOf binder, typeOf binder, Nothing) $ whnfExpr body
      return $ Lam ann binder body'

  -- showInferExit res
  return res

whnfArg :: WHNFMonad m => CheckedArg -> m CheckedArg
whnfArg arg
  | visibilityOf arg == Implicit = do
    declCtx <- ask
    traverse (whnfExpr <=< whnf declCtx) arg
  | otherwise = traverse whnfExpr arg

currentPhase :: Doc ()
currentPhase = "normalisation of inserted implicit types"
