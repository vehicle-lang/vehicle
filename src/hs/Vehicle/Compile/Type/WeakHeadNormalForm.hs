module Vehicle.Compile.Type.WeakHeadNormalForm
  ( WHNFable(..)
  , whnfExprWithMetas
  , whnfConstraintWithMetas
  , whnf
  ) where

import Control.Monad ( (<=<) )
import Control.Monad.Reader (MonadReader, runReaderT)

import Vehicle.Compile.Prelude
import Vehicle.Compile.Error
import Vehicle.Compile.Type.VariableContext
import Vehicle.Compile.Type.Meta
import Vehicle.Compile.Type.Constraint
import Vehicle.Compile.Normalise

--------------------------------------------------------------------------------
-- Weak head options

whnf :: (MonadCompile m, MonadReader TypingVariableCtx m)
     => CheckedExpr
     -> m CheckedExpr
whnf e = do
  declCtx <- getNormalisationContext
  discardLoggerT $ normalise e Options
    { implicationsToDisjunctions  = False
    , expandOutPolynomials        = False
    , declContext                 = declCtx
    , boundContext                = mempty -- see issue #129
    , normaliseDeclApplications   = True
    , normaliseLambdaApplications = True
    , normaliseLetBindings        = True
    , normaliseAnnotations        = True
    , normaliseStdLibApplications = True
    , normaliseBuiltin            = const True
    , normaliseWeakly             = False
    }

--------------------------------------------------------------------------------
-- WHNF combined with meta variable subsitution

whnfExprWithMetas :: MonadMeta m
                  => TypingVariableCtx
                  -> CheckedExpr
                  -> m CheckedExpr
whnfExprWithMetas ctx e = do
  e' <- substMetas e
  runReaderT (whnf e') ctx

whnfConstraintWithMetas :: MonadMeta m
                        => Constraint
                        -> m Constraint
whnfConstraintWithMetas = \case
  UC ctx (Unify (e1, e2)) -> do
    e1' <- whnfExprWithMetas (varContext ctx) e1
    e2' <- whnfExprWithMetas (varContext ctx) e2
    return $ UC ctx (Unify (e1', e2'))

  TC ctx (Has m tc args) -> do
    args' <- traverse (traverseArgExpr (whnfExprWithMetas (varContext ctx))) args
    return $ TC ctx (Has m tc args')

--------------------------------------------------------------------------------
-- Recursively go through a check program and convert all implicit argument
-- types to whnf. This is needed because we pattern match on implicit args to
-- do type-directed compilation.

class WHNFable a where
  convertImplicitArgsToWHNF :: MonadCompile m => a -> m a

instance WHNFable CheckedProg where
  convertImplicitArgsToWHNF p = runReaderT (whnfProg p) emptyVariableCtx

instance WHNFable CheckedDecl where
  convertImplicitArgsToWHNF d = runReaderT (whnfDecl d) emptyVariableCtx

instance WHNFable CheckedExpr where
  convertImplicitArgsToWHNF e = runReaderT (whnfExpr e) emptyVariableCtx

whnfProg :: (MonadCompile m, MonadReader TypingVariableCtx m) => CheckedProg -> m CheckedProg
whnfProg (Main ds) = Main <$> whnfDecls ds

whnfDecls :: (MonadCompile m, MonadReader TypingVariableCtx m)
          => [CheckedDecl]
          -> m [CheckedDecl]
whnfDecls [] = return []
whnfDecls (d : ds) = do
  decl <- whnfDecl d
  decls <- addDeclToCtx decl $ whnfDecls ds
  return $ decl : decls

whnfDecl :: (MonadCompile m, MonadReader TypingVariableCtx m)
          => CheckedDecl
          -> m CheckedDecl
whnfDecl = traverseDeclExprs whnfExpr

whnfExpr :: (MonadCompile m, MonadReader TypingVariableCtx m) => CheckedExpr -> m CheckedExpr
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
      e2' <- addToBoundCtx (nameOf binder, typeOf binder, Just e1') $ whnfExpr e2
      return (Let ann e1' binder e2')

    Lam ann binder body -> do
      body' <- addToBoundCtx (nameOf binder, typeOf binder, Nothing) $ whnfExpr body
      return $ Lam ann binder body'

  -- showInferExit res
  return res

whnfArg :: (MonadCompile m, MonadReader TypingVariableCtx m) => CheckedArg -> m CheckedArg
whnfArg arg
  | visibilityOf arg == Implicit = traverseArgExpr (whnfExpr <=< whnf) arg
  | otherwise = traverseArgExpr whnfExpr arg

currentPhase :: Doc ()
currentPhase = "normalisation of inserted implicit types"