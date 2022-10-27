module Vehicle.Compile.Type.WeakHeadNormalForm
  ( whnfExprWithMetas
  , whnfConstraintWithMetas
  , whnf
  ) where

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
