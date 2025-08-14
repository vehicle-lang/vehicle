module Vehicle.Compile.Type.Constraint.ApplicationSolver
  ( runApplicationSolver,
  )
where

import Data.Proxy (Proxy)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Bidirectional (solveArgInsertionProblem)
import Vehicle.Compile.Type.Constraint.Core
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Monad (solveMeta)
import Vehicle.Compile.Type.Monad.Class
import Vehicle.Compile.Type.System

-- | Attempts to solve as many type-class constraints as possible.
runApplicationSolver :: (TCM builtin m) => Proxy builtin -> m ()
runApplicationSolver proxy = do
  logCompilerSection2 MaxDetail "application solver run" $
    runConstraintSolver
      getActiveApplicationConstraints
      setApplicationConstraints
      solveApplicationConstraint
      True
      proxy

solveApplicationConstraint ::
  (TCM builtin m) =>
  WithContext (ApplicationConstraint builtin) ->
  m ()
solveApplicationConstraint (WithContext InferArgs {..} ctx) = do
  let boundCtx = boundContextOf ctx
  result <- solveArgInsertionProblem boundCtx argInsertionProblem
  case result of
    Right (finalExpr, finalType) -> do
      solveMeta exprSolution finalExpr boundCtx
      solveMeta typeSolution finalType boundCtx
    Left (blockedProblem, blockingMetas) -> do
      let newConstraint = InferArgs {argInsertionProblem = blockedProblem, ..}
      let finalConstraint = WithContext newConstraint (blockCtxOn blockingMetas ctx)
      addApplicationConstraint finalConstraint
