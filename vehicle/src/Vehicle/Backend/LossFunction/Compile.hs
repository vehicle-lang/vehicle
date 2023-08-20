module Vehicle.Backend.LossFunction.Compile
  ( compile,
  )
where

import Control.Monad.Except (MonadError (..))
import Data.Maybe (fromMaybe)
import Vehicle.Backend.LossFunction.Logics
import Vehicle.Backend.LossFunction.TypeSystem
import Vehicle.Backend.LossFunction.TypeSystem.InstanceBuiltins (lossBuiltinInstances)
import Vehicle.Backend.Prelude (DifferentiableLogicID (..))
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Subsystem (resolveInstanceArguments, typeCheckWithSubsystem)
import Vehicle.Compile.Type.Subsystem.Standard.Core qualified as S
import Vehicle.Expr.BuiltinInterface
import Vehicle.Expr.BuiltinPatterns
import Vehicle.Libraries.StandardLibrary
import Vehicle.Syntax.Builtin (Builtin)

--------------------------------------------------------------------------------
-- Compilation

-- | The translation into the LExpr (this is the exported top compile function)
compile ::
  (MonadCompile m) =>
  DifferentiableLogicID ->
  Prog Ix Builtin ->
  m (Prog Ix LossBuiltin)
compile logic typedProg =
  logCompilerPass MinDetail currentPass $ do
    let logicImplementation = implementationOf logic

    -- Some logics require some preprocessing (e.g. DL2 requires that negation
    -- is pushed all the way in.)
    reformattedProg <- preprocessLogicalOperators logicImplementation typedProg

    let instanceCandidates = lossBuiltinInstances logicImplementation
    lossProgWithInstances <- typeCheckWithSubsystem instanceCandidates reformattedProg

    lossProg <- resolveInstanceArguments lossProgWithInstances

    return lossProg

--------------------------------------------------------------------------------
-- Utilities

currentPass :: Doc a
currentPass = "compilation to loss functions"

--------------------------------------------------------------------------------
-- Preprocessing logical operators

preprocessLogicalOperators ::
  forall m.
  (MonadCompile m) =>
  DifferentialLogicImplementation ->
  Prog Ix Builtin ->
  m (Prog Ix Builtin)
preprocessLogicalOperators logic = traverse (traverseBuiltinsM builtinUpdateFunction)
  where
    builtinUpdateFunction :: BuiltinUpdate m Ix Builtin Builtin
    builtinUpdateFunction p1 p2 b args = do
      maybeUpdatedExpr <- case b of
        S.BuiltinFunction S.Not -> case compileNot logic of
          TryToEliminate -> Just <$> lowerNot p2 (argExpr $ head args)
          UnaryNot {} -> return Nothing
        _ -> return Nothing

      let unchangedExpr = normAppList p1 (Builtin p2 b) args
      return $ fromMaybe unchangedExpr maybeUpdatedExpr

    lowerNot :: Provenance -> Expr Ix Builtin -> m (Expr Ix Builtin)
    lowerNot notProv arg = case arg of
      -- Base cases
      BoolLiteral p b -> return $ BoolLiteral p (not b)
      OrderExpr p dom ord args -> return $ OrderExpr p dom (neg ord) args
      EqualityExpr p dom eq args -> return $ EqualityExpr p dom (neg eq) args
      NotExpr _ [e] -> return $ argExpr e
      -- Inductive cases
      ForallExpr p binder body -> ExistsExpr p binder <$> lowerNot notProv body
      ExistsExpr p binder body -> ForallExpr p binder <$> lowerNot notProv body
      ImpliesExpr p [e1, e2] -> do
        ne2 <- traverse (lowerNot notProv) e2
        return $ AndExpr p [e1, ne2]
      OrExpr p args -> AndExpr p <$> traverse (traverse (lowerNot notProv)) args
      AndExpr p args -> OrExpr p <$> traverse (traverse (lowerNot notProv)) args
      IfExpr p tRes [c, e1, e2] -> do
        ne1 <- traverse (lowerNot notProv) e1
        ne2 <- traverse (lowerNot notProv) e2
        return $ IfExpr p tRes [c, ne1, ne2]
      App p1 (FreeVar p2 ident) args
        | ident == identifierOf StdEqualsVector -> return $ App p1 (FreeVar p2 (identifierOf StdNotEqualsVector)) args
        | ident == identifierOf StdNotEqualsVector -> return $ App p1 (FreeVar p2 (identifierOf StdEqualsVector)) args
      -- Errors
      _ -> throwError $ UnsupportedNegatedOperation (logicID logic) notProv
