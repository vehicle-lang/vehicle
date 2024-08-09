module Vehicle.Backend.LossFunction
  ( convertToLossTensors,
  )
where

import Data.Maybe (maybeToList)
import Data.Proxy (Proxy (..))
import Vehicle.Backend.LossFunction.Core (DifferentiableLogicImplementation, MixedLossValue, preservedStdLibOps)
import Vehicle.Backend.LossFunction.TensorCompilation (convertExprToTensorValue, runMonadTensorT)
import Vehicle.Backend.Prelude (DifferentiableLogicID)
import Vehicle.Compile.Context.Bound.Class (MonadBoundContext)
import Vehicle.Compile.Context.Bound.Instance (runFreshBoundContextT)
import Vehicle.Compile.Context.Free (MonadFreeContext, addDeclEntryToContext, hideStdLibDecls, runFreshFreeContextT)
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.NBE (normaliseInEnv)
import Vehicle.Compile.Normalise.Quote qualified as Quote
import Vehicle.Compile.Prelude
import Vehicle.Data.Builtin.Tensor (TensorBuiltin)
import Vehicle.Syntax.Builtin

convertToLossTensors ::
  (MonadCompile m) =>
  DifferentiableLogicID ->
  DifferentiableLogicImplementation ->
  Prog Ix Builtin ->
  m (Prog Ix TensorBuiltin)
convertToLossTensors logicID logic (Main ds) =
  logCompilerPass MinDetail currentPass $
    runFreshFreeContextT (Proxy @Builtin) $
      runFreshBoundContextT (Proxy @MixedLossValue) $
        Main <$> convertDecls logicID logic ds

convertDecls ::
  (MonadCompile m, MonadFreeContext Builtin m, MonadBoundContext MixedLossValue m) =>
  DifferentiableLogicID ->
  DifferentiableLogicImplementation ->
  [Decl Ix Builtin] ->
  m [Decl Ix TensorBuiltin]
convertDecls logicID logic = \case
  [] -> return []
  decl : decls -> do
    let ident = identifierOf decl
    let declProv = (ident, provenanceOf decl)

    (normDecl, maybeTensorDecl) <-
      logCompilerPass MinDetail ("declaration" <+> quotePretty ident) $ do
        hideStdLibDecls (Proxy @Builtin) preservedStdLibOps $ do
          -- Deciding on the best ordering of converting to loss functions and converting to tensor code
          -- is tricky. There are three approaches:
          --
          -- Loss functions -> Tensors
          --    Disadvantages
          --        - Vector representation contains higher order structure (e.g. folds) that
          --          can be converted to tensor operations, but which `not` cannot easily
          --          be pushed through in general for DL2 loss. e.g. in
          --          fold (\ x -> \ y -> x and y) True (foreachIndex 2 (\ i -> - 3.25 <= x ! i and x ! i <= 3.25))
          --
          -- Tensors -> Loss functions
          --    Disadvantages
          --        - Loss functions need to be specified in terms of tensors.
          --        - Can't reuse not/if-elimination code
          normStandardDecl <- traverse (normaliseInEnv mempty) decl
          maybeTensorDecl <-
            if not (isPropertyDecl decl) && not (isAbstractDecl decl)
              then return Nothing
              else do
                normTensorDecl <-
                  runMonadTensorT logicID declProv logic $
                    traverse (convertExprToTensorValue mempty) decl
                let tensorDecl = fmap (Quote.unnormalise 0) normTensorDecl
                return $ Just tensorDecl

          return
            ( normStandardDecl,
              maybeTensorDecl
            )

    addDeclEntryToContext (decl, normDecl) $ do
      decls' <- convertDecls logicID logic decls
      return $ maybeToList maybeTensorDecl ++ decls'

currentPass :: Doc a
currentPass = "loss function compilation"
