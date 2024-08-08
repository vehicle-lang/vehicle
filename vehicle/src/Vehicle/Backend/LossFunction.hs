module Vehicle.Backend.LossFunction
  ( convertToLossTensors,
  )
where

import Data.Map qualified as Map
import Data.Maybe (maybeToList)
import Vehicle.Backend.LossFunction.Core (DifferentiableLogicImplementation, MixedFreeEnv)
import Vehicle.Backend.LossFunction.TensorCompilation (convertExprToTensorValue, runMonadTensorT)
import Vehicle.Backend.Prelude (DifferentiableLogicID)
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.NBE (eval)
import Vehicle.Compile.Normalise.Quote qualified as Quote
import Vehicle.Compile.Prelude
import Vehicle.Data.Builtin.Tensor (TensorBuiltin)
import Vehicle.Data.Expr.Normalised (WHNFClosure)
import Vehicle.Syntax.Builtin

convertToLossTensors ::
  (MonadCompile m) =>
  DifferentiableLogicID ->
  DifferentiableLogicImplementation ->
  Prog Ix Builtin ->
  m (Prog Ix TensorBuiltin)
convertToLossTensors logicID logic (Main ds) =
  logCompilerPass MinDetail currentPass $ do
    Main <$> convertDecls logicID logic mempty mempty ds

convertDecls ::
  (MonadCompile m) =>
  DifferentiableLogicID ->
  DifferentiableLogicImplementation ->
  FreeEnv (WHNFClosure Builtin) Builtin ->
  MixedFreeEnv ->
  [Decl Ix Builtin] ->
  m [Decl Ix TensorBuiltin]
convertDecls logicID logic standardFreeEnv lossFreeEnv = \case
  [] -> return []
  decl : decls -> do
    let ident = identifierOf decl
    let declProv = (ident, provenanceOf decl)

    (standardDecl, maybeTensorDecl) <-
      logCompilerPass MinDetail ("declaration" <+> quotePretty ident) $ do
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
        normStandardDecl <- traverse (eval standardFreeEnv mempty) decl
        maybeTensorDecl <-
          if not (isPropertyDecl decl) && not (isAbstractDecl decl)
            then return Nothing
            else do
              normTensorDecl <-
                runMonadTensorT logicID declProv logic standardFreeEnv lossFreeEnv $
                  traverse (convertExprToTensorValue mempty) decl
              let tensorDecl = fmap (Quote.unnormalise 0) normTensorDecl
              return $ Just tensorDecl

        return
          ( normStandardDecl,
            maybeTensorDecl
          )

    let newStandardFreeEnv = Map.insert ident standardDecl standardFreeEnv
    let newLossFreeEnv = lossFreeEnv -- Map.insert ident lossDecl lossFreeEnv
    decls' <- convertDecls logicID logic newStandardFreeEnv newLossFreeEnv decls
    return $ maybeToList maybeTensorDecl ++ decls'

currentPass :: Doc a
currentPass = "loss function compilation"
