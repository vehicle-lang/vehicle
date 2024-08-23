module Vehicle.Backend.LossFunction
  ( convertToLossTensors,
  )
where

import Data.Maybe (maybeToList)
import Data.Proxy (Proxy (..))
import Vehicle.Backend.LossFunction.Core (CompiledDifferentiableLogic, preservedStdLibOps)
import Vehicle.Backend.LossFunction.LossCompilation (runMonadLogicT)
import Vehicle.Backend.LossFunction.LossCompilation qualified as Loss (convertValue)
import Vehicle.Backend.LossFunction.TensorCompilation (convertExpr, runMonadTensorT)
import Vehicle.Backend.LossFunction.ZeroTensorLifting (liftZeroDimensionalTensors)
import Vehicle.Compile.Context.Free (MonadFreeContext, addDeclEntryToContext, hideStdLibDecls, runFreshFreeContextT)
import Vehicle.Compile.Context.Name (MonadNameContext, runFreshNameContextT)
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.NBE (normaliseInEnv)
import Vehicle.Compile.Normalise.Quote (unnormalise)
import Vehicle.Compile.Prelude
import Vehicle.Data.Builtin.Core
import Vehicle.Data.Builtin.Loss (LossTensorBuiltin)

convertToLossTensors ::
  (MonadCompile m) =>
  CompiledDifferentiableLogic ->
  Prog Builtin ->
  m (Prog LossTensorBuiltin)
convertToLossTensors logic (Main ds) =
  logCompilerPass MinDetail currentPass $
    runFreshFreeContextT (Proxy @Builtin) $
      runFreshNameContextT $
        Main <$> convertDecls logic ds

convertDecls ::
  (MonadCompile m, MonadFreeContext Builtin m, MonadNameContext m) =>
  CompiledDifferentiableLogic ->
  [Decl Builtin] ->
  m [Decl LossTensorBuiltin]
convertDecls logic = \case
  [] -> return []
  decl : decls -> do
    (normDecl, maybeLossTensorDecl) <- do
      let ident = identifierOf decl
      logCompilerPass MinDetail ("declaration" <+> quotePretty ident) $ do
        hideStdLibDecls (Proxy @Builtin) preservedStdLibOps $ do
          normStandardDecl <- traverse (normaliseInEnv mempty) decl
          maybeTensorDecl <-
            if not (isPropertyDecl decl)
              then return Nothing
              else do
                let declProv = (ident, provenanceOf decl)
                tensorDecl <- runMonadTensorT declProv $ traverse (convertExpr mempty) decl
                normLossTensorDecl <- runMonadLogicT logic declProv $ traverse Loss.convertValue tensorDecl
                liftedTensorDecl <- liftZeroDimensionalTensors normLossTensorDecl
                let lossTensorDecl = fmap (unnormalise 0) liftedTensorDecl
                return $ Just lossTensorDecl

          return
            ( normStandardDecl,
              maybeTensorDecl
            )

    decls' <- addDeclEntryToContext (decl, normDecl) $ convertDecls logic decls
    return $ maybeToList maybeLossTensorDecl ++ decls'

currentPass :: Doc a
currentPass = "loss function compilation"
