module Vehicle.Backend.LossFunction
  ( convertToLossTensors,
  )
where

import Data.Maybe (maybeToList)
import Data.Proxy (Proxy (..))
import Vehicle.Backend.LossFunction.Core (CompiledDifferentiableLogic)
import Vehicle.Backend.LossFunction.LossCompilation (runMonadLogicT)
import Vehicle.Backend.LossFunction.LossCompilation qualified as Loss (convertValue)
import Vehicle.Compile.Context.Free (MonadFreeContext, addDeclEntryToContext, runFreshFreeContextT)
import Vehicle.Compile.Context.Name (MonadNameContext, runFreshNameContextT)
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.NBE (normaliseInEnv)
import Vehicle.Compile.Normalise.Quote (unnormalise)
import Vehicle.Compile.Prelude
import Vehicle.Data.Builtin.Core
import Vehicle.Data.Builtin.Loss (LossBuiltin)
import Vehicle.Data.Builtin.Standard.Normalise ()

convertToLossTensors ::
  (MonadCompile m) =>
  CompiledDifferentiableLogic ->
  Prog Builtin ->
  m (Prog LossBuiltin)
convertToLossTensors logic (Main ds) =
  logCompilerPass MinDetail currentPass $
    runFreshFreeContextT (Proxy @Builtin) $
      runFreshNameContextT $
        Main <$> convertDecls logic ds

convertDecls ::
  (MonadCompile m, MonadFreeContext Builtin m, MonadNameContext m) =>
  CompiledDifferentiableLogic ->
  [Decl Builtin] ->
  m [Decl LossBuiltin]
convertDecls logic = \case
  [] -> return []
  decl : decls -> do
    (normDecl, maybeLossTensorDecl) <- do
      let ident = identifierOf decl
      logCompilerPass MinDetail ("declaration" <+> quotePretty ident) $ do
        normStandardDecl <- traverse (normaliseInEnv mempty) decl
        maybeTensorDecl <-
          if not (isPropertyDecl decl)
            then return Nothing
            else do
              let declProv = (ident, provenanceOf decl)
              normLossTensorDecl <- runMonadLogicT logic declProv $ traverse Loss.convertValue normStandardDecl
              let lossTensorDecl = fmap (unnormalise 0) normLossTensorDecl
              return $ Just lossTensorDecl

        return
          ( normStandardDecl,
            maybeTensorDecl
          )

    decls' <- addDeclEntryToContext (decl, normDecl) $ convertDecls logic decls
    return $ maybeToList maybeLossTensorDecl ++ decls'

currentPass :: Doc a
currentPass = "loss function compilation"
