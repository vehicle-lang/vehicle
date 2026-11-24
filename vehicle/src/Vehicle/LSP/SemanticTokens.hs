module Vehicle.LSP.SemanticTokens
  ( collectSemanticTokens,
    LspErrorMessage,
  )
where

import Colog.Core (LogAction, Severity (..), WithSeverity (..), (<&))
import Control.Lens ((^.))
import Control.Monad.Reader (runReaderT)
import Data.List
import Data.Map (elems)
import Data.Maybe (catMaybes)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Language.LSP.Protocol.Lens
import Language.LSP.Protocol.Types as Lsp
  ( SemanticTokenAbsolute (..),
    SemanticTokens,
    defaultSemanticTokensLegend,
    makeSemanticTokens,
  )
import Vehicle.Compile.Print.Error (FormattableError (..), logCompileError)
import Vehicle.Data.AST.Expr.Scoped
import Vehicle.Data.AST.Prog (GenericProg (..))
import Vehicle.Data.Builtin.Standard
  ( Builtin (..),
  )
import Vehicle.Data.Variable.Bound.Context.Generic (runFreshBoundContextT)
import Vehicle.Data.Variable.Free.Context (MonadFreeContext, addDeclToContext, runFreshFreeContextT)
import Vehicle.LSP.SemanticTokens.Internal (collectDeclsTokens)
import Vehicle.LSP.State (MonadVehicleLsp)
import Vehicle.Prelude.Logging (runSilentLoggerT)
import Vehicle.Prelude.Prettyprinter (Pretty (..), layoutAsText)
import Vehicle.TypeCheck (loadUserSpecification)

--------------------------------------------------------------------------------
-- API

-- Inherited from `Language.LSP.Protocol.Types.SemanticTokens.makeSemanticTokens`,
-- type-aliased for readability.
type LspErrorMessage = Text

collectSemanticTokens ::
  (MonadVehicleLsp m) =>
  LogAction m (WithSeverity Text) ->
  FilePath ->
  m (Either LspErrorMessage Lsp.SemanticTokens)
collectSemanticTokens logger filePath = do
  (errorOrToks, _warnings) <-
    runSilentLoggerT $ logCompileError $ do
      -- TODO: This means a specification has to successfully type check before we can
      -- produce semantic tokens for it, which sucks. We should just do scope checking
      -- here instead.
      (Main userDecls, builtinModules, _) <- loadUserSpecification filePath
      let builtinDecls = concat (elems builtinModules)

      maybeToks <-
        runFreshFreeContextT (Proxy @Builtin) $
          populateFreeContext builtinDecls $
            runFreshBoundContextT (Proxy @(Expr Builtin)) $
              flip runReaderT filePath $
                collectDeclsTokens userDecls

      return $ catMaybes maybeToks

  case errorOrToks of
    Left err -> do
      logger <& (layoutAsText $ pretty $ formatError err) `WithSeverity` Warning
      return $ makeLspSemanticTokens []
    Right toks ->
      return $ makeLspSemanticTokens toks
  where
    makeLspSemanticTokens :: [Lsp.SemanticTokenAbsolute] -> Either LspErrorMessage Lsp.SemanticTokens
    makeLspSemanticTokens tokens = do
      let sortedTokens = sortOn (\t -> (t ^. line, t ^. startChar)) tokens
      makeSemanticTokens defaultSemanticTokensLegend sortedTokens

    populateFreeContext :: (MonadFreeContext Builtin n) => [Decl Builtin] -> n a -> n a
    populateFreeContext [] f = f
    populateFreeContext (d : ds) f = addDeclToContext d $ populateFreeContext ds f
