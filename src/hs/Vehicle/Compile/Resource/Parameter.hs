module Vehicle.Compile.Resource.Parameter
  ( expandParameters
  ) where

import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Except
import Data.Map qualified as Map
import Data.Text (unpack)
import Data.Set (Set)
import Data.Set qualified as Set

import Vehicle.Compile.Prelude
import Vehicle.Compile.Error
import Vehicle.Compile.Elaborate.External
import Vehicle.Compile.Parse

expandParameters :: MonadCompile m
                 => ParameterValues
                 -> InputProg
                 -> m InputProg
expandParameters params prog = do
  logDebug MinDetail $ "Beginning" <+> phase
  incrCallDepth

  (result, foundParams) <- runWriterT (runReaderT (expand prog) params)
  warnIfUnusedResources Parameter (Map.keysSet params) foundParams

  decrCallDepth
  logDebug MinDetail $ "Finished" <+> phase <> line
  return result

phase :: Doc a
phase = "insertion of parameters"

type MonadExpand m =
  ( MonadCompile m
  , MonadReader ParameterValues m
  , MonadWriter (Set Symbol) m
  )

class Expand a where
  expand :: MonadExpand m => a -> m a

instance Expand InputProg where
  expand (Main ds) = Main <$> traverse expand ds

instance Expand InputDecl where
  expand (DefResource p Parameter ident (t :: InputExpr)) = do
    let name = nameOf ident
    maybeValue <- asks (Map.lookup name)
    body <- case maybeValue of
      Nothing    -> throwError $ ResourceNotProvided ident p Parameter
      Just value -> case parseExternalExpr value of
        Left  _ -> throwError $ UnableToParseResource ident p Parameter (unpack value)
        Right e -> do
          v <- fmap (const (parameterProvenance name)) <$> elabExpr e
          logDebug MinDetail $ "inserting" <+> pretty ident <+> "=" <+> pretty value
          tell (Set.singleton name)
          return v

    return $ DefFunction p ident t body

  expand d = return d