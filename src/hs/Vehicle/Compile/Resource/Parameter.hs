module Vehicle.Compile.Resource.Parameter
  ( expandParameters
  ) where

import Control.Monad.Reader
import Control.Monad.Except
import Data.Map qualified as Map
import Data.Text (unpack)

import Vehicle.Compile.Prelude
import Vehicle.Compile.Error
import Vehicle.Compile.Elaborate.External
import Vehicle.Compile.Parse

expandParameters :: MonadCompile m
                 => ParameterValues
                 -> InputProg
                 -> m InputProg
expandParameters params prog = do
  logDebug $ "Beginning" <+> phase
  incrCallDepth

  result <- runReaderT (expand prog) params

  decrCallDepth
  logDebug $ "Finished" <+> phase <> line
  logDebug $ pretty $ show prog
  return result

phase :: Doc a
phase = "insertion of parameters"

class Expand a where
  expand :: (MonadCompile m, MonadReader ParameterValues m) => a -> m a

instance Expand InputProg where
  expand (Main ds) = Main <$> traverse expand ds

instance Expand InputDecl where
  expand (DefResource p Parameter ident (t :: InputExpr)) = do
    maybeValue <- asks (Map.lookup (nameOf ident))
    body <- case maybeValue of
      Nothing    -> throwError $ ResourceNotProvided ident p Parameter
      Just value -> case parseExternalExpr value of
        Left  _ -> throwError $ UnableToParseResource ident p Parameter (unpack value)
        Right e -> do
          v <- elabExpr e
          logDebug $ "inserting" <+> pretty ident <+> "=" <+> pretty value
          return v

    return $ DefFunction p ident t body

  expand d = return d