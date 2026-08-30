module Vehicle.Prelude.Logging.Class
  ( CompilerPass (..),
    allCompilerPasses,
    CompilerStack,
    LoggingLevel (..),
    emptyStack,
    stackMatches,
    consStack,
    defaultLoggingLevel,
    allLoggingLevels,
    DebugMessage,
    MonadLogger (..),
    logDebug,
    logDebugM,
    logCompilerPass,
    logCompilerPassOutput,
    logCompilerSection,
    logCompilerSection2,
    logCompileDecl,
    logIndent,
  )
where

import Control.Monad (when)
import Control.Monad.Except (ExceptT (..), mapExceptT)
import Control.Monad.Identity (IdentityT (..), mapIdentityT)
import Control.Monad.Reader (ReaderT (..), mapReaderT)
import Control.Monad.State (StateT (..), mapStateT)
import Control.Monad.Trans (MonadTrans (..))
import Control.Monad.Trans.Maybe (MaybeT, mapMaybeT)
import Control.Monad.Writer.Strict (WriterT (..), mapWriterT)
import Data.List.Split (splitOn)
import Data.Text (Text)
import Data.Text qualified as Text (unpack)
import System.Console.ANSI (Color (..))
import Text.Read (readMaybe)
import Vehicle.Data.AST.Decl (GenericDecl)
import Vehicle.Data.AST.Name (HasIdentifier (identifierOf), Identifier, nameOf)
import Vehicle.Data.MaybeTrivial (MaybeTrivialT, mapMaybeTrivialT)
import Vehicle.Prelude.Misc (enumerate, setTextColour)
import Vehicle.Prelude.Prettyprinter
import Vehicle.Prelude.Supply (SupplyT, mapSupplyT)
import Vehicle.Prelude.Warning

--------------------------------------------------------------------------------
-- Settings

data CompilerPass
  = Scoping
  | Typing
  | Solver
  | ITP
  | LossBounds
  | LossLogic
  | TypingSubsystem
  | Verification
  deriving (Eq, Show, Read, Bounded, Enum)

instance Pretty CompilerPass where
  pretty = \case
    Scoping -> "scope checking"
    Typing -> "type checking"
    Solver -> "solver compilation"
    ITP -> "ITP compilation"
    LossBounds -> "loss bounds compilation"
    LossLogic -> "loss logic compilation"
    TypingSubsystem -> "subsystem type checking"
    Verification -> "actual verification"

allCompilerPasses :: [String]
allCompilerPasses = map show (enumerate @CompilerPass)

newtype CompilerStack = CS [Either CompilerPass String]
  deriving (Show, Eq)

instance Read CompilerStack where
  readsPrec _r s = do
    let parts = splitOn "." s
    let items = fmap (\v -> maybe (Right v) Left (readMaybe @CompilerPass v)) parts
    return (CS (reverse items), [])

emptyStack :: CompilerStack
emptyStack = CS []

consStack :: Either CompilerPass String -> CompilerStack -> CompilerStack
consStack item (CS items) = CS (item : items)

stackMatches :: CompilerStack -> CompilerStack -> Bool
stackMatches (CS _) (CS []) = True
stackMatches (CS []) (CS (_ : _)) = False
stackMatches (CS (d : ds)) (CS (t : ts))
  | d == t = stackMatches (CS ds) (CS ts)
  | otherwise = stackMatches (CS ds) (CS (t : ts))

--------------------------------------------------------------------------------
-- Logging levels

data LoggingLevel
  = NoDetail
  | MinDetail
  | MidDetail
  | MaxDetail
  deriving (Eq, Ord, Show, Read, Bounded, Enum)

allLoggingLevels :: [String]
allLoggingLevels = map show (enumerate @LoggingLevel)

defaultLoggingLevel :: LoggingLevel
defaultLoggingLevel = NoDetail

--------------------------------------------------------------------------------
-- Messages

newtype DebugMessage = DebugMessage Text

instance Show DebugMessage where
  show (DebugMessage t) = setTextColour Green $ Text.unpack t

type CallDepth = Int

--------------------------------------------------------------------------------
-- Logging monad

class (Monad m) => MonadLogger m where
  runCompilerPass :: CompilerPass -> m a -> m a
  runCompileDecl :: Identifier -> m a -> m a
  setCallDepth :: CallDepth -> m ()
  getCallDepth :: m CallDepth
  incrCallDepth :: m ()
  decrCallDepth :: m ()
  getDebugLevel :: m LoggingLevel
  logMessage :: DebugMessage -> m ()
  logWarning :: CompileWarning -> m ()

instance (MonadLogger m) => MonadLogger (StateT s m) where
  runCompilerPass = mapStateT . runCompilerPass
  runCompileDecl = mapStateT . runCompileDecl
  setCallDepth = lift . setCallDepth
  getCallDepth = lift getCallDepth
  incrCallDepth = lift incrCallDepth
  decrCallDepth = lift decrCallDepth
  getDebugLevel = lift getDebugLevel
  logMessage = lift . logMessage
  logWarning = lift . logWarning

instance (MonadLogger m) => MonadLogger (ReaderT s m) where
  runCompilerPass = mapReaderT . runCompilerPass
  runCompileDecl = mapReaderT . runCompileDecl
  setCallDepth = lift . setCallDepth
  getCallDepth = lift getCallDepth
  incrCallDepth = lift incrCallDepth
  decrCallDepth = lift decrCallDepth
  getDebugLevel = lift getDebugLevel
  logMessage = lift . logMessage
  logWarning = lift . logWarning
  {-# INLINEABLE runCompilerPass #-}
  {-# INLINEABLE runCompileDecl #-}
  {-# INLINEABLE setCallDepth #-}
  {-# INLINEABLE getCallDepth #-}
  {-# INLINEABLE incrCallDepth #-}
  {-# INLINEABLE decrCallDepth #-}
  {-# INLINEABLE getDebugLevel #-}
  {-# INLINEABLE logMessage #-}
  {-# INLINEABLE logWarning #-}

instance (Monoid w, MonadLogger m) => MonadLogger (WriterT w m) where
  runCompilerPass = mapWriterT . runCompilerPass
  runCompileDecl = mapWriterT . runCompileDecl
  setCallDepth = lift . setCallDepth
  getCallDepth = lift getCallDepth
  incrCallDepth = lift incrCallDepth
  decrCallDepth = lift decrCallDepth
  getDebugLevel = lift getDebugLevel
  logMessage = lift . logMessage
  logWarning = lift . logWarning

instance (MonadLogger m) => MonadLogger (ExceptT e m) where
  runCompilerPass = mapExceptT . runCompilerPass
  runCompileDecl = mapExceptT . runCompileDecl
  setCallDepth = lift . setCallDepth
  getCallDepth = lift getCallDepth
  incrCallDepth = lift incrCallDepth
  decrCallDepth = lift decrCallDepth
  getDebugLevel = lift getDebugLevel
  logMessage = lift . logMessage
  logWarning = lift . logWarning

instance (MonadLogger m) => MonadLogger (SupplyT s m) where
  runCompilerPass = mapSupplyT . runCompilerPass
  runCompileDecl = mapSupplyT . runCompileDecl
  setCallDepth = lift . setCallDepth
  getCallDepth = lift getCallDepth
  incrCallDepth = lift incrCallDepth
  decrCallDepth = lift decrCallDepth
  getDebugLevel = lift getDebugLevel
  logMessage = lift . logMessage
  logWarning = lift . logWarning

instance (MonadLogger m) => MonadLogger (IdentityT m) where
  runCompilerPass = mapIdentityT . runCompilerPass
  runCompileDecl = mapIdentityT . runCompileDecl
  setCallDepth = lift . setCallDepth
  getCallDepth = lift getCallDepth
  incrCallDepth = lift incrCallDepth
  decrCallDepth = lift decrCallDepth
  getDebugLevel = lift getDebugLevel
  logMessage = lift . logMessage
  logWarning = lift . logWarning

instance (MonadLogger m) => MonadLogger (MaybeT m) where
  runCompilerPass = mapMaybeT . runCompilerPass
  runCompileDecl = mapMaybeT . runCompileDecl
  setCallDepth = lift . setCallDepth
  getCallDepth = lift getCallDepth
  incrCallDepth = lift incrCallDepth
  decrCallDepth = lift decrCallDepth
  getDebugLevel = lift getDebugLevel
  logMessage = lift . logMessage
  logWarning = lift . logWarning

instance (MonadLogger m) => MonadLogger (MaybeTrivialT m) where
  runCompilerPass = mapMaybeTrivialT . runCompilerPass
  runCompileDecl = mapMaybeTrivialT . runCompileDecl
  setCallDepth = lift . setCallDepth
  getCallDepth = lift getCallDepth
  incrCallDepth = lift incrCallDepth
  decrCallDepth = lift decrCallDepth
  getDebugLevel = lift getDebugLevel
  logMessage = lift . logMessage
  logWarning = lift . logWarning

logDebugM :: (MonadLogger m) => LoggingLevel -> m (Doc a) -> m ()
logDebugM level getText = do
  -- traceShow text $ do
  debugLevel <- getDebugLevel
  when (level <= debugLevel) $ do
    text <- getText
    depth <- getCallDepth
    logMessage $ DebugMessage (layoutAsText (indent depth text))

-- TODO try implement via logDebugM but check performance first.
logDebug :: (MonadLogger m) => LoggingLevel -> Doc a -> m ()
logDebug level text = logDebugM level (return text)

logCompilerPass :: (MonadLogger m) => CompilerPass -> m b -> m b
logCompilerPass pass performPass = do
  runCompilerPass pass $ do
    let passName = pretty pass
    result <- logIndent MinDetail ("Starting" <+> passName) performPass
    logDebug MinDetail $ "Finished" <+> passName <> line
    return result

logCompileDecl :: (MonadLogger m) => Doc a -> GenericDecl builtin -> m b -> m b
logCompileDecl action decl performPass = do
  runCompileDecl (identifierOf decl) $ do
    let text = action <+> quotePretty (nameOf decl)
    result <- logIndent MidDetail ("Starting" <+> text) performPass
    logDebug MidDetail $ "Finished" <+> text <> line
    return result

logCompilerSection2 :: (MonadLogger m) => LoggingLevel -> Doc a -> m b -> m b
logCompilerSection2 level passName performPass = do
  result <- logIndent level ("Starting" <+> passName) performPass
  logDebug level $ "Finished" <+> passName
  return result

logCompilerSection :: (MonadLogger m) => LoggingLevel -> Doc a -> m b -> m b
logCompilerSection level sectionName performPass = do
  result <- logIndent level sectionName performPass
  logDebug level ""
  return result

logIndent :: (MonadLogger m) => LoggingLevel -> Doc a -> m b -> m b
logIndent level sectionName performPass = do
  logDebug level sectionName
  incrCallDepth
  result <- performPass
  decrCallDepth
  return result

logCompilerPassOutput :: (MonadLogger m) => Doc a -> m ()
logCompilerPassOutput result = do
  logDebug MidDetail "Result:"
  incrCallDepth
  logDebug MidDetail result
  decrCallDepth
