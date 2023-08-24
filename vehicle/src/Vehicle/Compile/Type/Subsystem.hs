module Vehicle.Compile.Type.Subsystem
  ( typeCheckWithSubsystem,
    resolveInstanceArguments,
  )
where

import Control.Monad.Except (runExceptT)
import Data.List.NonEmpty qualified as NonEmpty
import Vehicle.Compile.Error
import Vehicle.Compile.Monomorphisation (monomorphise, removeLiteralCoercions)
import Vehicle.Compile.Normalise.NBE (findInstanceArg)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyExternal)
import Vehicle.Compile.Type (typeCheckProg)
import Vehicle.Compile.Type.Core (InstanceCandidateDatabase)
import Vehicle.Compile.Type.Irrelevance (removeIrrelevantCodeFromProg)
import Vehicle.Compile.Type.Monad
import Vehicle.Compile.Type.Subsystem.Standard
import Vehicle.Expr.BuiltinInterface
import Vehicle.Expr.DeBruijn

typeCheckWithSubsystem ::
  forall builtin m.
  (HasTypeSystem builtin, MonadCompile m) =>
  InstanceCandidateDatabase builtin ->
  (forall a. CompileError -> m a) ->
  Prog Ix Builtin ->
  m (Prog Ix builtin)
typeCheckWithSubsystem instanceCandidates errorHandler prog = do
  typeClassFreeProg <- resolveInstanceArguments prog
  irrelevantFreeProg <- removeIrrelevantCodeFromProg typeClassFreeProg
  monomorphisedProg <- monomorphise isPropertyDecl "-" irrelevantFreeProg
  literalFreeProg <- removeLiteralCoercions "-" monomorphisedProg
  implicitFreeProg <- removeImplicitAndInstanceArgs literalFreeProg

  resultOrError <- runExceptT $ typeCheckProg instanceCandidates mempty implicitFreeProg
  case resultOrError of
    Right value -> return value
    Left err -> errorHandler err

resolveInstanceArguments ::
  forall m builtin.
  (MonadCompile m, HasStandardData builtin, Show builtin) =>
  Prog Ix builtin ->
  m (Prog Ix builtin)
resolveInstanceArguments prog =
  logCompilerPass MaxDetail "resolution of instance arguments" $ do
    flip traverseDecls prog $ \decl -> do
      result <- traverse (traverseBuiltinsM builtinUpdateFunction) decl
      return result
  where
    builtinUpdateFunction :: BuiltinUpdate m Ix builtin builtin
    builtinUpdateFunction p1 p2 b args
      | isTypeClassOp b = do
          (inst, remainingArgs) <- findInstanceArg b args
          return $ substArgs p1 inst remainingArgs
      | otherwise = return $ normAppList p1 (Builtin p2 b) args

removeImplicitAndInstanceArgs ::
  forall m builtin.
  (MonadCompile m, PrintableBuiltin builtin) =>
  Prog Ix builtin ->
  m (Prog Ix builtin)
removeImplicitAndInstanceArgs prog =
  logCompilerPass MaxDetail "removal of implicit arguments" $ do
    result <- traverse go prog
    logCompilerPassOutput $ prettyExternal result
    return result
  where
    go :: Expr Ix builtin -> m (Expr Ix builtin)
    go expr = case expr of
      App p fun args -> do
        fun' <- go fun
        let nonImplicitArgs = NonEmpty.filter isExplicit args
        nonImplicitArgs' <- traverse (traverse go) nonImplicitArgs
        return $ normAppList p fun' nonImplicitArgs'
      BoundVar {} -> return expr
      FreeVar {} -> return expr
      Universe {} -> return expr
      Meta {} -> return expr
      Hole {} -> return expr
      Builtin {} -> return expr
      Pi p binder res -> Pi p <$> traverse go binder <*> go res
      Lam p binder body
        | isExplicit binder || not (isTypeUniverse (typeOf binder)) ->
            Lam p <$> traverse go binder <*> go body
        | otherwise -> do
            -- TODO This is a massive hack to get around the unused implicit
            -- {l} argument in `mapVector` in the standard library that isn't
            -- handled by monomorphisation.
            -- STILL NEEDED?
            body' <- go body
            let removedBody = Hole p "_" `substDBInto` body'
            return removedBody
      Let p bound binder body -> Let p <$> go bound <*> traverse go binder <*> go body
