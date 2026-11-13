{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Backend.Loss.LogicCompilation
  ( findAndLiftLogic,
    isLogicDecl,
  )
where

import Control.Monad.Except (MonadError (..), runExceptT)
import Control.Monad.State (MonadState, StateT (..), modify)
import Data.List (find)
import Data.Map.Ordered.Strict (OMap, (|>))
import Data.Map.Ordered.Strict qualified as OMap
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Set qualified as Set
import Vehicle.Backend.Prelude (DifferentiableLogicID (..))
import Vehicle.Compile.Dependency (createAdjacencyGraph, topologicalSort)
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.Force
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyFriendly)
import Vehicle.Compile.Unblock (noUnblocking, unblockBoolExpr)
import Vehicle.Data.Builtin.Interface (Accessor (..))
import Vehicle.Data.Builtin.Loss (ComparisonOp (..))
import Vehicle.Data.Builtin.Standard (Builtin)
import Vehicle.Data.Code.ForcedValue
import Vehicle.Data.Code.Interface
import Vehicle.Data.DifferentiableLogic
import Vehicle.Data.Variable.Bound.Context.Name (runFreshNameBoundContextT)
import Vehicle.Data.Variable.Free.Context

--------------------------------------------------------------------------------
-- Interface

-- | Locates the requested differentiable logic and lifts it, and all its
-- dependencies, to the top of the program. The lifting is required because the
-- user may write  the logic at the bottom of the file with boolean operations
-- that need to be translated above it.
findAndLiftLogic ::
  (MonadCompile m) =>
  DifferentiableLogicID ->
  Prog Builtin ->
  m (Maybe (Identifier, Prog Builtin))
findAndLiftLogic logicID prog@(Main decls) = logCompilerPass LossLogic $ do
  (declMap, MonadLossState {..}) <- runMonadLossT $ searchDecls logicID prog

  case maybeImplementation of
    Nothing -> return Nothing
    Just definition -> do
      let logicIdent = identifierOf definition
      let logicDependencies = topologicalSort logicIdent (createAdjacencyGraph decls)
      newProg <- liftLogicAndDependencies declMap logicDependencies
      return $ Just (logicIdent, newProg)

--------------------------------------------------------------------------------
-- Monad

data MonadLossState = MonadLossState
  { maybeImplementation :: Maybe (Decl Builtin),
    foundLogics :: [Identifier]
  }

type MonadLoss m =
  ( MonadCompile m,
    MonadFreeContext Builtin m,
    MonadState MonadLossState m
  )

runMonadLossT ::
  (MonadCompile m) =>
  FreeContextT Builtin (StateT MonadLossState m) a ->
  m (a, MonadLossState)
runMonadLossT action = do
  let freshState = MonadLossState Nothing mempty
  flip runStateT freshState $
    runFreshFreeContextT
      (Proxy @Builtin)
      action

registerUnmatchedLogic ::
  (MonadLoss m) =>
  Identifier ->
  m ()
registerUnmatchedLogic ident = modify $
  \MonadLossState {..} -> do
    MonadLossState
      { foundLogics = ident : foundLogics,
        ..
      }

registerMatchedLogic ::
  (MonadLoss m) =>
  Decl Builtin ->
  m ()
registerMatchedLogic implementation = modify $
  \MonadLossState {..} -> do
    MonadLossState
      { maybeImplementation = Just implementation,
        ..
      }

--------------------------------------------------------------------------------
-- Monad

searchDecls ::
  forall m.
  (MonadLoss m) =>
  DifferentiableLogicID ->
  Prog Builtin ->
  m (OMap Identifier (Decl Builtin))
searchDecls logicID (Main ds) =
  logCompilerSection2 MidDetail ("search for logic" <+> quotePretty logicID) $ do
    runFreshFreeContextT (Proxy @Builtin) (go OMap.empty ds)
  where
    go :: OMap Identifier (Decl Builtin) -> [Decl Builtin] -> FreeContextT Builtin m (OMap Identifier (Decl Builtin))
    go seenDecls = \case
      [] -> return seenDecls
      decl : decls -> do
        searchLogicDecl logicID decl
        let newSeenDecls = seenDecls |> (identifierOf decl, decl)
        addDeclEntryToContext decl $ go newSeenDecls decls

searchLogicDecl ::
  (MonadLoss m) =>
  DifferentiableLogicID ->
  Decl Builtin ->
  m ()
searchLogicDecl logicID decl =
  case decl of
    DefFunction p ident _ann typ body -> do
      isLogic <- isLogicDecl typ
      if not isLogic
        then return ()
        else do
          if nameOf logicID /= nameOf ident
            then registerUnmatchedLogic ident
            else case body of
              Record _ _ fields -> do
                let declProv = (identifierOf decl, provenanceOf decl)
                checkLogicDirection declProv fields
                registerMatchedLogic decl
              _ -> throwError $ UnreducableDifferentiableLogic (ident, p)
          return ()
    _ -> return ()

isLogicDecl :: (MonadFreeContext Builtin m) => Type Builtin -> m Bool
isLogicDecl typ = do
  normType <- runFreshNameBoundContextT $ forceThunk $ Unforced @NoMeta emptyBoundEnv typ
  return $ case normType of
    VFreeVar ident [] -> nameOf ident `elem` ([elementLogicName, tensorLogicName] :: [Name])
    _ -> False

-- | Checks that the logic goes in the right direction by evaluating at `true < false`
checkLogicDirection ::
  (MonadLoss m) =>
  DeclProvenance ->
  RecordFields Builtin ->
  m ()
checkLogicDirection declProv fields = do
  let comparisonExpr = do
        let trueValue = lookupLogicField TruthityElement fields
        let falseValue = lookupLogicField FalsityElement fields
        let args =
              TensorComparisonArgs
                { tensorPointwiseDims = Forced IDimNil,
                  tensorReduceDims = Forced IDimNil,
                  tensorOp2Arg1 = Unforced emptyBoundEnv trueValue,
                  tensorOp2Arg2 = Unforced emptyBoundEnv falseValue
                }
        Forced $ mkExpr accessCompareRatTensor (Lt, args)

  logCompilerSection2 MinDetail "testing logic direction" $ do
    errorOrResult <- runExceptT $ runFreshNameBoundContextT $ forceThunk =<< unblockBoolExpr noUnblocking comparisonExpr
    case errorOrResult of
      Right (IBoolLiteral result) ->
        if result
          then return ()
          else throwError $ BackwardsDifferentiableLogic declProv comparisonExpr
      Left blockingErr -> throwError $ UnorderableDifferentiableLogic declProv comparisonExpr (Left blockingErr)
      Right result -> throwError $ UnorderableDifferentiableLogic declProv comparisonExpr (Right result)

lookupLogicField :: TensorDifferentiableLogicField -> RecordFields Builtin -> Expr Builtin
lookupLogicField field logicFields = do
  let maybeResult = find (\(f, _) -> nameOf f == nameOf field) logicFields
  let missingError = developerError $ "logic missing field" <+> squotes (pretty (nameOf field))
  snd $ fromMaybe missingError maybeResult

--------------------------------------------------------------------------------
-- Lifting

liftLogicAndDependencies ::
  (MonadLogger m) =>
  OMap Identifier (Decl Builtin) ->
  [Identifier] ->
  m (Prog Builtin)
liftLogicAndDependencies orderedProg logicAndDependencies =
  logCompilerSection2 MinDetail "lifting logic and dependencies" $ do
    -- Extract the dependencies
    let initialDecls = do
          let handleMissingDecl = fromMaybe (developerError "Missing decl in orderedProg")
          fmap (\ident -> handleMissingDecl $ OMap.lookup ident orderedProg) logicAndDependencies

    -- Filter the dependencies out of the remaining program
    let remainingDecls = do
          let logicAndDependenciesSet = Set.fromList logicAndDependencies
          let orderedRemainingDecls = OMap.filter (\i _ -> i `Set.notMember` logicAndDependenciesSet) orderedProg
          snd <$> OMap.assocs orderedRemainingDecls

    -- Reconstruct the new program
    let liftedProg = Main $ initialDecls <> remainingDecls
    logDebug MidDetail $ prettyFriendly liftedProg
    return liftedProg
