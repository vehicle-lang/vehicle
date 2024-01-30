module Vehicle.Backend.Queries.UserVariableElimination.Unblocking
  ( tryUnblockBool,
  )
where

import Control.Monad.Reader (asks)
import Control.Monad.State (MonadState (..))
import Control.Monad.Writer (MonadWriter (..))
import Data.LinkedHashMap qualified as LinkedHashMap
import Data.Map qualified as Map
import Vehicle.Backend.Queries.UserVariableElimination.Core
import Vehicle.Compile.Context.Free (MonadFreeContext)
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.Builtin (traverseBuiltinBlockingArgs)
import Vehicle.Compile.Normalise.NBE
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyFriendly)
import Vehicle.Compile.Resource (NetworkTensorType (..), NetworkType (..))
import Vehicle.Compile.Type.Subsystem.Standard
import Vehicle.Data.BuiltinInterface.Value
import Vehicle.Data.NormalisedExpr
import Vehicle.Libraries.StandardLibrary.Definitions (StdLibFunction (StdAddVector, StdEqualsVector, StdNotEqualsVector, StdSubVector), findStdLibFunction)
import Vehicle.Verify.Core

--------------------------------------------------------------------------------
-- Main function

tryUnblockBool ::
  (MonadQueryStructure m, MonadWriter [WHNFValue QueryBuiltin] m) =>
  WHNFValue QueryBuiltin ->
  (WHNFValue QueryBuiltin -> m a) ->
  m a ->
  m a
tryUnblockBool expr success failure = do
  oldCtx <- getGlobalBoundCtx
  logDebug MaxDetail $ line <> "Trying to unblock" <+> squotes (prettyFriendly (WithContext expr oldCtx))
  incrCallDepth

  unblockedExpr <- unblockExpr False expr
  if expr /= unblockedExpr
    then do
      newCtx <- getGlobalBoundCtx
      logDebug MaxDetail $ "Unblocked to" <+> squotes (prettyFriendly (WithContext unblockedExpr newCtx))
      decrCallDepth
      success unblockedExpr
    else do
      logDebug MaxDetail "No progress made"
      decrCallDepth
      failure

--------------------------------------------------------------------------------
-- Recursive if-lifting operations

type MonadUnblock m = (MonadQueryStructure m, MonadWriter [WHNFValue QueryBuiltin] m)

type CompilingVecEq = Bool

-- | Lifts all `if`s in the provided expression `e` to the top-level, while
-- preserving the guarantee that the expression is normalised as much as
-- possible.
-- Does not recurse into function type arguments for higher-order builtins
-- such as `map` and `fold`.
unblockExpr ::
  (MonadUnblock m) =>
  CompilingVecEq ->
  WHNFValue QueryBuiltin ->
  m (WHNFValue QueryBuiltin)
unblockExpr compilingVecEq expr = case expr of
  ----------------------
  -- Impossible cases --
  ----------------------
  VPi {} -> unexpectedTypeInExprError currentPass "Pi"
  VUniverse {} -> unexpectedTypeInExprError currentPass "Universe"
  VMeta {} -> unexpectedExprError currentPass "Meta"
  -- The only way we can encounter a lambda in a normalised value is for
  -- it to be present as the argument for a `map`/`fold`. However, we
  -- explicitly don't recurse into those positions when we encounter those
  -- builtins.
  VLam {} -> unexpectedExprError currentPass "Lam"
  ----------------
  -- Base cases --
  ----------------
  VBoolLiteral {} -> return expr
  VAnd {} -> return expr
  VOr {} -> return expr
  VNot {} -> return expr
  VIf {} -> return expr
  VForall {} -> return expr
  VExists {} -> return expr
  ------------------
  -- Actual cases --
  ------------------
  VBoundVar lv []
    | compilingVecEq -> return $ VBoundVar lv []
    | otherwise -> unblockBoundVar lv
  VFreeVar v spine -> unblockFreeVar compilingVecEq v spine
  VBuiltin b spine -> unblockBuiltin compilingVecEq b spine
  VBoundVar v spine -> do
    ctx <- getGlobalBoundCtx
    compilerDeveloperError $
      "Found bound variable with arguments:"
        <> prettyFriendly (WithContext (VBoundVar v spine) ctx)

unblockFreeVar ::
  (MonadUnblock m) =>
  CompilingVecEq ->
  Identifier ->
  WHNFSpine QueryBuiltin ->
  m (WHNFValue QueryBuiltin)
unblockFreeVar compilingVecEq ident spine = do
  networkContext <- asks networkCtx
  case Map.lookup (nameOf ident) networkContext of
    Just networkInfo -> unblockNetwork ident networkInfo spine
    _ -> case findStdLibFunction ident of
      Just StdEqualsVector -> unblockVectorOp2 True StdEqualsVector spine
      Just StdNotEqualsVector -> unblockVectorOp2 True StdNotEqualsVector spine
      Just StdAddVector -> unblockVectorOp2 compilingVecEq StdAddVector spine
      Just StdSubVector -> unblockVectorOp2 compilingVecEq StdSubVector spine
      _ -> compilerDeveloperError $ "Unexpected free variable found while unblocking:" <+> pretty ident

unblockNetwork :: (MonadUnblock m) => Identifier -> NetworkContextInfo -> WHNFSpine QueryBuiltin -> m (WHNFValue QueryBuiltin)
unblockNetwork ident networkInfo spine = do
  unblockedSpine <- traverse (traverse (unblockExpr True)) spine
  liftIfSpine unblockedSpine $ \unblockedSpine' ->
    if unblockedSpine' /= unblockedSpine
      then return $ VFreeVar ident unblockedSpine'
      else do
        let networkName = nameOf ident
        let networkApp = (networkName, unblockedSpine')
        globalCtx <- get
        case spine of
          [inputArg] -> case LinkedHashMap.lookup networkApp (networkApplications globalCtx) of
            Just existingAppInfo ->
              return $ outputVarExpr existingAppInfo
            Nothing -> do
              let (appInfo, newGlobalCtx) = addNetworkApplicationToGlobalCtx networkApp networkInfo globalCtx
              let inputDims = dimensions (inputTensor (networkType networkInfo))
              let inputEquality = mkVVectorEquality inputDims (inputVarExpr appInfo) (argExpr inputArg)
              put newGlobalCtx
              tell [inputEquality]
              return $ outputVarExpr appInfo
          _ -> do
            ctx <- getGlobalBoundCtx
            compilerDeveloperError $
              "Found network application with multiple arguments:"
                <> line
                <> indent 2 (pretty networkName <> hsep (fmap (\arg -> prettyFriendly (WithContext (argExpr arg) ctx)) spine))

unblockVectorOp2 :: (MonadUnblock m) => CompilingVecEq -> StdLibFunction -> WHNFSpine QueryBuiltin -> m (WHNFValue QueryBuiltin)
unblockVectorOp2 compilingVecEq fn spine = do
  unblockedSpine <- traverse (traverseExplicitArgExpr (unblockExpr compilingVecEq)) spine
  liftIfSpine unblockedSpine $ \unblockedSpine' -> do
    if compilingVecEq && isBlockedVectorSpine unblockedSpine'
      then return $ VFreeVar (identifierOf fn) unblockedSpine'
      else appStdlibDef fn unblockedSpine'

isBlockedVectorSpine :: WHNFSpine QueryBuiltin -> Bool
isBlockedVectorSpine (reverse -> (argExpr -> VVecLiteral {}) : (argExpr -> VVecLiteral {}) : _) = False
isBlockedVectorSpine _ = True

unblockBuiltin ::
  (MonadUnblock m) =>
  CompilingVecEq ->
  QueryBuiltin ->
  WHNFSpine QueryBuiltin ->
  m (WHNFValue QueryBuiltin)
unblockBuiltin compilingVecEq b spine = case VBuiltin b spine of
  VAt _ _ xs i -> case distributeAt i xs of
    Just distributed -> do logDebug MaxDetail "Shortcut!"; unblockExpr compilingVecEq =<< distributed
    Nothing -> standard
  _ -> standard
  where
    standard = do
      unblockedSpine <- traverseBuiltinBlockingArgs (unblockExpr False) b spine
      liftIfSpine unblockedSpine (evalBuiltin b)

distributeAt :: forall m. (MonadFreeContext QueryBuiltin m) => WHNFValue QueryBuiltin -> WHNFValue QueryBuiltin -> Maybe (m (WHNFValue QueryBuiltin))
distributeAt index = \case
  VBuiltinFunction MapVector [t, _, n, f, xs] -> appIndex f [(t, n, xs)]
  VBuiltinFunction ZipWithVector [t1, t2, _, n, f, xs, ys] -> appIndex f [(t1, n, xs), (t2, n, ys)]
  VStandardLib StdAddVector [t1, t2, _, n, f, xs, ys] -> appIndex f [(t1, n, xs), (t2, n, ys)]
  VStandardLib StdSubVector [t1, t2, _, n, f, xs, ys] -> appIndex f [(t1, n, xs), (t2, n, ys)]
  _ -> Nothing
  where
    appIndex ::
      WHNFArg QueryBuiltin ->
      [(WHNFArg QueryBuiltin, WHNFArg QueryBuiltin, WHNFArg QueryBuiltin)] ->
      Maybe (m (WHNFValue QueryBuiltin))
    appIndex f args =
      Just $
        normaliseApp
          (argExpr f)
          ( flip fmap args $ \(t, n, xs) ->
              Arg mempty Explicit Relevant (VAt t n (argExpr xs) index)
          )

--------------------------------------------------------------------------------
-- Basic if-lifting operations

liftIf ::
  (MonadCompile m) =>
  (WHNFValue QueryBuiltin -> m (WHNFValue QueryBuiltin)) ->
  WHNFValue QueryBuiltin ->
  m (WHNFValue QueryBuiltin)
liftIf k (VBuiltinFunction If [t, cond, e1, e2]) = do
  e1' <- traverse (liftIf k) e1
  e2' <- traverse (liftIf k) e2
  return $ VBuiltinFunction If [t, cond, e1', e2']
liftIf k e = k e

liftIfArg ::
  (MonadCompile m) =>
  (WHNFArg QueryBuiltin -> m (WHNFValue QueryBuiltin)) ->
  WHNFArg QueryBuiltin ->
  m (WHNFValue QueryBuiltin)
liftIfArg k (Arg p v r e) = liftIf (k . Arg p v r) e

liftIfSpine ::
  (MonadUnblock m) =>
  WHNFSpine QueryBuiltin ->
  (WHNFSpine QueryBuiltin -> m (WHNFValue QueryBuiltin)) ->
  m (WHNFValue QueryBuiltin)
liftIfSpine [] k = k []
liftIfSpine (x : xs) k = liftIfArg (\a -> liftIfSpine xs (\as -> k (a : as))) x

currentPass :: Doc a
currentPass = "if elimination"

--------------------------------------------------------------------------------
-- No network elimination

unblockBoundVar :: (MonadQueryStructure m) => Lv -> m (WHNFValue QueryBuiltin)
unblockBoundVar lv = do
  maybeReduction <- getReducedVariableExprFor lv
  case maybeReduction of
    Just vectorReduction -> return vectorReduction
    Nothing -> return $ VBoundVar lv []
