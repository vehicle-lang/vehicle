module Vehicle.Backend.LossTraining
  ( convertToLossTensors,
    convertResourceDecl,
    convertDeclType,
    convertMultiProperty,
  )
where

import Control.Monad.Reader (ReaderT)
import Data.Maybe (maybeToList)
import Data.Proxy (Proxy (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Vehicle.Backend.Loss.Core
import Vehicle.Backend.Loss.Domain (compileQuantifier)
import Vehicle.Backend.Loss.LogicCompilation (findAndCompileLogic)
import Vehicle.Backend.Loss.LossCompilation
import Vehicle.Backend.Loss.LossCompilation qualified as Loss ()
import Vehicle.Backend.Prelude (DifferentiableLogicID)
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.Quote (unnormalise)
import Vehicle.Compile.Prelude
import Vehicle.Data.Builtin.Loss
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Builtin.Standard.Normalise ()
import Vehicle.Data.Code.BooleanExpr (unDisjunctAll)
import Vehicle.Data.Code.ForcedValue
import Vehicle.Data.DifferentiableLogic
import Vehicle.Data.Variable.Bound.Context.Tensor (TensorBoundContextT)
import Vehicle.Data.Variable.Free.Context (MonadFreeContext (..), addDeclEntryToContext, addDeclToContext, runFreshFreeContextT)

convertToLossTensors ::
  (MonadCompile m) =>
  DifferentiableLogicID ->
  Set Name ->
  Prog Builtin ->
  m (Prog LossBuiltin)
convertToLossTensors logicID requestedDecls prog@(Main ds) = do
  -- First find and compile the logic
  logic <- logCompilerPass LossLogic $ findAndCompileLogic logicID prog

  -- Then compile the program using that logic
  runFreshFreeContextT (Proxy @Builtin) $ do
    runFreshFreeContextT (Proxy @LossBuiltin) $
      logCompilerPass Loss $ do
        Main <$> convertDecls logicID logic requestedDecls ds

convertDecls ::
  (MonadCompile m, MonadFreeContext Builtin m, MonadFreeContext LossBuiltin m) =>
  DifferentiableLogicID ->
  DifferentiableLogicImplementation ->
  Set Name ->
  [Decl Builtin] ->
  m [Decl LossBuiltin]
convertDecls logicID logic requestedDecls = \case
  [] -> return []
  decl : decls -> do
    maybeLossDecl <- convertDecl logicID logic requestedDecls decl
    decls' <-
      maybe id addDeclToContext maybeLossDecl $
        addDeclEntryToContext decl $
          convertDecls logicID logic requestedDecls decls
    return $ maybeToList maybeLossDecl ++ decls'

convertDecl ::
  forall m.
  (MonadCompile m, MonadFreeContext Builtin m, MonadFreeContext LossBuiltin m) =>
  DifferentiableLogicID ->
  DifferentiableLogicImplementation ->
  Set Name ->
  Decl Builtin ->
  m (Maybe (Decl LossBuiltin))
convertDecl logicID logic requestedDecls decl = case decl of
  DefAbstract p ident sort typ
    | isAnnotatedAsExternalResource sort -> do
        let normType = Unforced emptyBoundEnv typ
        runConversion $ convertResourceDecl p ident sort normType
    | otherwise -> return Nothing
  DefFunction p ident ann typ expr
    | isAnnotatedAsProperty ann || nameOf decl `Set.member` requestedDecls -> do
        let normType = Unforced emptyBoundEnv typ
        let normExpr = Unforced emptyBoundEnv expr
        runConversion $ convertPropertyDecl p ident ann normType normExpr
    | otherwise -> return Nothing
  DefRecord {} -> return Nothing
  where
    runConversion :: TensorBoundContextT (ReaderT LossCtx m) (Decl LossBuiltin) -> m (Maybe (Decl LossBuiltin))
    runConversion action = do
      logCompilerSection2 MidDetail ("translation of" <+> quotePretty (identifierOf decl)) $ do
        Just <$> runMonadLogicT logicID logic (identifierOf decl, provenanceOf decl) action

convertResourceDecl ::
  (MonadLogic m) =>
  Provenance ->
  Identifier ->
  DefAbstractSort ->
  UnforcedType Builtin ->
  m (Decl LossBuiltin)
convertResourceDecl p ident sort typ = do
  -- Keep resource declarations, converting their type appropriately.
  -- TODO what about boolean parameters?
  typ' <- convertDeclType typ
  return $ DefAbstract p ident sort typ'

convertPropertyDecl ::
  (MonadLogic m) =>
  Provenance ->
  Identifier ->
  DefFunctionSort ->
  UnforcedType Builtin ->
  Thunk Builtin ->
  m (Decl LossBuiltin)
convertPropertyDecl p ident ann typ body = do
  lossType <- convertDeclType typ
  lossBody <- convertMultiProperty body
  return $ DefFunction p ident ann lossType lossBody

convertDeclType :: (MonadLogic m) => UnforcedType Builtin -> m (Type LossBuiltin)
convertDeclType typ = unnormalise 0 <$> convertThunk Nothing typ

convertMultiProperty :: (MonadLogic m) => Thunk Builtin -> m (Expr LossBuiltin)
convertMultiProperty body = do
  let compQuantifier args = do
        disjuncts <- compileQuantifier args
        foldrM1 orLossValue $ unDisjunctAll disjuncts
  unnormalise 0 <$> convertThunk (Just compQuantifier) body
