module Vehicle.Compile.ExpandResources.Parameter
  ( parseParameterValue,
  )
where

import Control.Monad.Except
import Data.Map qualified as Map
import Data.Text (pack)
import Data.Text.Read (rational)
import Text.Read (readMaybe)
import Vehicle.Compile.Error
import Vehicle.Compile.ExpandResources.Core
import Vehicle.Compile.Normalise.NBEForced
import Vehicle.Compile.Normalise.TypedValueForced
import Vehicle.Compile.Prelude
import Vehicle.Compile.Resource (ParameterType (..))
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Code.ForcedValue
import Vehicle.Data.Code.Interface
import Vehicle.Data.Real (ExtendedRational (..))
import Vehicle.Data.Variable.Bound.Context.Name (runFreshNameBoundContextT)

--------------------------------------------------------------------------------
-- Parameter parsing

parseParameterValue ::
  (MonadExpandResources m) =>
  DeclProvenance ->
  Type Builtin ->
  String ->
  m (Thunk Builtin)
parseParameterValue decl parameterType providedValue = do
  implicitParams <- getInferableParameterContext
  parameterSort <- getParameterType $ Unforced emptyBoundEnv parameterType

  case parameterSort of
    ParameterBoolType -> parseBool decl providedValue
    ParameterRealType -> parseRat decl providedValue
    ParameterNatType -> parseNat decl providedValue
    -- TODO check that Index dimension is constant, or at least will be after
    -- implicit parameters are filled in (the tricky bit).
    ParameterIndexType size -> do
      forcedSize <- runFreshNameBoundContextT $ forceThunk size
      case toNatValue forcedSize of
        VNatLiteral n ->
          parseIndex n decl providedValue
        VNatParameter varIdent
          | Map.member varIdent implicitParams ->
              throwError $ ParameterTypeInferableParameterIndex decl varIdent
        _ -> do
          let gluedType = Glued parameterType (fromParameterType parameterSort)
          throwError $ ParameterTypeVariableSizeIndex decl gluedType forcedSize

getParameterType ::
  forall m.
  (MonadExpandResources m) =>
  UnforcedType Builtin ->
  m (ParameterType (Thunk Builtin))
getParameterType typ = do
  forcedType <- runFreshNameBoundContextT $ forceThunk typ
  case toTypeValue forcedType of
    VTensorType tType _ -> do
      forcedElementType <-
        runFreshNameBoundContextT $
          forceThunk tType
      case forcedElementType of
        IBoolType -> return ParameterBoolType
        IRatType -> return ParameterRealType
        _ -> resourceTypingError "parameter" forcedType
    VNatType {} -> return ParameterNatType
    VIndexType size -> return $ ParameterIndexType size
    _ -> resourceTypingError "parameter" forcedType

parseBool :: (MonadCompile m) => DeclProvenance -> String -> m (Thunk Builtin)
parseBool decl value = case readMaybe value of
  Just v -> return $ Forced $ IBoolLiteral v
  Nothing -> throwError $ ParameterValueUnparsable decl value BoolType

parseNat :: (MonadCompile m) => DeclProvenance -> String -> m (Thunk Builtin)
parseNat decl value = case readMaybe value of
  Just v
    | v >= 0 -> return $ Forced $ INatLiteral v
    | otherwise -> throwError $ ParameterValueInvalidNat decl v
  Nothing -> throwError $ ParameterValueUnparsable decl value NatType

parseRat :: (MonadCompile m) => DeclProvenance -> String -> m (Thunk Builtin)
parseRat decl value = case rational (pack value) of
  Left _err -> throwError $ ParameterValueUnparsable decl value RatType
  Right (v, _) -> return $ Forced $ IRatLiteral $ Finite v

parseIndex :: (MonadCompile m) => Int -> DeclProvenance -> String -> m (Thunk Builtin)
parseIndex n decl value = case readMaybe value of
  Nothing -> throwError $ ParameterValueUnparsable decl value IndexType
  Just v ->
    if v >= 0 && v < n
      then return $ Forced $ IIndexLiteral v (Forced $ INatLiteral n)
      else throwError $ ParameterValueInvalidIndex decl v n
