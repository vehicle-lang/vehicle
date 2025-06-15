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
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Code.Interface
import Vehicle.Data.Code.TypedView
import Vehicle.Data.Code.Value

--------------------------------------------------------------------------------
-- Parameter parsing

parseParameterValue ::
  (MonadExpandResources m) =>
  ParameterValues ->
  DeclProvenance ->
  GluedType Builtin ->
  m (Value Builtin)
parseParameterValue parameterValues decl@(ident, _) parameterType = do
  implicitParams <- getInferableParameterContext

  parser <- case toTypeValue $ normalised parameterType of
    VBoolTensorType INil {} -> return parseBool
    VNatType {} -> return parseNat
    VRatTensorType INil {} -> return parseRat
    -- TODO check that Index dimension is constant, or at least will be after
    -- implicit parameters are filled in (the tricky bit).
    VIndexType size -> case toNatValue size of
      VNatParameter varIdent
        | Map.member varIdent implicitParams ->
            throwError $ ParameterTypeInferableParameterIndex decl varIdent
      VNatLiteral n -> return (parseIndex n)
      _ -> throwError $ ParameterTypeVariableSizeIndex decl parameterType size
    _ ->
      compilerDeveloperError $
        "Invalid parameter type"
          <+> squotes (prettyVerbose (normalised parameterType))
          <+> "should have been caught during type-checking"

  case Map.lookup (nameOf ident) parameterValues of
    Nothing -> throwError $ ResourceNotProvided decl Parameter
    Just value -> parser decl value

parseBool :: (MonadCompile m) => DeclProvenance -> String -> m (Value Builtin)
parseBool decl value = case readMaybe value of
  Just v -> return $ IBoolLiteral v
  Nothing -> throwError $ ParameterValueUnparsable decl value BoolType

parseNat :: (MonadCompile m) => DeclProvenance -> String -> m (Value Builtin)
parseNat decl value = case readMaybe value of
  Just v
    | v >= 0 -> return $ INatLiteral v
    | otherwise -> throwError $ ParameterValueInvalidNat decl v
  Nothing -> throwError $ ParameterValueUnparsable decl value NatType

parseRat :: (MonadCompile m) => DeclProvenance -> String -> m (Value Builtin)
parseRat decl value = case rational (pack value) of
  Left _err -> throwError $ ParameterValueUnparsable decl value RatType
  Right (v, _) -> return $ IRatLiteral v

parseIndex :: (MonadCompile m) => Int -> DeclProvenance -> String -> m (Value Builtin)
parseIndex n decl value = case readMaybe value of
  Nothing -> throwError $ ParameterValueUnparsable decl value IndexType
  Just v ->
    if v >= 0 && v < n
      then return $ IIndexLiteral v
      else throwError $ ParameterValueInvalidIndex decl v n
