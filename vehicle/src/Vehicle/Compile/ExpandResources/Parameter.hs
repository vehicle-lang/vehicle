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
import Vehicle.Compile.Type.Subsystem.Standard
import Vehicle.Data.BuiltinInterface
import Vehicle.Data.NormalisedExpr

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

  parser <- case normalised parameterType of
    VBoolType {} -> return parseBool
    VNatType {} -> return parseNat
    VIntType {} -> return parseInt
    VRatType {} -> return parseRat
    -- TODO check that Index dimension is constant, or at least will be after
    -- implicit parameters are filled in (the tricky bit).
    VIndexType (VNatLiteral n) ->
      return (parseIndex n)
    VIndexType (VFreeVar varIdent _)
      | Map.member varIdent implicitParams ->
          throwError $
            ParameterTypeInferableParameterIndex decl varIdent
    VIndexType {} ->
      throwError $
        ParameterTypeVariableSizeIndex decl parameterType
    otherType ->
      compilerDeveloperError $
        "Invalid parameter type"
          <+> squotes (prettyVerbose otherType)
          <+> "should have been caught during type-checking"

  case Map.lookup (nameOf ident) parameterValues of
    Nothing -> throwError $ ResourceNotProvided decl Parameter
    Just value -> parser decl value

parseBool :: (MonadCompile m) => DeclProvenance -> String -> m (Value Builtin)
parseBool decl value = case readMaybe value of
  Just v -> return $ VBoolLiteral v
  Nothing -> throwError $ ParameterValueUnparsable decl value Bool

parseNat :: (MonadCompile m) => DeclProvenance -> String -> m (Value Builtin)
parseNat decl value = case readMaybe value of
  Just v
    | v >= 0 -> return $ VNatLiteral v
    | otherwise -> throwError $ ParameterValueInvalidNat decl v
  Nothing -> throwError $ ParameterValueUnparsable decl value Nat

parseInt :: (MonadCompile m) => DeclProvenance -> String -> m (Value Builtin)
parseInt decl value = case readMaybe value of
  Just v -> return $ VIntLiteral v
  Nothing -> throwError $ ParameterValueUnparsable decl value Int

parseRat :: (MonadCompile m) => DeclProvenance -> String -> m (Value Builtin)
parseRat decl value = case rational (pack value) of
  Left _err -> throwError $ ParameterValueUnparsable decl value Rat
  Right (v, _) -> return $ VRatLiteral v

parseIndex :: (MonadCompile m) => Int -> DeclProvenance -> String -> m (Value Builtin)
parseIndex n decl value = case readMaybe value of
  Nothing -> throwError $ ParameterValueUnparsable decl value Index
  Just v ->
    if v >= 0 && v < n
      then return $ VIndexLiteral v
      else throwError $ ParameterValueInvalidIndex decl v n
