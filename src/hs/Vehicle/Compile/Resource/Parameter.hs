module Vehicle.Compile.Resource.Parameter
  ( checkParameterType
  , parseParameterValue
  ) where

import Control.Monad.Except
import Data.Map qualified as Map
import Text.Read (readMaybe)

import Vehicle.Language.Print
import Vehicle.Compile.Prelude
import Vehicle.Compile.Error
import Vehicle.Compile.Resource.Core

--------------------------------------------------------------------------------
-- Parameter typing

getParameterType :: Provenance
                 -> Identifier
                 -> CheckedExpr
                 -> Either CompileError ParameterType
getParameterType ann ident paramType = case paramType of
  BoolType{}    -> Right ParamBoolType
  NatType{}     -> Right ParamNatType
  IntType{}     -> Right ParamIntType
  RatType{}     -> Right ParamRatType
  -- TODO check that Index dimension is constant, or at least will be after
  -- implicit parameters are filled in (the tricky bit).
  IndexType _ n -> Right $ ParamIndexType n
  _             -> Left $ ParameterTypeUnsupported ident ann paramType

checkParameterType :: MonadCompile m
                   => Provenance
                   -> Identifier
                   -> CheckedExpr
                   -> m ()
checkParameterType ann ident paramType =
  case getParameterType ann ident paramType of
    Left err -> throwError err
    Right{}  -> return ()

--------------------------------------------------------------------------------
-- Parameter parsing

parseParameterValue :: MonadCompile m
                    => ParameterValues
                    -> Provenance
                    -> Identifier
                    -> CheckedExpr
                    -> m CheckedExpr
parseParameterValue parameterValues ann ident paramType = do
  let name = nameOf ident
  parser <- case getParameterType ann ident paramType of
    Left{} -> compilerDeveloperError $
      "Invalid parameter type" <+> squotes (prettySimple paramType) <+>
      "should have been caught during type-checking"
    Right validType -> case validType of
      ParamBoolType       -> return parseBool
      ParamNatType        -> return parseNat
      ParamIntType        -> return parseInt
      ParamRatType        -> return parseRat
      ParamIndexType tDim -> case tDim of
        (NatLiteralExpr _ _ n) -> return (parseIndex paramType n)
        _                      ->
          throwError $ ParameterTypeVariableSizeIndex ident ann paramType

  case Map.lookup name parameterValues of
    Nothing    -> throwError $ ResourceNotProvided ident ann Parameter
    Just value -> case parser ann value of
      Just e -> return e
      Nothing -> throwError $ UnableToParseResource ident ann Parameter value

parseBool :: Provenance -> String -> Maybe CheckedExpr
parseBool ann value = fmap (BoolLiteralExpr ann) (readMaybe value)

parseNat :: Provenance -> String -> Maybe CheckedExpr
parseNat ann value = fmap (NatLiteralExpr ann (NatType ann)) (readMaybe value)

parseInt :: Provenance -> String -> Maybe CheckedExpr
parseInt ann value = fmap (IntLiteralExpr ann (IntType ann)) (readMaybe value)

parseRat :: Provenance -> String -> Maybe CheckedExpr
parseRat ann value = fmap (RatLiteralExpr ann (RatType ann)) (readMaybe value)

parseIndex :: CheckedExpr -> Int -> Provenance -> String -> Maybe CheckedExpr
parseIndex finType n ann value = readMaybe value >>= \v ->
  if v < n
    then Just $ NatLiteralExpr ann finType v
    else Nothing
