module Vehicle.Compile.ExpandResources.Parameter
  ( parseParameterValue
  ) where

import Control.Monad.Except
import Data.Map qualified as Map
import Text.Read (readMaybe)

import Vehicle.Language.Print
import Vehicle.Compile.Prelude
import Vehicle.Compile.Error

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
  parser <- case paramType of
    BoolType{}            -> return parseBool
    NatType{}             -> return parseNat
    IntType{}             -> return parseInt
    RatType{}             -> return parseRat
    ConcreteIndexType _ n -> return (parseIndex paramType n)

    -- TODO check that Index dimension is constant, or at least will be after
    -- implicit parameters are filled in (the tricky bit).
    IndexType{} -> throwError $
      ParameterTypeVariableSizeIndex (ident, ann) paramType

    _ -> compilerDeveloperError $
      "Invalid parameter type" <+> squotes (prettySimple paramType) <+>
      "should have been caught during type-checking"

  case Map.lookup name parameterValues of
    Nothing    -> throwError $ ResourceNotProvided (ident, ann) Parameter
    Just value -> case parser ann value of
      Just e -> return e
      Nothing -> throwError $ UnableToParseResource (ident, ann) Parameter value

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
