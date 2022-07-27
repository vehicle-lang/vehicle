module Vehicle.Compile.ExpandResources.Parameter
  ( parseParameterValue
  ) where

import Control.Monad.Except
import Control.Monad.State
import Data.Map qualified as Map
import Text.Read (readMaybe)

import Vehicle.Language.Print
import Vehicle.Compile.Prelude
import Vehicle.Compile.Error
import Vehicle.Compile.ExpandResources.Core

--------------------------------------------------------------------------------
-- Parameter parsing

parseParameterValue :: MonadExpandResources m
                    => ParameterValues
                    -> DeclProvenance
                    -> CheckedExpr
                    -> m CheckedExpr
parseParameterValue parameterValues decl@(ident, p) paramType = do
  implicitParams <- get

  parser <- case paramType of
    BoolType{}            -> return parseBool
    NatType{}             -> return parseNat
    IntType{}             -> return parseInt
    RatType{}             -> return parseRat

    -- TODO check that Index dimension is constant, or at least will be after
    -- implicit parameters are filled in (the tricky bit).
    ConcreteIndexType _ n ->
      return (parseIndex n)

    IndexType _ (FreeVar _ varIdent)
      | Map.member (nameOf varIdent) implicitParams -> throwError $
        ParameterTypeImplicitParamIndex decl varIdent

    IndexType{} -> throwError $
      ParameterTypeVariableSizeIndex decl paramType

    _ -> compilerDeveloperError $
      "Invalid parameter type" <+> squotes (prettySimple paramType) <+>
      "should have been caught during type-checking"

  case Map.lookup (nameOf ident) parameterValues of
    Nothing    -> throwError $ ResourceNotProvided decl Parameter
    Just value -> case parser p value of
      Just e -> return e
      Nothing -> throwError $ UnableToParseResource decl Parameter value

parseBool :: Provenance -> String -> Maybe CheckedExpr
parseBool p value = fmap (BoolLiteral p) (readMaybe value)

parseNat :: Provenance -> String -> Maybe CheckedExpr
parseNat p value = fmap (NatLiteral p) (readMaybe value)

parseInt :: Provenance -> String -> Maybe CheckedExpr
parseInt p value = fmap (IntLiteral p) (readMaybe value)

parseRat :: Provenance -> String -> Maybe CheckedExpr
parseRat p value = fmap (RatLiteral p) (readMaybe value)

parseIndex :: Int -> Provenance -> String -> Maybe CheckedExpr
parseIndex n p value = readMaybe value >>= \v ->
  if v < n
    then Just $ IndexLiteral p n v
    else Nothing
