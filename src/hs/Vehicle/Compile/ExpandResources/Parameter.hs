module Vehicle.Compile.ExpandResources.Parameter
  ( parseParameterValue
  ) where

import Control.Monad.Except
import Control.Monad.State
import Data.Map qualified as Map
import Data.Text (pack)
import Text.Read (readMaybe)
import Data.Text.Read (rational)

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
parseParameterValue parameterValues decl@(ident, _) paramType = do
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
    Just value -> parser decl value

parseBool :: MonadCompile m => DeclProvenance -> String -> m CheckedExpr
parseBool decl@(_, p) value = case readMaybe value of
  Just v  -> return $ BoolLiteral p v
  Nothing -> throwError $ ParameterValueUnparsable decl value Bool

parseNat :: MonadCompile m => DeclProvenance -> String -> m CheckedExpr
parseNat decl@(_, p) value = case readMaybe value of
  Just v
    | v >= 0    -> return $ NatLiteral p v
    | otherwise -> throwError $ ParameterValueInvalidNat decl v
  Nothing -> throwError $ ParameterValueUnparsable decl value Nat

parseInt :: MonadCompile m => DeclProvenance -> String -> m CheckedExpr
parseInt decl@(_, p) value = case readMaybe value of
  Just v  -> return $ IntLiteral p v
  Nothing -> throwError $ ParameterValueUnparsable decl value Int

parseRat :: MonadCompile m => DeclProvenance -> String -> m CheckedExpr
parseRat decl@(_, p) value = case rational (pack value) of
  Left  _err   -> throwError $ ParameterValueUnparsable decl value Rat
  Right (v, _) -> return $ RatLiteral p v

parseIndex :: MonadCompile m => Int -> DeclProvenance -> String -> m CheckedExpr
parseIndex n decl@(_, p) value = case readMaybe value of
  Nothing -> throwError $ ParameterValueUnparsable decl value Index
  Just v  -> if v >= 0 && v < n
    then return $ IndexLiteral p n v
    else throwError $ ParameterValueInvalidIndex decl v n
