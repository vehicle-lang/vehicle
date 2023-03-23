module Vehicle.Compile.Prelude.Utils where

import Data.Functor.Foldable (Recursive (..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty (toList)
import Data.Maybe (mapMaybe)
import Vehicle.Expr.DeBruijn
import Vehicle.Prelude
import Vehicle.Syntax.AST

--------------------------------------------------------------------------------
-- Utility functions

isTypeUniverse :: Expr binder var builtin -> Bool
isTypeUniverse TypeUniverse {} = True
isTypeUniverse _ = False

--------------------------------------------------------------------------------
-- Enumeration functions

freeNamesIn :: Expr binder var builtin -> [Identifier]
freeNamesIn = cata $ \case
  FreeVarF _ ident -> [ident]
  BoundVarF {} -> []
  UniverseF {} -> []
  HoleF {} -> []
  MetaF {} -> []
  BuiltinF {} -> []
  AnnF _ e t -> e <> t
  AppF _ fun args -> fun <> concatMap argExpr args
  PiF _ binder result -> binderType binder <> result
  LetF _ bound binder body -> bound <> binderType binder <> body
  LamF _ binder body -> binderType binder <> body

--------------------------------------------------------------------------------
-- Destruction functions

toHead :: Expr binder var builtin -> (Expr binder var builtin, [Arg binder var builtin])
toHead (App _ fun args) = (fun, NonEmpty.toList args)
toHead e = (e, [])

exprHead :: Expr binder var builtin -> Expr binder var builtin
exprHead = fst . toHead

onlyExplicit :: NonEmpty (GenericArg expr) -> [expr]
onlyExplicit args = argExpr <$> filter isExplicit (NonEmpty.toList args)

--------------------------------------------------------------------------------
-- Views

getMetaID :: Expr binder var builtin -> Maybe MetaID
getMetaID e = case exprHead e of
  Meta _ m -> Just m
  _ -> Nothing

getBinderName :: GenericBinder DBBinding expr -> Name
getBinderName binder = case binderNamingForm binder of
  NameAndType name -> name
  OnlyName name -> name
  OnlyType -> developerError "Binder unexpectedly does not appear to have a name"

getExplicitArg :: GenericArg expr -> Maybe expr
getExplicitArg (ExplicitArg _ expr) = Just expr
getExplicitArg _ = Nothing

getImplicitArg :: Arg binder var builtin -> Maybe (Expr binder var builtin)
getImplicitArg (ImplicitArg _ arg) = Just arg
getImplicitArg _ = Nothing

filterOutNonExplicitArgs :: NonEmpty (Arg binder var builtin) -> [Expr binder var builtin]
filterOutNonExplicitArgs args = mapMaybe getExplicitArg (NonEmpty.toList args)

findInstanceArg :: [GenericArg a] -> (a, [GenericArg a])
findInstanceArg (InstanceArg _ inst : xs) = (inst, xs)
findInstanceArg (_ : xs) = findInstanceArg xs
findInstanceArg [] = developerError "Malformed type class operation"
