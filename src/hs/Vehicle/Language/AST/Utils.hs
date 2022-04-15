module Vehicle.Language.AST.Utils where

import Control.Monad (void)
import Data.Functor.Foldable (Recursive(..))
import Data.List.NonEmpty qualified as NonEmpty (toList)
import Data.Text (pack)

import Vehicle.Prelude
import Vehicle.Language.AST.Core
import Vehicle.Language.AST.DeBruijn
import Vehicle.Language.AST.Builtin
import Vehicle.Language.AST.BuiltinPatterns
import Vehicle.Language.AST.Name

--------------------------------------------------------------------------------
-- Utility functions

isHole :: Expr binder var ann -> Bool
isHole (Hole _ _ ) = True
isHole _           = False

isType :: Expr binder var ann -> Bool
isType (Type _ _) = True
isType _          = False

isMeta :: Expr binder var ann -> Bool
isMeta (Meta _ _)           = True
isMeta (App _ (Meta _ _) _) = True
isMeta _                    = False

isProperty :: Expr binder var ann -> Bool
isProperty (Builtin _ (BooleanType Prop)) = True
isProperty _                              = False

freeNames :: Expr binder DBVar ann -> [Identifier]
freeNames = cata $ \case
  TypeF     _ _                 -> []
  HoleF     _   _               -> []
  PrimDictF _ _                 -> []
  MetaF     _ _                 -> []
  LiteralF  _ _                 -> []
  BuiltinF  _ _                 -> []
  AnnF      _ e t               -> e <> t
  AppF      _ fun args          -> fun <> concatMap (freeNames . argExpr) args
  PiF       _ binder result     -> freeNames (typeOf binder) <> result
  VarF      _ (Free ident)      -> [ident]
  VarF      _ (Bound _)         -> []
  LetF      _ bound binder body -> bound <> freeNames (typeOf binder) <> body
  LamF      _ binder body       -> freeNames (typeOf binder) <> body
  LSeqF     _ _ xs              -> concat xs

--------------------------------------------------------------------------------
-- Destruction functions

toHead :: Expr binder var ann -> (Expr binder var ann, [Arg binder var ann])
toHead (App _ann fun args ) = (fun, NonEmpty.toList args)
toHead e                    = (e, [])

exprHead :: Expr binder var ann -> Expr binder var ann
exprHead = fst . toHead

--------------------------------------------------------------------------------
-- Views

getBinderSymbol :: Binder DBBinding var ann -> Symbol
getBinderSymbol binder = case nameOf binder of
  Just symbol -> symbol
  Nothing     -> developerError "Binder unexpectedly does not appear to have a name"

getContainerElem :: Expr binder var ann -> Maybe (Expr binder var ann)
getContainerElem (ListType   _ t)   = Just t
getContainerElem (TensorType _ t _) = Just t
getContainerElem _                  = Nothing

getDimension :: Expr binder var ann -> Maybe Int
getDimension (NatLiteralExpr _ _ n) = return n
getDimension _                      = Nothing

getDimensions :: Expr binder var ann -> Maybe [Int]
getDimensions (SeqExpr _ _ _ es) = traverse getDimension es
getDimensions _                  = Nothing

--------------------------------------------------------------------------------
-- Construction functions

-- | Generates a name for a variable based on the indices, e.g. x [1,2,3] -> x_1_2_3
mkNameWithIndices :: Symbol -> [Int] -> Symbol
mkNameWithIndices n indices = mconcat (n : [pack (show index) | index <- indices])

removeAnnotations :: Functor (t binder var) => t binder var ann -> t binder var ()
removeAnnotations = void