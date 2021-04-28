{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Vehicle.Core.Normalise.Quantifier
  ( normQuantifier
  , Quantifier(..)
  ) where

import Vehicle.Core.Type ( Tree(..), Sort(EXPR) )
import Data.Range (Range, fromRanges)
import Data.Text (Text)
import Vehicle.Prelude (Position)
import Vehicle.Core.DeBruijn.Substitution as DeBruijn
import Control.Monad.Error.Class (throwError)
import Vehicle.Core.Normalise.Core

-- FIX THIS WHEN TYPE-CHECKER UP AND RUNNING
type TempType = Text

data Quantifier = Any | All

quantifierText :: Quantifier -> Text
quantifierText Any = "any"
quantifierText All = "all"

quantifierOp :: Quantifier -> Text
quantifierOp Any = "or"
quantifierOp All = "and"

normQuantifier
  :: (MonadNorm m)
  => Quantifier
  -> NormExpr ann
  -> NormExpr ann
  -> ann 'EXPR -> ann 'EXPR -> ann 'EXPR
  -> Position
  -> m (NormExpr ann)
normQuantifier quant domain condition ann0 ann1 ann2 pos = case getQuantifierValues domain ann0 pos of
  -- Cannot expand quantifier
  Nothing -> return $ EOp2 (quantifierText quant) domain condition ann0 ann1 ann2 pos
  -- Could expand quantifier but domain appears to be trivially empty
  Just [] -> throwError $ EmptyQuantifierDomain pos
  -- Can expand quantifier with non-empty domain
  Just es ->
    let
      quantOp = \x y -> EOp2 (quantifierOp quant) x y ann0 ann1 ann2 pos
      substValue = \v -> DeBruijn.subst 0 v condition
    in
      return $ foldr1 quantOp (map substValue es)

getType :: NormExpr ann -> TempType
getType _ = "Bool"

getQuantifierValues
  :: NormExpr ann
  -> ann 'EXPR
  -> Position
  -> Maybe [NormExpr ann]
getQuantifierValues domain ann pos = case getType domain of
  "Bool" -> Just [mkBool True ann pos , mkBool False ann pos]
  "Int" -> getIntQuantifierValues domain ann pos
  _ -> Nothing

getIntQuantifierValues
  :: NormExpr ann 
  -> ann 'EXPR
  -> Position 
  -> Maybe [NormExpr ann]
getIntQuantifierValues domain ann pos = case getIntVariableRanges 0 domain of
  Nothing -> Nothing
  Just ranges -> Just $ map (ELitInt ann) (fromRanges ranges)

-- TODO
getIntVariableRanges :: Int
                     -> NormExpr ann
                     -> Maybe [Range Integer]
getIntVariableRanges i expr = Nothing