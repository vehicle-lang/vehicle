{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Vehicle.Frontend.AST.Instance.Recursive where

import Control.Monad (join)
import Data.Functor.Foldable.TH
import Vehicle.Frontend.AST.Core

$(join <$> traverse makeBaseFunctor [''Kind, ''Type, ''Expr])
