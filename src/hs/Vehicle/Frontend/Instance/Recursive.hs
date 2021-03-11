{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Vehicle.Frontend.Instance.Recursive where

import Control.Monad (join)
import Data.Functor.Foldable.TH
import Vehicle.Frontend.Abs

$(join <$> traverse makeBaseFunctor [''Kind, ''Type, ''Expr])
