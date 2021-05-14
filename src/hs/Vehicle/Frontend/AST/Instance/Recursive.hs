{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Vehicle.Frontend.AST.Instance.Recursive where

import Data.Functor.Foldable.TH
import Vehicle.Frontend.AST.Core

$(makeBaseFunctor ''Kind)
$(makeBaseFunctor ''Type)
$(makeBaseFunctor ''Expr)
