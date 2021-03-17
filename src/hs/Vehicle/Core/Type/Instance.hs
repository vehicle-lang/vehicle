{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeApplications #-}

module Vehicle.Core.Type.Instance
  ( module X
  ) where

import Vehicle.Core.Type.Instance.Core ()
import Vehicle.Core.Type.Instance.Recursive as X
import Vehicle.Core.Type.Instance.SortedTrifunctor as X
