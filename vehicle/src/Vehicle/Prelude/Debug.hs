{-# LANGUAGE CPP #-}

module Vehicle.Prelude.Debug
  ( Box(Box)
  , saveClosures
  , pause
  , resume
  )
  where

#if ghcDebug
import GHC.Debug.Stub (Box(Box), saveClosures, pause, resume)
#else
data Box = forall a . Box a

saveClosures :: [Box] -> IO ()
saveClosures _ = return ()
{-# INLINE saveClosures #-}

pause :: IO ()
pause = return ()
{-# INLINE pause #-}

resume :: IO ()
resume = return ()
{-# INLINE resume #-}
#endif
