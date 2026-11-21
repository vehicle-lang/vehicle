{-# LANGUAGE CPP #-}
#ifdef ghcDebug
#else
{-# LANGUAGE ExistentialQuantification #-}
#endif

module Vehicle.Syntax.Debug
  ( -- * Export 'ghc-debug-stub'
    Box (Box),
    saveClosures,
    pause,
    resume,
  )
where

-- Import ghc-debug-stub:
#ifdef ghcDebug
import GHC.Debug.Stub (Box (Box), pause, resume, saveClosures)
#endif

-- Define ghc-debug operators if needed:
#ifdef ghcDebug
-- The operators are imported from GHC.Debug.Stub,
-- so we do not need to define anything.
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
