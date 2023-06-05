{-# LANGUAGE CPP #-}
#ifdef nothunks
{-# LANGUAGE BangPatterns              #-}
#endif
#ifdef ghcDebug
#else
{-# LANGUAGE ExistentialQuantification #-}
#endif

module Vehicle.Debug
  ( -- * Export 'ghc-debug-stub'
    Box (Box),
    saveClosures,
    pause,
    resume,

    -- * Export 'nothunks'
    unsafeCheckThunks,
  )
where

-- Import ghc-debug-stub:
#ifdef ghcDebug
import GHC.Debug.Stub (Box (Box), pause, resume, saveClosures)
#endif

-- Import nothunks and helpers:
#ifdef nothunks
import GHC.Stack (HasCallStack, callStack)
import NoThunks.Class (NoThunks, unsafeNoThunks)
#ifdef ghcDebug
import Control.Monad (void)
import System.IO.Unsafe (unsafePerformIO)
#endif
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

-- Define unsafeCheckThunks:
#ifdef nothunks
#ifdef ghcDebug
-- If nothunks is defined & ghcDebug is defined:
-- If we find a thunk, we pass it to ghc-debug, and pause execution.
unsafeCheckThunks :: (HasCallStack, NoThunks a) => a -> a
unsafeCheckThunks !x = case unsafeNoThunks x of
    Nothing    -> x
    Just thunk ->
      case unsafePerformIO $ do
        putStrLn $
               "THUNK ALERT:\n    "
            ++ show thunk ++ "\n    "
            ++ show callStack ++ "\n"
            ++ "pausing for ghc-debug analysis"
        saveClosures [Box x]
        void getLine
      of
        () -> x
#else
-- If nothunks is defined & ghcDebug is NOT defined:
-- If we find a thunk, we throw an error.
unsafeCheckThunks :: (HasCallStack, NoThunks a) => a -> a
unsafeCheckThunks !x = case unsafeNoThunks x of
    Nothing    -> x
    Just thunk -> error $ unlines
      [ "Found a thunk: ",
        "- " <> show thunk,
        "- " <> show callStack
      ]
{-# INLINE unsafeCheckThunks #-}
#endif
#else
-- If nothunks is NOT defined:
-- We cannot check for thunks, so we do nothing.
unsafeCheckThunks :: a -> a
unsafeCheckThunks x = x
{-# INLINE unsafeCheckThunks #-}
#endif
