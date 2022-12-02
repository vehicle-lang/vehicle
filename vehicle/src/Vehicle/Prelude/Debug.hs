{-# LANGUAGE CPP #-}

module Vehicle.Prelude.Debug
  ( -- * Export 'ghc-debug-stub'
    Box(Box)
  , saveClosures
  , pause
  , resume
    -- * Export 'nothunks'
  , unsafeCheckThunks
  )
  where

#if ghcDebug
import GHC.Debug.Stub (Box (Box), pause, resume, saveClosures)
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

#if ghcDebug
unsafeCheckThunks :: NoThunks a => a -> a
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
unsafeCheckThunks :: a -> a
unsafeCheckThunks x = x
{-# INLINE unsafeCheckThunks #-}
#endif
