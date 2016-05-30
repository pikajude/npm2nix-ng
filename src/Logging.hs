{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Logging where

import Control.Concurrent.Chan.Lifted
import Control.Concurrent.Lifted
import Control.Exception.Lifted
import Control.Monad.Trans.Control
import GHC.IO.Handle
import Prelude                        hiding (putStr, putStrLn)
import System.Console.ANSI            hiding (setSGR)
import System.Exit
import System.IO                      (stderr)
import System.IO.Unsafe               (unsafePerformIO)

logChan :: Chan (LogM ())
logChan = unsafePerformIO newChan
{-# NOINLINE logChan #-}

log :: MonadBaseControl IO m => LogM () -> m ()
log = writeChan logChan

logError :: MonadBaseControl IO m => LogM () -> m a
logError io = do
    writeChan logChan $ do
        logSGR [SetColor Foreground Dull Red]
        logStr "ERROR "
        logSGR [Reset]
        io
        LogM exitFailure
    threadDelay maxBound
    return undefined

runLoggingWithParent :: ThreadId -> IO ()
runLoggingWithParent tid = do
    io <- readChan logChan
    res <- try (unLogM io)
    case res of
        Right () -> runLoggingWithParent tid
        Left e -> throwTo tid (e :: ExitCode)

newtype LogM a = LogM { unLogM :: IO a }

instance Functor LogM where
    fmap f (LogM a) = LogM (fmap f a)

instance Applicative LogM where
    LogM f <*> LogM a = LogM (f <*> a)
    pure = LogM . pure

instance Monad LogM where
    return = LogM . return
    LogM a >>= f = LogM $ a >>= unLogM . f

logStr, logStrLn :: String -> LogM ()
logSGR :: [SGR] -> LogM ()
logPrint :: Show a => a -> LogM ()

logStrLn s = logStr (s ++ "\n")
logStr = LogM . hPutStr stderr
logSGR = LogM . hSetSGR stderr
logPrint = logStrLn . show
