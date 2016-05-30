{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}

module Logging where

import Control.Concurrent.Chan.Lifted
import Control.Concurrent.Lifted
import Control.Monad.Trans.Control
import Prelude                        hiding (log)
import System.Exit
import System.IO                      (stderr)
import System.IO.Unsafe               (unsafePerformIO)
import Text.PrettyPrint.ANSI.Leijen

logChan :: Chan (Either () Doc)
logChan = unsafePerformIO newChan
{-# NOINLINE logChan #-}

log :: MonadBaseControl IO m => Doc -> m ()
log = writeChan logChan . Right . (<> hardline)

logError :: MonadBaseControl IO m => Doc -> m a
logError msg = do
    log $ red "ERROR" <+> msg
    writeChan logChan (Left ())
    threadDelay maxBound
    return undefined

runLoggingWithParent :: ThreadId -> IO ()
runLoggingWithParent tid = do
    io <- readChan logChan
    case io of
        Left () -> throwTo tid $ ExitFailure 1
        Right i -> hPutDoc stderr i >> runLoggingWithParent tid
