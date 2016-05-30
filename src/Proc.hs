{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Proc where

import Control.Monad.IO.Class
import Data.Text              (Text, strip)
import Logging
import Prelude                hiding (putStr)
import System.Console.ANSI    hiding (setSGR)
import System.Exit
import System.Process         hiding (readCreateProcessWithExitCode)
import System.Process.Text
import Types

runProc :: MonadFetch m => String -> [String] -> m Text
runProc prc args = runCreateProc (proc prc args)

runCreateProc :: MonadFetch m => CreateProcess -> m Text
runCreateProc inproc@CreateProcess { cmdspec = ~(RawCommand prc args) } = do
    (ec, t1, t2) <- liftIO $ readCreateProcessWithExitCode inproc ""
    case ec of
        ExitSuccess -> return $ strip t1
        ExitFailure _ -> logError $ do
            logSGR [SetColor Foreground Dull Yellow]
            logStr $ unwords (unpack prc : args) ++ " "
            logSGR [Reset]
            logStrLn $ unpack t2
