{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Prefetch where

import Control.Lens         ((^?!))
import Control.Monad.Reader
import Data.Aeson.Lens
import Data.Monoid
import Data.Text            (Text)
import Logging
import Network.URI
import Prelude              hiding (log)
import Proc
import System.Console.ANSI
import Types

addAuth :: MonadFetch m => URI -> m URI
addAuth u@URI { uriAuthority } = do
    mauth <- asks authorization
    case mauth of
        Just x -> return u { uriAuthority = fmap (\ a -> a { uriUserInfo = x <> "@" }) uriAuthority }
        Nothing -> return u

prefetchUrl :: MonadFetch m => Text -> m Text
prefetchUrl txt = do
    log $ do
        logSGR [SetColor Foreground Dull Blue]
        logStr "nix-prefetch-url "
        logSGR [Reset]
        logStrLn $ unpack txt
    let Just parsedUrl = parseURI $ unpack txt
    newUrl <- addAuth parsedUrl
    runProc "nix-prefetch-url" [uriToText newUrl]

prefetchGit :: MonadFetch m => String -> Text -> m Text
prefetchGit url rev = do
    log $ do
        logSGR [SetColor Foreground Dull Magenta]
        logStr "nix-prefetch-git "
        logSGR [Reset]
        logStrLn $ url ++ " " ++ unpack rev
    out <- runProc "nix-prefetch-git" [url, unpack rev, "--fetch-submodules"]
    return $ out ^?! key "sha256" . _String
