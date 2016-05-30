{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Prefetch where

import Control.Lens                 ((^?!))
import Control.Monad.Reader
import Data.Aeson.Lens
import Data.Text                    (Text)
import Logging
import Network.URI
import Prelude                      hiding (log)
import Proc
import Text.PrettyPrint.ANSI.Leijen
import Types

addAuth :: MonadFetch m => URI -> m URI
addAuth u@URI { uriAuthority } = do
    mauth <- asks authorization
    case mauth of
        Just x -> return u { uriAuthority =
                        fmap (\ a -> a { uriUserInfo = x `mappend` "@" }) uriAuthority }
        Nothing -> return u

prefetchUrl :: MonadFetch m => Text -> m Text
prefetchUrl txt = do
    log $ dullblue "nix-prefetch-url" <+> string (unpack txt)
    let Just parsedUrl = parseURI $ unpack txt
    newUrl <- addAuth parsedUrl
    runProc "nix-prefetch-url" [uriToText newUrl]

prefetchGit :: MonadFetch m => String -> Text -> m Text
prefetchGit url rev = do
    log $ dullmagenta "nix-prefetch-git" <+> string url <+> string (unpack rev)
    out <- runProc "nix-prefetch-git" [url, unpack rev, "--fetch-submodules"]
    return $ out ^?! key "sha256" . _String
