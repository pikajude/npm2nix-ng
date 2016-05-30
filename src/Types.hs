{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

module Types where

import Control.Lens                (( # ), (^.))
import Control.Monad.Catch         (MonadMask)
import Control.Monad.IO.Class      (MonadIO)
import Control.Monad.Reader        (MonadReader)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Text.Lens
import Network.URI                 (URI, uriToString)

type MonadFetch m = (MonadBaseControl IO m, MonadReader Fetcher m, MonadIO m, MonadMask m)

data Fetcher = Fetcher
             { registry      :: String
             , authorization :: Maybe String
             } deriving Show

unpack :: IsText s => s -> String
unpack = (^. _Text)

uriToText :: IsText s => URI -> s
uriToText uri = _Text # uriToString id uri ""
