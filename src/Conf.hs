{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}

module Conf where

import           Control.Applicative
import           Control.Arrow
import           Control.Monad
import           Data.Char
import           Data.List           (isPrefixOf)
import           Data.Map            (Map, fromList)
import qualified Data.Map            as M
import           Data.Monoid
import           Prelude             hiding (lookup)
import           System.Directory
import           System.Environment
import           System.FilePath
import           Text.Trifecta

newtype Conf = Conf { unConf :: Map String String }
               deriving (Show, Monoid)

lookup :: String -> Conf -> Maybe String
lookup k (Conf v) = M.lookup k v

getConf :: Bool -> IO Conf
getConf userConfig = do
    ce <- confEnv >>= expandConf
    cu <- if userConfig then confUser (lookup "userconfig" ce) >>= expandConf else return $ Conf mempty
    return $ ce <> cu

confEnv :: IO Conf
confEnv = Conf . fromList . map (first (drop 11))
        . filter (isConfOption . fst)
        . map (first (map toLower))
      <$> getEnvironment
    where isConfOption x = "npm_config_" `isPrefixOf` x

confUser :: Maybe FilePath -> IO Conf
confUser mfp = do
    path <- maybe ((</> ".npmrc") <$> getHomeDirectory) return mfp

    -- can't use parseFromFileEx because it fails for /dev/null
    contents <- readFile path

    let res = parseString parseConf mempty contents
    case res of
        Success x -> return x
        Failure doc' -> error $ show doc'

expandConf :: Conf -> IO Conf
expandConf (Conf v) = do
    h <- getHomeDirectory
    return $ Conf $ fmap (replaceTildes h) v where
        replaceTildes path ('~':xs) = path ++ replaceTildes path xs
        replaceTildes path (x:xs) = x : replaceTildes path xs
        replaceTildes _ [] = []

parseConf :: Parser Conf
parseConf = Conf . fromList
        <$> manyTill (setting <* newline) eof where
    setting = (,) <$> (token (some (letter <|> oneOf "-_")) <* char '=')
                  <*> many (satisfy (not . isSpace))
