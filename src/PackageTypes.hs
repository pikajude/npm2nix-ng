{-# LANGUAGE OverloadedStrings #-}

module PackageTypes where

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.HashMap.Strict as H
import qualified Data.Map            as M
import           Data.Maybe
import           Data.SemVer
import           Data.SemVer.Parser
import           Data.Text           (Text, splitOn)
import           Data.Text.Lens
import           Network.URI

newtype SemVerJSON = SemVerJSON { unSVJ :: SemVer }

instance FromJSON SemVerJSON where
    parseJSON (String s) = case parseSemVer s of
        Left s' -> fail (show s')
        Right v -> pure (SemVerJSON v)

    parseJSON q = typeMismatch "SemVerJSON" q

data TaggedRange = TaggedRange
                 { actual   :: SemVerRange
                 , original :: Text
                 } deriving (Show, Ord, Eq)

data PackageRange = PackageRange
                  { rName :: Text
                  , range :: TaggedRange
                  } deriving (Show, Ord, Eq)

data Req = VersionRange TaggedRange
         | GitHub Text Text
         | Git Text
         | Http Text
         deriving (Eq, Ord, Show)

data PackageReq = PackageReq
                { prName :: Text
                , req    :: Req
                } deriving (Show, Ord, Eq)

data Source = SourceURL
            { url :: Text
            , sha :: Text
            } |
              SourceGit
            { url :: Text
            , sha :: Text
            , rev :: Text
            } |
              SourceFile { path :: Text }
            deriving (Show, Eq, Ord)

data PackageMatch = PackageMatch
                  { pmName  :: Text
                  , version :: SemVer
                  , source  :: Maybe Source
                  , bin     :: Bool
                  } deriving (Show, Ord, Eq)

data PackageSpec = PackageSpec
                 { psMatch            :: PackageMatch
                 , psDependencies     :: [PackageReq]
                 , psDevDependencies  :: [PackageReq]
                 , psPeerDependencies :: [PackageReq]
                 } deriving (Show, Ord, Eq)

data PackageTree = PackageTree
                 { ptMatch            :: PackageMatch
                 , ptDependencies     :: [(PackageReq, PackageTree)]
                 , ptDevDependencies  :: [(PackageReq, PackageTree)]
                 , ptPeerDependencies :: [(PackageReq, PackageTree)]
                 } deriving (Show, Ord, Eq)

instance FromJSON PackageSpec where
    parseJSON (Object v) = do
        match <- parseJSON (Object v)
        (\ d w p -> PackageSpec match (f d) (f w) (f p))
            <$> v .:? "dependencies" .!= mempty
            <*> v .:? "devDependencies" .!= mempty
            <*> v .:? "peerDependencies" .!= mempty
            where f = map (uncurry PackageReq) . M.toList

    parseJSON q = typeMismatch "PackageSpec" q

instance FromJSON PackageMatch where
    parseJSON (Object v) = PackageMatch
        <$> v .: "name"
        <*> fmap unSVJ (v .: "version")
        <*> pure Nothing
        <*> pure (isJust $ H.lookup "bin" v)

    parseJSON q = typeMismatch "PackageMatch" q

instance FromJSON Req where
    parseJSON (String "") = parseJSON (String "*")
    parseJSON (String s) = case parseSemVerRange s of
        Right v -> pure (VersionRange (TaggedRange v s))
        Left _ -> case parseURI (s ^. _Text) of
            Just uri -> case uriScheme uri of
                "github:" -> case splitOn "/" (_Text # uriPath uri) of
                    [u, h] -> pure (GitHub u h)
                    _ -> fail $ uriPath uri ++ " doesn't look like a valid github req"
                x | x `elem` ["git+https:", "git+http:", "git:"]
                    -> pure (Git (_Text # show uri))
                _ -> fail $ "unreadable URI " ++ show uri
            Nothing -> fail $ (s ^. _Text) ++ " not a URI nor a version"

    parseJSON q = typeMismatch "Req" q
