{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module PackageTypes where

import           Control.Concurrent.MVar (MVar)
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.HashMap.Strict     as H
import qualified Data.Map                as M
import           Data.Maybe
import           Data.SemVer
import           Data.Text               (Text, splitOn)
import           Data.Text.Lens
import           Network.URI

instance Show (MVar Source) where
    show _ = "<fetching>"

instance Ord (MVar Source) where
    compare _ _ = EQ

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
         | GitHub URI
         | Git URI
         | Http URI
         | File URI
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
                  , source  :: MVar Source
                  , bin     :: Bool
                  } deriving (Show, Ord, Eq)

data PackageSpec = PackageSpec
                 { psReq              :: PackageReq
                 , psMatch            :: PackageMatch
                 , psDependencies     :: [PackageReq]
                 , psDevDependencies  :: [PackageReq]
                 , psPeerDependencies :: [PackageReq]
                 } deriving (Show, Ord, Eq)

data PackageTree = PackageTree
                 { ptMatch            :: PackageMatch
                 , ptDependencies     :: [(PackageReq, PackageTree)]
                 , ptDevDependencies  :: [(PackageReq, PackageTree)]
                 , ptPeerDependencies :: [(PackageReq, PackageTree)]
                 }
                 | CYCLE PackageMatch
                 deriving Show

instance Eq PackageTree where
    PackageTree m _ _ _ == PackageTree m1 _ _ _ = m == m1
    CYCLE a == CYCLE b = a == b
    _ == _ = False

instance Ord PackageTree where
    compare (PackageTree m _ _ _) (PackageTree m1 _ _ _) = compare m m1
    compare (CYCLE a) (CYCLE b) = compare a b
    compare PackageTree{} CYCLE{} = GT
    compare CYCLE{} PackageTree{} = LT

instance FromJSON PackageSpec where
    parseJSON (Object v) = do
        match <- parseJSON (Object v)
        (\ d w p -> PackageSpec undefined match (f d) (f w) (f p))
            <$> v .:? "dependencies" .!= mempty
            <*> v .:? "devDependencies" .!= mempty
            <*> v .:? "peerDependencies" .!= mempty
            where f = map (uncurry PackageReq) . M.toList

    parseJSON (Array a) = (\ d -> PackageSpec undefined fakeMatch d [] [])
        <$> mapM parseJSON (a ^.. traverse)
        where fakeMatch = PackageMatch "deps" (SemVer 0 0 0 []) (error "source not registered") False

    parseJSON q = typeMismatch "PackageSpec" q

instance FromJSON PackageReq where
    parseJSON (String s) = pure (PackageReq s (VersionRange (TaggedRange (Geq (SemVer 0 0 0 [])) "*")))
    parseJSON (Object o)
        | [(pkg, vers)] <- H.toList o = PackageReq pkg <$> parseJSON vers
        | otherwise = fail "Error trying to parse top-level dependencies: objects should only have one key and value"

    parseJSON q = typeMismatch "PackageReq" q

instance FromJSON PackageMatch where
    parseJSON (Object v) = PackageMatch
        <$> v .: "name"
        <*> fmap unSVJ (v .: "version")
        <*> pure (error "source not registered")
        <*> pure (isJust $ H.lookup "bin" v)

    parseJSON q = typeMismatch "PackageMatch" q

instance FromJSON Req where
    parseJSON (String s)
        | s `elem` ["", "latest"] = parseJSON (String "*")
    parseJSON (String s) = case parseSemVerRange s of
        Right v -> pure (VersionRange (TaggedRange v s))
        Left _ -> case parseURI (s ^. _Text) of
            Just uri -> case uriScheme uri of
                "github:" -> case splitOn "/" (_Text # uriPath uri) of
                    [_, _] -> pure (GitHub uri)
                    _ -> fail $ uriPath uri ++ " doesn't look like a valid github req"
                x | x `elem` ["git+https:", "git+http:", "git:"] -> pure (Git uri)
                x | x `elem` ["https:", "http:"] -> pure (Http uri)
                x | x `elem` ["file:"] -> pure (File uri)
                _ -> fail $ "unreadable URI " ++ show uri
            Nothing -> fail $ (s ^. _Text) ++ " not a URI nor a version"

    parseJSON q = typeMismatch "Req" q
