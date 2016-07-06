{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}

module Main where

import           Conf
import           Control.Concurrent.Async.Lifted
import           Control.Concurrent.Lifted       (fork, myThreadId)
import           Control.Concurrent.MVar.Lifted
import           Control.Lens
import           Control.Monad.Catch             (MonadMask)
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.Trans.Control     (MonadBaseControl)
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.ByteString                 as B
import qualified Data.ByteString.Base64          as B64
import           Data.ByteString.Lazy            (ByteString)
import           Data.ByteString.UTF8            (fromString, toString)
import           Data.List                       (isPrefixOf)
import           Data.Map                        (Map)
import qualified Data.Map                        as M
import           Data.Maybe
import           Data.SemVer
import qualified Data.Set                        as S
import           Data.Text                       (Text)
import           Data.Text.Lens
-- import           Debug.Trace
import           Logging
import           Network.HTTP.Client.OpenSSL
import           Network.URI
import           Network.Wreq
import           OpenSSL.Session
import           Options.Applicative
import           PackageTypes
import           Prefetch
import           Prelude                         hiding (log)
import           Proc
import           System.Directory
import           System.FilePath
import           System.IO
import           System.IO.Temp
import           System.IO.Unsafe
import           System.Process                  (CreateProcess (..), proc)
import           Text.PrettyPrint.ANSI.Leijen    hiding ((<$>), (</>), (<>))
import           Types

promise :: MonadBaseControl IO m => m a -> m (MVar a)
promise io = do
    mv <- newEmptyMVar
    fork (io >>= putMVar mv) >> return mv

data Opts = Opts
          { userConfig :: Bool
          , inFile     :: FilePath
          , outFile    :: Maybe FilePath
          } deriving Show

opts :: Parser Opts
opts = Opts
    <$> switch (long "user-config" <> help "Whether to read ~/.npmrc (default: no)")
    <*> strArgument (metavar "INFILE" <> help "Input package.json or node-packages.json")
    <*> optional (strArgument (metavar "OUTFILE" <> help "Where to put generated.nix"))

allOpts :: ParserInfo Opts
allOpts = info (helper <*> opts)
    (fullDesc
  <> progDesc "npm2nix-ng: Generate nix expressions for npm dependencies"
  <> Options.Applicative.header "npm2nix-ng")

requestOpts :: MonadReader Fetcher m => m Options
requestOpts = do
    Fetcher{..} <- ask
    return $ defaults
        & Network.Wreq.header "Authorization"
            .~ maybeToList (fmap (mappend "Basic " . B64.encode . fromString) authorization)
        & manager .~ Left (opensslManagerSettings context)

type Memcache key value = MVar (Map key (MVar value))

getCache :: (MonadBaseControl IO m, Ord key)
         => Memcache key value -> key -> m value -> m value
getCache cache k seeder = do
    (target, shouldWait) <- modifyMVar cache $ \ c ->
        case M.lookup k c of
            Just mv -> return (c, (mv, True))
            Nothing -> do
                v <- newEmptyMVar
                return (M.insert k v c, (v, False))

    if shouldWait
        then readMVar target
        else do
            result <- seeder
            putMVar target result
            return result

specCache :: Memcache Text ByteString
specCache = unsafePerformIO $ newMVar mempty
{-# NOINLINE specCache #-}

reqCache :: Memcache PackageReq PackageSpec
reqCache = unsafePerformIO $ newMVar mempty
{-# NOINLINE reqCache #-}

sourceCache :: Memcache PackageMatch (MVar Source)
sourceCache = unsafePerformIO $ newMVar mempty
{-# NOINLINE sourceCache #-}

getSpec :: MonadFetch m => Text -> m ByteString
getSpec pkg = getCache specCache pkg $ do
    Fetcher{..} <- ask
    r <- requestOpts
    log $ dullgreen "fetch"
      <+> ondullblack "registry"
      <+> string (registry <> "/" <> unpack pkg)
    resp <- liftIO $ getWith r (registry <> "/" <> unpack pkg)
    return $ resp ^. responseBody

getSpecMatching :: MonadFetch m => PackageReq -> m PackageSpec
getSpecMatching r@(PackageReq pkg vRange) = getCache reqCache r $ do
    sp <- case vRange of
        VersionRange tr -> getRegistryMatching pkg tr
        GitHub _ -> logError $ string $ "getGitHubMatching " ++ show r
        Git uri -> getGit uri
        Http _ -> logError $ string $ "getTarballMatching " ++ show r
        File _ -> logError $ string $ "file " ++ show r
    return sp { psReq = r }

getRegistryMatching :: MonadFetch m => Text -> TaggedRange -> m PackageSpec
getRegistryMatching pkg (TaggedRange targetSV targetText) = do
    spec <- getSpec pkg
    let allVersions = spec ^.. key "versions" . _Object . ifolded . asIndex
        Right allSVs = mapM parseSemVer allVersions
    vers <- case bestMatch' targetSV allSVs of
        Right vers -> return $ renderSV vers
        Left _ -> logError $ fillSep
            [ "No"
            , squotes (string $ unpack pkg)
            , "version matching"
            , string (unpack targetText)
            , "in"
            , string (show allVersions)
            ]
    let spec2 = fromJSON $ fromMaybe (error "missing versions.{?}")
                         $ spec ^? key "versions" . key vers
    case spec2 of
        Data.Aeson.Success p@PackageSpec{..} -> do
            src <- getCache sourceCache psMatch $ do
                let o = fromMaybe (error "missing versions.{?}.dist")
                      $ spec ^? key "versions" . key vers . key "dist"
                promise $ do
                    let tb = fromMaybe (error "missing tarball")
                           $ o ^? key "tarball" . _String
                    shasum <- prefetchUrl tb
                    return $ SourceURL tb shasum
            return p { psMatch = psMatch { source = src } }
        Error s -> logError $ yellow ("fetching" <+> string (showShortSpec pkg targetText)) <+> string s
                         <$$> string (show $ fromMaybe (error "missing versions.{?}")
                                           $ spec ^? key "versions" . key vers)
    where
        bestMatch' range vs = case filter (matches' range) vs of
            [] -> Left ("No matching versions" :: String)
            vs' -> Right $ maximum vs'
        matches' range version = case (sharedReleaseTags' range, svReleaseTags version) of
            (_, []) -> matchesSimple range version
            (Just rTags, vTags)
                | rTags == vTags -> matchesSimple range version
                | otherwise -> matchesTags range rTags vTags
            (_, _) -> False
        sharedReleaseTags' range = listToMaybe $ map svReleaseTags $ versionsOf range

getGit :: (MonadMask m, MonadFetch m) => URI -> m PackageSpec
getGit uri = do
    log $ dullgreen "fetch" <+> ondullblack "git" <+> string (show uri)
    withSystemTempDirectory "npm2nix-git-" $ \ fp -> do
        let uriStr = uriToString id (uri { uriFragment = "" }) ""
            targetUri = (if "git+" `isPrefixOf` uriStr then drop 4 else id) uriStr
            targetHash = case uriFragment uri of
                             "#" -> "master"
                             '#':bar -> bar
                             _ -> "master"
        _ <- runProc "git" ["clone", targetUri, fp]

        _ <- runCreateProc $ (proc "git" ["checkout", targetHash]) { cwd = Just fp }

        rev <- runCreateProc $ (proc "git" ["rev-parse", "@"]) { cwd = Just fp }

        pkgJson'@PackageSpec{..} <- either error return =<< eitherDecodeStrict
                                <$> liftIO (B.readFile (fp </> "package.json"))

        src <- promise $ do
            sha <- prefetchGit targetUri rev
            return $ SourceGit (_Text # targetUri) sha rev

        return pkgJson' { psMatch = psMatch { source = src } }

showShortSpec :: Text -> Text -> String
showShortSpec pkg txt = unpack pkg ++ "@" ++ unpack txt

gotten :: MVar (S.Set PackageReq)
gotten = unsafePerformIO (newMVar mempty)
{-# NOINLINE gotten #-}

getDependencies :: MonadFetch m => PackageSpec -> m PackageTree
getDependencies PackageSpec{..} = do
    b <- S.member psReq <$> readMVar gotten
    if b
        then return $ CYCLE psMatch
        else do
            modifyMVar_ gotten (return . S.insert psReq)
            resolved <- mapConcurrently (getSpecMatching >=> getDependencies) psDependencies
            return PackageTree { ptMatch = psMatch
                               , ptDependencies = zip psDependencies resolved
                               , ptDevDependencies = []
                               , ptPeerDependencies = []
                               }

main :: IO ()
main = withOpenSSL $ execParser allOpts >>= \ o -> do
    c <- getConf (userConfig o)

    tid <- myThreadId
    _ <- fork $ runLoggingWithParent tid

    let registry = fromMaybe "https://registry.npmjs.org"
                 $ Conf.lookup "registry" c
        authorization = toString . B64.decodeLenient . fromString <$> Conf.lookup "_auth" c
        fetcher = Fetcher{..}

    infile <- canonicalizePath (inFile o)

    topSrcMV <- newMVar (SourceFile $ _Text # takeDirectory infile)

    pkgJson'@PackageSpec{..} <- either error return =<< eitherDecodeStrict <$> B.readFile infile
    let pkgJson = pkgJson' { psMatch = psMatch { source = topSrcMV }
                           , psReq = PackageReq "<toplevel>" (case fromJSON "" of ~(Data.Aeson.Success a) -> a)
                           }

    ptree@PackageTree{..} <- runReaderT (getDependencies pkgJson) fetcher

    let fakeReq = (PackageReq (pmName psMatch) (VersionRange $ TaggedRange (Eq $ version psMatch) "latest")
                  , ptree)

    (case outFile o of Just fp -> withFile fp WriteMode; Nothing -> ($ stdout)) $ \ h -> do
        hPutStrLn h "{ self, fetchurl, fetchgit ? null, lib }:"
        hPutStrLn h ""
        hPutStrLn h "{"
        let PackageSpec{ psMatch = m@PackageMatch{..} } = pkgJson
        if pmName == "deps"
            then forM_ (ptDependencies ++ ptDevDependencies ++ ptPeerDependencies) $ \ (req, tree) ->
                hPutStrLn h $ "  \"" ++ unpack (prName req) ++ "\" = self.by-version." ++ showMatch (PackageTypes.ptMatch tree) ++ ";"
            else hPutStrLn h $ "  \"" ++ unpack pmName ++ "\" = self.by-version." ++ showMatch m ++ ";"
        hPutStrLn h ""
        forM_ (M.toList $ reshape fakeReq) (hPrintTree h)
        hPutStrLn h "}"

reshape :: (PackageReq, PackageTree) -> Map PackageTree (S.Set PackageReq)
reshape (req, tree)
    = treeMerge (M.singleton tree (S.singleton req))
          (foldr (treeMerge . reshape) mempty deps)
        where treeMerge = M.unionWith (<>)
              deps = case tree of
                         PackageTree { ptDependencies } -> ptDependencies
                         CYCLE _ -> []

hPrintTree :: Handle -> (PackageTree, S.Set PackageReq) -> IO ()
hPrintTree _ (CYCLE _, _) = return ()
hPrintTree h (PackageTree m@PackageMatch { pmName, version, bin = _, source } deps _ _, reqs)
    | pmName == "deps" = return ()
    | otherwise = do
    forM_ (S.toList reqs) $ \ req -> do
        hPutStrLn h $     "  by-spec." ++ showReq req ++ " =";
        hPutStrLn h $     "    self.by-version." ++ showMatch m ++ ";";

    hPutStrLn h $     "  by-version." ++ showMatch m ++ " = self.buildNodePackage {"
    hPutStrLn h $     "    name = \"" ++ unpack pmName ++ "\";"
    hPutStrLn h $     "    version = \"" ++ unpack (renderSV version) ++ "\";"
    readMVar source >>= showSource h
    hPutStrLn h       "    bin = false;"
    hPutStrLn h       "    deps = {";
    forM_ deps $ \ (req, ptree) ->
        hPutStrLn h $ "      \"" ++ getTreeName ptree ++ "\" = self.by-spec." ++ showReq req ++ ";"
    hPutStrLn h       "    };";
    hPutStrLn h       "  };"
    where
        getTreeName PackageTree { ptMatch = PackageMatch { pmName = pmName' } } = unpack pmName'
        getTreeName (CYCLE PackageMatch { pmName = pmName' }) = unpack pmName'

showSource :: Handle -> Source -> IO ()
showSource h (SourceURL u s) = do
    hPutStrLn h   "    src = fetchurl {"
    hPutStrLn h $ "      url = \"" ++ unpack u ++ "\";"
    hPutStrLn h $ "      sha256 = \"" ++ unpack s ++ "\";";
    hPutStrLn h   "    };"
showSource h (SourceGit u s r) = do
    hPutStrLn h   "    src = fetchgit {"
    hPutStrLn h   "      name = \"fetchgit-src\";"
    hPutStrLn h $ "      url = \"" ++ unpack u ++ "\";"
    hPutStrLn h $ "      sha256 = \"" ++ unpack s ++ "\";"
    hPutStrLn h $ "      rev = \"" ++ unpack r ++ "\";"
    hPutStrLn h   "    };"
showSource h (SourceFile f) = do
    hPutStrLn h   "    src = {"
    hPutStrLn h $ "      outPath = " ++ unpack f ++ ";"
    hPutStrLn h   "      name = \"src\";"
    hPutStrLn h   "    };"

showReq :: PackageReq -> String
showReq (PackageReq n r) = "\"" ++ unpack n ++ "\".\""
                                ++ showReqText r ++ "\""
    where
        showReqText (VersionRange (TaggedRange _ t)) = unpack t
        showReqText (Git uri) = uriToText uri
        showReqText e = error $ show e

showMatch :: PackageMatch -> String
showMatch PackageMatch { pmName, version } =
    "\"" ++ unpack pmName ++ "\".\""
         ++ unpack (renderSV version) ++ "\""
