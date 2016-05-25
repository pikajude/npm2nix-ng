{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}

module Main where

import           Conf
import           Control.Concurrent.Async.Lifted
import           Control.Concurrent.Chan.Lifted
import           Control.Concurrent.Lifted       (fork)
import           Control.Concurrent.MVar.Lifted
import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.Trans.Control     (MonadBaseControl)
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.ByteString                 as B
import           Data.ByteString.Lazy            (ByteString)
import           Data.ByteString.UTF8            (fromString)
import           Data.Map                        (Map)
import qualified Data.Map                        as M
import           Data.Maybe
import           Data.SemVer                     (SemVerRange (Eq), bestMatch,
                                                  renderSV)
import           Data.SemVer.Parser
import qualified Data.Set                        as S
import           Data.Text                       (Text, strip)
import           Data.Text.Lens
import           Network.Wreq
import           Options.Applicative
import           PackageTypes
import           Prelude                         hiding (log)
import           System.Console.ANSI
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.IO
import           System.IO.Unsafe
import           System.Process.Text

data Opts = Opts
          { userConfig :: Bool
          , inFile     :: FilePath
          , outFile    :: Maybe FilePath
          } deriving Show

data Fetcher = Fetcher
             { registry      :: String
             , authorization :: Maybe String
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
            .~ maybeToList (fmap (mappend "Basic " . fromString) authorization)

type MonadFetch m = (MonadBaseControl IO m, MonadReader Fetcher m, MonadIO m)

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

sourceCache :: Memcache PackageMatch (Maybe Source)
sourceCache = unsafePerformIO $ newMVar mempty
{-# NOINLINE sourceCache #-}

logChan :: Chan (IO ())
logChan = unsafePerformIO newChan
{-# NOINLINE logChan #-}

log :: MonadBaseControl IO m => IO () -> m ()
log = writeChan logChan

getSpec :: MonadFetch m => Text -> m ByteString
getSpec pkg = getCache specCache pkg $ do
    Fetcher{..} <- ask
    r <- requestOpts
    log $ do
        setSGR [SetColor Foreground Dull Green]
        putStr "fetch "
        setSGR [Reset]
        putStrLn $ registry <> "/" <> (pkg ^. _Text)
    resp <- liftIO $ getWith r (registry <> "/" <> (pkg ^. _Text))
    return $ resp ^. responseBody

getSpecMatching :: MonadFetch m => PackageReq -> m PackageSpec
getSpecMatching r@(PackageReq pkg vRange) = getCache reqCache r $ case vRange of
    VersionRange tr -> getRegistryMatching pkg tr
    GitHub _ _ -> error "getGitHubMatching r"
    Git _ -> error "getGitMatching r"
    Http _ -> error "getTarballMatching r"

getRegistryMatching :: MonadFetch m => Text -> TaggedRange -> m PackageSpec
getRegistryMatching pkg (TaggedRange targetSV targetText) = do
    spec <- getSpec pkg
    let allVersions = spec ^.. key "versions" . _Object . ifolded . asIndex
        Right allSVs = mapM parseSemVer allVersions
    vers <- case bestMatch targetSV allSVs of
        Right vers -> return $ renderSV vers
        Left _ -> do
            log $ do
                setSGR [SetColor Foreground Dull Red]
                putStr "ERROR "
                setSGR [Reset]
                putStrLn $ "No version matching " ++ (targetText ^. _Text) ++ " in " ++ show allVersions
                exitFailure
            return undefined
    let spec2 = fromJSON $ spec ^?! key "versions" . key vers :: Result PackageSpec
    case spec2 of
        Data.Aeson.Success p@PackageSpec{..} -> do
            src <- getCache sourceCache psMatch $ do
                case spec ^? key "versions" . key vers . key "dist" of
                    Just o -> do
                        let tb = o ^?! key "tarball" . _String
                        shasum <- prefetchUrl tb
                        return $ Just $ SourceURL tb shasum
                    Nothing -> return Nothing
            return p { psMatch = psMatch { source = src } }
        Error s -> do
            log $ do
                setSGR [SetColor Foreground Dull Red]
                putStr "ERROR "
                setSGR [SetColor Foreground Dull Yellow]
                putStr $ "fetching " ++ showShortSpec pkg targetText ++ " "
                setSGR [Reset]
                putStrLn s
                print (spec ^?! key "versions" . key vers)
                exitFailure
            return undefined

prefetchUrl :: (MonadIO m, MonadBaseControl IO m) => Text -> m Text
prefetchUrl txt = do
    log $ do
        setSGR [SetColor Foreground Dull Blue]
        putStr "nix-prefetch-url "
        setSGR [Reset]
        putStrLn (txt ^. _Text)
    (ec, t1, t2) <- liftIO $ readProcessWithExitCode "nix-prefetch-url" [txt ^. _Text] ""
    case ec of
        ExitSuccess -> return $ strip t1
        ExitFailure _ -> do
            log $ do
                setSGR [SetColor Foreground Dull Red]
                putStr "ERROR "
                setSGR [SetColor Foreground Dull Yellow]
                putStr $ "prefetching " ++ txt ^. _Text ++ " "
                setSGR [Reset]
                putStrLn $ t2 ^. _Text
                exitFailure
            return $ error "fetch failure"

showShortSpec :: Text -> Text -> String
showShortSpec pkg txt = pkg ^. _Text ++ "@" ++ txt ^. _Text

getDependencies :: MonadFetch m => PackageSpec -> m PackageTree
getDependencies PackageSpec{..} = do
    resolved <- mapConcurrently (getSpecMatching >=> getDependencies) psDependencies
    return PackageTree { ptMatch = psMatch
                       , ptDependencies = zip psDependencies resolved
                       , ptDevDependencies = []
                       , ptPeerDependencies = []
                       }

main :: IO ()
main = execParser allOpts >>= \ o -> do
    c <- getConf (userConfig o)

    _tid <- fork $ forever $ join (readChan logChan)

    let registry = fromMaybe "https://registry.npmjs.org"
                 $ Conf.lookup "registry" c
        authorization = Conf.lookup "_auth" c
        fetcher = Fetcher{..}

    infile <- canonicalizePath (inFile o)

    pkgJson'@PackageSpec{..} <- either error return =<< eitherDecodeStrict <$> B.readFile infile
    let pkgJson = pkgJson' { psMatch = psMatch { source = Just (SourceFile $ _Text # takeDirectory infile) } }

    ptree@PackageTree{..} <- runReaderT (getDependencies pkgJson) fetcher

    let fakeReq = (PackageReq (pmName psMatch) (VersionRange $ TaggedRange (Eq $ version psMatch) "latest")
                  , ptree)

    (case outFile o of Just fp -> withFile fp WriteMode; Nothing -> ($ stdout)) $ \ h -> do
        hPutStrLn h "{ self, fetchurl, fetchgit ? null, lib }:"
        hPutStrLn h ""
        hPutStrLn h "{"
        let PackageSpec{ psMatch = m@PackageMatch{..} } = pkgJson
        hPutStrLn h $ "  \"" ++ pmName ^. _Text ++ "\" = self.by-version." ++ showMatch m ++ ";"
        hPutStrLn h ""
        forM_ (M.toList $ reshape fakeReq) (hPrintTree h)
        hPutStrLn h "}"

reshape :: (PackageReq, PackageTree) -> Map PackageTree (S.Set PackageReq)
reshape (req, tree@PackageTree { ptDependencies })
    = treeMerge (M.singleton tree (S.singleton req))
          (foldr (treeMerge . reshape) mempty ptDependencies)
        where treeMerge = M.unionWith (<>)

hPrintTree :: Handle -> (PackageTree, S.Set PackageReq) -> IO ()
hPrintTree h (PackageTree m@PackageMatch { pmName, version, bin = _, source } deps _ _, reqs) = do
    forM_ (S.toList reqs) $ \ req -> do
        hPutStrLn h $     "  by-spec." ++ showReq req ++ " =";
        hPutStrLn h $     "    self.by-version." ++ showMatch m ++ ";";

    hPutStrLn h $     "  by-version." ++ showMatch m ++ " = self.buildNodePackage {"
    hPutStrLn h $     "    name = \"" ++ pmName ^. _Text ++ "\";"
    hPutStrLn h $     "    version = \"" ++ renderSV version ^. _Text ++ "\";"
    showSource h source
    hPutStrLn h       "    bin = false;"
    hPutStrLn h       "    deps = {";
    forM_ deps $ \ (req, ptree) ->
        hPutStrLn h $ "      \"" ++ getTreeName ptree ++ "\" = self.by-spec." ++ showReq req ++ ";"
    hPutStrLn h       "    };";
    hPutStrLn h       "  };"
    where
        getTreeName PackageTree { ptMatch = PackageMatch { pmName = pmName' } } = pmName' ^. _Text

showSource :: Handle -> Maybe Source -> IO ()
showSource h Nothing = hPutStrLn h "    src = builtins.abort \"Source unknown\";"
showSource h (Just (SourceURL u s)) = do
    hPutStrLn h $ "    src = fetchurl {"
    hPutStrLn h $ "      url = \"" ++ u ^. _Text ++ "\";"
    hPutStrLn h $ "      sha256 = \"" ++ s ^. _Text ++ "\";";
    hPutStrLn h $ "    };"
showSource h (Just (SourceGit u s r)) = do
    hPutStrLn h $ "    src = fetchurl {"
    hPutStrLn h $ "      url = \"" ++ u ^. _Text ++ "\";"
    hPutStrLn h $ "      sha256 = \"" ++ s ^. _Text ++ "\";";
    hPutStrLn h $ "      rev = \"" ++ r ^. _Text ++ "\";";
    hPutStrLn h $ "    };"
showSource h (Just (SourceFile f)) = do
    hPutStrLn h $ "    src = {"
    hPutStrLn h $ "      outPath = " ++ f ^. _Text ++ ";";
    hPutStrLn h $ "      name = \"src\";";
    hPutStrLn h $ "    };"

showReq :: PackageReq -> String
showReq (PackageReq n r) = "\"" ++ n ^. _Text ++ "\".\""
                                ++ showReqText r ++ "\""
    where
        showReqText (VersionRange (TaggedRange _ t)) = t ^. _Text
        showReqText e = error $ show e

showMatch :: PackageMatch -> String
showMatch PackageMatch { pmName, version } =
    "\"" ++ pmName ^. _Text ++ "\".\""
         ++ renderSV version ^. _Text ++ "\""
