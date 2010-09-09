{-

  Exposes the repositories.
  Allows you to:
    * copy down kit packages (getKit)
    * read kit specs (getKitSpec)
-}

module Kit.Repository (
    getKit,
    getKitSpec,
    webRepo,
    fileRepo,
    KitRepository
  ) where

  import Kit.Model
  import Kit.Util
  import Data.List
  import Network.HTTP
  import Network.URI
  import Network.BufferType
  import Data.Maybe
  import Data.Traversable
  import Control.Monad
  import Control.Monad.Trans
  import Text.JSON
  import qualified Data.ByteString as BS

  data KitRepository = KitRepository {
    repoSave :: String -> FilePath -> IO (Maybe ()),
    repoRead :: String -> IO (Maybe String)
  }

  getKit :: KitRepository -> Kit -> FilePath -> IO (Maybe ())
  getKit kr k = repoSave kr (kitPackagePath k)

  getKitSpec :: KitRepository -> Kit -> KitIO KitSpec
  getKitSpec kr k = do
    mbKitStuff <- liftIO $ repoRead kr (kitSpecPath k)
    maybe (kitError $ "Missing " ++ kitFileName k) f mbKitStuff
    where f contents = maybeToKitIO ("Invalid KitSpec file for " ++ kitFileName k) $ case (decode contents) of
                          Ok a -> Just a
                          Error _ -> Nothing

  webRepo :: String -> KitRepository
  webRepo baseUrl = KitRepository save read where
    save = download
    read = getBody

  fileRepo :: String -> KitRepository
  fileRepo baseDir = KitRepository save read where
    save src destPath = Just <$> copyFile (baseDir </> src) destPath
    read path = let file = (baseDir </> path) in do
      exists <- doesFileExist file
      sequenceA $ justTrue exists $ readFile file

  -- private!
  baseKitPath :: Kit -> String
  baseKitPath k = joinS ["kits", kitName k, kitVersion k] "/"
    where joinS xs x = foldl1 (++) $ intersperse x xs

  kitPackagePath k = baseKitPath k ++ "/" ++ kitFileName k ++ ".tar.gz"
  kitSpecPath k = baseKitPath k ++ "/" ++ "KitSpec"

  getBody :: BufferType a => HStream a => String -> IO (Maybe a)
  getBody path = let
      request = defaultGETRequest_ . fromJust . parseURI
      checkResponse r = justTrue (rspCode r == (2,0,0)) r
      leftMaybe = either (const Nothing) Just
    in do
      rr <- Network.HTTP.simpleHTTP $ request path
      return $ fmap rspBody $ leftMaybe rr

  download :: String -> FilePath -> IO (Maybe ())
  download url destination = do
      body <- getBody url
      sequenceA $ fmap (BS.writeFile destination) body



