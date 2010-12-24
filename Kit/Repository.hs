module Kit.Repository (
    getKit,
    getKitSpec,
    webRepo,
    fileRepo,
    KitRepository
  ) where

  import Kit.Spec
  import Kit.Util

  import Network.BufferType
  import Network.HTTP
  import Network.URI
  import Control.Monad.Error
  import Data.Maybe
  import qualified Data.List as L
  import qualified Data.Traversable as T
  import qualified Data.ByteString as BS

  data KitRepository = KitRepository {
    repoSave :: String -> FilePath -> IO (Maybe ()),
    repoRead :: String -> IO (Maybe BS.ByteString)
  }

  getKit :: KitRepository -> Kit -> FilePath -> IO (Maybe ())
  getKit kr k = repoSave kr (kitPackagePath k)

  getKitSpec :: KitRepository -> Kit -> KitIO KitSpec
  getKitSpec kr k = do
    mbKitStuff <- liftIO $ repoRead kr (kitSpecPath k)
    maybe (throwError $ "Missing " ++ packageFileName k) f mbKitStuff
    where f contents = maybeToKitIO ("Invalid KitSpec file for " ++ packageFileName k) $ decodeSpec contents

  -- TODO: Use the base url!
  webRepo :: String -> KitRepository
  webRepo _ = KitRepository save doRead where
    save = download
    doRead = getBody

  fileRepo :: String -> KitRepository
  fileRepo baseDir = KitRepository save doRead where
    save src destPath = Just <$> copyFile (baseDir </> src) destPath
    doRead fp = let file = (baseDir </> fp) in do
      exists <- doesFileExist file
      T.sequenceA $ justTrue exists $ BS.readFile file

  -- private!
  baseKitPath :: Kit -> String
  baseKitPath k = joinS ["kits", kitName k, kitVersion k] "/"
    where joinS xs x = foldl1 (++) $ L.intersperse x xs

  kitPackagePath, kitSpecPath :: Kit -> String

  kitPackagePath k = baseKitPath k ++ "/" ++ packageFileName k ++ ".tar.gz"
  kitSpecPath k = baseKitPath k ++ "/" ++ "KitSpec"

  getBody :: BufferType a => HStream a => String -> IO (Maybe a)
  getBody p = let
      request = defaultGETRequest_ . fromJust . parseURI
      leftMaybe = either (const Nothing) Just
    in do
      rr <- Network.HTTP.simpleHTTP $ request p 
      return $ fmap rspBody $ leftMaybe rr

  download :: String -> FilePath -> IO (Maybe ())
  download url destination = do
      body <- getBody url
      T.sequenceA $ fmap (BS.writeFile destination) body



