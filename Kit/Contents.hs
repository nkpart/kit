{-# LANGUAGE PackageImports #-}
module Kit.Contents (
  KitContents(..),
  readKitContents',
  namedPrefix
  ) where

import Kit.Spec
import Kit.Util
import Kit.Xcode.XCConfig
import "mtl" Control.Monad.Trans

import qualified Data.Traversable as T

-- | The determined contents of a particular Kit
data KitContents = KitContents { 
  contentSpec :: KitSpec, -- ^ The dependency the contents were created from
  contentBaseDir :: FilePath, -- ^ Path to where the content was loaded from
  contentHeaders :: [FilePath],     -- ^ Paths to headers
  contentSources :: [FilePath],     -- ^ Paths to source files
  contentLibs :: [FilePath],        -- ^ Paths to static libs
  contentConfig :: Maybe XCConfig,  -- ^ Contents of the xcconfig base file
  contentPrefix :: Maybe String,     -- ^ Contents of the prefix header
  contentResourceDir :: Maybe FilePath
}

namedPrefix :: KitContents -> Maybe String
namedPrefix kc = fmap (\s -> "//" ++ (packageFileName . contentSpec $ kc) ++ "\n" ++ s) $ contentPrefix kc

readKitContents' :: (Applicative m, MonadIO m) => FilePath -> KitSpec -> m KitContents
readKitContents' kitDir spec =
  let find dir tpe = liftIO $ inDirectory kitDir $ do
        files <- glob ((dir </> "**/*") ++ tpe)
        mapM canonicalizePath files
      findSrc = find $ specSourceDirectory spec
      headers = findSrc ".h"
      sources = findSrc .=<<. [".m", ".mm", ".c"]
      libs = find (specLibDirectory spec) ".a" 
      config = liftIO $ readConfig kitDir spec
      prefix = liftIO $ readHeader kitDir spec
      resourceDir = liftIO $ do
        let r = kitDir </> specResourcesDirectory spec
        b <- doesDirectoryExist r
        if b 
          then Just <$> canonicalizePath r
          else return Nothing
  in  KitContents spec kitDir <$> headers <*> sources <*> libs <*> config <*> prefix <*> resourceDir

-- TODO report missing file
readHeader :: FilePath -> KitSpec -> IO (Maybe String)
readHeader kitDir spec = do
  let fp = kitDir </> specPrefixFile spec
  exists <- doesFileExist fp
  T.sequence (fmap readFile $ ifTrue exists fp)

-- TODO report missing file
readConfig :: FilePath -> KitSpec -> IO (Maybe XCConfig)
readConfig kitDir spec = do
  let fp = kitDir </> specConfigFile spec
  exists <- doesFileExist fp
  contents <- T.sequence (fmap readFile $ ifTrue exists fp)
  return $ fmap (fileContentsToXCC $ packageName spec) contents

