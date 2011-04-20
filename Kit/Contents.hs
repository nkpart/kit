module Kit.Contents (
  KitContents(..),
  readKitContents,
  namedPrefix,
  makeContentsRelative 
  ) where

import Kit.Spec
import Kit.Util
import Kit.Xcode.XCConfig

import qualified Data.Traversable as T

-- | The determined contents of a particular Kit
data KitContents = KitContents { 
  contentKit :: Kit,
  contentHeaders :: [FilePath],     -- ^ Paths to headers
  contentSources :: [FilePath],     -- ^ Paths to source files
  contentLibs :: [FilePath],        -- ^ Paths to static libs
  contentConfig :: Maybe XCConfig,  -- ^ Contents of the xcconfig base file
  contentPrefix :: Maybe String,     -- ^ Contents of the prefix header
  contentResourceDir :: Maybe FilePath
}

makeContentsRelative :: FilePath -> KitContents -> KitContents
makeContentsRelative base kc = let f p = fmap (makeRelative base) (p kc)
                                in kc { contentHeaders = f contentHeaders,
                                        contentSources = f contentSources,
                                        contentLibs = f contentLibs,
                                        contentResourceDir = f contentResourceDir
                                      } 

namedPrefix :: KitContents -> Maybe String
namedPrefix kc = fmap (\s -> "//" ++ (packageFileName . contentKit $ kc) ++ "\n" ++ s) $ contentPrefix kc

-- | Determine the contents for a Kit, assumes that we're in a 
-- folder containing exploded kits
readKitContents :: KitSpec -> IO KitContents
readKitContents spec =
  let kitDir = packageFileName spec
      find dir tpe = do
        files <- glob ((kitDir </> dir </> "**/*") ++ tpe)
        mapM canonicalizePath files
      findSrc = find $ specSourceDirectory spec
      headers = findSrc ".h"
      sources = join <$> mapM findSrc [".m", ".mm", ".c"]
      libs = find (specLibDirectory spec) ".a" 
      config = readConfig spec
      prefix = readHeader spec
      resourceDir = do
        b <- doesDirectoryExist (kitDir </> specResourcesDirectory spec)
        if b then
                Just <$> canonicalizePath (kitDir </> specResourcesDirectory spec)
             else
                return Nothing
  in  KitContents (specKit spec) <$> headers <*> sources <*> libs <*> config <*> prefix <*> resourceDir

-- TODO report missing file
readHeader :: KitSpec -> IO (Maybe String)
readHeader spec = do
  let fp = packageFileName spec </> specPrefixFile spec
  exists <- doesFileExist fp
  T.sequence (fmap readFile $ ifTrue exists fp)

-- TODO report missing file
readConfig :: KitSpec -> IO (Maybe XCConfig)
readConfig spec = do
  let fp = packageFileName spec </> specConfigFile spec
  exists <- doesFileExist fp
  contents <- T.sequence (fmap readFile $ ifTrue exists fp)
  return $ fmap (fileContentsToXCC $ packageName spec) contents

