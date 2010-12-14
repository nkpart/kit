module Kit.Contents (
  KitContents(..),
  readKitContents
  ) where

import Kit.Spec
import Kit.Util
import Kit.XCode.XCConfig

import qualified Data.Traversable as T

-- | The determined contents of a particular Kit
data KitContents = KitContents { 
  contentHeaders :: [FilePath],     -- ^ Paths to headers
  contentSources :: [FilePath],     -- ^ Paths to source files
  contentLibs :: [FilePath],        -- ^ Paths to static libs
  contentConfig :: Maybe XCConfig,  -- ^ Contents of the xcconfig base file
  contentPrefix :: Maybe String     -- ^ Contents of the prefix header
}

-- | Determine the contents for a Kit, assumes that we're in a folder containing the exploded
-- kits.
readKitContents :: KitSpec -> IO KitContents
readKitContents spec  =
  let kitDir = packageFileName spec
      find dir tpe = glob ((kitDir </> dir </> "**/*") ++ tpe)
      findSrc = find $ specSourceDirectory spec
      headers = findSrc ".h"
      sources = join <$> mapM findSrc [".m", ".mm", ".c"]
      libs = find (specLibDirectory spec) ".a" 
      config = readConfig spec
      prefix = readHeader spec
  in  KitContents <$> headers <*> sources <*> libs <*> config <*> prefix

-- Report missing file
readHeader :: KitSpec -> IO (Maybe String)
readHeader spec = do
  let fp = packageFileName spec </> specPrefixFile spec
  exists <- doesFileExist fp
  T.sequence (fmap readFile $ justTrue exists fp)

-- TODO make this report missing file
readConfig :: KitSpec -> IO (Maybe XCConfig)
readConfig spec = do
  let fp = packageFileName spec </> specConfigFile spec
  exists <- doesFileExist fp
  contents <- T.sequence (fmap readFile $ justTrue exists fp)
  return $ fmap (fileContentsToXCC $ packageName spec) contents

