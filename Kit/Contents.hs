module Kit.Contents (
  KitContents(..),
  readKitContents,
  namedPrefix
  ) where

import Kit.Spec
import Kit.Util
import Kit.Xcode.XCConfig
import Kit.AbsolutePath
import Kit.FlaggedFile

-- | The determined contents of a particular Kit
data KitContents = KitContents { 
  contentSpec :: KitSpec, -- ^ The dependency the contents were created from
  contentBaseDir :: FilePath, -- ^ Path to where the content was loaded from
  contentHeaders :: [FlaggedFile],     -- ^ Paths to headers
  contentSources :: [FlaggedFile],     -- ^ Paths to source files
  contentLibs :: [FlaggedFile],        -- ^ Paths to static libs
  contentConfig :: Maybe XCConfig,  -- ^ Contents of the xcconfig base file
  contentPrefix :: Maybe String,     -- ^ Contents of the prefix header
  contentResourceDir :: Maybe FilePath
}

instance Packageable KitContents where
  packageName = packageName . contentSpec
  packageVersion = packageVersion . contentSpec

namedPrefix :: KitContents -> Maybe String
namedPrefix kc = fmap (\s -> "//" ++ packageFileName kc ++ "\n" ++ s) $ contentPrefix kc

readKitContents :: (Applicative m, MonadIO m) => AbsolutePath -> KitSpec -> m KitContents
readKitContents absKitDir spec =
  let kitDir = filePath absKitDir 
      -- We need to flag every file with objc-arc or not, if we do it
      -- project wide things get really messy.
      flags = if specWithARC spec then "-fobjc-arc" else "-fno-objc-arc"
      flgFilePath f = flaggedFile f flags 
      find dir tpe = fmap flgFilePath <$> findFiles kitDir dir tpe
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
   in KitContents spec kitDir <$> headers <*> sources <*> libs <*> config <*> prefix <*> resourceDir

-- TODO report missing file
readHeader :: FilePath -> KitSpec -> IO (Maybe String)
readHeader kitDir spec = readFile' $ kitDir </> specPrefixFile spec

-- TODO report missing file
readConfig :: FilePath -> KitSpec -> IO (Maybe XCConfig)
readConfig kitDir spec = do
  contents <- readFile' $ kitDir </> specConfigFile spec
  return $ fmap (fileContentsToXCC $ packageName spec) contents

