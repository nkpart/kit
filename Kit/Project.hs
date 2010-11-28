module Kit.Project (
  getMyDeps,
  installKit,
  generateXCodeProject,
  myKitSpec)
    where

  import Control.Monad.Trans
  import Control.Monad
  import Control.Monad.Error
  import Control.Applicative
  import qualified Data.Foldable as F
  import Data.Maybe
  import Data.List
  import Data.Tree
  import Data.Monoid
  import System.Cmd
  import Kit.Model
  import Kit.Repository
  import Kit.Util
  import Kit.XCode.Builder
  import Kit.XCode.XCConfig
  import Text.JSON
  import qualified Data.Traversable as T

  -- Paths
  kitDir = "." </> "Kits"
  projectDir = "KitDeps.xcodeproj"
  prefixFile = "KitDeps_Prefix.pch"
  projectFile = projectDir </> "project.pbxproj"
  xcodeConfigFile = "Kit.xcconfig"
  kitUpdateMakeFilePath = "Makefile"
  kitUpdateMakeFile = "kit: Kit.xcconfig\nKit.xcconfig: ../KitSpec\n\tcd .. && kit update && exit 1\n"

  prefixDefault = "#ifdef __OBJC__\n" ++ "    #import <Foundation/Foundation.h>\n    #import <UIKit/UIKit.h>\n" ++ "#endif\n"

  -- Represents an extracted project
  -- Headers, Sources, Config, Prefix content
  data KitContents = KitContents { contentHeaders :: [FilePath], contentSources :: [FilePath], contentLibs :: [FilePath], contentConfig :: Maybe XCConfig, contentPrefix :: Maybe String }

  readKitContents :: KitSpec -> IO KitContents
  readKitContents spec  =
    let kit = specKit spec
        kitDir = kitFileName kit
        find dir tpe = glob ((kitDir </> dir </> "**/*") ++ tpe)
        findSrc = find $ specSourceDirectory spec
        headers = findSrc ".h"
        sources = fmap join $ T.sequence [findSrc ".m", findSrc ".mm", findSrc ".c"]
        libs = find (specLibDirectory spec) ".a" 
        config = readConfig kit
        prefix = readHeader kit
    in  KitContents <$> headers <*> sources <*> libs <*> config <*> prefix

  generateXCodeProject :: [Kit] -> KitIO ()
  generateXCodeProject deps = do
    liftIO $ mkdir_p kitDir
    specs <- forM deps $ \kit -> readSpec (kitDir </> kitFileName kit </> "KitSpec")  
    inDirectory kitDir $ do
      kitsContents <- forM specs (liftIO . readKitContents)
      liftIO $ createProjectFile kitsContents
      liftIO $ createHeader kitsContents
      liftIO $ createConfig kitsContents specs
    where kitFileNames = map kitFileName deps
          sourceDirs specs = specs >>= (\spec -> [
              kitDir </> (kitFileName . specKit $ spec) </> specSourceDirectory spec, 
              (kitFileName . specKit $ spec) </> specSourceDirectory spec
            ])
          createProjectFile cs = do
            let headers = cs >>= contentHeaders
            let sources = cs >>= contentSources
            let libs = cs >>= contentLibs
            mkdir_p projectDir
            writeFile projectFile $ buildXCodeProject headers sources
          createHeader cs = do
            let headers = mapMaybe contentPrefix cs
            let combinedHeader = stringJoin "\n" headers
            writeFile prefixFile $ prefixDefault ++ combinedHeader ++ "\n"
          createConfig cs specs = do
            let configs = mapMaybe contentConfig cs
            let combinedConfig = multiConfig "KitConfig" configs
            let kitHeaders = "HEADER_SEARCH_PATHS = $(HEADER_SEARCH_PATHS) " ++ (stringJoin " " $ sourceDirs specs)
            let prefixHeaders = "GCC_PRECOMPILE_PREFIX_HEADER = YES\nGCC_PREFIX_HEADER = $(SRCROOT)/KitDeps_Prefix.pch\n"
            writeFile xcodeConfigFile $ kitHeaders ++ "\n" ++  prefixHeaders ++ "\n" ++ configToString combinedConfig
            writeFile kitUpdateMakeFilePath kitUpdateMakeFile

  kitPrefixFile = "Prefix.pch"

  readHeader :: Kit -> IO (Maybe String)
  readHeader kit = do
    let fp = kitFileName kit </> kitPrefixFile
    exists <- doesFileExist fp
    contents <- T.sequence (fmap readFile $ justTrue exists fp)
    return contents

  readConfig :: Kit -> IO (Maybe XCConfig)
  readConfig kit = do
    let fp = kitConfigFile kit
    exists <- doesFileExist fp
    contents <- T.sequence (fmap readFile $ justTrue exists fp)
    return $ fmap (fileContentsToXCC $ kitName kit) contents

  refineDeps :: Tree Kit -> [Kit]
  refineDeps = nub . concat . reverse . drop 1 . levels

  depsForSpec' :: KitRepository -> KitSpec -> KitIO [Kit]
  depsForSpec' kr spec = do
      tree <- unfoldTreeM (unfoldDeps kr) spec
      -- todo: check for conflicts
      return $ refineDeps tree

  unfoldDeps :: KitRepository -> KitSpec -> KitIO (Kit, [KitSpec])
  unfoldDeps kr ks = do
      let kit = specKit ks
      let depKits = specDependencies ks
      specs <- mapM (getKitSpec kr) depKits
      return (kit, specs)

  getMyDeps :: KitRepository -> KitIO [Kit]
  getMyDeps kr = myKitSpec >>= depsForSpec' kr

  installKit :: KitRepository -> Kit -> IO ()
  installKit kr kit = do
      tmpDir <- getTemporaryDirectory
      let fp = tmpDir </> (kitFileName kit ++ ".tar.gz")
      putStrLn $ " -> Installing " ++ kitFileName kit
      fmap fromJust $ getKit kr kit fp
      let dest = kitDir
      mkdir_p dest
      inDirectory dest $ sh ("tar zxf " ++ fp)
      return ()
    where sh = system

  readSpec :: FilePath -> KitIO KitSpec
  readSpec kitSpecPath = readSpecContents kitSpecPath >>= ErrorT . return . parses

  myKitSpec :: KitIO KitSpec
  myKitSpec = readSpec "KitSpec"

  -- private!
  checkExists :: FilePath -> KitIO FilePath
  checkExists kitSpecPath = do
    doesExist <- liftIO $ doesFileExist kitSpecPath
    maybeToKitIO ("Couldn't find the spec at " ++ kitSpecPath) (justTrue doesExist kitSpecPath)

  readSpecContents :: FilePath -> KitIO String
  readSpecContents kitSpecPath = checkExists kitSpecPath >>= liftIO . readFile

  parses :: String -> Either KitError KitSpec
  parses contents = case (decode contents) of
                      Ok a -> Right a
                      Error a -> Left a

