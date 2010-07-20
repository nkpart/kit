module Kit.Project (
  getDeps,
  getMyDeps,
  installKit,
  generateXCodeConfig,
  generateXCodeProject,
  myKitSpec)
    where
      
  import Control.Monad.Trans
  import Control.Monad
  import Control.Applicative
  import Data.Foldable
  import Data.Maybe
  import Data.List
  import Data.Monoid
  import System.Cmd
  import Kit.Kit
  import Kit.Repository
  import Kit.Util
  import Kit.Spec
  import Kit.XCode.Builder
  import Kit.XCode.XCConfig
  import Kit.XCode.Prefix
  import Text.JSON
  import Kit.JSON
  import qualified Data.Traversable as T
  
  -- Paths
  kitDir = "." </> "Kits"
  projectDir = "KitDeps.xcodeproj"
  prefixFile = "KitDeps_Prefix.pch"
  projectFile = projectDir </> "project.pbxproj"
  
  prefixDefault = "#ifdef __OBJC__\n" ++ "    #import <Foundation/Foundation.h>\n    #import <UIKit/UIKit.h>\n" ++ "#endif\n"
    
  -- Represents an extracted project
  -- Headers, Sources, Config, Prefix content
  data KitContents = KitContents [String] [String] -- (Maybe XCConfig) (Maybe String)
  
  readKitContents :: KitSpec -> IO KitContents
  readKitContents (KitSpec kit spec)  = 
    let kitDir = kitFileName kit
        find tpe = glob ((kitDir </> "src/**/*") ++ tpe)
        headers = find ".h"
        sources = find ".m"
        --config = error("todo")
        --prefix = error("todo")
    in  KitContents <$> headers <*> sources -- <*> config <*> prefix
  
  generateXCodeProject :: [Kit] -> IO ()
  generateXCodeProject deps = do
    let kitFileNames = deps |> kitFileName
    mkdir_p kitDir
    inDirectory kitDir $ do
      let find tpe inDir = glob (inDir ++ "/src/**/*" ++ tpe)
      let find' tpe = fmap join . T.for kitFileNames $ find tpe
      headers <- find' ".h"
      sources <- (\a b c -> a ++ b ++ c) <$> glob "**/src/**/*.m" <*> glob "**/src/**/*.mm" <*> glob "**/src/**/*.c"
      mkdir_p projectDir
      writeFile projectFile $ buildXCodeProject headers sources
      combinedHeader <- generatePrefixHeader kitFileNames
      writeFile prefixFile $ prefixDefault ++ combinedHeader ++ "\n"

  readConfig' :: Kit -> IO (Maybe XCConfig)
  readConfig' kit = do
    exists <- doesFileExist fp
    contents <- T.sequence (fmap readFile $ justTrue exists fp)
    return $ fmap (fileContentsToXCC $ kitName kit) contents
        where
          fp = kitConfigFile kit

  generateXCodeConfig :: [FilePath] -> IO ()
  generateXCodeConfig kitFileNames = do
    inDirectory kitDir $ unKitIO $ do
      ca <- readMany kitFileNames "KitSpec" (\x -> readSpec x |> specKit >>= (liftIO . readConfig'))
      let combinedConfig = multiConfig "KitConfig" ca
      let contents = configToString combinedConfig
      let xcconfig = "HEADER_SEARCH_PATHS = $(HEADER_SEARCH_PATHS) " ++ (stringJoin " "  $ kitFileNames >>= (\x -> [kitDir </> x </> "src", x </> "src"])) ++ "\n" ++ "GCC_PRECOMPILE_PREFIX_HEADER = YES\nGCC_PREFIX_HEADER = $(SRCROOT)/KitDeps_Prefix.pch\n"
      liftIO $ writeFile "Kit.xcconfig" $ xcconfig ++ "\n" ++ contents
    return ()
      
  depsForSpec :: KitRepository -> KitSpec -> KitIO [Kit]
  depsForSpec kr spec = do
      deps <- mapM (getDeps kr) (specDependencies spec)
      return $ specDependencies spec ++ join deps      
  
  getDeps :: KitRepository -> Kit -> KitIO [Kit]
  getDeps kr kit = getKitSpec kr kit >>= depsForSpec kr

  getMyDeps :: KitRepository -> KitIO [Kit]
  getMyDeps kr = myKitSpec >>= depsForSpec kr

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
  readSpec kitSpecPath = readSpecContents kitSpecPath >>= KitIO . return . parses
  
  myKitSpec :: KitIO KitSpec
  myKitSpec = readSpec "KitSpec"

  -- private!
  checkExists :: FilePath -> KitIO FilePath
  checkExists kitSpecPath = do
    doesExist <- liftIO $ doesFileExist kitSpecPath
    maybeToKitIO ("Couldn't find the spec at " ++ kitSpecPath) (justTrue doesExist kitSpecPath)
  
  readSpecContents :: FilePath -> KitIO String
  readSpecContents kitSpecPath = checkExists kitSpecPath >>= liftIO . readFile
  
  parses :: String -> Either [KitError] KitSpec
  parses contents = case (decode contents) of 
                      Ok a -> Right a
                      Error a -> Left [a]
  
