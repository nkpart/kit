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
  import System.Directory
  import System.Cmd
  import System.FilePath.Posix
  import Kit.Kit
  import Kit.Repository
  import Kit.Util
  import Kit.Spec
  import Kit.XCode.Builder
  import Text.JSON
  import Kit.JSON
  import qualified Data.Traversable as T
  
  xxx kr spec = mapM (getDeps kr) (specDependencies spec)
  
  kitDir = "." </> "Kits"
  projectDir = "KitDeps.xcodeproj"
  prefixFile = "KitDeps_Prefix.pch"
  projectFile = projectDir </> "project.pbxproj"
  prefixDefault = "#ifdef __OBJC__\n" ++ "    #import <Foundation/Foundation.h>\n    #import <UIKit/UIKit.h>\n" ++ "#endif\n"
  
  kitPrefixFile = "Prefix.pch"
  
  generatePrefixHeader :: [FilePath] -> IO String
  generatePrefixHeader kitFileNames = do
      ca <- readMany kitFileNames kitPrefixFile $ \s -> do
        exists <- doesFileExist s
        T.for (justTrue exists s) readFile
      return $ stringJoin "\n" ca
      
  generateXCodeProject :: [FilePath] -> IO ()
  generateXCodeProject kitFileNames = do
    createDirectoryIfMissing True kitDir
    inDirectory kitDir $ do
      headers <- glob "**/src/**/*.h"
      sources <- glob "**/src/**/*.m"
      createDirectoryIfMissing True projectDir
      writeFile projectFile $ buildXCodeProject headers sources
      combinedHeader <- generatePrefixHeader kitFileNames
      writeFile prefixFile $ prefixDefault ++ combinedHeader ++ "\n"

  readConfig :: Kit -> IO (Maybe String)
  readConfig kit = do
          exists <- doesFileExist fp
          contents <- T.sequence (fmap readFile $ justTrue exists fp)
          return $ fmap prependKitName contents
        where
          fp = kitConfig kit
          prependKitName c = "// " ++ kitFileName kit ++ "\n" ++ c

  generateXCodeConfig :: [FilePath] -> IO ()
  generateXCodeConfig kitFileNames = do
    inDirectory kitDir $ unKitIO $ do
      ca <- readMany kitFileNames "KitSpec" (\x -> readSpec x |> specKit >>= (liftIO . readConfig))
      let contents = stringJoin "\n" ca
      let xcconfig = "HEADER_SEARCH_PATHS = $(HEADER_SEARCH_PATHS) " ++ (stringJoin " "  $ kitFileNames >>= (\x -> [kitDir </> x </> "src", x </> "src"])) ++ "\n" ++ "GCC_PRECOMPILE_PREFIX_HEADER = YES\nGCC_PREFIX_HEADER = $(SRCROOT)/KitDeps_Prefix.pch\n"
      liftIO $ writeFile "Kit.xcconfig" $ xcconfig ++ "\n" ++ contents
    return ()
      
  getDeps :: KitRepository -> Kit -> KitIO [Kit]
  getDeps kr kit = do
    spec <- getKitSpec kr kit
    deps <- xxx kr spec
    return $ specDependencies spec ++ join deps

  installKit :: KitRepository -> Kit -> IO ()
  installKit kr kit = do
      tmpDir <- getTemporaryDirectory
      let fp = tmpDir </> (kitFileName kit ++ ".tar.gz")
      putStrLn $ " -> Installing " ++ kitFileName kit
      fmap fromJust $ getKit kr kit fp
      let dest = kitDir
      createDirectoryIfMissing True dest
      inDirectory dest $ sh ("tar zxf " ++ fp)
      return ()
    where sh = system

  getMyDeps :: KitRepository -> KitIO [Kit]
  getMyDeps kr = do
    mySpec <- myKitSpec
    deps <- xxx kr mySpec
    return $ join deps ++ specDependencies mySpec
  
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
    
  readMany :: MonadIO m => [FilePath] -> FilePath -> (String -> m (Maybe a)) -> m [a]
  readMany dirs fileInDir f = do
    directories <- liftIO $ filterM doesDirectoryExist dirs
    let kitSpecFiles = map (</> fileInDir) directories
    kitContents <- mapM f kitSpecFiles
    return $ catMaybes kitContents
