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
  import System.Process
  import System.FilePath.Posix
  import Kit.Kit
  import Kit.Repository
  import Kit.Util
  import Kit.Spec
  import Kit.XCode.Builder
  
  xxx kr spec = mapM (getDeps kr) (specDependencies spec)
  
  kitDir = "." </> "Kits"
  
  generateXCodeProject :: IO ()
  generateXCodeProject = do
    dir <- getCurrentDirectory
    kitExists <- doesDirectoryExist kitDir
    when (not kitExists) $ createDirectory kitDir
    setCurrentDirectory kitDir
    let find x = fmap (filter (const True) . lines) (readProcess "ruby" ["-e", "puts Dir.glob(\"" ++ x ++ "\")"] [])
    headers <- find "**/src/**/*.h"
    sources <- find "**/src/**/*.m"
    let projDir = "KitDeps.xcodeproj"
    createDirectoryIfMissing True projDir
    writeFile (projDir </> "project.pbxproj") $ buildXCodeProject headers sources
    writeFile "KitDeps_Prefix.pch" $ "#ifdef __OBJC__\n" ++ "    #import <Foundation/Foundation.h>\n" ++ "#endif\n"
    setCurrentDirectory dir
    
  generateXCodeConfig :: IO ()
  generateXCodeConfig = do
    let base = "$(SRCROOT)" </> "Kits"
    oldDir <- getCurrentDirectory
    setCurrentDirectory kitDir
    contents <- getDirectoryContents "."
    directories <- filterM doesDirectoryExist contents
    let srcDirs = filter (\d -> not ("." `isPrefixOf` d) && not ("xcodeproj" `isSuffixOf` d) && not ("build" == d)) directories
    let xcconfig = "HEADER_SEARCH_PATHS = $(HEADER_SEARCH_PATHS) " ++ (mconcat . intersperse " " . map (\x -> x </> "src") $ srcDirs)
    writeFile "Kit.XCConfig" $ xcconfig ++ "\n"
    setCurrentDirectory oldDir
--    HEADER_SEARCH_PATHS =  /External/three20/src $(SRCROOT)/Source/External/phoenix/Source/External/functionalkit/Source/Main $(SRCROOT)/Source/External/phoenix/Source/External/motive/Source/Main $(SRCROOT)/Source/External/phoenix/Source/Main $(SRCROOT)/Source/External/phoenix/Source/External/asi-http-request/Classes
    
  
  getDeps :: KitRepository -> Kit -> KitIO [Kit]
  getDeps kr kit = do
    spec <- getKitSpec kr kit
    deps <- xxx kr spec
    return $ specDependencies spec ++ join deps

  installKit :: KitRepository -> Kit -> IO ()
  installKit kr kit = do
    tmpDir <- getTemporaryDirectory
    let fp = tmpDir </> (kitFileName kit ++ ".tar.gz")
    fmap fromJust $ getKit kr kit fp
    let dest = kitDir
    createDirectoryIfMissing True dest
    current <- getCurrentDirectory
    setCurrentDirectory dest
    sh ("tar zxvf " ++ fp)
    setCurrentDirectory current
      where sh = system

  getMyDeps :: KitRepository -> KitIO [Kit]
  getMyDeps kr = do
    mySpec <- myKitSpec
    deps <- xxx kr mySpec
    return $ join deps ++ specDependencies mySpec
  
  myKitSpec :: KitIO KitSpec
  myKitSpec = doRead >>= (\c -> KitIO . return $ parses c)

  -- private!
  specIfExists :: KitIO FilePath
  specIfExists = let kitSpecPath = "KitSpec" in do
    doesExist <- liftIO $ doesFileExist kitSpecPath
    maybeToKitIO "Couldn't find the spec file" (justTrue doesExist kitSpecPath)
  
  doRead :: KitIO String
  doRead = do
    fp <- specIfExists
    liftIO $ readFile fp
  
  parses :: String -> Either [KitError] KitSpec
  parses contents = maybeToRight ["Could not parse spec."] $ maybeRead contents
  
  