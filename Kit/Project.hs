{-# LANGUAGE TupleSections #-}
module Kit.Project (
  totalSpecDependencies,
  unpackKit,
  writeKitProjectFromSpecs,
  dependencyTree
  )
    where

import Kit.Spec
import Kit.Contents
import Kit.Repository
import Kit.Util
import Kit.Util.FSAction
import Kit.Xcode.Builder
import Kit.Xcode.XCConfig

import Control.Monad.Error
import Data.Maybe
import Data.List
import qualified Data.Map as M
import Data.Tree
import System.Cmd

-- Paths

kitDir, projectDir, prefixFile, projectFile, xcodeConfigFile, depsConfigFile, kitUpdateMakeFilePath, kitResourceDir :: FilePath

kitDir = "." </> "Kits"
projectDir = "KitDeps.xcodeproj"
prefixFile = "Prefix.pch"
projectFile = projectDir </> "project.pbxproj"
xcodeConfigFile = "Kit.xcconfig"
depsConfigFile = "DepsOnly.xcconfig"
kitUpdateMakeFilePath = "Makefile"
kitResourceDir = "Resources"

kitUpdateMakeFile :: String
kitUpdateMakeFile = "kit: Kit.xcconfig\n" ++
                    "Kit.xcconfig: ../KitSpec\n" ++
                    "\tcd .. && kit update && exit 1\n"

prefixDefault :: String
prefixDefault = "#ifdef __OBJC__\n" ++ 
                "    #import <Foundation/Foundation.h>\n" ++ 
                "    #import <UIKit/UIKit.h>\n" ++ 
                "#endif\n"

data KitProject = KitProject {
  kitProjectFile :: String,
  kitProjectPrefix :: String,
  kitProjectConfig :: String,
  kitProjectDepsConfig :: String,
  kitProjectResourceDirs :: [(FilePath, FilePath)]
} deriving (Eq, Show)

projectFromSpecs :: [KitSpec] -> Maybe String -> KitIO KitProject 
projectFromSpecs specs depsOnlyConfig = do
  kitsContents <- readAllKitsContents specs
  return $ makeXcodeProjectFromContents kitsContents depsOnlyConfig

writeKitProject :: KitProject -> KitIO ()
writeKitProject kp = liftIO $ inDirectory kitDir $ do
  runAction $ FileCreate projectFile $ kitProjectFile kp
  runAction $ FileCreate prefixFile $ kitProjectPrefix kp
  runAction $ FileCreate xcodeConfigFile $ kitProjectConfig kp
  runAction $ FileCreate kitUpdateMakeFilePath kitUpdateMakeFile
  runAction $ FileCreate depsConfigFile $ kitProjectDepsConfig kp
  let resourceDirs = kitProjectResourceDirs kp
  when (not $ null resourceDirs) $ do
    puts $ " -> Linking resources: " ++ stringJoin ", " (map fst resourceDirs)
    mapM_ (\(tgt,name) -> runAction $ Symlink tgt name) resourceDirs

writeKitProjectFromSpecs :: [KitSpec] -> Maybe String -> KitIO ()
writeKitProjectFromSpecs specs depsOnlyConfig = writeKitProject =<< projectFromSpecs specs depsOnlyConfig 

readAllKitsContents specs = do
    -- changing this is hard, because it makes an assumption that it's running from within Kits. Fix!
    originalContents <- mapM (readKitContents' (("Kits" </>) . packageFileName)) specs
    currentDir <- liftIO getCurrentDirectory
    puts $ show currentDir </> "Kits"
    puts $ show $ take 5 $ concatMap contentSources originalContents
    let vs = fmap (makeContentsRelative (currentDir </> "Kits")) originalContents
    puts $ show $ take 5 $ concatMap contentSources vs
    return vs
    

makeXcodeProjectFromContents :: [KitContents] -> Maybe String -> KitProject
makeXcodeProjectFromContents kitsContents depsOnlyConfig = 
  let pf = createProjectFile kitsContents
      header = createHeader kitsContents
      config = createConfig kitsContents
  -- TODO: Make this specify an xcconfig
      depsConfig = "#include \"" ++ xcodeConfigFile ++ "\"\n\nSKIP_INSTALL=YES\n\n" ++ fromMaybe "" depsOnlyConfig 
      resources = mapMaybe resourceLink kitsContents
   in KitProject pf header config depsConfig resources
  where createProjectFile cs = do
          let headers = concatMap contentHeaders cs
          let sources = concatMap contentSources cs
          let libs = concatMap contentLibs cs
          renderXcodeProject headers sources libs "libKitDeps.a"
        createHeader cs = do
          let headers = mapMaybe namedPrefix cs
          let combinedHeader = stringJoin "\n" headers
          prefixDefault ++ combinedHeader ++ "\n"
        createConfig cs = do
          let sourceDirs = map ((\spec -> packageFileName spec </> specSourceDirectory spec) . contentSpec) cs >>= (\s -> [s, kitDir </> s])
          let configs = mapMaybe contentConfig cs
          let parentConfig = XCC "Base" (M.fromList [
                                                    ("HEADER_SEARCH_PATHS", "$(HEADER_SEARCH_PATHS) " ++ stringJoin " " sourceDirs),
                                                    ("GCC_PRECOMPILE_PREFIX_HEADER", "YES"),
                                                    ("GCC_PREFIX_HEADER","$(SRCROOT)/Prefix.pch")
                                                  ]) []
          let combinedConfig = multiConfig "KitConfig" (parentConfig:configs)
          configToString combinedConfig ++ "\n"

resourceLink :: KitContents -> Maybe (FilePath, FilePath) 
resourceLink contents = 
  let specResources = contentResourceDir contents
      linkName = kitResourceDir </> packageName (contentSpec contents)
   in (,linkName) <$> specResources

-- | Return all the (unique) children of this tree (except the top node), in reverse depth order.
refineDeps :: Eq a => Tree a -> [a]
refineDeps = nub . concat . reverse . drop 1 . levels

-- todo: check for conflicts
-- todo: check for version ranges :)
totalSpecDependencies :: KitRepository -> KitSpec -> KitIO [KitSpec]
totalSpecDependencies kr spec = refineDeps <$> unfoldTreeM (unfoldDeps kr) spec

dependencyTree :: KitRepository -> KitSpec -> KitIO (Tree KitSpec)
dependencyTree kr spec = unfoldTreeM (unfoldDeps kr) spec

unfoldDeps :: KitRepository -> KitSpec -> KitIO (KitSpec, [KitSpec])
unfoldDeps kr ks = (ks,) <$> mapM (readKitSpec kr) (specDependencies ks) -- s/mapM/traverse ?

-- Note: this is here because it unpacks to the 'Kits', directory, which is kinda related to the project...
-- TODO: this will become a repository function when it unpacks into the exploded dir of kits within the 
-- local repo. then we won't need unpacking into 'Kits'!
unpackKit :: KitRepository -> Kit -> IO ()
unpackKit kr kit = do
    tmpDir <- getTemporaryDirectory
    let fp = tmpDir </> (packageFileName kit ++ ".tar.gz")
    putStrLn $ " -> Installing " ++ packageFileName kit
    copyKitPackage kr kit fp
    mkdirP kitDir 
    inDirectory kitDir $ system ("tar zxf " ++ fp)
    return ()

