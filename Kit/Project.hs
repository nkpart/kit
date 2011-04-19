{-# LANGUAGE TupleSections #-}
module Kit.Project (
  totalSpecDependencies,
  unpackKit,
  generateKitProjectFromSpecs,
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

kitDir, projectDir, prefixFile, projectFile, xcodeConfigFile, depsConfigFile, kitUpdateMakeFilePath :: FilePath

kitDir = "." </> "Kits"
projectDir = "KitDeps.xcodeproj"
prefixFile = "Prefix.pch"
projectFile = projectDir </> "project.pbxproj"
xcodeConfigFile = "Kit.xcconfig"
depsConfigFile = "DepsOnly.xcconfig"
kitUpdateMakeFilePath = "Makefile"

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

generateKitProject :: KitProject -> KitIO ()
generateKitProject kp = liftIO $ inDirectory kitDir $ do
  runAction $ FileCreate projectFile $ kitProjectFile kp
  runAction $ FileCreate prefixFile $ kitProjectPrefix kp
  runAction $ FileCreate xcodeConfigFile $ kitProjectConfig kp
  runAction $ FileCreate kitUpdateMakeFilePath kitUpdateMakeFile
  runAction $ FileCreate depsConfigFile $ kitProjectDepsConfig kp
  when (not . null . kitProjectResourceDirs $ kp) $ do
    puts $ " -> Linking resources: " ++ stringJoin ", " (map fst $ kitProjectResourceDirs kp)
    mapM_ (\(tgt,name) -> runAction $ Symlink tgt name) $ kitProjectResourceDirs kp

generateKitProjectFromSpecs :: [KitSpec] -> Maybe String -> KitIO ()
generateKitProjectFromSpecs specs depsOnlyConfig = do
  kp <- generateXcodeProject specs depsOnlyConfig 
  generateKitProject kp

generateXcodeProject :: [KitSpec] -> Maybe String -> KitIO KitProject 
generateXcodeProject specs depsOnlyConfig = do
  liftIO $ inDirectory kitDir $ do
    currentDir <- getCurrentDirectory
    kitsContents <- mapM (fmap (makeContentsRelative currentDir) . readKitContents) specs
    let pf = createProjectFile kitsContents
    let header = createHeader kitsContents
    let config = createConfig kitsContents
    -- TODO: Make this specify an xcconfig
    let depsConfig = "#include \"" ++ xcodeConfigFile ++ "\"\n\nSKIP_INSTALL=YES\n\n" ++ fromMaybe "" depsOnlyConfig 
    resources <- filterM (doesDirectoryExist . fst) $ map resourceLink specs 
    return $ KitProject pf header config depsConfig resources
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
          let sourceDirs = map (\spec -> packageFileName spec </> specSourceDirectory spec) specs >>= (\s -> [s, kitDir </> s])
          let configs = mapMaybe contentConfig cs
          let parentConfig = XCC "Base" (M.fromList [
                                                    ("HEADER_SEARCH_PATHS", "$(HEADER_SEARCH_PATHS) " ++ stringJoin " " sourceDirs),
                                                    ("GCC_PRECOMPILE_PREFIX_HEADER", "YES"),
                                                    ("GCC_PREFIX_HEADER","$(SRCROOT)/Prefix.pch")
                                                  ]) []
          let combinedConfig = multiConfig "KitConfig" (parentConfig:configs)
          configToString combinedConfig ++ "\n"

resourceLink :: KitSpec -> (FilePath, FilePath) 
resourceLink spec = 
  let specResources = packageFileName spec </> specResourcesDirectory spec
      linkName = "Resources" </> packageName spec
   in (specResources, linkName)

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

unpackKit :: KitRepository -> Kit -> IO ()
unpackKit kr kit = do
    tmpDir <- getTemporaryDirectory
    let fp = tmpDir </> (packageFileName kit ++ ".tar.gz")
    putStrLn $ " -> Installing " ++ packageFileName kit
    copyKitPackage kr kit fp
    mkdirP kitDir 
    inDirectory kitDir $ system ("tar zxf " ++ fp)
    return ()

