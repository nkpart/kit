{-# LANGUAGE TupleSections #-}
module Kit.Project (
  writeKitProjectFromContents
  )
    where

import Kit.Spec
import Kit.Contents
import Kit.Util
import Kit.Util.FSAction
import Kit.Xcode.Builder
import Kit.Xcode.XCConfig

import Control.Monad.Error
import Data.Maybe
import qualified Data.Map as M

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

writeKitProjectFromContents :: [KitContents] -> Maybe String -> FilePath -> KitIO ()
writeKitProjectFromContents contents depsOnlyConfig packagesDirectory = writeKitProject $ makeKitProject contents depsOnlyConfig packagesDirectory

writeKitProject :: KitProject -> KitIO ()
writeKitProject kp = do
    liftIO $ mapM_ (runAction . within kitDir) [
          FileCreate projectFile (kitProjectFile kp),
          FileCreate prefixFile (kitProjectPrefix kp),
          FileCreate xcodeConfigFile (kitProjectConfig kp),
          FileCreate kitUpdateMakeFilePath kitUpdateMakeFile,
          FileCreate depsConfigFile (kitProjectDepsConfig kp)
        ]
    mkdirP kitDir
    liftIO $ inDirectory kitDir $ do
      let resourceDirs = kitProjectResourceDirs kp
      when (not $ null resourceDirs) $ do
        puts $ " -> Linking resources: " ++ stringJoin ", " (map fst resourceDirs)
        mapM_ (\(tgt,name) -> runAction $ Symlink tgt name) resourceDirs

resourceLink :: KitContents -> Maybe (FilePath, FilePath) 
resourceLink contents = let specResources = contentResourceDir contents
                            linkName = kitResourceDir </> packageName (contentSpec contents)
                         in (,linkName) <$> specResources

makeKitProject :: [KitContents] -> Maybe String -> FilePath -> KitProject
makeKitProject kitsContents depsOnlyConfig packagesDirectory = 
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
          let sourceDirs = map ((\spec -> packageFileName spec </> specSourceDirectory spec) . contentSpec) cs >>= (\s -> [s, packagesDirectory </> s])
          let configs = mapMaybe contentConfig cs
          let parentConfig = XCC "Base" (M.fromList [
                                                    ("HEADER_SEARCH_PATHS", "$(HEADER_SEARCH_PATHS) " ++ stringJoin " " sourceDirs),
                                                    ("GCC_PRECOMPILE_PREFIX_HEADER", "YES"),
                                                    ("GCC_PREFIX_HEADER","$(SRCROOT)/Prefix.pch")
                                                  ]) []
          let combinedConfig = multiConfig "KitConfig" (parentConfig:configs)
          configToString combinedConfig ++ "\n"

