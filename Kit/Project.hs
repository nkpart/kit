{-# LANGUAGE TupleSections #-}
module Kit.Project (
  makeKitProject,
  KitProject(..),
  kitProjectActions
  )
    where

import Kit.AbsolutePath (filePath)
import Kit.Spec
import Kit.Contents
import Kit.Util
import Kit.Util.FSAction
import Kit.Xcode.Builder
import Kit.Xcode.XCConfig
import Data.Maybe
import qualified Data.Map as M
import Kit.FlaggedFile

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
kitFrameworksDir = "Frameworks"

kitUpdateMakeFile :: String
kitUpdateMakeFile = "kit: Kit.xcconfig\n" ++
                    "Kit.xcconfig: ../KitSpec\n" ++
                    "\tcd .. && kit update && exit 1\n"

prefixDefault :: String
prefixDefault = "#ifdef __OBJC__\n" ++
                "    #import <Foundation/Foundation.h>\n" ++
                "    #if TARGET_OS_MAC\n" ++
                "        #import <Cocoa/Cocoa.h>\n" ++
                "    #else\n" ++
                "        #import <UIKit/UIKit.h>\n" ++
                "    #endif\n" ++
                "#endif\n"

data KitProject = KitProject {
  kitProjectFile :: String,
  kitProjectPrefix :: String,
  kitProjectConfig :: String,
  kitProjectDepsConfig :: String,
  kitProjectResourceDirs :: [(FilePath, FilePath)]
} deriving (Eq, Show)

kitProjectActions :: KitProject -> [FSAction]
kitProjectActions kp = templatedFiles ++ resourceLinks where 
        resourceLinks = map (within kitDir . uncurry Symlink) $ kitProjectResourceDirs kp
        templatedFiles = map (within kitDir) [
              FileCreate projectFile (kitProjectFile kp),
              FileCreate prefixFile (kitProjectPrefix kp),
              FileCreate xcodeConfigFile (kitProjectConfig kp),
              FileCreate kitUpdateMakeFilePath kitUpdateMakeFile,
              FileCreate depsConfigFile (kitProjectDepsConfig kp)
            ]

resourceLink :: KitContents -> Maybe (FilePath, FilePath) 
resourceLink contents =  fmap (,linkName) $ contentResourceDir contents where 
                            linkName = kitResourceDir </> packageName contents

frameworkLink :: KitContents -> [(FilePath, FilePath)]
frameworkLink contents =  fmap f $ contentFrameworks contents where 
							f absFilePath = let fp = filePath absFilePath in (fp,kitFrameworksDir </> takeFileName fp)

makeKitProject :: [KitContents] -> Maybe String -> KitProject
makeKitProject kitsContents depsOnlyConfig = 
  let pf = createProjectFile kitsContents
      header = createHeader kitsContents
      config = createConfig kitsContents
      -- TODO: Make this specify an xcconfig data type
      depsConfig = "#include \"" ++ xcodeConfigFile ++ "\"\n\nSKIP_INSTALL=YES\nSDKROOT=iphoneos\n\n" ++ fromMaybe "" depsOnlyConfig 
      frameworks = concatMap frameworkLink kitsContents
      resources = mapMaybe resourceLink kitsContents
   in KitProject pf header config depsConfig (frameworks ++ resources)
  where createProjectFile cs = let
                 toSourceGroup kc = SourceGroup (packageFileName kc) (contentHeaders kc) (contentSources kc) (contentLibs kc) (map (flaggedFile "") $ contentFrameworks kc)
              in renderXcodeProject (map toSourceGroup cs) "libKitDeps.a"
        createHeader cs = let
                 headers = mapMaybe namedPrefix cs
                 combinedHeader = stringJoin "\n" headers
              in prefixDefault ++ combinedHeader ++ "\n"
        createConfig cs = let
             configs = mapMaybe contentConfig cs
             sourceDirs = map (\kc -> contentBaseDir kc </> specSourceDirectory (contentSpec kc)) cs 
             parentConfig = XCC "Base" (M.fromList [
                                                    ("HEADER_SEARCH_PATHS", "$(HEADER_SEARCH_PATHS) " ++ stringJoin " " sourceDirs),
                                                    ("GCC_PRECOMPILE_PREFIX_HEADER", "YES"),
                                                    ("GCC_PREFIX_HEADER","$(SRCROOT)/Prefix.pch")
                                                  ]) []
             combinedConfig = multiConfig "KitConfig" (parentConfig:configs)
          in configToString combinedConfig ++ "\n"

