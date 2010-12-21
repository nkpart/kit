{-# LANGUAGE TupleSections #-}
module Kit.Project (
  totalSpecDependencies,
  installKit,
  generateXcodeProject,
  readSpec
  )
    where

import Kit.Spec
import Kit.Contents
import Kit.Repository
import Kit.Util
import Kit.Xcode.Builder
import Kit.Xcode.XCConfig

import Control.Monad.Error
import Data.Maybe
import Data.List
import Data.Tree
import System.Cmd

import qualified Data.ByteString as BS

-- Paths
kitDir = "." </> "Kits"
projectDir = "KitDeps.xcodeproj"
prefixFile = "Prefix.pch"
projectFile = projectDir </> "project.pbxproj"
xcodeConfigFile = "Kit.xcconfig"
depsConfigFile = "DepsOnly.xcconfig"
kitUpdateMakeFilePath = "Makefile"

kitUpdateMakeFile = "kit: Kit.xcconfig\n" ++
                    "Kit.xcconfig: ../KitSpec\n" ++
                    "\tcd .. && kit update && exit 1\n"

prefixDefault = "#ifdef __OBJC__\n" ++ 
                "    #import <Foundation/Foundation.h>\n" ++ 
                "    #import <UIKit/UIKit.h>\n" ++ 
                "#endif\n"

generateXcodeProject :: [Kit] -> Maybe String -> KitIO ()
generateXcodeProject deps depsOnlyConfig = do
  specs <- forM deps $ \kit -> readSpec (kitDir </> packageFileName kit </> "KitSpec")  
  liftIO $ inDirectory kitDir $ do
    kitsContents <- forM specs readKitContents
    createProjectFile kitsContents
    createHeader kitsContents
    createConfig kitsContents specs
    writeFile depsConfigFile $ "#include \"" ++ xcodeConfigFile ++ "\"\n" ++ fromMaybe "" depsOnlyConfig 
  where sourceDirs specs = specs >>= (\spec -> [
            kitDir </> packageFileName spec </> specSourceDirectory spec, 
            packageFileName spec </> specSourceDirectory spec
          ])
        createProjectFile cs = do
          let headers = cs >>= contentHeaders
          let sources = cs >>= contentSources
          let libs = cs >>= contentLibs
          mkdir_p projectDir
          writeFile projectFile $ renderXcodeProject headers sources libs "libKitDeps.a"
        createHeader cs = do
          let headers = mapMaybe contentPrefix cs
          let combinedHeader = stringJoin "\n" headers
          writeFile prefixFile $ prefixDefault ++ combinedHeader ++ "\n"
        createConfig cs specs = do
          let configs = mapMaybe contentConfig cs
          let combinedConfig = multiConfig "KitConfig" configs
          let kitHeaders = "HEADER_SEARCH_PATHS = $(HEADER_SEARCH_PATHS) " ++ stringJoin " " (sourceDirs specs)
          let prefixHeaders = "GCC_PRECOMPILE_PREFIX_HEADER = YES\nGCC_PREFIX_HEADER = $(SRCROOT)/Prefix.pch\n"
          writeFile xcodeConfigFile $ kitHeaders ++ "\n" ++  prefixHeaders ++ "\n" ++ configToString combinedConfig
          writeFile kitUpdateMakeFilePath kitUpdateMakeFile

-- | Return all the (unique) children of this tree (except the top node), in reverse depth order.
refineDeps :: Eq a => Tree a -> [a]
refineDeps = nub . concat . reverse . drop 1 . levels

totalSpecDependencies :: KitRepository -> KitSpec -> KitIO [Kit]
totalSpecDependencies kr spec = refineDeps <$> unfoldTreeM (unfoldDeps kr) spec
    -- todo: check for conflicts
    -- todo: check for version ranges :)

unfoldDeps :: KitRepository -> KitSpec -> KitIO (Kit, [KitSpec])
unfoldDeps kr ks = (specKit ks,) <$> mapM (getKitSpec kr) (specDependencies ks) -- s/mapM/traverse ?

installKit :: KitRepository -> Kit -> IO ()
installKit kr kit = do
    tmpDir <- getTemporaryDirectory
    let fp = tmpDir </> (packageFileName kit ++ ".tar.gz")
    putStrLn $ " -> Installing " ++ packageFileName kit
    fmap fromJust $ getKit kr kit fp
    mkdir_p kitDir 
    inDirectory kitDir $ system ("tar zxf " ++ fp)
    return ()

readSpec :: FilePath -> KitIO KitSpec
readSpec kitSpecPath = checkExists kitSpecPath >>= liftIO . BS.readFile >>= ErrorT . return . parses
  where checkExists kitSpecPath = do
          doesExist <- liftIO $ doesFileExist kitSpecPath
          if doesExist then return kitSpecPath else throwError $ "Couldn't find the spec at " ++ kitSpecPath
        parses = maybeToRight "Parse error in KitSpec file" . decodeSpec

