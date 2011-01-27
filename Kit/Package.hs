module Kit.Package (package) where
  import Kit.Spec
  import Kit.Util
  import System.Cmd
  import Control.Monad.Trans
  import Data.List
  import Debug.Trace

  fileBelongsInPackage :: KitSpec -> FilePath -> Bool
  fileBelongsInPackage config fp = let
    isCore = elem fp [specResourcesDirectory config, specSourceDirectory config, specTestDirectory config, specLibDirectory config, "KitSpec"]
    isProject = any (`isSuffixOf` fp) ["xcodeproj", "xcconfig", ".pch"] -- TODO this could just check for the defined config and prefi header
      in isCore || isProject

  package :: KitSpec -> IO ()
  package spec = do
      contents <- filter (fileBelongsInPackage spec) <$> getDirectoryContents "."
      mkdirP distDir
      sh $ "tar -czf " ++ (distDir </> (kitPath ++ ".tar.gz")) ++ " -s ,^," ++ kitPath ++ "/, " ++ join (intersperse " " contents)
      return ()
    where
      distDir = "dist"
      kitPath = packageFileName spec
      sh c = liftIO . system $ trace c c

