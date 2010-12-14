module Kit.Package (package) where

  import Kit.Spec
  import Kit.Util
  import System.Cmd
  import Control.Monad.Trans
  import Data.List
  import Debug.Trace

  fileBelongsInPackage :: KitSpec -> FilePath -> Bool
  fileBelongsInPackage config fp = let
    isCore = elem fp [specSourceDirectory config, specTestDirectory config, specLibDirectory config, "KitSpec"]
    isProject = or $ map (`isSuffixOf` fp) ["xcodeproj", "xcconfig", ".pch"]
      in isCore || isProject

  package :: KitSpec -> IO ()
  package spec = do
      tempDir <- getTemporaryDirectory
      distDir <- (</> "dist") <$> getCurrentDirectory
      let kd = tempDir </> kitPath
      cleanOrCreate kd
      contents <- getDirectoryContents "."
      mapM_ (copyAllTo kd) (filter (fileBelongsInPackage spec) contents)
      mkdir_p distDir
      inDirectory tempDir $ do
        sh $ "tar czf " ++ (distDir </> (kitPath ++ ".tar.gz")) ++ " " ++ kitPath
      return ()
    where
      kitPath = packageFileName spec
      sh c = liftIO $ system (trace c c)
      copyAllTo kd c = sh $ "cp -r " ++ c ++ " " ++ kd ++ "/"


