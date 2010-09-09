module Kit.Package (package) where

  import Kit.Model
  import Kit.Util
  import Kit.Project
  import System.Cmd
  import Control.Monad.Trans
  import Control.Monad
  import Data.List
  import Data.Maybe
  import Debug.Trace

  {-
    Package format
     src/ | configurable source dir
     test/
     KitSpec
     *.codeproj
  -}

  fileBelongsInPackage :: KitConfiguration -> FilePath -> Bool
  fileBelongsInPackage config fp = let
    a = elem fp [sourceDir config, "test", "KitSpec"]
    b = "xcodeproj" `isSuffixOf` fp
    c = "xcconfig" `isSuffixOf` fp
    d = ".pch" `isSuffixOf` fp
      in a || b || c || d

  package :: KitSpec -> IO ()
  package spec = do
      tempDir <- getTemporaryDirectory
      current <- getCurrentDirectory
      let kd = tempDir </> kitPath
      cleanOrCreate kd
      contents <- getDirectoryContents "."
      mapM_ (cp_r_to kd) (filter (fileBelongsInPackage . specConfiguration $ spec) contents)
      inDirectory tempDir $ sh $ "tar czf " ++ (current </> (kitPath ++ ".tar.gz")) ++ " " ++ kitPath
      return ()
    where
      kitPath = kitFileName . specKit $ spec
      sh c = liftIO $ system (trace c c)
      puts c = liftIO $ putStrLn c
      p c = liftIO $ print c
      cp_r_to kd c = sh $ "cp -r " ++ c ++ " " ++ kd ++ "/"


