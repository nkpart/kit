module Kit.Package where

  import Kit.Kit
  import Kit.Spec
  import Kit.Util
  import Kit.Client
  import System.Cmd
  import System.FilePath.Posix
  import System.Directory
  import Control.Monad.Trans
  import Control.Monad
  import Data.List
  import Data.Maybe  
  import Debug.Trace
  
  {-
    Package format
     src/
     test/
     KitSpec
     *.codeproj
  -}
  
  toCopy :: FilePath -> Bool
  toCopy fp = let
    a = elem fp ["src", "test", "KitSpec"]
    b = "xcodeproj" `isSuffixOf` fp
      in a || b
      
  
  package :: KitSpec -> IO ()
  package spec = do
      tempDir <- getTemporaryDirectory
      cwd <- getCurrentDirectory
      let kd = tempDir </> kitPath
      exists <- doesDirectoryExist kd
      when exists $ removeDirectoryRecursive kd
      createDirectoryIfMissing True kd
      contents <- getDirectoryContents "."
      mapM_ (cp kd) (filter toCopy contents)
      setCurrentDirectory tempDir
      sh $ "tar czf " ++ (cwd </> (kitPath ++ ".tar.gz")) ++ " " ++ kitPath
      setCurrentDirectory cwd
      return ()
    where
      kitPath = kitFileName . specKit $ spec
      sh c = liftIO $ system (trace c c)
      puts c = liftIO $ putStrLn c
      p c = liftIO $ print c
      cp kd c = sh $ "cp -r " ++ c ++ " " ++ kd ++ "/"
  
  