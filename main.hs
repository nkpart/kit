module Main where
  import qualified Data.ByteString as BS

  import Kit.XCode.Builder
  import System.Environment
  import System.Directory
  import System.FilePath.Posix
  import Kit.Repository
  import Kit.Kit
  import Kit.Spec
  import Kit.Package
  import Kit.Project
  import Kit.Util   
  import Kit.XCode.Builder     
  import Data.List
  import Control.Monad.Trans
  import Data.Monoid
  import qualified Data.Traversable as T
  
  defaultLocalRepoPath = do
      home <- getHomeDirectory
      return $ home </> ".kit" </> "repository"
  defaultLocalRepository = fmap fileRepo defaultLocalRepoPath
        
  -- given a kit spec, 
  -- download all kits (and deps)
  -- extract all kits to directories
  -- manage KitProject
        
  me :: IO ()
  me = do
        r <- unKitIO g
        handleFails r
        where
    handleFails (Left e) = do
      putStrLn . show $ e
      return ()
    handleFails (Right _) = return ()
    g = do
      repo <- liftIO $ defaultLocalRepository
      deps <- getMyDeps repo
      puts "Dependencies: "
      puts . mconcat . intersperse "\n" $ map (("  * " ++) . kitFileName) deps 
      liftIO $ mapM (installKit repo) deps
      liftIO $ generateXCodeProject
      liftIO $ generateXCodeConfig
        where p x = liftIO $ print x
              puts x = liftIO $ putStrLn x
  
  handleArgs :: [String] -> IO ()
  handleArgs ["me"] = me
  
  handleArgs ["package"] = do
      mySpec <- unKitIO $ myKitSpec
      T.for mySpec package
      return ()
      
  handleArgs ["deploy-local"] = do
    mySpec <- unKitIO $ myKitSpec
    T.for mySpec package
    T.for mySpec x
    return ()
      where
        x :: KitSpec -> IO ()
        x spec = let 
              k = specKit spec
              kf = kitFileName . specKit $ spec
              pkg = (kf ++ ".tar.gz")
            in do
              repo <- defaultLocalRepoPath
              let thisKitDir = repo </> "kits" </> kitName k </> kitVersion k
              createDirectoryIfMissing True thisKitDir
              copyFile pkg $ thisKitDir </> pkg
              copyFile "KitSpec" $ thisKitDir </> "KitSpec"
      
  handleArgs _ = putStrLn "Usage: TODO"
    
  main :: IO ()
  main = do
      localRepo <- defaultLocalRepository
      path <- defaultLocalRepoPath
      createDirectoryIfMissing True path
      args <- getArgs
      handleArgs args

      
          
  