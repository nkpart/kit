module Main where
  import qualified Data.ByteString as BS

  import Kit.XCode.Builder
  import System.Environment
  import Kit.Repository
  import Kit.Kit
  import Kit.Spec
  import Kit.Package
  import Kit.Client
  import Kit.Util   
  import Kit.XCode.Builder     
  import Data.List
  import Control.Monad.Trans
  import Data.Monoid
  import qualified Data.Traversable as T
  protoRepo = fileRepo "prototype/server"  
        
  -- given a kit spec, 
  -- download all kits (and deps)
  -- extract all kits to directories
  -- manage KitProject
        
  me :: IO ()
  me = do
        r <- unKitIO g
        return $ handleFails r
        where
    handleFails r = () -- TODO
    g = do
      deps <- getMyDeps protoRepo
      puts "Dependencies: "
      puts . mconcat . intersperse "\n" $ map (("  * " ++) . kitFileName) deps 
      liftIO $ mapM (installKit protoRepo) deps
      liftIO $ generateXCodeProject
        where p x = liftIO $ print x
              puts x = liftIO $ putStrLn x
  
  handleArgs :: [String] -> IO ()
  handleArgs ["me"] = me
  handleArgs ["package"] = do
      mySpec <- unKitIO $ myKitSpec
      T.for mySpec package
      return ()
  handleArgs ["show", a, b] = do
          res <- unKitIO $ getKitSpec protoRepo $ Kit a b
          print res
          
  -- handleArgs ["test"] = testBuilder
  handleArgs ["install", a, v] = do
    putStrLn $ "Installing " ++ kitFileName kit 
    installKit protoRepo kit
    putStrLn "Done."
      where kit = Kit a v
  handleArgs _ = putStrLn "Usage: none."
    
  main :: IO ()
  main = do
      args <- getArgs
      handleArgs args

      
          
  