module Main where
  import qualified Data.ByteString as BS
        
  import System.Environment
  import Kit.Repository
  import Kit.Kit
  import Kit.Spec
  import Kit.Client
  import Kit.Util        
  import Data.List
  import Control.Monad.Trans
  import Data.Monoid
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
        where p x = liftIO $ print x
              puts x = liftIO $ putStrLn x
  
  handleArgs :: [String] -> IO ()
  handleArgs ["me"] = me
  
  handleArgs ["show", a, b] = do
          res <- unKitIO $ getKitSpec protoRepo $ Kit a b
          print res
          
  handleArgs ["install", a, v] = do
    putStrLn $ "Installing " ++ kitFileName kit 
    unKitIO $ installKit protoRepo kit
    putStrLn "Done."
      where kit = Kit a v
  handleArgs _ = putStrLn "Usage: none."
    
  main :: IO ()
  main = do
      args <- getArgs
      handleArgs args

      
          
  