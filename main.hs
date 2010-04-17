module Main where
  import qualified Data.ByteString as BS
  
  import System.Environment
  import Kit.Repository
  import Kit.Kit
  import Kit.Spec
  import Kit.Client
  
  protoRepo = fileRepo "prototype/server"  
  
  -- given a kit spec, 
  -- download all kits (and deps)
  -- extract all kits to directories
  -- manage KitProject

  
  me :: IO ()
  me = do
    kit <- myKitSpec
    
    putStrLn $ show $ kit
  
  handleArgs :: [String] -> IO ()
  handleArgs ["me"] = me
  
  handleArgs ["show", a, b] = do
          res <- getKitSpec protoRepo $ Kit a b
          putStrLn . show $ res
          
  handleArgs _ = putStrLn "Usage: none."
    
  main :: IO ()
  main = do
      args <- getArgs
      handleArgs args

      
          
  