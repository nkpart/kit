module Main where

  import qualified Data.ByteString as BS
  
  import Kit.Repository
  import Kit.Kit
  import Kit.Spec
  
  motiveKit = Kit "motive" "0.1"
  fkKit = Kit "functional-kit" "0.1"
  phoenixKit = Kit "phoenix" "0.1"
  
  motiveSpec = KitSpec motiveKit []
  fkSpec = KitSpec fkKit []
  phoenixSpec = KitSpec phoenixKit [motiveKit, fkKit]
  
  -- given a kit spec, 
  -- download all kits (and deps)
  -- extract all kits to directories
  -- manage KitProject
  
  main :: IO ()
  main = do
    putStrLn $ "HI"
          
  