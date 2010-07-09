module Main where
  import Kit.Main
  import qualified Data.Map as M
  import Kit.XCode.XCConfig
  
  conf1 = XCC "conf1" (M.fromList [("A", "1")]) []
  conf2 = XCC "conf2" (M.fromList [("A", "2")]) []
  
  --main = putStrLn $ show $ multiConfig "combo" [conf1, conf2]
  main = kitMain