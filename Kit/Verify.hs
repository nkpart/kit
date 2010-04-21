module Kit.Verify (verifyKit) where

  import Kit.Util
  import Kit.Kit
  
  verifyKit :: Kit -> IO ()
  verifyKit _ = do
    putStrLn "Hello World"