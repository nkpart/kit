{-# LANGUAGE DeriveDataTypeable #-}

module Kit.Cmd where
  import System.Console.CmdArgs
  data KitCmd = Me 
              | Package
              | DeployLocal
              | Verify {sdk :: String}
              | CreateSpec
              deriving (Show, Data, Typeable)
  
  
  me_ = mode Me
  package_ = mode Package
  deployLocal_ = mode DeployLocal
  verify_ = mode $ Verify { sdk = def &= empty "iphonesimulator3.0" & typ "sdk" }
  createSpec_ = mode CreateSpec
  
  main2 = print =<< cmdArgs "Sample v2, (C) Neil Mitchell 2009" [me_, package_, deployLocal_,  verify_, createSpec_]
  