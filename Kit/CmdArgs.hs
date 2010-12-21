{-# LANGUAGE DeriveDataTypeable #-}
module Kit.CmdArgs (parseArgs, KitCmdArgs(..)) where

  import System.Console.CmdArgs as CA

  appVersion :: String
  appVersion = "0.5.2" -- TODO how to keep this up to date with cabal?

  data KitCmdArgs = Update
                  | Package
                  | PublishLocal
                  | Verify { sdk :: String }
                  | CreateSpec { name :: String, version :: String } deriving (Show, Data, Typeable)

  parseArgs = cmdArgs $ modes [
        Update &= help "Create an Xcode project to wrap the dependencies defined in `KitSpec`"
               &= auto -- The default mode to run. This is most common usage.
      , CreateSpec { Kit.CmdArgs.name = (def &= typ "NAME") &= argPos 0, version = (def &= typ "VERSION") &= argPos 1 } &=
                            explicit &=
                            CA.name "create-spec" &=
                            help "Write out a KitSpec file using the given name and version."
      , Package &=
                    help "Create a tar.gz wrapping this kit"
      , PublishLocal &= explicit &= CA.name "publish-local" &=
                    help "Package this kit, then deploy it to the local repository (~/.kit/repository)"
      , Verify { sdk = "iphonesimulator4.0" &= typ "SDK" &= help "iphoneos, iphonesimulator4.0, etc."} &=
                    help "Package this kit, then attempt to use it as a dependency in an empty project. This will assert that the kit and all its dependencies can be compiled together."
    ] &= program "kit"
      &= summary ("Kit v" ++ appVersion ++ ". It's a dependency manager for Objective-C projects built with Xcode.")
      

