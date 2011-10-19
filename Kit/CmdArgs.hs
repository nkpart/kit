{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Kit.CmdArgs (parseArgs, KitCmdArgs(..)) where
  import System.Console.CmdArgs as CA
  import Distribution.PackageDescription.TH

  appVersion :: String
  appVersion = $(packageVariable (pkgVersion . package))

  data KitCmdArgs = Update
                  | Package
                  | Info
                  | PublishLocal { tag :: Maybe String } 
                  | Verify { sdk :: String }
                  | CreateSpec { name :: String, version :: String } 
                  | ShowTree
                  deriving (Show, Data, Typeable)

  parseMode :: KitCmdArgs
  parseMode = modes [
        Update &= help "Create an Xcode project to wrap the dependencies defined in `KitSpec`"
               &= auto -- The default mode to run. This is most common usage.
      , CreateSpec { Kit.CmdArgs.name = (def &= typ "NAME") &= argPos 0, version = (def &= typ "VERSION") &= argPos 1 } &=
                            explicit &=
                            CA.name "create-spec" &=
                            help "Write out a KitSpec file using the given name and version."
      , Info &= help "Show name and version of this kit"
      , Package &=
                    help "Create a tar.gz wrapping this kit"
      , PublishLocal { tag = Nothing } &= explicit &= CA.name "publish-local" &=
                    help "Package this kit, then deploy it to the local repository (~/.kit/repository)"
      , Verify { sdk = "iphonesimulator4.0" &= typ "SDK" &= help "iphoneos, iphonesimulator4.0, etc."} &=
                    help "Package this kit, then attempt to use it as a dependency in an empty project. This will assert that the kit and all its dependencies can be compiled together."
      , ShowTree &= help "Print out the dependency tree for this spec"
    ]

  parseArgs :: IO KitCmdArgs
  parseArgs = cmdArgs $ parseMode &= program "kit" &= summary ("Kit " ++ appVersion) &= versionArg [CA.name "v"] &= helpArg [CA.name "h"]

