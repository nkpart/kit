{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Kit.CmdArgs (parseArgs, KitCmdArgs(..)) where
  import System.Console.CmdArgs as CA
  import Distribution.PackageDescription.TH

  appVersion :: String
  appVersion = $(packageVariable (pkgVersion . package))

  data KitCmdArgs = Update { repositoryDir :: Maybe String }
                  | Package { repositoryDir :: Maybe String }
                  | PublishLocal { tag :: Maybe String, repositoryDir :: Maybe String }
                  | Verify { sdk :: String, repositoryDir :: Maybe String}
                  | CreateSpec { name :: String, version :: String, repositoryDir :: Maybe String } 
                  | ShowTree { repositoryDir :: Maybe String }
                  deriving (Show, Data, Typeable)

  parseMode :: KitCmdArgs
  parseMode = modes [
        Update def &= help "Create an Xcode project to wrap the dependencies defined in `KitSpec`"
               &= auto -- The default mode to run. This is most common usage.
      , CreateSpec { repositoryDir = def, Kit.CmdArgs.name = (def &= typ "NAME") &= argPos 0, version = (def &= typ "VERSION") &= argPos 1 } &=
                            explicit &=
                            CA.name "create-spec" &=
                            help "Write out a KitSpec file using the given name and version."
      , Package def &=
                    help "Create a tar.gz wrapping this kit"
      , PublishLocal { repositoryDir = def, tag = Nothing } &= explicit &= CA.name "publish-local" &=
                    help "Package this kit, then deploy it to the local repository (~/.kit/repository)"
      , Verify { repositoryDir = def, sdk = "iphonesimulator" &= typ "SDK" &= help "iphoneos, iphonesimulator, etc."} &=
                    help "Package this kit, then attempt to use it as a dependency in an empty project. This will assert that the kit and all its dependencies can be compiled together."
      , ShowTree def &= help "Print out the dependency tree for this spec"
    ]

  parseArgs :: IO KitCmdArgs
  parseArgs = cmdArgs $ parseMode &= program "kit" &= summary ("Kit " ++ appVersion) &= versionArg [CA.name "v"] &= helpArg [CA.name "h"]

