{-# LANGUAGE DeriveDataTypeable #-}
module Kit.Main where

  import Control.Monad.Trans
  import Control.Monad.Error
  import Kit.Commands
  import Kit.Spec
  import Kit.Package
  import Kit.Project
  import Kit.Repository
  import Kit.Util
  import qualified Data.Traversable as T
  import System.Cmd
  import System.Console.CmdArgs as CA
  import Control.Arrow

  import Data.Object.Yaml
  import qualified Data.ByteString as BS

  appVersion :: String
  appVersion = "0.5.2" -- TODO how to keep this up to date with cabal?

  doUpdate :: Command ()
  doUpdate = do
              repo <- myRepository
              spec <- mySpec
              deps <- liftKit $ totalSpecDependencies repo spec
              puts $ "Dependencies: " ++ stringJoin ", " (map packageFileName deps)
              liftIO $ mapM_ (installKit repo) deps
              puts " -> Generating XCode project..."
              liftKit $ generateXCodeProject deps (specKitDepsXcodeFlags spec)
              puts "Kit complete. You may need to restart XCode for it to pick up any changes."

  doPackageKit :: Command ()
  doPackageKit = mySpec >>= liftIO . package 

  doDeployLocal :: Command ()
  doDeployLocal = mySpec >>= \spec -> liftIO $ do 
    package spec
    publishLocal spec 
    return ()
      where
        publishLocal :: KitSpec -> IO ()
        publishLocal spec = let pkg = (packageFileName spec ++ ".tar.gz")
                            in do
                              repo <- defaultLocalRepoPath
                              let thisKitDir = repo </> "kits" </> packageName spec </> packageVersion spec
                              mkdir_p thisKitDir
                              copyFile ("dist" </> pkg) (thisKitDir </> pkg)
                              copyFile "KitSpec" (thisKitDir </> "KitSpec")

  doVerify :: String -> Command ()
  doVerify sdk = do
        mySpec <- mySpec
        puts "Checking that the kit can be depended upon..."
        puts " #> Deploying locally"
        doDeployLocal
        puts " #> Building temporary parent project"
        tmp <- liftIO getTemporaryDirectory
        inDirectory tmp $ do
          let kitVerifyDir = "kit-verify"
          cleanOrCreate kitVerifyDir
          inDirectory kitVerifyDir $ do
            writeSpec "KitSpec" (defaultSpec "verify-kit" "1.0") { specDependencies = [specKit mySpec] }
            doUpdate
            inDirectory "Kits" $ do
              liftIO $ system "open KitDeps.xcodeproj"
              liftIO $ system $ "xcodebuild -sdk " ++ sdk
          puts "OK."
        puts "End checks."

  doCreateSpec :: String -> String -> Command ()
  doCreateSpec name version = do
    let spec = defaultSpec name version 
    writeSpec "KitSpec" spec
    puts $ "Created KitSpec for " ++ packageFileName spec

  data KitCmdArgs = Update
                  | Package
                  | PublishLocal
                  | Verify { sdk :: String }
                  | CreateSpec { name :: String, version :: String } deriving (Show, Data, Typeable)

  parseArgs = cmdArgs $ modes [
        Update &= help "Create an Xcode project to wrap the dependencies defined in `KitSpec`"
               &= auto -- The default mode to run. This is most common usage.
      , CreateSpec { Kit.Main.name = (def &= typ "NAME") &= argPos 0, version = (def &= typ "VERSION") &= argPos 1 } &=
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
      &= summary ("Kit v" ++ appVersion ++ ". It's a dependency manager for Objective-C projects built with XCode.")

  handleArgs :: KitCmdArgs -> Command ()
  handleArgs Update = doUpdate
  handleArgs Package = doPackageKit
  handleArgs PublishLocal = doDeployLocal
  handleArgs (Verify sdkName) = doVerify sdkName
  handleArgs (CreateSpec name version) = doCreateSpec name version

  kitMain :: IO ()
  kitMain = do
      mkdir_p =<< defaultLocalRepoPath 
      args <- parseArgs
      runCommand $ handleArgs args 

