module Kit.Main where

  import Control.Monad.Trans
  import Control.Monad.Error
  import Kit.CmdArgs
  import Kit.Commands
  import Kit.Spec
  import Kit.Package
  import Kit.Project
  import Kit.Repository
  import Kit.Util
  import qualified Data.Traversable as T
  import System.Cmd
  import Control.Arrow

  import Data.Object.Yaml
  import qualified Data.ByteString as BS

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

