{-# LANGUAGE DeriveDataTypeable #-}
module Kit.Main where

  import Control.Monad.Trans
  import Control.Monad.Error
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

  defaultLocalRepoPath :: IO FilePath
  defaultLocalRepoPath = getHomeDirectory >>= \h -> return $ h </> ".kit" </> "repository"

  defaultLocalRepository :: IO KitRepository
  defaultLocalRepository = fileRepo <$> defaultLocalRepoPath

  handleFails :: Either KitError a -> IO ()
  handleFails = either (putStrLn . ("kit error: " ++)) (const $ return ())

  run :: KitIO a -> IO ()
  run f = runErrorT f >>= handleFails

  doUpdate :: IO ()
  doUpdate = run $ do
              repo <- liftIO defaultLocalRepository
              spec <- myKitSpec
              deps <- totalSpecDependencies repo spec
              puts $ "Dependencies: " ++ stringJoin ", " (map packageFileName deps)
              liftIO $ mapM_ (installKit repo) deps
              puts " -> Generating XCode project..."
              generateXCodeProject deps (specKitDepsXcodeFlags spec)
              puts "Kit complete. You may need to restart XCode for it to pick up any changes."

  doPackageKit :: IO ()
  doPackageKit = do
      mySpec <- runErrorT myKitSpec
      T.for mySpec package
      return ()

  doDeployLocal :: IO ()
  doDeployLocal = do
    mySpec <- runErrorT myKitSpec
    T.for mySpec package
    T.for mySpec publishLocal
    handleFails mySpec
      where
        publishLocal :: KitSpec -> IO ()
        publishLocal spec = let
              pkg = (packageFileName spec ++ ".tar.gz")
            in do
              repo <- defaultLocalRepoPath
              let thisKitDir = repo </> "kits" </> packageName spec </> packageVersion spec
              mkdir_p thisKitDir
              copyFile ("dist" </> pkg) $ thisKitDir </> pkg
              copyFile "KitSpec" $ thisKitDir </> "KitSpec"

  doVerify :: String -> IO ()
  doVerify sdk = run $ do
        mySpec <- myKitSpec
        puts "Checking that the kit can be depended upon..."
        puts " #> Deploying locally"
        liftIO doDeployLocal
        puts " #> Building temporary parent project"
        tmp <- liftIO getTemporaryDirectory
        liftIO $ inDirectory tmp $ do
          let kitVerifyDir = "kit-verify"
          cleanOrCreate kitVerifyDir
          inDirectory kitVerifyDir $ do
            let verifySpec = (defaultSpec "verify-kit" "1.0"){ specDependencies = [specKit mySpec] }
            BS.writeFile "KitSpec" $ encodeSpec verifySpec
            doUpdate
            inDirectory "Kits" $ do
              system "open KitDeps.xcodeproj"
              system $ "xcodebuild -sdk " ++ sdk
          puts "OK."
        puts "End checks."

  doCreateSpec :: String -> String -> IO ()
  doCreateSpec name version = do
    let spec = defaultSpec name version 
    BS.writeFile "KitSpec" $ encodeSpec spec
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

  handleArgs :: KitCmdArgs -> IO ()
  handleArgs Update = doUpdate
  handleArgs Package = doPackageKit
  handleArgs PublishLocal = doDeployLocal
  handleArgs (Verify sdkName) = doVerify sdkName
  handleArgs (CreateSpec name version) = doCreateSpec name version

  kitMain :: IO ()
  kitMain = do
      mkdir_p =<< defaultLocalRepoPath 
      handleArgs =<< parseArgs -- =<< getArgs

