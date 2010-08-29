{-# LANGUAGE DeriveDataTypeable #-}
module Kit.Main where
  import qualified Data.ByteString as BS

  import Control.Monad.Trans
  import Data.List
  import Data.Maybe
  import Data.Monoid
  import Kit.Kit
  import Kit.Package
  import Kit.Project
  import Kit.Repository
  import Kit.Spec
  import Kit.Util
  import Kit.XCode.Builder
  import qualified Data.Traversable as T
  import System.Cmd
  import System.Environment
  import Text.JSON
  import System.Console.CmdArgs
  import System.Console.CmdArgs as CA

  import Data.Data
  import Data.Typeable

  appVersion = "0.3"

  defaultLocalRepoPath = do
      home <- getHomeDirectory
      return $ home </> ".kit" </> "repository"
  defaultLocalRepository = fmap fileRepo defaultLocalRepoPath

  handleFails (Left e) = do
    print e
    return ()
  handleFails (Right _) = return ()

  doUpdate :: IO ()
  doUpdate = unKitIO g >>= handleFails
      where g = do
              repo <- liftIO defaultLocalRepository
              deps <- getMyDeps repo
              puts $ "Dependencies: " ++ (stringJoin ", " $ map kitFileName deps)
              liftIO $ mapM (installKit repo) deps
              puts " -> Generating XCode project..."
              liftIO $ generateXCodeProject deps
              puts "Kit complete. You may need to restart XCode for it to pick up any changes."
                where p x = liftIO $ print x
                      puts x = liftIO $ putStrLn x

  doPackageKit :: IO ()
  doPackageKit = do
      mySpec <- unKitIO myKitSpec
      T.for mySpec package
      return ()

  doDeployLocal :: IO ()
  doDeployLocal = do
    mySpec <- unKitIO myKitSpec
    T.for mySpec package
    T.for mySpec x
    handleFails mySpec
      where
        x :: KitSpec -> IO ()
        x spec = let
              k = specKit spec
              kf = kitFileName . specKit $ spec
              pkg = (kf ++ ".tar.gz")
            in do
              repo <- defaultLocalRepoPath
              let thisKitDir = repo </> "kits" </> kitName k </> kitVersion k
              mkdir_p thisKitDir
              copyFile pkg $ thisKitDir </> pkg
              copyFile "KitSpec" $ thisKitDir </> "KitSpec"

  doVerify :: String -> IO ()
  doVerify sdk = (unKitIO f >>= handleFails)
    where
      f = do
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
            writeFile "KitSpec" $ encode (KitSpec (Kit "verify-kit" "1.0") $ defaultConfiguration{kitConfigDependencies=[specKit mySpec]})
            doUpdate
            inDirectory "Kits" $ do
              system $ "xcodebuild -sdk " ++ sdk
          putStrLn "OK."
        puts "End checks."
      puts = liftIO . putStrLn

  doCreateSpec :: String -> String -> IO ()
  doCreateSpec name version = do
    let kit =(Kit name version)
    let spec = KitSpec kit defaultConfiguration
    writeFile "KitSpec" $ encode spec
    putStrLn $ "Created KitSpec for " ++ kitFileName kit
    return ()

  data KitCmdArgs = Update
                  | Package
                  | PublishLocal
                  | Verify { sdk :: String }
                  | CreateSpec { name :: String, version :: String } deriving (Show, Data, Typeable)

  parseArgs = cmdArgs $ modes [
        Update &= help "Create an Xcode project to wrap the dependencies defined in `KitSpec`"
               &= auto -- The default mode to run. This is most common usage.
      , (CreateSpec { Kit.Main.name = (def &= typ "NAME") &= argPos 0, version = (def &= typ "VERSION") &= argPos 1 }) &=
                            explicit &=
                            CA.name "create-spec" &=
                            help "Write out a KitSpec file using the given name and version."
      , Package &=
                    help "Create a tar.gz wrapping this kit"
      , PublishLocal &= explicit &= CA.name "publish-local" &=
                    help "Package this kit, then deploy it to the local repository (~/.kit/repository)"
      , Verify { sdk = "iphonesimulator4.0" &= typ "SDK" &= help "iphoneos3.1, iphonesimulator4.0, etc."} &=
                    help "Package this kit, then attempt to use it as a dependency in an empty project. This will assert that the kit and all its dependencies can be compiled together."
    ] &= program "kit"
      &= summary ("Kit v" ++ appVersion ++ ". It's a dependency manager for Objective-C projects built with XCode.")

  handleArgs :: KitCmdArgs -> IO ()
  handleArgs Update = doUpdate
  handleArgs Package = doPackageKit
  handleArgs PublishLocal = doDeployLocal
  handleArgs (Verify sdk) = doVerify sdk
  handleArgs (CreateSpec name version) = doCreateSpec name version

  kitMain :: IO ()
  kitMain = do
      localRepo <- defaultLocalRepository
      path <- defaultLocalRepoPath
      mkdir_p path
      handleArgs =<< parseArgs -- =<< getArgs



