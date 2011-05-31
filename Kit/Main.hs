module Kit.Main where

  import Control.Monad.Trans
  import qualified Kit.CmdArgs as KA 
  import Kit.Commands
  import Kit.Spec
  import Kit.Package
  import Kit.Project
  import Kit.Util
  import Kit.Contents
  import Kit.Dependency
  import Kit.WorkingCopy
  import System.Cmd
  import System.Exit
  import Data.Tree (drawTree)
  import Data.List (partition)
  import Kit.Repository (KitRepository, unpackKit, packagesDirectory, publishLocally)

  loadKitFromBase :: (Applicative m, MonadIO m) => FilePath -> FilePath -> KitSpec -> m KitContents
  loadKitFromBase base kitDir spec = do
    absoluteBase <- liftIO $ canonicalizePath base
    readKitContents' (absoluteBase </> kitDir) spec

  dependencyContents :: (Applicative m, MonadIO m) => KitRepository -> Dependency -> m KitContents
  dependencyContents repo dep = dependency inRepo local dep where -- TODO look at all these applications of dep
    inRepo spec = loadKitFromBase (packagesDirectory repo) (packageFileName spec) spec
    local spec fp = loadKitFromBase devKitDir fp spec

  doUpdate :: Command ()
  doUpdate = do
              repo <- myRepository
              workingCopy <- myWorkingCopy
              let spec = workingKitSpec workingCopy
              liftKit $ do
                deps <- totalSpecDependencies repo workingCopy
                let (devPackages, notDevPackages) = partition isDevDep deps
                mapM_ ((unpackKit repo . specKit) . depSpec) notDevPackages
                mapM_ (\s -> say Red $ " -> Using dev package: " ++ packageName (depSpec s)) devPackages
                puts " -> Generating Xcode project..."
                allContents <- mapM (dependencyContents repo) deps
                writeKitProjectFromContents allContents (specKitDepsXcodeFlags spec) (packagesDirectory repo)
                say Green "\n\tKit complete. You may need to restart Xcode for it to pick up any changes.\n"

  doShowTree :: Command ()
  doShowTree = do
    repo <- myRepository
    wc <- myWorkingCopy
    tree <- liftKit $ dependencyTree repo wc
    puts $ drawTree $ fmap (packageFileName . depSpec) tree

  doPackageKit :: Command ()
  doPackageKit = mySpec >>= liftIO . package 

  doPublishLocal :: Maybe String -> Command ()
  doPublishLocal versionTag = do
    spec <- mySpec 
    specFile <- workingSpecFile <$> myWorkingCopy
    repo <- myRepository
    let updatedSpec = maybe spec (\tag -> updateVersion spec (++tag)) versionTag
    liftIO $ do
      package updatedSpec 
      publishLocally repo updatedSpec specFile $ "dist" </> packageFileName updatedSpec ++ ".tar.gz"
    return ()

  doVerify :: String -> Command ()
  doVerify sdk = do
        spec <- mySpec
        puts "Checking that the kit can be depended upon..."
        puts " #> Deploying locally"
        doPublishLocal Nothing -- TODO publish with a verify tag
        puts " #> Building temporary parent project"
        liftIO $ inDirectory getTemporaryDirectory $ do
          let kitVerifyDir = "kit-verify"
          cleanOrCreate kitVerifyDir
          inDirectory kitVerifyDir $ do
            writeSpec "KitSpec" (defaultSpec "verify-kit" "1.0") { specDependencies = [specKit spec] }
            runCommand doUpdate  
            inDirectory "Kits" $ do
              system "open ."
              system $ "xcodebuild -sdk " ++ sdk
          puts "OK."
        puts "End checks."

  doCreateSpec :: String -> String -> Command ()
  doCreateSpec name version = do
    let spec = defaultSpec name version 
    writeSpec "KitSpec" spec
    puts $ "Created KitSpec for " ++ packageFileName spec

  handleArgs :: KA.KitCmdArgs -> Command ()
  handleArgs KA.Update = doUpdate
  handleArgs KA.Package = doPackageKit
  handleArgs (KA.PublishLocal versionTag) = doPublishLocal versionTag 
  handleArgs (KA.Verify sdkName) = doVerify sdkName
  handleArgs (KA.CreateSpec name version) = doCreateSpec name version
  handleArgs KA.ShowTree = doShowTree

  warnOldRepo c = do
      say Red $ "Warning: Kit now expects packages in '.kit/cache/local' instead of '.kit/repository/kits'. (Found " ++ show c ++ " packages in the old location.)"
      puts ""
      say Red "To fix it, move the old directory into the new path:"
      say Red "\t$ mkdir -p ~/.kit/cache/local"
      say Red "\t$ mv ~/.kit/repository/kits/* ~/.kit/cache/local"
      say Red "\t$ rmdir ~/.kit/repository/kits && rmdir ~/.kit/repository"
      puts ""

  kitMain :: IO ()
  kitMain = do
    let f = runCommand . handleArgs =<< KA.parseArgs
    fs <- inDirectory getHomeDirectory $ glob ".kit/repository/kits/*/*/*.tar.gz"
    unless (null fs) $ warnOldRepo (length fs)
    catch f $ \e -> do
        alert $ show e
        exitWith $ ExitFailure 1 

