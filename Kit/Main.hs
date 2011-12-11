{-# LANGUAGE PackageImports #-}
module Kit.Main where

  import qualified Kit.CmdArgs as KA 
  import Kit.Commands
  import Kit.Spec
  import Kit.Package
  import Kit.Project
  import Kit.Util
  import Kit.Contents
  import Kit.Dependency
  import Kit.WorkingCopy
  import Kit.Util.FSAction
  import System.Exit
  import Data.Tree (drawTree)
  import Data.List (partition)
  import Kit.Repository (KitRepository, unpackKit, packagesDirectory, publishLocally)

  kitMain :: IO ()
  kitMain = do
    as <- KA.parseArgs
    let repo = KA.repositoryDir as
    fs <- inDirectory getHomeDirectory $ glob ".kit/repository/kits/*/*/*.tar.gz"
    unless (null fs) $ warnOldRepo (length fs)
    let action = runCommand repo . handleArgs $ as
    catch action $ \e -> do
        alert $ show e
        exitWith $ ExitFailure 1 

  handleArgs :: KA.KitCmdArgs -> Command ()
  handleArgs (KA.Update _) = doUpdate
  handleArgs (KA.Package _) = doPackageKit
  handleArgs (KA.PublishLocal _ versionTag) = doPublishLocal versionTag 
  handleArgs (KA.Verify sdkName _) = doVerify sdkName
  handleArgs (KA.CreateSpec name version _) = doCreateSpec name version
  handleArgs (KA.ShowTree _) = doShowTree

  doUpdate :: Command ()
  doUpdate = do
              repo <- myRepository
              workingCopy <- myWorkingCopy
              liftKit $ do
                deps <- totalSpecDependencies repo workingCopy
                let (devPackages, repoPackages) = partition isDevDep deps
                mapM_ (unpackKit repo) repoPackages
                mapM_ (say Red . ("Using dev-package: " ++) . packageName) devPackages
                project <- buildProject repo workingCopy deps
                reportResources project
                writeProject project
                say Green "Kit complete. You may need to restart Xcode for it to pick up any changes."
      where buildProject repo workingCopy deps = do
                allContents <- mapM (dependencyContents repo) deps
                return $ makeKitProject allContents (specKitDepsXcodeFlags (workingKitSpec workingCopy))
            reportResources project = let resourceDirs = kitProjectResourceDirs project
                                       in unless (null resourceDirs) $ puts $ "Resources: " ++ stringJoin ", " (map fst resourceDirs)
            writeProject = liftIO . runActions . kitProjectActions

  dependencyContents :: (Applicative m, MonadIO m) => KitRepository -> Dependency -> m KitContents
  dependencyContents repo dep = dependency inRepo local dep where -- TODO look at all these applications of dep
    inRepo spec = loadKitFromBase (packagesDirectory repo) (packageFileName spec) spec
    local spec fp = loadKitFromBase devKitDir fp spec

  loadKitFromBase :: (Applicative m, MonadIO m) => FilePath -> FilePath -> KitSpec -> m KitContents
  loadKitFromBase base kitDir spec = do
    absoluteBase <- liftIO $ canonicalizePath base
    readKitContents (absoluteBase </> kitDir) spec

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

  doShowTree :: Command ()
  doShowTree = do
    repo <- myRepository
    wc <- myWorkingCopy
    tree <- liftKit $ dependencyTree repo wc
    puts $ drawTree $ fmap packageFileName tree

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
            runCommand Nothing doUpdate 
            inDirectory "Kits" $ do
              shell "open ."
              shell $ "xcodebuild -sdk " ++ sdk
          puts "OK."
        puts "End checks."

  doCreateSpec :: String -> String -> Command ()
  doCreateSpec name version = do
    let spec = defaultSpec name version 
    writeSpec "KitSpec" spec
    puts $ "Created KitSpec for " ++ packageFileName spec

  warnOldRepo :: MonadIO m => Int -> m ()
  warnOldRepo c = do
      say Red $ "Warning: Kit now expects packages in '.kit/cache/local' instead of '.kit/repository/kits'. (Found " ++ show c ++ " packages in the old location.)"
      puts ""
      say Red "To fix it, move the old directory into the new path:"
      say Red "\t$ mkdir -p ~/.kit/cache/local"
      say Red "\t$ mv ~/.kit/repository/kits/* ~/.kit/cache/local"
      say Red "\t$ rmdir ~/.kit/repository/kits && rmdir ~/.kit/repository"
      puts ""

