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
  import Kit.Repository (unpackKit, packagesDirectory, publishLocally)

  f2 :: KitSpec -> Command KitContents 
  f2 spec = do 
          base <- liftIO $ canonicalizePath devKitDir
          readKitContents' base packageName spec

  f1 :: FilePath -> KitSpec -> Command KitContents 
  f1 pkgDir spec = do
          base <- liftIO $ canonicalizePath pkgDir
          readKitContents' base packageFileName spec 

  doUpdate :: Command ()
  doUpdate = do
              repo <- myRepository
              workingCopy <- myWorkingCopy
              let spec = workingKitSpec workingCopy
              deps <- liftKit $ totalSpecDependencies repo workingCopy
              let (devPackages, notDevPackages) = partition isDevDep deps
              liftIO $ mapM_ ((unpackKit repo . specKit) . depSpec) notDevPackages
              liftIO $ mapM_ (\s -> say Red $ " -> Using dev package: " ++ packageName (depSpec s)) devPackages
              puts " -> Generating Xcode project..."
              devSpecs <- mapM (f2 . depSpec) devPackages
              notDevSpecs <- mapM (f1 (packagesDirectory repo) . depSpec) notDevPackages
              liftKit $ writeKitProjectFromContents (devSpecs ++ notDevSpecs) (specKitDepsXcodeFlags spec) (packagesDirectory repo)
              say Green "\n\tKit complete. You may need to restart Xcode for it to pick up any changes.\n"

  doShowTree :: Command ()
  doShowTree = do
    repo <- myRepository
    wc <- myWorkingCopy
    tree <- liftKit $ dependencyTree repo wc
    liftIO $ putStrLn $ drawTree $ fmap (packageFileName . depSpec) tree

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
        tmp <- liftIO getTemporaryDirectory
        inDirectory tmp $ do
          let kitVerifyDir = "kit-verify"
          cleanOrCreate kitVerifyDir
          inDirectory kitVerifyDir $ do
            writeSpec "KitSpec" (defaultSpec "verify-kit" "1.0") { specDependencies = [specKit spec] }
            liftIO $ runCommand doUpdate  
            inDirectory "Kits" $ do
              liftIO $ system "open ."
              liftIO $ system $ "xcodebuild -sdk " ++ sdk
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
    h <- getHomeDirectory
    fs <- inDirectory h $ glob ".kit/repository/kits/*/*/*.tar.gz"
    let c = length fs
    when (c > 0) $ warnOldRepo c
    catch f $ \e -> do
        alert $ show e
        exitWith $ ExitFailure 1 

