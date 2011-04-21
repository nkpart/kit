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
  import Kit.Repository (unpackKit, packagesDirectory)

  f2 spec = do 
          base <- liftIO $ canonicalizePath devKitDir
          readKitContents' base packageName spec

  f1 packagesDirectory spec = do
          base <- liftIO $ canonicalizePath packagesDirectory
          readKitContents' base packageFileName spec 

  doUpdate :: Command ()
  doUpdate = do
              repo <- myRepository
              workingCopy <- myWorkingCopy
              let spec = workingKitSpec workingCopy
              deps <- liftKit $ totalSpecDependencies repo workingCopy
              let (devPackages, notDevPackages) = partition isDevDep deps
              liftIO $ mapM_ (unpackKit repo . specKit) $ map depSpec notDevPackages
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
    liftIO $ putStrLn $ drawTree $ fmap (packageFileName . depSpec) $ tree

  doPackageKit :: Command ()
  doPackageKit = mySpec >>= liftIO . package 

  doPublishLocal :: Maybe String -> Command ()
  doPublishLocal versionTag = do
    spec <- mySpec 
    specFile <- mySpecFile
    let updatedSpec = maybe spec (\tag -> updateVersion spec (++tag)) versionTag
    liftIO $ do
      package updatedSpec 
      publishLocal updatedSpec specFile
    return ()
      where
        publishLocal :: KitSpec -> FilePath -> IO ()
        publishLocal spec specFile = let pkg = (packageFileName spec ++ ".tar.gz")
                            in do
                              repo <- defaultLocalRepoPath
                              let thisKitDir = repo </> "kits" </> packageName spec </> packageVersion spec
                              mkdirP thisKitDir
                              copyFile ("dist" </> pkg) (thisKitDir </> pkg)
                              copyFile specFile (thisKitDir </> "KitSpec") 

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

  kitMain :: IO ()
  kitMain = let f = do
                      mkdirP =<< defaultLocalRepoPath 
                      runCommand . handleArgs =<< KA.parseArgs
            in catch f $ \e -> do
                alert $ show e
                exitWith $ ExitFailure 1 

