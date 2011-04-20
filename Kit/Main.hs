module Kit.Main where

  import Control.Monad.Trans
  import qualified Kit.CmdArgs as KA 
  import Kit.Commands
  import Kit.Spec
  import Kit.Package
  import Kit.Project
  import Kit.Util
  import System.Cmd
  import System.Exit
  import Data.Tree (drawTree)
  import Kit.Repository (unpackKit, packagesDirectory)

  import System.Console.ANSI
  
  say :: MonadIO m => Color -> String -> m ()
  say color msg = do
    liftIO $ setSGR [SetColor Foreground Vivid color]
    puts msg
    liftIO $ setSGR []

  alert :: MonadIO m => String -> m ()
  alert = say Red

  doUpdate :: Command ()
  doUpdate = do
              repo <- myRepository
              spec <- mySpec
              deps <- liftKit $ totalSpecDependencies repo spec
              liftIO $ mapM_ (unpackKit repo . specKit) deps
              puts " -> Generating Xcode project..."
              liftKit $ writeKitProjectFromSpecs deps (specKitDepsXcodeFlags spec) (packagesDirectory repo)
              say Green "\n\tKit complete. You may need to restart Xcode for it to pick up any changes.\n"

  doShowTree :: Command ()
  doShowTree = do
    repo <- myRepository
    spec <- mySpec
    tree <- liftKit $ dependencyTree repo spec
    liftIO $ putStrLn $ drawTree $ fmap packageFileName $ tree

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

