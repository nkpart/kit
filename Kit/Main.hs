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
  import Data.List (partition, groupBy, sortBy, maximumBy, nub, intercalate)
  import Data.Function (on)
  import Kit.Repository (KitRepository, unpackKit, packagesDirectory, publishLocally)
  import Control.Monad.State

  kitMain :: IO ()
  kitMain = do
    args <- KA.parseArgs
    let action = runCommand (KA.repositoryDir args) . handleArgs $ args
    catch action $ \e -> do
        sayError $ show e
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
                rawDeps <- totalSpecDependencies repo workingCopy
                (deps, didResolveConflict) <- unconflict rawDeps
                let (devPackages, repoPackages) = partition isDevDep deps
                mapM_ (unpackKit repo) repoPackages
                mapM_ (sayError . ("Using dev-package: " ++) . packageName) devPackages
                project <- buildProject repo workingCopy deps
                reportResources project
                writeProject project
                (if didResolveConflict then sayWarn else say Green) "Kit complete. You may need to restart Xcode for it to pick up any changes."
                liftIO . when didResolveConflict . exitWith . ExitFailure $ 1
      where buildProject repo workingCopy deps = do
                allContents <- mapM (dependencyContents repo) deps
                return $ makeKitProject allContents (specKitDepsXcodeFlags (workingKitSpec workingCopy))
            reportResources project = let resourceDirs = kitProjectResourceDirs project
                                       in unless (null resourceDirs) $ puts $ "Resources: " ++ stringJoin ", " (map fst resourceDirs)
            writeProject = liftIO . runActions . kitProjectActions

  unconflict :: (Packageable b, MonadIO m) => [b] -> m ([b], Bool)
  unconflict deps = let byName = groupBy ((==) `on` (packageName . snd)) . sortBy (compare `on` (packageName . snd)) $ zip [1..] deps
                        stripIndex (xs, hadConflict) = (map snd . sortBy (compare `on` fst) $ xs, hadConflict)
                in liftM stripIndex $ flip runStateT False $ forM byName $ \all@(b:bs) ->
                    if null bs
                      then return b
                      else do
                        let versions = nub $ map (packageVersion . snd) all
                        let maxVersion = maximumBy (compare `on` (packageVersion . snd)) all
                        sayWarn $ "Dependency conflict: " ++ packageName (snd b) ++ " => " ++ intercalate ", " versions
                        sayWarn $ "Selected maximum version: " ++ (packageVersion . snd $ maxVersion)
                        put True
                        return maxVersion

  dependencyContents :: (Applicative m, MonadIO m) => KitRepository -> Dependency -> m KitContents
  dependencyContents repo dep = readKitContents' baseDir (depSpec dep) where
    baseDir = dependency ((packagesDirectory repo </>) . packageFileName) (\fp spec -> devKitDir </> fp) dep
    readKitContents' base spec = do
      absoluteBase <- liftIO $ canonicalizePath base
      readKitContents base spec

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

