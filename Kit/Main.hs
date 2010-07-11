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

  defaultLocalRepoPath = do
      home <- getHomeDirectory
      return $ home </> ".kit" </> "repository"
  defaultLocalRepository = fmap fileRepo defaultLocalRepoPath
  
  handleFails (Left e) = do
    print e
    return ()
  handleFails (Right _) = return ()
  
  update :: IO ()
  update = unKitIO g >>= handleFails
      where g = do
              repo <- liftIO defaultLocalRepository
              deps <- getMyDeps repo
              puts $ "Dependencies: " ++ (stringJoin ", " $ map kitFileName deps)
              liftIO $ mapM (installKit repo) deps
              puts " -> Generating XCode project..."
              liftIO $ generateXCodeProject $ deps |> kitFileName
              liftIO $ generateXCodeConfig $ deps |> kitFileName
              puts "Kit complete."
                where p x = liftIO $ print x
                      puts x = liftIO $ putStrLn x
  
  packageKit :: IO ()
  packageKit = do
      mySpec <- unKitIO myKitSpec
      T.for mySpec package
      return ()
  
  deployLocal :: IO ()
  deployLocal = do
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
  
  verify :: Maybe String -> IO ()
  verify sdk = (unKitIO f >>= handleFails)
    where
      f = do
        mySpec <- myKitSpec
        puts "Checking that the kit can be depended upon..."
        puts " #> Deploying locally"
        liftIO deployLocal
        puts " #> Building temporary parent project"
        tmp <- liftIO getTemporaryDirectory
        liftIO $ inDirectory tmp $ do
          let kitVerifyDir = "kit-verify"
          cleanOrCreate kitVerifyDir
          inDirectory kitVerifyDir $ do
            writeFile "KitSpec" $ encode (KitSpec (Kit "verify-kit" "1.0") $ defaultConfiguration{kitConfigDependencies=[specKit mySpec]})
            update
            inDirectory "Kits" $ do
              system $ "xcodebuild -sdk " ++ (fromMaybe "iphonesimulator4.0" sdk)
          putStrLn "OK."
        puts "End checks."
      puts = liftIO . putStrLn
      
  createSpec :: String -> String -> IO ()
  createSpec name version = do
    let kit =(Kit name version)
    let spec = KitSpec kit defaultConfiguration
    writeFile "KitSpec" $ encode spec
    putStrLn $ "Created KitSpec for " ++ kitFileName kit
    return ()
  
  handleArgs :: [String] -> IO ()
  handleArgs ["update"] = update
  handleArgs ["package"] = packageKit
  handleArgs ["publish-local"] = deployLocal
  handleArgs ["verify"] = verify Nothing
  handleArgs ["verify", sdk] = verify $ Just sdk
  handleArgs ["create-spec", name, version] = createSpec name version
    
  handleArgs _ = putStrLn f where
    f = "Usage: kit command [arguments]" `nl`
        "Commands:" `nl`
        "\tupdate\t\t\t\tCreate an Xcode project to wrap the dependencies defined in ./KitSpec" `nl`
        "\tcreate-spec NAME VERSION\tWrite out a KitSpec file using the given name and version." `nl`
        "\tpackage\t\t\t\tCreate a tar.gz wrapping this kit" `nl`
        "\tverify [SDK]\t\t\tPackage this kit, then attempt to use it as a dependency in an empty project." `nl`
        "\tpublish-local\t\t\tPackage this kit, then deploy it to the local repository (~/.kit/repository)" 
    nl a b = a ++ "\n" ++ b
    
  kitMain :: IO ()
  kitMain = do
      localRepo <- defaultLocalRepository
      path <- defaultLocalRepoPath
      mkdir_p path
      args <- getArgs
      handleArgs args

      
          
