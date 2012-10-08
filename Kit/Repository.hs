module Kit.Repository (
    --
    KitRepository,
    makeRepository,
    --
    readKitSpec,
    unpackKit,
    packagesDirectory,
    publishLocally
  ) where

  import Kit.Spec
  import Kit.Util
  import Data.Yaml (decodeFile)

  data KitRepository = KitRepository { dotKitDir :: FilePath } deriving (Eq, Show)

  localCacheDir :: KitRepository -> FilePath
  localCacheDir kr = dotKitDir kr </> "cache" </> "local"

  makeRepository :: FilePath -> IO KitRepository
  makeRepository fp = let repo = KitRepository fp in do 
    mkdirP $ localCacheDir repo
    return repo

  readKitSpec :: KitRepository -> Kit -> KitIO KitSpec
  readKitSpec repo kit = do
    mbLoaded <- liftIO $ decodeFile (localCacheDir repo </> kitSpecPath kit)
    tryJust ("Invalid KitSpec file for " ++ packageFileName kit) mbLoaded

  baseKitPath :: Packageable a => a -> String
  baseKitPath k = packageName k </> packageVersion k

  kitPackagePath :: Packageable a => a -> String
  kitPackagePath k = baseKitPath k </> packageFileName k ++ ".tar.gz"

  kitSpecPath :: Packageable a => a -> String
  kitSpecPath k = baseKitPath k </> "KitSpec"

  packagesDirectory :: KitRepository -> FilePath
  packagesDirectory kr = dotKitDir kr </> "packages"

  unpackKit :: (Packageable a, MonadIO m) => KitRepository -> a -> m ()
  unpackKit kr kit = do
      let source = localCacheDir kr </> kitPackagePath kit
      let dest = packagesDirectory kr
      d <- liftIO $ doesDirectoryExist $ dest </> packageFileName kit
      if not d 
        then do
          puts $ "Unpacking: " ++ packageFileName kit
          mkdirP dest 
          liftIO $ inDirectory dest $ shell ("tar zxf " ++ source)
          return ()
        else puts $ "Using: " ++ packageFileName kit
      return ()

  publishLocally :: KitRepository -> KitSpec -> FilePath -> FilePath -> IO ()
  publishLocally kr ks specFile packageFile = do
                              let cacheDir = localCacheDir kr
                              let thisKitDir = cacheDir </> baseKitPath ks
                              mkdirP thisKitDir
                              let fname = takeFileName packageFile
                              copyFile packageFile (thisKitDir </> fname)
                              copyFile specFile (thisKitDir </> "KitSpec")
                              let pkg = packagesDirectory kr </> packageFileName ks
                              d <- doesDirectoryExist pkg
                              when d $ removeDirectoryRecursive pkg

