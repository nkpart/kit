{-# LANGUAGE PackageImports #-}
module Kit.Repository (
    --
    KitRepository,
    makeRepository,
    --
    copyKitPackage,
    explodePackage,
    readKitSpec,
    unpackKit,
    packagesDirectory,
    publishLocally
  ) where

  import Kit.Spec
  import Kit.Util

  import System.Process (system)

  import "mtl" Control.Monad.Error
  import qualified Data.Traversable as T
  import qualified Data.ByteString as BS

  data KitRepository = KitRepository { dotKitDir :: FilePath } deriving (Eq, Show)

  localCacheDir :: KitRepository -> FilePath
  localCacheDir kr = dotKitDir kr </> "cache" </> "local"

  makeRepository :: FilePath -> IO KitRepository
  makeRepository fp = do 
    let kr = KitRepository fp
    mkdirP $ localCacheDir kr
    return kr

  explodePackage :: KitRepository -> Kit -> IO ()
  explodePackage kr kit = do
    let packagesDir = localCacheDir kr
    let packagePath = localCacheDir kr </> kitPackagePath kit
    mkdirP packagesDir
    inDirectory packagesDir $ system ("tar zxvf " ++ packagePath)
    return ()

  copyKitPackage :: KitRepository -> Kit -> FilePath -> IO ()
  copyKitPackage repo kit = copyFile (localCacheDir repo </> kitPackagePath kit)

  readKitSpec :: KitRepository -> Kit -> KitIO KitSpec
  readKitSpec repo kit = do
    mbKitStuff <- liftIO $ readIfExists (localCacheDir repo </> kitSpecPath kit)
    maybe (throwError $ "Missing " ++ packageFileName kit) f mbKitStuff
    where f contents = maybeToKitIO ("Invalid KitSpec file for " ++ packageFileName kit) $ decodeSpec contents

  readIfExists :: String -> IO (Maybe BS.ByteString) 
  readIfExists file = do
    exists <- doesFileExist file
    T.sequenceA $ ifTrue exists $ BS.readFile file

  baseKitPath :: Kit -> String
  baseKitPath k = kitName k </> kitVersion k

  kitPackagePath :: Kit -> String
  kitPackagePath k = baseKitPath k </> packageFileName k ++ ".tar.gz"

  kitSpecPath :: Kit -> String
  kitSpecPath k = baseKitPath k </> "KitSpec"

  packagesDirectory :: KitRepository -> FilePath
  packagesDirectory kr = dotKitDir kr </> "packages"

  unpackKit :: KitRepository -> Kit -> IO ()
  unpackKit kr kit = do
      let source = (localCacheDir kr </> kitPackagePath kit)
      let dest = packagesDirectory kr
      d <- doesDirectoryExist $ dest </> packageFileName kit
      if not d then do
          putStrLn $ " -> Unpacking from cache: " ++ packageFileName kit
          mkdirP dest 
          inDirectory dest $ system ("tar zxf " ++ source)
          return ()
        else 
          putStrLn $ " -> Using local package: " ++ packageFileName kit
      return ()

  -- TODO: on publish local, need to flush this particular name/version out
  -- of the packages dir if it exists, so if this is overriding a version, that
  -- all works.
  publishLocally :: KitRepository -> KitSpec -> FilePath -> FilePath -> IO ()
  publishLocally kr ks specFile packageFile = do
                              let cacheDir = localCacheDir kr
                              let thisKitDir = cacheDir </> baseKitPath (specKit ks)
                              mkdirP thisKitDir
                              let fname = takeFileName packageFile
                              copyFile packageFile (thisKitDir </> fname)
                              copyFile specFile (thisKitDir </> "KitSpec") 

