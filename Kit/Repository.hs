{-# LANGUAGE PackageImports #-}
module Kit.Repository (
    --
    KitRepository(KitRepository),
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

  data KitRepository = KitRepository { repositoryBase :: FilePath } deriving (Eq, Show)

  explodePackage :: KitRepository -> Kit -> IO ()
  explodePackage kr kit = do
    let packagesDir = repositoryBase kr </> "kits"
    let packagePath = repositoryBase kr </> kitPackagePath kit
    mkdirP packagesDir
    inDirectory packagesDir $ system ("tar zxvf " ++ packagePath)
    return ()

  copyKitPackage :: KitRepository -> Kit -> FilePath -> IO ()
  copyKitPackage repo kit destPath = copyFile (repositoryBase repo </> kitPackagePath kit) destPath

  readKitSpec :: KitRepository -> Kit -> KitIO KitSpec
  readKitSpec repo kit = do
    mbKitStuff <- liftIO $ doRead repo (kitSpecPath kit)
    maybe (throwError $ "Missing " ++ packageFileName kit) f mbKitStuff
    where f contents = maybeToKitIO ("Invalid KitSpec file for " ++ packageFileName kit) $ decodeSpec contents

  doRead :: KitRepository -> String -> IO (Maybe BS.ByteString) 
  doRead kr fp = let file = (repositoryBase kr </> fp) in do
    exists <- doesFileExist file
    T.sequenceA $ ifTrue exists $ BS.readFile file

  baseKitPath :: Kit -> String
  baseKitPath k = "kits" </> kitName k </> kitVersion k

  kitPackagePath :: Kit -> String
  kitPackagePath k = baseKitPath k </> packageFileName k ++ ".tar.gz"

  kitSpecPath :: Kit -> String
  kitSpecPath k = baseKitPath k </> "KitSpec"

  packagesDirectory :: KitRepository -> FilePath
  packagesDirectory kr = (repositoryBase kr </> ".." </> "packages")

  unpackKit :: KitRepository -> Kit -> IO ()
  unpackKit kr kit = do
      let source = (repositoryBase kr </> kitPackagePath kit)
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

  publishLocally :: KitRepository -> KitSpec -> FilePath -> FilePath -> IO ()
  publishLocally kr ks specFile packageFile = do
                              let repo = repositoryBase kr
                              let thisKitDir = repo </> baseKitPath (specKit ks)
                              mkdirP thisKitDir
                              let fname = takeFileName packageFile
                              copyFile packageFile (thisKitDir </> fname)
                              copyFile specFile (thisKitDir </> "KitSpec") 

