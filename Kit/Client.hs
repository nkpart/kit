module Kit.Client (
  getDeps, 
  getMyDeps,
  installKit,
  myKitSpec) 
    where
      
  import Control.Monad.Trans
  import Control.Monad
  import Control.Applicative
  import Data.Foldable
  import Data.Maybe
  import System.Directory
  import System.Cmd
  import System.FilePath.Posix
  import Kit.Kit
  import Kit.Repository
  import Kit.Util
  import Kit.Spec
  
  xxx kr spec = mapM (getDeps kr) (specDependencies spec)
  
  getDeps :: KitRepository -> Kit -> KitIO [Kit]
  getDeps kr kit = do
    spec <- getKitSpec kr kit
    deps <- xxx kr spec
    return $ specDependencies spec ++ join deps

  installKit :: KitRepository -> Kit -> IO ()
  installKit kr kit = do
    tmpDir <- getTemporaryDirectory
    let fp = tmpDir </> (kitFileName kit ++ ".tar.gz")
    fmap fromJust $ getKit kr kit fp
    let dest = "." </> "Kits"
    createDirectoryIfMissing True dest
    setCurrentDirectory dest
    sh ("tar zxvf " ++ fp)
    setCurrentDirectory ".."
      where sh = system

  getMyDeps :: KitRepository -> KitIO [Kit]
  getMyDeps kr = do
    mySpec <- myKitSpec
    deps <- xxx kr mySpec 
    return $ join deps ++ specDependencies mySpec
  
  myKitSpec :: KitIO KitSpec
  myKitSpec = doRead >>= (\c -> KitIO . return $ parses c)

  -- private!
  specIfExists :: KitIO FilePath
  specIfExists = let kitSpecPath = "KitSpec" in do
    doesExist <- liftIO $ doesFileExist kitSpecPath
    maybeToKitIO "Couldn't find the spec file" (justTrue doesExist kitSpecPath)
  
  doRead :: KitIO String
  doRead = do
    fp <- specIfExists
    liftIO $ readFile fp
  
  parses :: String -> Either [KitError] KitSpec
  parses contents = maybeToRight ["Could not parse spec."] $ maybeRead contents
  
  