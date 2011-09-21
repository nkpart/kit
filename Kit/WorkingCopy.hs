{-# LANGUAGE PackageImports, TupleSections #-}
module Kit.WorkingCopy (
    WorkingCopy(..),
    currentWorkingCopy,
    DevPackages,
    findDevPackage,
    devKitDir
    ) where

import Kit.Util
import Kit.Spec
import qualified Data.List as L

import "mtl" Control.Monad.Trans
import "mtl" Control.Monad.Error
import qualified Data.ByteString as BS

devKitDir :: String
devKitDir = "dev-packages"

data WorkingCopy = WorkingCopy {
  workingKitSpec :: KitSpec,
  workingSpecFile :: FilePath,
  workingDevPackages :: DevPackages
} deriving (Eq, Show)

newtype DevPackages = DevPackages [(KitSpec, FilePath)] deriving (Eq, Show)

findDevPackage :: Packageable a => DevPackages -> a -> Maybe (KitSpec, FilePath)
findDevPackage (DevPackages pkgs) kit = L.find ((packageName kit ==) . packageName . fst) pkgs

currentWorkingCopy :: KitIO WorkingCopy
currentWorkingCopy = do
  let specFile = "KitSpec"
  spec <- readSpec specFile
  devKitSpecFiles <- liftIO $ glob $ devKitDir </> "*/KitSpec"
  let f path = (,takeFileName . takeDirectory $ path) <$> readSpec path
  devKitSpecs <- mapM f devKitSpecFiles
  return $ WorkingCopy spec specFile $ DevPackages devKitSpecs

readSpec :: FilePath -> KitIO KitSpec
readSpec path = checkExists path >>= liftIO . BS.readFile >>= ErrorT . return . parses
  where checkExists pathToSpec = do
          doesExist <- liftIO $ doesFileExist pathToSpec 
          if doesExist
            then return pathToSpec 
            else throwError ("Couldn't find the spec at " ++ pathToSpec)
        parses = maybeToRight "Parse error in KitSpec file" . decodeSpec
