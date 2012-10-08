{-# LANGUAGE PackageImports, TupleSections #-}
module Kit.WorkingCopy (
    WorkingCopy(..),
    currentWorkingCopy,
    DevPackages,
    findDevPackage,
    devKitDir
    ) where

import Kit.Util
import Data.Yaml (decodeFile)
import Kit.Spec
import qualified Data.List as L
import Control.Error

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

currentWorkingCopy :: Script WorkingCopy
currentWorkingCopy = do
  let specFile = "KitSpec"
  spec <- readSpec specFile
  devKitSpecFiles <- liftIO $ glob $ devKitDir </> "*/KitSpec"
  let f path = (,takeFileName . takeDirectory $ path) <$> readSpec path
  devKitSpecs <- mapM f devKitSpecFiles
  return $ WorkingCopy spec specFile $ DevPackages devKitSpecs

readSpec :: FilePath -> Script KitSpec
readSpec path = do
    mbKitSpec <- liftIO $ decodeFile path
    tryJust ("Error loading spec file " ++ path) mbKitSpec
