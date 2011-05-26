{-# LANGUAGE PackageImports, TupleSections #-}
module Kit.WorkingCopy (
    WorkingCopy(..),
    currentWorkingCopy,
    isDevPackage,
    devKitDir
    ) where

import Kit.Util
import Kit.Spec

import "mtl" Control.Monad.Trans
import "mtl" Control.Monad.Error
import qualified Data.ByteString as BS

devKitDir :: String
devKitDir = "dev-packages"

data WorkingCopy = WorkingCopy {
  workingKitSpec :: KitSpec,
  workingSpecFile :: FilePath,
  workingDevPackages :: [(KitSpec, FilePath)]
} deriving (Eq, Show)

currentWorkingCopy :: KitIO WorkingCopy
currentWorkingCopy = do
  let specFile = "KitSpec"
  spec <- readSpec specFile
  devKitSpecFiles <- liftIO $ glob "dev-packages/*/KitSpec"
  let f path = (,takeFileName . takeDirectory $ path) <$> readSpec path
  devKitSpecs <- mapM f devKitSpecFiles
  return $ WorkingCopy spec specFile devKitSpecs

isDevPackage :: WorkingCopy -> KitSpec -> Bool
isDevPackage wc spec = any (\s -> packageName spec == packageName s) $ map fst (workingDevPackages wc)

readSpec :: FilePath -> KitIO KitSpec
readSpec path = checkExists path >>= liftIO . BS.readFile >>= ErrorT . return . parses
  where checkExists pathToSpec = do
          doesExist <- liftIO $ doesFileExist pathToSpec 
          if doesExist 
            then return pathToSpec 
            else throwError ("Couldn't find the spec at " ++ pathToSpec)
        parses = maybeToRight "Parse error in KitSpec file" . decodeSpec

