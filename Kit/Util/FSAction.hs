module Kit.Util.FSAction where

import System.FilePath
import System.Posix.Files

import Control.Error

import Kit.Util
import Data.List (intersperse)

data FSAction = 
    FileCreate FilePath String
  | Symlink FilePath FilePath
  | InDir FilePath FSAction
  deriving (Eq, Show)

within :: FilePath -> FSAction -> FSAction
within = InDir

runAction :: FSAction -> IO ()
runAction (FileCreate atPath contents) = do
    mkdirP $ dropFileName atPath 
    writeFile tempPath contents
    renameFile tempPath atPath
  where tempPath = atPath ++ "_"
runAction (Symlink target name) = do
  _ <- runEitherT . scriptIO $ removeLink name
  -- When a `name` has parent directories, symbolic link target needs to be made relative to that file
  -- need to consider "./name"
  let relFix = join $ intersperse "/" $ map (const "..") (init $ splitDirectories name)
  mkdirP $ dropFileName name
  let printError e = "An error occured when creating a symlink to " ++ target ++ " called " ++ name ++ ": " ++ show e
  runScript . mapEitherT printError id . scriptIO $ when' (fileExist target) (createSymbolicLink (relFix </> target) name)

runAction (InDir dir action) = do
  mkdirP dir
  inDirectory dir $ runAction action

runActions :: [FSAction] -> IO ()
runActions = mapM_ runAction

