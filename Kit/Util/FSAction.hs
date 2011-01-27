module Kit.Util.FSAction where

import System.FilePath
import System.Posix.Files

import Kit.Util

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
  writeFile atPath contents
runAction (Symlink target name) = do
  when' (fileExist name) $ removeLink name 
  createSymbolicLink target name
runAction (InDir dir action) = do
  mkdirP dir
  inDirectory dir $ runAction action

