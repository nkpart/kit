module Kit.Util.FSAction where

import System.FilePath
import System.Posix.Files

import Kit.Util

data FSAction = 
    FileCreate FilePath String
  | Symlink FilePath FilePath
  deriving (Eq, Show)

within :: FilePath -> FSAction -> FSAction
within fp (FileCreate path c) = FileCreate (fp </> path) c
within fp (Symlink target dest) = Symlink target (fp </> dest)

{-makeProject = FileCreate "Project" "lol"-}
{-makeProjectInKits = within "Kits" makeProject-}

runAction :: FSAction -> IO ()
runAction (FileCreate atPath contents) = do
  mkdirP $ dropFileName atPath 
  writeFile atPath contents
runAction (Symlink target name) = do
  when' (fileExist name) $ removeLink name 
  createSymbolicLink target name

