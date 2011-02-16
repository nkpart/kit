module Kit.Util.FSAction where

import System.FilePath
import System.Posix.Files

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
  writeFile atPath contents
runAction (Symlink target name) = do
  catch (removeLink name) (\_ -> return ())
  -- When a `name` has parent directories, symbolic link target needs to be made relative to that file
  -- need to consider "./name"
  let relFix = join $ intersperse "/" $ map (const "..") (init $ splitDirectories name)
  when' (fileExist target) $ createSymbolicLink (relFix </> target) name
runAction (InDir dir action) = do
  mkdirP dir
  inDirectory dir $ runAction action

