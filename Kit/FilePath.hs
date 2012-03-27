module Kit.FilePath (
    AbsolutePath,
    filePath,
    absolutePath
    ) where

import System.Directory (canonicalizePath)

data AbsolutePath = AbsolutePath FilePath deriving (Eq, Show)

filePath :: AbsolutePath -> FilePath
filePath (AbsolutePath fp) = fp

absolutePath :: FilePath -> IO AbsolutePath
absolutePath path = fmap AbsolutePath $ canonicalizePath path
