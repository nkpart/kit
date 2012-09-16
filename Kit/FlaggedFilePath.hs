module Kit.FlaggedFilePath (
    FlaggedFilePath(..),
    takeFlaggedFilePath,
    takeFlags,
    flaggedFilePath,
    ffp
    ) where

import System.Directory (canonicalizePath)

data FlaggedFilePath = FlaggedFilePath FilePath String deriving (Show)

ffp :: FilePath -> String -> FlaggedFilePath
ffp f cf = FlaggedFilePath f cf 

takeFlaggedFilePath :: FlaggedFilePath -> FilePath
takeFlaggedFilePath (FlaggedFilePath fp _) = fp

takeFlags :: FlaggedFilePath -> String
takeFlags (FlaggedFilePath _ flags) = flags

flaggedFilePath :: FilePath -> String -> IO FlaggedFilePath
flaggedFilePath path flags = fmap (\x -> FlaggedFilePath x flags) $ canonicalizePath path 
