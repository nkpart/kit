module Kit.FlaggedFile (
    FlaggedFile,
    flaggedFile,
    flaggedFilePath,
    flaggedFileFlags
    ) where

import Kit.AbsolutePath as AF

data FlaggedFile = FlaggedFile {
                     flaggedFileAbsolutePath :: AbsolutePath,
                     flaggedFileFlags :: String 
                     } deriving (Show)

flaggedFile :: AbsolutePath -> String -> FlaggedFile
flaggedFile = FlaggedFile

flaggedFilePath :: FlaggedFile -> FilePath
flaggedFilePath = AF.filePath . flaggedFileAbsolutePath
