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
                     } deriving (Eq, Show)

flaggedFile :: String -> AbsolutePath -> FlaggedFile
flaggedFile a b = FlaggedFile b a

flaggedFilePath :: FlaggedFile -> FilePath
flaggedFilePath = AF.filePath . flaggedFileAbsolutePath
