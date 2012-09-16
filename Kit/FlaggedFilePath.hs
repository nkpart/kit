module Kit.FlaggedFilePath (
    FlaggedFilePath(..),
    takeflaggedFilePath,
    takeFlags,
    ffp
    ) where

data FlaggedFilePath = FlaggedFilePath FilePath String deriving (Show)

ffp :: FilePath -> String -> FlaggedFilePath
ffp f cf = FlaggedFilePath f cf 

takeflaggedFilePath :: FlaggedFilePath -> FilePath
takeflaggedFilePath (FlaggedFilePath fp _) = fp

takeFlags :: FlaggedFilePath -> String
takeFlags (FlaggedFilePath _ flags) = flags

