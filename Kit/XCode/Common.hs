module Kit.XCode.Common where
  import Data.Monoid
  import Data.List
  import System.FilePath.Posix
  
  type UUID = String
  
  data FileType = Header | Source | Unknown
  
  fileType :: String -> FileType
  fileType x | ".h" `isSuffixOf` x = Header
  fileType x | ".m" `isSuffixOf` x = Source
  fileType x | ".mm" `isSuffixOf` x = Source
  fileType x | ".c" `isSuffixOf` x = Source
  fileType _ = Unknown
  
  data PBXBuildFile = PBXBuildFile {
    buildFileId :: UUID,
    buildFileReference :: PBXFileReference
  }
  
  data PBXFileReference = PBXFileReference {
    fileReferenceId :: UUID,
    fileReferencePath :: String
  }
  
  fileReferenceName = takeFileName . fileReferencePath
  
  uuid :: Integer -> UUID
  uuid i = let
       s = show i
       pad = 24 - length s
    in replicate pad '0' ++ s 
