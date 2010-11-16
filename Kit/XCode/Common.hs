module Kit.XCode.Common where
  import Data.Monoid
  import Data.List
  import System.FilePath.Posix
  import Kit.XCode.OldPList 
  
  type UUID = String
  
  data FileType = Header | Source | Unknown
  
  fileType :: String -> FileType
  fileType x | ".h" `isSuffixOf` x = Header
  fileType x | ".m" `isSuffixOf` x = Source
  fileType x | ".mm" `isSuffixOf` x = Source
  fileType x | ".c" `isSuffixOf` x = Source
  fileType _ = Unknown
  
  data PBXBuildFile = PBXBuildFile {
    buildFileId :: String,
    buildFileReference :: PBXFileReference
  }
  
  data PBXFileReference = PBXFileReference {
    fileReferenceId :: String,
    fileReferencePath :: String
  }
  
  fileReferenceName = takeFileName . fileReferencePath
  
  lineItem :: UUID -> String -> PListType -> String
  lineItem uuid comment value = printItem $ PListObjectItem uuid (Just comment) value
  
  uuid :: Integer -> UUID
  uuid i = let
      s = show i
      lengthS = length s
      pad = 24 - lengthS
    in
      replicate pad '0' ++ s 
