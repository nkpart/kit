module Kit.XCode.Common where
  import Data.Monoid
  import Data.List
  import System.FilePath.Posix
  
  import Text.PList

  
  type UUID = String
  
  data FileType = Header | Source | Archive | Unknown
  
  fileType :: String -> FileType
  fileType x | ".h" `isSuffixOf` x = Header
  fileType x | ".m" `isSuffixOf` x = Source
  fileType x | ".mm" `isSuffixOf` x = Source
  fileType x | ".c" `isSuffixOf` x = Source
  fileType x | ".a" `isSuffixOf` x = Archive
  fileType _ = Unknown

  fileTypeBit :: FileType -> String
  fileTypeBit Header = "sourcecode.c.h"
  fileTypeBit Source = "sourcecode.c.objc"
  fileTypeBit Unknown = "sourcecode.unknown"
  fileTypeBit Archive = "archive.ar"

  data PBXBuildFile = PBXBuildFile {
    buildFileId :: UUID,
    buildFileReference :: PBXFileReference
  } deriving (Eq, Show)
  
  data PBXFileReference = PBXFileReference {
    fileReferenceId :: UUID,
    fileReferencePath :: String
  } deriving (Eq, Show)
  
  buildFileItem :: PBXBuildFile -> PListObjectItem 
  buildFileItem bf = i ~> dict
    where fr = buildFileReference bf
          i = buildFileId bf
          dict = obj [
              "isa" ~> val "PBXBuildFile",
              "fileRef" ~> PListValue (fileReferenceId fr) 
            ]

  fileReferenceItem :: PBXFileReference -> PListObjectItem
  fileReferenceItem fr = (fileReferenceId fr) ~> dict
    where
      fileName = fileReferenceName fr
      dict = obj [
          "isa" ~> val "PBXFileReference",
          "fileEncoding" ~> val "4",
          "lastKnownFileType" ~> val (fileTypeBit . fileType $ fileName),
          "name" ~> val fileName,
          "path" ~> (val $ fileReferencePath fr),
          "sourceTree" ~> val "<group>"
        ]


  fileReferenceName = takeFileName . fileReferencePath
  
  uuid :: Integer -> UUID
  uuid i = let
       s = show i
       pad = 24 - length s
    in replicate pad '0' ++ s 
