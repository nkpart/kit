module Kit.Xcode.Common where
  
import Data.List
import System.FilePath.Posix
import Text.PList

type UUID = String
  
data FileType = Header | Source | Archive | Unknown
  
fileType :: String -> String 
fileType x | ".h" `isSuffixOf` x = "sourcecode.c.h"
fileType x | ".m" `isSuffixOf` x = "sourcecode.c.objc"
fileType x | ".mm" `isSuffixOf` x = "sourcecode.c.objc" 
fileType x | ".c" `isSuffixOf` x = "sourcecode.c.objc"
fileType x | ".a" `isSuffixOf` x = "archive.ar" 
fileType x | ".framework" `isSuffixOf` x = "wrapper.framework"
fileType x | ".xcconfig" `isSuffixOf` x = "text.xcconfig"
fileType _ = "sourcecode.unknown"

data PBXBuildFile = PBXBuildFile {
  buildFileId :: UUID,
  buildFileReference :: PBXFileReference,
  buildFileFlags :: String
} deriving (Eq, Show)
  
data PBXFileReference = PBXFileReference {
  fileReferenceId :: UUID,
  fileReferencePath :: String
} deriving (Eq, Show)
 
group :: String -> [PListType] -> PListType 
group name children = obj [
    "isa" ~> val "PBXGroup",
    "name" ~> val name,
    "sourceTree" ~> val "<group>",
    "children" ~> arr children
  ]

buildFile :: String -> String -> PListType
buildFile refUUID flags = obj [ "isa" ~> val "PBXBuildFile", "fileRef" ~> val refUUID, "settings" ~> obj ["COMPILER_FLAGS" ~> val flags ]]

buildFileItem :: PBXBuildFile -> PListObjectItem 
buildFileItem bf = buildFileId bf ~> (buildFile (fileReferenceId . buildFileReference $ bf) (buildFileFlags bf))

fileReferenceItem :: PBXFileReference -> PListObjectItem
fileReferenceItem fr = fileReferenceId fr ~> dict
  where
    fileName = fileReferenceName fr
    path = fileReferencePath fr
    dict = obj [
        "isa" ~> val "PBXFileReference",
        "fileEncoding" ~> val "4",
        "lastKnownFileType" ~> val (fileType fileName),
        "name" ~> val fileName,
        "path" ~> val path,
        "sourceTree" ~> val (if isAbsolute path then "<absolute>" else "<group>")
      ]

fileReferenceName :: PBXFileReference -> String
fileReferenceName = takeFileName . fileReferencePath
  
uuid :: Integer -> UUID
uuid i = let
     s = show i
     pad = 24 - length s
  in replicate pad '0' ++ s 

