module Kit.XCode.Common where

  import Data.Monoid
  import Data.List
  import System.FilePath.Posix
          
  type UUID = String
  type Dict = [(String, String)]
  
  data FileType = Header | Source | Unknown
  
  fileType :: String -> FileType
  fileType x | ".h" `isSuffixOf` x = Header
  fileType x | ".m" `isSuffixOf` x = Source
  fileType _ = Unknown
  
  (~>) = (,)
  
  data PBXBuildFile = PBXBuildFile {
    buildFileId :: String,
    buildFileReference :: PBXFileReference
  }
  
  data PBXFileReference = PBXFileReference {
    fileReferenceId :: String,
    fileReferencePath :: String
  }
  
  fileReferenceName = takeFileName . fileReferencePath
  
  lineItem :: UUID -> String -> Dict -> String
  lineItem uuid comment dict = uuid ++ " /* " ++ comment ++ " */ = " ++ buildDict dict ++ ";"
  
  buildDict :: Dict -> String
  buildDict ps = g . mconcat . map (\x -> fst x ++ " = " ++ snd x ++ "; ") $ ps
    where g s = "{"  ++ s ++ "}"
  
  uuid :: Integer -> UUID
  uuid i = let
      s = show i
      lengthS = length s
      pad = 24 - lengthS
    in
      replicate pad '0' ++ s 