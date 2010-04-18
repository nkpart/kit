module Kit.XCode.Builder where

  import Data.Monoid
  import Data.List
  import System.FilePath.Posix
  import Kit.XCode.Common
  
  buildXCodeProject :: [FilePath] -> [FilePath] -> String
  buildXCodeProject headers sources = undefined
    
  xxx :: FilePath -> UUID -> UUID -> PBXBuildFile
  xxx fp buildId fileId = PBXBuildFile buildId (PBXFileReference fileId fp)
  
  -- 47021EC1117A7776003DB5B7 /* motive.m in Sources */ = {isa = PBXBuildFile; fileRef = 47021EBF117A7776003DB5B7 /* motive.m */; };
  buildFileItem :: PBXBuildFile -> String
  buildFileItem bf = lineItem i comment dict
    where fr = buildFileReference bf
          i = buildFileId bf
          comment = let ft Unknown = "Unknown"
                        ft Header = "Headers"
                        ft Source = "Sources"
                        bit = ft . fileType . fileReferencePath $ fr
                    in fileReferenceName fr ++ " in " ++ bit
          dict = [
              "isa" ~> "PBXBuildFile",
              "fileRef" ~> (fileReferenceId fr ++ " /* " ++ fileReferenceName fr ++ " */")
            ]
  
  
  fileTypeBit :: FileType -> String
  fileTypeBit Header = "sourcecode.c.h"
  fileTypeBit Source = "sourcecode.c.objc"
  fileTypeBit Unknown = "sourcecode.unknown"
  
  -- 47021EBE117A7776003DB5B7 /* motive.h */ = {isa = PBXFileReference; fileEncoding = 4;
  -- lastKnownFileType = sourcecode.c.h; name = motive.h; path = "kits/motive-0.1/src/motive.h"; sourceTree = "<group>"; };
  fileReferenceItem :: PBXFileReference -> String
  fileReferenceItem fr = lineItem fid fileName dict
    where
      fid = fileReferenceId fr
      path = fileReferencePath fr
      fileName = fileReferenceName fr
      dict = [
          "isa" ~> "PBXFileReference",
          "fileEncoding" ~> "4",
          "lastKnownFileType" ~> (fileTypeBit . fileType $ fileName),
          "name" ~> fileName,
          "path" ~> show path,
          "sourceTree" ~> show "<group>"
        ]
  
  buildFileSection :: [PBXBuildFile] -> String
  buildFileSection bfs = let 
    lines = ("/* Begin PBXBuildFile section */" : (map buildFileItem bfs)) ++ [
      "AA747D9F0F9514B9006C5449 /* KitDeps_Prefix.pch in Headers */ = {isa = PBXBuildFile; fileRef = AA747D9E0F9514B9006C5449 /* KitDeps_Prefix.pch */; };",
		  "AACBBE4A0F95108600F1A2B1 /* Foundation.framework in Frameworks */ = {isa = PBXBuildFile; fileRef = AACBBE490F95108600F1A2B1 /* Foundation.framework */; };",
      "/* End PBXBuildFile section */"
      ] 
      in
    mconcat $ intersperse "\n" lines
      
  testBuilder = let
    header = PBXFileReference "1" "fk/fk.h"
    source = PBXFileReference "2" "fk/fk.m"
    headerBF = PBXBuildFile "10" header
    sourceBF = PBXBuildFile "20" source
      in do
          putStrLn . fileReferenceItem $ header
          putStrLn . buildFileItem $ headerBF
          putStrLn . fileReferenceItem $ source
          putStrLn . buildFileItem $ headerBF
          putStrLn . buildFileSection $  []
  
  