
module Kit.XCode.Builder (buildXCodeProject) where

  import Data.Monoid
  import Data.List
  import Kit.XCode.Common
  import Kit.XCode.ProjectFile
  import Kit.Util

  createBuildFile :: Integer -> FilePath -> PBXBuildFile
  createBuildFile i path = PBXBuildFile uuid1 $ PBXFileReference uuid2 path
    where uuid1 = uuid i
          uuid2 = uuid $ i + 10000000

  buildXCodeProject :: [FilePath] -> [FilePath] -> String
  buildXCodeProject headers sources =
      projectPbxProj bfs frs classes hs srcs
    where
      sourceStart = toInteger (length headers + 1)
      headerBuildFiles = zipWith createBuildFile [1..] headers
      sourceBuildFiles = zipWith createBuildFile [sourceStart..] sources
      allBuildFiles = (sourceBuildFiles ++ headerBuildFiles)
      bfs = buildFileSection allBuildFiles
      frs = fileReferenceSection $ map buildFileReference allBuildFiles
      classes = classesSection $ map buildFileReference (sourceBuildFiles ++ headerBuildFiles)
      hs = headersSection headerBuildFiles
      srcs = sourcesSection sourceBuildFiles

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
          "name" ~> show fileName,
          "path" ~> show path,
          "sourceTree" ~> show "<group>"
        ]

  layoutSection :: String -> [String] -> String
  layoutSection name body = let
      front  = "/* Begin " ++ name ++ " section */"
      middle = map ("    " ++) body
      back   = "/* End " ++ name ++ " section */"
      in mconcat $ intersperse "\n" ((front : middle) ++ [back])

  buildFileSection :: [PBXBuildFile] -> String
  buildFileSection bfs = layoutSection "PBXBuildFile" (map buildFileItem bfs ++ [
      "4728C530117C02B10027D7D1 /* Kit.xcconfig in Resources */ = {isa = PBXBuildFile; fileRef = 4728C52F117C02B10027D7D1 /* Kit.xcconfig */; };",
      "AA747D9F0F9514B9006C5449 /* KitDeps_Prefix.pch in Headers */ = {isa = PBXBuildFile; fileRef = AA747D9E0F9514B9006C5449 /* KitDeps_Prefix.pch */; };",
      "AACBBE4A0F95108600F1A2B1 /* Foundation.framework in Frameworks */ = {isa = PBXBuildFile; fileRef = AACBBE490F95108600F1A2B1 /* Foundation.framework */; };"
    ])

  fileReferenceSection :: [PBXFileReference] -> String
  fileReferenceSection refs = layoutSection "PBXFileReference" (map fileReferenceItem refs ++ [
  		"4728C52F117C02B10027D7D1 /* Kit.xcconfig */ = {isa = PBXFileReference; fileEncoding = 4; lastKnownFileType = text.xcconfig; path = Kit.xcconfig; sourceTree = \"<group>\"; };",
      "AA747D9E0F9514B9006C5449 /* KitDeps_Prefix.pch */ = {isa = PBXFileReference; fileEncoding = 4; lastKnownFileType = sourcecode.c.h; path = KitDeps_Prefix.pch; sourceTree = SOURCE_ROOT; };",
	    "AACBBE490F95108600F1A2B1 /* Foundation.framework */ = {isa = PBXFileReference; lastKnownFileType = wrapper.framework; name = Foundation.framework; path = System/Library/Frameworks/Foundation.framework; sourceTree = SDKROOT; };",
	    "D2AAC07E0554694100DB518D /* libKitDeps.a */ = {isa = PBXFileReference; explicitFileType = archive.ar; includeInIndex = 0; path = libKitDeps.a; sourceTree = BUILT_PRODUCTS_DIR; };"
    ])

  classesSection :: [PBXFileReference] -> String
  classesSection files = lineItem "08FB77AEFE84172EC02AAC07" "Classes" dict
	    where dict = [
            	    "isa" ~> "PBXGroup",
            	    "children" ~> ("(" ++ (mconcat (intersperse "," (map fileReferenceId files))) ++ ",)"),
            	    "name" ~> "Classes",
            	    "sourceTree" ~> show "<group>"
	                ]
	
	{-/* Begin PBXHeadersBuildPhase section */
  		D2AAC07A0554694100DB518D /* Headers */ = {
  			isa = PBXHeadersBuildPhase;
  			buildActionMask = 2147483647;
  			files = (
  				AA747D9F0F9514B9006C5449 /* KitDeps_Prefix.pch in Headers */,
  				4791C244117A7893001EB278 /* motive.h in Headers */,
  			);
  			runOnlyForDeploymentPostprocessing = 0;
  		};
  /* End PBXHeadersBuildPhase section */-}
  headersSection :: [PBXBuildFile] -> String
  headersSection bfs = layoutSection "PBXHeadersBuildPhase" [lineItem "D2AAC07A0554694100DB518D" "Headers" [
    "isa" ~> "PBXHeadersBuildPhase",
    "buildActionMask" ~> "2147483647",
    "files" ~> ("(" ++ (stringJoin "," $ prefixPchUUID : map buildFileId bfs) ++ ",)"),
    "runOnlyForDeploymentPostprocessing" ~> "0"
	  ]]
	  where prefixPchUUID = "AA747D9F0F9514B9006C5449"
	
	{-
    /* Begin PBXSourcesBuildPhase section */
    		D2AAC07B0554694100DB518D /* Sources */ = {
    			isa = PBXSourcesBuildPhase;
    			buildActionMask = 2147483647;
    			files = (
    				47021EC1117A7776003DB5B7 /* motive.m in Sources */,
    			);
    			runOnlyForDeploymentPostprocessing = 0;
    		};
    /* End PBXSourcesBuildPhase section */
	-}
  sourcesSection :: [PBXBuildFile] -> String
  sourcesSection bfs = layoutSection "PBXSourcesBuildPhase" [lineItem "D2AAC07B0554694100DB518D" "Sources" [
    "isa" ~> "PBXSourcesBuildPhase",
    "buildActionMask" ~> "2147483647",
    "files" ~> ("(" ++ (stringJoin "," $ map buildFileId bfs) ++ ",)"),
    "runOnlyForDeploymentPostprocessing" ~> "0"
    ]]

  testBuilder = let
    header = PBXFileReference "1" "fk/fk.h"
    source = PBXFileReference "2" "fk/fk.m"
    headerBF = PBXBuildFile "10" header
    sourceBF = PBXBuildFile "20" source
      in do
          putStrLn . buildFileSection $  [headerBF, sourceBF]
          putStrLn . fileReferenceSection $ [header, source]

