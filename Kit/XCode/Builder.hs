
module Kit.XCode.Builder (buildXCodeProject) where

  import Data.Monoid
  import Data.List
  import Kit.XCode.Common
  import Kit.XCode.ProjectFile
  import Kit.XCode.OldPList
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
  buildFileItem :: PBXBuildFile -> PListObjectItem 
  buildFileItem bf = lineItem i (comment c) dict
    where fr = buildFileReference bf
          i = buildFileId bf
          c = let ft Unknown = "Unknown"
                  ft Header = "Headers"
                  ft Source = "Sources"
                  bit = ft . fileType . fileReferencePath $ fr
               in fileReferenceName fr ++ " in " ++ bit
          dict = obj [
              "isa" ~> val "PBXBuildFile",
              "fileRef" ~> PListValue (fileReferenceId fr) (comment $ fileReferenceName fr)
            ]

  fileTypeBit :: FileType -> String
  fileTypeBit Header = "sourcecode.c.h"
  fileTypeBit Source = "sourcecode.c.objc"
  fileTypeBit Unknown = "sourcecode.unknown"

  -- 47021EBE117A7776003DB5B7 /* motive.h */ = {isa = PBXFileReference; fileEncoding = 4;
  -- lastKnownFileType = sourcecode.c.h; name = motive.h; path = "kits/motive-0.1/src/motive.h"; sourceTree = "<group>"; };
  fileReferenceItem :: PBXFileReference -> PListObjectItem
  fileReferenceItem fr = lineItem fid (comment fileName) dict
    where
      fid = fileReferenceId fr
      path = fileReferencePath fr
      fileName = fileReferenceName fr
      dict = obj [
          "isa" ~> val "PBXFileReference",
          "fileEncoding" ~> val "4",
          "lastKnownFileType" ~> val (fileTypeBit . fileType $ fileName),
          "name" ~> val fileName,
          "path" ~> val path,
          "sourceTree" ~> val "<group>"
        ]

  layoutSection :: [PListObjectItem] -> String
  layoutSection body = let
      middle = map (("    " ++) . show) body
      in stringJoin "\n" middle

  buildFileSection :: [PBXBuildFile] -> String
  buildFileSection bfs = layoutSection (map buildFileItem bfs ++ [
      lineItem "4728C530117C02B10027D7D1" (comment "Kit.xcconfig in Resources") $ obj [
          "isa" ~> val "PBXBuildFile",
          "fileRef" ~> val "4728C52F117C02B10027D7D1" /*/ "Kit.xcconfig" 
        ],
      lineItem "AA747D9F0F9514B9006C5449" (comment "KitDeps_Prefix.pch in Headers") $ obj [
          "isa" ~> val "PBXBuildFile", 
          "fileRef" ~> val "AA747D9E0F9514B9006C5449" /*/ "KitDeps_Prefix.pch"
          ],
      lineItem "AACBBE4A0F95108600F1A2B1" (comment "Foundation.framework in Frameworks") $ obj [
          "isa" ~> val "PBXBuildFile",
          "fileRef" ~>  val "AACBBE490F95108600F1A2B1" /*/ "Foundation.framework" 
          ]
    ])

  fileReferenceSection :: [PBXFileReference] -> String
  fileReferenceSection refs = layoutSection (map fileReferenceItem refs ++ [
  		lineItem "4728C52F117C02B10027D7D1" (comment "Kit.xcconfig") $ obj [ 
        "isa" ~> val "PBXFileReference",
        "fileEncoding" ~> val "4",
        "lastKnownFileType" ~> val "text.xcconfig",
        "path" ~> val "Kit.xcconfig",
        "sourceTree" ~> val "<group>"
        ],
      lineItem "AA747D9E0F9514B9006C5449" (comment "KitDeps_Prefix.pch") $ obj [
        "isa" ~> val "PBXFileReference",
        "fileEncoding" ~> val "4",
        "lastKnownFileType" ~> val "sourcecode.c.h",
        "path" ~> val "KitDeps_Prefix.pch",
        "sourceTree" ~> val "SOURCE_ROOT"
        ],
      lineItem "AACBBE490F95108600F1A2B1" (comment "Foundation.framework") $ obj [
        "isa" ~> val "PBXFileReference",
        "lastKnownFileType" ~> val "wrapper.framework",
        "name" ~> val "Foundation.framework", 
        "path" ~> val "System/Library/Frameworks/Foundation.framework",
        "sourceTree" ~> val "SDKROOT"
        ],
      lineItem "D2AAC07E0554694100DB518D" (comment "libKitDeps.a") $ obj [
        "isa" ~> val "PBXFileReference",
        "explicitFileType" ~> val "archive.ar",
        "includeInIndex" ~> val "0",
        "path" ~> val "libKitDeps.a",
        "sourceTree" ~> val "BUILT_PRODUCTS_DIR"
      ]
    ])

  classesSection :: [PBXFileReference] -> String 
  classesSection files = show $ lineItem "08FB77AEFE84172EC02AAC07" (comment "Classes") dict
	    where dict = obj [
            	    "isa" ~> val "PBXGroup",
            	    "children" ~> val ("(" ++ (mconcat (intersperse "," (map fileReferenceId files))) ++ ",)"),
            	    "name" ~> val "Classes",
            	    "sourceTree" ~> val "<group>"
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
  headersSection bfs = layoutSection [lineItem "D2AAC07A0554694100DB518D" (comment "Headers") $ obj [
    "isa" ~> val "PBXHeadersBuildPhase",
    "buildActionMask" ~> val "2147483647",
    "files" ~> arr (val prefixPchUUID : map (val . buildFileId) bfs),
    "runOnlyForDeploymentPostprocessing" ~> val "0"
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
  sourcesSection bfs = layoutSection [lineItem "D2AAC07B0554694100DB518D" (comment "Sources") $ obj [
    "isa" ~> val "PBXSourcesBuildPhase",
    "buildActionMask" ~> val "2147483647",
    "files" ~>  arr (map (val . buildFileId) bfs),
    "runOnlyForDeploymentPostprocessing" ~> val "0"
    ]]


