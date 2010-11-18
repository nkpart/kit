
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

  buildFileItem :: PBXBuildFile -> PListObjectItem 
  buildFileItem bf = lineItem i dict
    where fr = buildFileReference bf
          i = buildFileId bf
          dict = obj [
              "isa" ~> val "PBXBuildFile",
              "fileRef" ~> PListValue (fileReferenceId fr) 
            ]

  fileTypeBit :: FileType -> String
  fileTypeBit Header = "sourcecode.c.h"
  fileTypeBit Source = "sourcecode.c.objc"
  fileTypeBit Unknown = "sourcecode.unknown"

  fileReferenceItem :: PBXFileReference -> PListObjectItem
  fileReferenceItem fr = lineItem (fileReferenceId fr) dict
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

  layoutSection :: [PListObjectItem] -> String
  layoutSection body = stringJoin "\n" $ map (("    " ++) . show) body

  buildFileSection :: [PBXBuildFile] -> String
  buildFileSection bfs = layoutSection (map buildFileItem bfs ++ [
      lineItem "4728C530117C02B10027D7D1" $ obj [
          "isa" ~> val "PBXBuildFile",
          "fileRef" ~> val "4728C52F117C02B10027D7D1" 
        ],
      lineItem "AA747D9F0F9514B9006C5449" $ obj [
          "isa" ~> val "PBXBuildFile", 
          "fileRef" ~> val "AA747D9E0F9514B9006C5449" 
          ],
      lineItem "AACBBE4A0F95108600F1A2B1" $ obj [
          "isa" ~> val "PBXBuildFile",
          "fileRef" ~>  val "AACBBE490F95108600F1A2B1"
          ]
    ])

  fileReferenceSection :: [PBXFileReference] -> String
  fileReferenceSection refs = layoutSection (map fileReferenceItem refs ++ [
  		lineItem "4728C52F117C02B10027D7D1" $ obj [ 
        "isa" ~> val "PBXFileReference",
        "fileEncoding" ~> val "4",
        "lastKnownFileType" ~> val "text.xcconfig",
        "path" ~> val "Kit.xcconfig",
        "sourceTree" ~> val "<group>"
        ],
      lineItem "AA747D9E0F9514B9006C5449" $ obj [
        "isa" ~> val "PBXFileReference",
        "fileEncoding" ~> val "4",
        "lastKnownFileType" ~> val "sourcecode.c.h",
        "path" ~> val "KitDeps_Prefix.pch",
        "sourceTree" ~> val "SOURCE_ROOT"
        ],
      lineItem "AACBBE490F95108600F1A2B1" $ obj [
        "isa" ~> val "PBXFileReference",
        "lastKnownFileType" ~> val "wrapper.framework",
        "name" ~> val "Foundation.framework", 
        "path" ~> val "System/Library/Frameworks/Foundation.framework",
        "sourceTree" ~> val "SDKROOT"
        ],
      lineItem "D2AAC07E0554694100DB518D" $ obj [
        "isa" ~> val "PBXFileReference",
        "explicitFileType" ~> val "archive.ar",
        "includeInIndex" ~> val "0",
        "path" ~> val "libKitDeps.a",
        "sourceTree" ~> val "BUILT_PRODUCTS_DIR"
      ]
    ])

  classesSection :: [PBXFileReference] -> String 
  classesSection files = show $ lineItem "08FB77AEFE84172EC02AAC07" dict
	    where dict = obj [
            	    "isa" ~> val "PBXGroup",
            	    "children" ~> val ("(" ++ (mconcat (intersperse "," (map fileReferenceId files))) ++ ",)"),
            	    "name" ~> val "Classes",
            	    "sourceTree" ~> val "<group>"
	                ]
	
  headersSection :: [PBXBuildFile] -> String
  headersSection bfs = layoutSection [lineItem "D2AAC07A0554694100DB518D" $ obj [
    "isa" ~> val "PBXHeadersBuildPhase",
    "buildActionMask" ~> val "2147483647",
    "files" ~> arr (val "AA747D9F0F9514B9006C5449" : map (val . buildFileId) bfs),
    "runOnlyForDeploymentPostprocessing" ~> val "0"
	  ]]
	
  sourcesSection :: [PBXBuildFile] -> String
  sourcesSection bfs = layoutSection [lineItem "D2AAC07B0554694100DB518D" $ obj [
    "isa" ~> val "PBXSourcesBuildPhase",
    "buildActionMask" ~> val "2147483647",
    "files" ~>  arr (map (val . buildFileId) bfs),
    "runOnlyForDeploymentPostprocessing" ~> val "0"
    ]]


