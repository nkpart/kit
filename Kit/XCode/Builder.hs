
module Kit.XCode.Builder (buildXCodeProject) where
  import Data.Monoid
  import Data.List
  import Kit.XCode.Common
  import Kit.XCode.ProjectFileTemplate
  import Text.PList
  import Kit.Util

  createBuildFile :: Integer -> FilePath -> PBXBuildFile
  createBuildFile i path = PBXBuildFile uuid1 $ PBXFileReference uuid2 path
    where uuid1 = uuid i
          uuid2 = uuid $ i + 10000000

  buildXCodeProject :: [FilePath] -> [FilePath] -> String
  buildXCodeProject headers sources = show $ makeProjectPList bfs frs classes hs srcs
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

  buildFileSection :: [PBXBuildFile] -> [PListObjectItem]
  buildFileSection bfs = (map buildFileItem bfs ++ [
      "4728C530117C02B10027D7D1" ~> obj [
          "isa" ~> val "PBXBuildFile",
          "fileRef" ~> val "4728C52F117C02B10027D7D1" 
        ],
      "AA747D9F0F9514B9006C5449" ~> obj [
          "isa" ~> val "PBXBuildFile", 
          "fileRef" ~> val "AA747D9E0F9514B9006C5449" 
          ],
      "AACBBE4A0F95108600F1A2B1" ~> obj [
          "isa" ~> val "PBXBuildFile",
          "fileRef" ~>  val "AACBBE490F95108600F1A2B1"
          ]
    ])

  fileReferenceSection :: [PBXFileReference] -> [PListObjectItem]
  fileReferenceSection refs = map fileReferenceItem refs ++ [
  		"4728C52F117C02B10027D7D1" ~> obj [ 
        "isa" ~> val "PBXFileReference",
        "fileEncoding" ~> val "4",
        "lastKnownFileType" ~> val "text.xcconfig",
        "path" ~> val "Kit.xcconfig",
        "sourceTree" ~> val "<group>"
        ],
      "AA747D9E0F9514B9006C5449" ~> obj [
        "isa" ~> val "PBXFileReference",
        "fileEncoding" ~> val "4",
        "lastKnownFileType" ~> val "sourcecode.c.h",
        "path" ~> val "KitDeps_Prefix.pch",
        "sourceTree" ~> val "SOURCE_ROOT"
        ],
      "AACBBE490F95108600F1A2B1" ~> obj [
        "isa" ~> val "PBXFileReference",
        "lastKnownFileType" ~> val "wrapper.framework",
        "name" ~> val "Foundation.framework", 
        "path" ~> val "System/Library/Frameworks/Foundation.framework",
        "sourceTree" ~> val "SDKROOT"
        ],
      "D2AAC07E0554694100DB518D" ~> obj [
        "isa" ~> val "PBXFileReference",
        "explicitFileType" ~> val "archive.ar",
        "includeInIndex" ~> val "0",
        "path" ~> val "libKitDeps.a",
        "sourceTree" ~> val "BUILT_PRODUCTS_DIR"
      ]
    ]

  classesSection :: [PBXFileReference] -> PListObjectItem 
  classesSection files = "08FB77AEFE84172EC02AAC07" ~> dict
	    where dict = obj [
            	    "isa" ~> val "PBXGroup",
            	    "children" ~> arr (map (val . fileReferenceId) files),
            	    "name" ~> val "Classes",
            	    "sourceTree" ~> val "<group>"
	                ]
	
  headersSection :: [PBXBuildFile] -> PListObjectItem
  headersSection bfs = "D2AAC07A0554694100DB518D" ~> obj [
    "isa" ~> val "PBXHeadersBuildPhase",
    "buildActionMask" ~> val "2147483647",
    "files" ~> arr (val "AA747D9F0F9514B9006C5449" : map (val . buildFileId) bfs),
    "runOnlyForDeploymentPostprocessing" ~> val "0"
	  ]
	
  sourcesSection :: [PBXBuildFile] -> PListObjectItem
  sourcesSection bfs = "D2AAC07B0554694100DB518D" ~> obj [
    "isa" ~> val "PBXSourcesBuildPhase",
    "buildActionMask" ~> val "2147483647",
    "files" ~>  arr (map (val . buildFileId) bfs),
    "runOnlyForDeploymentPostprocessing" ~> val "0"
    ]


