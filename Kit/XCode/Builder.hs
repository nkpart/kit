
module Kit.XCode.Builder (buildXCodeProject) where
  import Kit.XCode.Common
  import Kit.XCode.ProjectFileTemplate
  import Text.PList
  import Kit.Util
  import Data.List (nub)
  import Control.Monad.State

  createBuildFile :: Integer -> FilePath -> PBXBuildFile
  createBuildFile i path = PBXBuildFile uuid1 $ PBXFileReference uuid2 path
    where uuid1 = uuid i
          uuid2 = uuid $ i + 10000000

  xxx2 :: FilePath -> State [Integer] PBXBuildFile
  xxx2 x = flip createBuildFile x `fmap` popS 

  buildXCodeProject :: [FilePath] -> [FilePath] -> [FilePath] -> String
  buildXCodeProject headers sources libs = fst . flip runState [1..] $ do
      headerBuildFiles <- mapM xxx2 headers
      sourceBuildFiles <- mapM xxx2 sources
      libBuildFiles <- mapM xxx2 libs
      -- Build File Items
      let allBuildFiles = (sourceBuildFiles ++ headerBuildFiles ++ libBuildFiles)
          -- Build And FileRef sections
          bfs = buildFileSection allBuildFiles
          frs = fileReferenceSection (map buildFileReference allBuildFiles) "libKitDeps.a"
          -- Groups
          classes = classesGroup $ map buildFileReference (sourceBuildFiles ++ headerBuildFiles)
          fg = frameworksGroup $ map buildFileReference libBuildFiles
          -- Phases
          headersPhase = headersBuildPhase headerBuildFiles
          srcsPhase = sourcesBuildPhase sourceBuildFiles
          frameworksPhase = frameworksBuildPhase libBuildFiles
          -- UUID indices
          libDirs = nub $ map dropFileName libs 
      return . show $ makeProjectPList (bfs ++ frs ++ [classes, headersPhase, srcsPhase, frameworksPhase, fg]) libDirs

  buildFileSection :: [PBXBuildFile] -> [PListObjectItem]
  buildFileSection bfs = map buildFileItem bfs ++ [
      "4728C530117C02B10027D7D1" ~> buildFile kitConfigRefUUID,
      "AA747D9F0F9514B9006C5449" ~> buildFile "AA747D9E0F9514B9006C5449",
      "AACBBE4A0F95108600F1A2B1" ~> buildFile "AACBBE490F95108600F1A2B1"
    ]

  fileReferenceSection :: [PBXFileReference] -> String -> [PListObjectItem]
  fileReferenceSection refs archiveName = map fileReferenceItem refs ++ [
  		kitConfigRefUUID ~> obj [ 
        "isa" ~> val "PBXFileReference",
        "fileEncoding" ~> val "4",
        "lastKnownFileType" ~> val "text.xcconfig",
        "path" ~> val "DepsOnly.xcconfig",
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
      productRefUUID ~> obj [
        "isa" ~> val "PBXFileReference",
        "explicitFileType" ~> val "archive.ar",
        "includeInIndex" ~> val "0",
        "path" ~> val archiveName,
        "sourceTree" ~> val "BUILT_PRODUCTS_DIR"
      ]
    ]

  classesGroup :: [PBXFileReference] -> PListObjectItem 
  classesGroup files = classesGroupUUID ~> group "Classes" (map (val . fileReferenceId) files)
	
  frameworksGroup :: [PBXFileReference] -> PListObjectItem
  frameworksGroup files = frameworksGroupUUID ~> group "Frameworks" (val "AACBBE490F95108600F1A2B1" :  map (val . fileReferenceId) files)

  frameworksBuildPhase :: [PBXBuildFile] -> PListObjectItem  
  frameworksBuildPhase libs = frameworksBuildPhaseUUID ~> obj [
            "isa" ~> val "PBXFrameworksBuildPhase",
            "buildActionMask" ~> val "2147483647",
            "files" ~> arr (val "AACBBE4A0F95108600F1A2B1" : map (val . buildFileId) libs),
            "runOnlyForDeploymentPostprocessing" ~> val "0"
          ]

  headersBuildPhase :: [PBXBuildFile] -> PListObjectItem
  headersBuildPhase bfs = headersBuildPhaseUUID ~> obj [
    "isa" ~> val "PBXHeadersBuildPhase",
    "buildActionMask" ~> val "2147483647",
    "files" ~> arr (val "AA747D9F0F9514B9006C5449" : map (val . buildFileId) bfs),
    "runOnlyForDeploymentPostprocessing" ~> val "0"
	  ]
	
  sourcesBuildPhase :: [PBXBuildFile] -> PListObjectItem
  sourcesBuildPhase bfs = sourcesBuildPhaseUUID ~> obj [
    "isa" ~> val "PBXSourcesBuildPhase",
    "buildActionMask" ~> val "22147483647147483647",
    "files" ~>  arr (map (val . buildFileId) bfs),
    "runOnlyForDeploymentPostprocessing" ~> val "0"
    ]


