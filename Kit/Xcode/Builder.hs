{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}

module Kit.Xcode.Builder (renderXcodeProject, SourceGroup(..)) where
  import Prelude hiding (mapM)
  import Data.Traversable as T (mapM, Traversable)
  import Data.Foldable (Foldable)
  import Kit.Xcode.Common
  import Kit.Xcode.ProjectFileTemplate
  import Kit.FlaggedFile
  import Text.PList
  import qualified Text.PList.PrettyPrint as PList (ppFlat)
  import Kit.Util
  import Data.List (nub, sortBy)
  import "mtl" Control.Monad.State
  import System.FilePath
  import Data.Function (on)

  data SourceGroup a = SourceGroup { 
                                 sourceGroupName :: String,
                                 sourceGroupHeaders :: [a],
                                 sourceGroupSources :: [a],
                                 sourceGroupLibs :: [a],
                                 sourceGroupFrameworks :: [a]
                                 } deriving (Functor, Foldable, Traversable)

  createBuildFile :: Integer -> FlaggedFile -> PBXBuildFile
  createBuildFile i ffpath = PBXBuildFile uuid1 (PBXFileReference uuid2 path) compileFlags
    where uuid1 = uuid i
          uuid2 = uuid $ i + 10000000
          path = flaggedFilePath ffpath
          compileFlags = flaggedFileFlags ffpath

  buildFileFromState :: FlaggedFile -> State [Integer] PBXBuildFile
  buildFileFromState filePath = flip createBuildFile filePath <$> popS 

  -- | Render an Xcode project!
  renderXcodeProject :: 
       [SourceGroup FlaggedFile]
    -> String     -- ^ Output lib name
    -> String     
  renderXcodeProject sourceGroups outputLibName = fst . flip runState [(1::Integer)..] $ do
      let libs = concatMap sourceGroupLibs sourceGroups
      buildFileGroups <- T.mapM (T.mapM buildFileFromState) sourceGroups
      let headerBuildFiles = concatMap sourceGroupHeaders buildFileGroups
          sourceBuildFiles = concatMap sourceGroupSources buildFileGroups
          libBuildFiles = concatMap sourceGroupLibs buildFileGroups -- Build File Items
          frameworkBuildFiles = concatMap sourceGroupFrameworks buildFileGroups
      let allBuildFiles = sourceBuildFiles ++ headerBuildFiles ++ libBuildFiles ++ frameworkBuildFiles
          -- Build And FileRef sections
          bfs = buildFileSection allBuildFiles
          frs = fileReferenceSection (map buildFileReference allBuildFiles) outputLibName 
          -- Groups
          classes = classesGroup $ filter (not . null . snd) $ map groupFileRefs buildFileGroups
            where groupFileRefs (SourceGroup n hs ss _ _) = (n, sortBy (compare `on` (takeFileName . fileReferencePath)) $ map buildFileReference (hs ++ ss))
          fg = frameworksGroup $ filter (not . null . snd) $ map groupLibRefs buildFileGroups 
            where groupLibRefs (SourceGroup n _ _ ls fs) = (n, map buildFileReference (ls ++ fs))
          -- classes = classesGroup $ sortBy (compare `on` (takeFileName . fileReferencePath)) $ map buildFileReference (sourceBuildFiles ++ headerBuildFiles)
          -- fg = frameworksGroup $ map buildFileReference (libBuildFiles ++ frameworkBuildFiles)
          -- Phases
          headersPhase = headersBuildPhase headerBuildFiles
          srcsPhase = sourcesBuildPhase sourceBuildFiles
          frameworksPhase = frameworksBuildPhase (libBuildFiles ++ frameworkBuildFiles)
          copyFrameworksPhase = copyFrameworksBuildPhase frameworkBuildFiles
          -- UUID indices
          libDirs = nub $ map (dropFileName . flaggedFilePath) libs 
      return . PList.ppFlat $ makeProjectPList (bfs ++ frs ++ [classes, headersPhase, srcsPhase, frameworksPhase, copyFrameworksPhase, fg]) libDirs

  buildFileSection :: [PBXBuildFile] -> [PListObjectItem]
  buildFileSection bfs = map buildFileItem bfs ++ [
      "4728C530117C02B10027D7D1" ~> buildFile kitConfigRefUUID "",
      "AA747D9F0F9514B9006C5449" ~> buildFile "AA747D9E0F9514B9006C5449" "",
      "AACBBE4A0F95108600F1A2B1" ~> buildFile "AACBBE490F95108600F1A2B1" ""
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
        "path" ~> val "Prefix.pch",
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

  classesGroup :: [(String, [PBXFileReference])] -> PListObjectItem 
  classesGroup files = classesGroupUUID ~> group "Classes" (map renderGroup files)
    where renderGroup (groupName, fs) = group groupName $ map (val . fileReferenceId) fs
	
  frameworksGroup :: [(String, [PBXFileReference])] -> PListObjectItem
  frameworksGroup files = frameworksGroupUUID ~> group "Frameworks" (val "AACBBE490F95108600F1A2B1" :  map renderGroup files)
    where renderGroup (groupName, fs) = group groupName $ map (val . fileReferenceId) fs

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

  copyFrameworksBuildPhase :: [PBXBuildFile] -> PListObjectItem
  copyFrameworksBuildPhase frameworks = copyFrameworksBuildPhaseUUID ~> obj [
    "isa" ~> val "PBXCopyFilesBuildPhase",
    "buildActionMask" ~> val "2147483647",
    "files" ~> arr (map (val . buildFileId) frameworks),
    "name" ~> val "Copy Frameworks",
    "dstPath" ~> val "\"\"",
    "dstSubfolderSpec" ~> val "10", -- this is the "Frameworks" destination, whatever
    "runOnlyForDeploymentPostprocessing" ~> val "0"
    ]
