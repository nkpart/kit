{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Kit.Xcode.ProjectFileTemplate (
      makeProjectPList
    , kitConfigRefUUID 
    , productRefUUID
    , headersBuildPhaseUUID
    , frameworksBuildPhaseUUID
    , classesGroupUUID
    , frameworksGroupUUID
    , sourcesBuildPhaseUUID
    ) where

import Kit.Xcode.Common
import Text.PList
import Control.Monad (join)
import Data.Maybe (maybeToList)

import qualified Data.List as L

makeProjectPList :: [PListObjectItem] -> [FilePath] -> PListFile 
makeProjectPList objects libDirs = projectFile objs projectRootUUID where
    objs = objects ++ groups ++ projectRoot ++ buildConfigurations libDirs

projectFile :: [PListObjectItem] -> String -> PListFile
projectFile objects rootId = plist "!$*UTF8*$!" rootId objects

projectRootUUID = "0867D690FE84028FC02AAC07" 

groupProductsUUID = "034768DFFF38A50411DB9C8B"
mainGroupUUID = "0867D691FE84028FC02AAC07"
classesGroupUUID = "08FB77AEFE84172EC02AAC07"
otherSourcesGroupUUID = "32C88DFF0371C24200C91783"
frameworksGroupUUID = "0867D69AFE84028FC02AAC07"

productRefUUID = "D2AAC07E0554694100DB518D"
kitConfigRefUUID = "4728C52F117C02B10027D7D1"

headersBuildPhaseUUID = "D2AAC07A0554694100DB518D"
sourcesBuildPhaseUUID = "D2AAC07B0554694100DB518D"
frameworksBuildPhaseUUID = "D2AAC07C0554694100DB518D"

projectBuildConfigurationsUUID = "1DEB922208733DC00010E9CD"
staticLibBuildConfigurationsUUID = "1DEB921E08733DC00010E9CD"

val_arr = arr . map val

groups = [ groupProductsUUID ~> group "Products" [ val productRefUUID ],
          mainGroupUUID ~> group "KitDeps" [
            val classesGroupUUID,
            val otherSourcesGroupUUID,
            val frameworksGroupUUID,
            val groupProductsUUID,
            val kitConfigRefUUID
          ],
          otherSourcesGroupUUID ~> group "Other Sources" [val "AA747D9E0F9514B9006C5449"]
        ]

projectRoot = [projectRootUUID ~> obj [
            "isa" ~> val "PBXProject",
            "buildConfigurationList" ~> val projectBuildConfigurationsUUID,
            "compatibilityVersion" ~> val "Xcode 3.2",
            "hasScannedForEncodings" ~> val "1",
            "mainGroup" ~> val mainGroupUUID ,
            "productRefGroup" ~> val groupProductsUUID,
            "projectDirPath" ~> val "",
            "projectRoot" ~> val "",
            "targets" ~> arr [ val staticLibTargetUUID, kitUpdateTarget ] 
        ], staticLibTarget]

staticLibTargetUUID = "D2AAC07D0554694100DB518D"

staticLibTarget = staticLibTargetUUID ~> obj [
            "isa" ~> val "PBXNativeTarget",
            "buildConfigurationList" ~> val staticLibBuildConfigurationsUUID,
            "buildPhases" ~> val_arr [ headersBuildPhaseUUID, sourcesBuildPhaseUUID, frameworksBuildPhaseUUID ],
            "buildRules" ~> arr [],
            "dependencies" ~> arr [],
            "name" ~> val "KitDeps",
            "productName" ~> val "KitDeps",
            "productReference" ~> val productRefUUID,
            "productType" ~> val "com.apple.product-type.library.static"
          ]

librarySearchPaths libDirs = "LIBRARY_SEARCH_PATHS" ~> val ("$(inherited) " ++ join (L.intersperse " " libDirs))

buildConfiguration name baseConfig settings = tobj $ [
    "isa" ~> val "XCBuildConfiguration",
    "buildSettings" ~> obj settings, 
    "name" ~> val name
  ] ++ (maybeToList baseConfig >>= (\c -> ["baseConfigurationReference" ~> val c]))

buildConfigurations libDirs = let libSearch = librarySearchPaths libDirs in [
          projectBuildConfigurationsUUID ~> obj [
              "isa" ~> val "XCConfigurationList",
              "buildConfigurations" ~> arr [buildConfiguration "Debug" (Just kitConfigRefUUID) [
                                                    "GCC_C_LANGUAGE_STANDARD" ~> val "c99",
                                                    "GCC_OPTIMIZATION_LEVEL" ~> val "0",
                                                    "GCC_WARN_ABOUT_RETURN_TYPE" ~> val "YES",
                                                    "GCC_WARN_UNUSED_VARIABLE" ~> val "YES",
                                                    "OTHER_LDFLAGS" ~> val "-ObjC",
                                                    "SDKROOT" ~> val "iphoneos",
                                                    libSearch ]
                                            , buildConfiguration "Release" (Just kitConfigRefUUID) [
                                                    "GCC_C_LANGUAGE_STANDARD" ~> val "c99",
                                                    "GCC_WARN_ABOUT_RETURN_TYPE" ~> val "YES",
                                                    "GCC_WARN_UNUSED_VARIABLE" ~> val "YES",
                                                    "OTHER_LDFLAGS" ~> val "-ObjC",
                                                    "SDKROOT" ~> val "iphoneos",
                                                    libSearch
                                                  ]],
              "defaultConfigurationIsVisible" ~> val "0",
              "defaultConfigurationName" ~> val "Release"
            ],
          staticLibBuildConfigurationsUUID ~> obj [
              "isa" ~> val "XCConfigurationList",
              "buildConfigurations" ~> arr [buildConfiguration "Debug" Nothing [
                  "ALWAYS_SEARCH_USER_PATHS" ~> val "NO",
                  "COPY_PHASE_STRIP" ~> val "NO",
                  "DSTROOT" ~> val "/tmp/KitDeps.dst",
                  "GCC_DYNAMIC_NO_PIC" ~> val "NO",
                  "GCC_MODEL_TUNING" ~> val "G5",
                  "GCC_OPTIMIZATION_LEVEL" ~> val "0",
                  "GCC_PRECOMPILE_PREFIX_HEADER" ~> val "YES",
                  "GCC_PREFIX_HEADER" ~> val "Prefix.pch",
                  "INSTALL_PATH" ~> val "/usr/local/lib",
                  "PRODUCT_NAME" ~> val "KitDeps",
                  libSearch 
                ], buildConfiguration "Release" Nothing [
                  "ALWAYS_SEARCH_USER_PATHS" ~> val "NO",
                  "DSTROOT" ~> val "/tmp/KitDeps.dst",
                  "GCC_MODEL_TUNING" ~> val "G5",
                  "GCC_PRECOMPILE_PREFIX_HEADER" ~> val "YES",
                  "GCC_PREFIX_HEADER" ~> val "Prefix.pch",
                  "INSTALL_PATH" ~> val "/usr/local/lib",
                  "PRODUCT_NAME" ~> val "KitDeps",
                  libSearch
                ]],
              "defaultConfigurationIsVisible" ~> val "0",
              "defaultConfigurationName" ~> val "Release"
            ]
        ]

kitUpdateTarget = tobj [
        "isa" ~> val "PBXLegacyTarget",
        "buildArgumentsString" ~> val "",
        "buildConfigurationList" ~> tobj [
          "isa" ~> val "XCConfigurationList",
          "buildConfigurations" ~> arr [ 
            buildConfiguration "Debug" Nothing [ "PRODUCT_NAME" ~> val "KitUpdate" ], 
            buildConfiguration "Release" Nothing ["PRODUCT_NAME" ~> val "KitUpdate" ]
            ],
          "defaultConfigurationIsVisible" ~> val "0",
          "defaultConfigurationName" ~> val "Debug"
          ],
        "buildPhases" ~> arr [],
        "buildToolPath" ~> val "/usr/bin/make",
        "buildWorkingDirectory" ~> val "",
        "dependencies" ~> arr [],
        "name" ~> val "KitUpdate",
        "passBuildSettingsInEnvironment" ~> val "1",
        "productName" ~> val "KitUpdate"
        ]


