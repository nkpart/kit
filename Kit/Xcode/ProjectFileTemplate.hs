{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Kit.Xcode.ProjectFileTemplate where

import Kit.Xcode.Common
import Text.PList
import Control.Monad (join)

import qualified Data.List as L

projectFile objects rootId = PListFile "!$*UTF8*$!" $ obj [
      "archiveVersion" ~> val "1",
      "classes" ~> obj [],
      "objectVersion" ~> val "46",
      "objects" ~> obj objects, 
      "rootObject" ~> val rootId 
  ]

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

makeProjectPList :: [PListObjectItem] -> [FilePath] -> PListFile 
makeProjectPList objects libDirs = projectFile objs "0867D690FE84028FC02AAC07" where
    objs = objects ++ groups ++ targets ++ buildConfigurations libDirs

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
      
targets = ("D2AAC07D0554694100DB518D" ~> obj [
            "isa" ~> val "PBXNativeTarget",
            "buildConfigurationList" ~> val "1DEB921E08733DC00010E9CD",
            "buildPhases" ~> arr [ val headersBuildPhaseUUID, val sourcesBuildPhaseUUID, val frameworksBuildPhaseUUID ],
            "buildRules" ~> arr [],
            "dependencies" ~> arr [],
            "name" ~> val "KitDeps",
            "productName" ~> val "KitDeps",
            "productReference" ~> val productRefUUID,
            "productType" ~> val "com.apple.product-type.library.static"
          ]) : kitUpdateTarget ++ ["0867D690FE84028FC02AAC07" ~> obj [
            "isa" ~> val "PBXProject",
            "buildConfigurationList" ~> val "1DEB922208733DC00010E9CD",
            "compatibilityVersion" ~> val "Xcode 3.2",
            "hasScannedForEncodings" ~> val "1",
            "mainGroup" ~> val mainGroupUUID ,
            "productRefGroup" ~> val groupProductsUUID,
            "projectDirPath" ~> val "",
            "projectRoot" ~> val "",
            "targets" ~> arr [ val "D2AAC07D0554694100DB518D", val "470E2D641287730A0084AE6F" ] 
        ]]

librarySearchPaths libDirs = "LIBRARY_SEARCH_PATHS" ~> val ("$(inherited) " ++ join (L.intersperse " " libDirs))

buildConfigurations libDirs = let libSearch = librarySearchPaths libDirs in ["1DEB921F08733DC00010E9CD" ~> obj [
            "isa" ~> val "XCBuildConfiguration",
            "buildSettings" ~> obj [
              "ALWAYS_SEARCH_USER_PATHS" ~> val "NO",
              "ARCHS" ~> val "$(ARCHS_STANDARD_32_BIT)",
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
            ],
            "name" ~> val "Debug"
          ],
          "1DEB922008733DC00010E9CD" ~> obj [
            "isa" ~> val "XCBuildConfiguration",
            "buildSettings" ~> obj [
              "ALWAYS_SEARCH_USER_PATHS" ~> val "NO",
              "ARCHS" ~> val "$(ARCHS_STANDARD_32_BIT)",
              "DSTROOT" ~> val "/tmp/KitDeps.dst",
              "GCC_MODEL_TUNING" ~> val "G5",
              "GCC_PRECOMPILE_PREFIX_HEADER" ~> val "YES",
              "GCC_PREFIX_HEADER" ~> val "Prefix.pch",
              "INSTALL_PATH" ~> val "/usr/local/lib",
              "PRODUCT_NAME" ~> val "KitDeps",
              libSearch
              ],
            "name" ~> val "Release"
            ],
          "1DEB922308733DC00010E9CD" ~> obj [
            "isa" ~> val "XCBuildConfiguration",
            "baseConfigurationReference" ~> val kitConfigRefUUID,
            "buildSettings" ~> obj [
              "ARCHS" ~> val "$(ARCHS_STANDARD_32_BIT)",
              "GCC_C_LANGUAGE_STANDARD" ~> val "c99",
              "GCC_OPTIMIZATION_LEVEL" ~> val "0",
              "GCC_WARN_ABOUT_RETURN_TYPE" ~> val "YES",
              "GCC_WARN_UNUSED_VARIABLE" ~> val "YES",
              "OTHER_LDFLAGS" ~> val "-ObjC",
              "SDKROOT" ~> val "iphoneos",
              libSearch
            ],
            "name" ~> val "Debug"
          ],
          "1DEB922408733DC00010E9CD" ~> obj [
            "isa" ~> val "XCBuildConfiguration",
            "baseConfigurationReference" ~> val kitConfigRefUUID,
            "buildSettings" ~> obj [
              "ARCHS" ~> val "$(ARCHS_STANDARD_32_BIT)",
              "GCC_C_LANGUAGE_STANDARD" ~> val "c99",
              "GCC_WARN_ABOUT_RETURN_TYPE" ~> val "YES",
              "GCC_WARN_UNUSED_VARIABLE" ~> val "YES",
              "OTHER_LDFLAGS" ~> val "-ObjC",
              "SDKROOT" ~> val "iphoneos",
              libSearch
            ],
            "name" ~> val "Release"
          ],
          "1DEB921E08733DC00010E9CD" ~> obj [
            "isa" ~> val "XCConfigurationList",
            "buildConfigurations" ~> arr [
              val "1DEB921F08733DC00010E9CD",
              val "1DEB922008733DC00010E9CD"
            ],
            "defaultConfigurationIsVisible" ~> val "0",
            "defaultConfigurationName" ~> val "Release"
          ],
          "1DEB922208733DC00010E9CD" ~> obj [
            "isa" ~> val "XCConfigurationList",
            "buildConfigurations" ~> arr [
              val "1DEB922308733DC00010E9CD",
              val "1DEB922408733DC00010E9CD"
            ],
            "defaultConfigurationIsVisible" ~> val "0",
            "defaultConfigurationName" ~> val "Release"
          ]
        ]

kitUpdateTarget = [
      "470E2D641287730A0084AE6F" ~> obj [ 
        "isa" ~> val "PBXLegacyTarget",
        "buildArgumentsString" ~> val "",
        "buildConfigurationList" ~> val "470E2D721287731E0084AE6F",
        "buildPhases" ~> arr [],
        "buildToolPath" ~> val "/usr/bin/make",
        "buildWorkingDirectory" ~> val "",
        "dependencies" ~> arr [],
        "name" ~> val "KitUpdate",
        "passBuildSettingsInEnvironment" ~> val "1",
        "productName" ~> val "KitUpdate"
      ],
      "470E2D721287731E0084AE6F" ~> obj [
        "isa" ~> val "XCConfigurationList",
        "buildConfigurations" ~> arr [ val "470E2D651287730B0084AE6F", val "470E2D661287730B0084AE6F" ],
        "defaultConfigurationIsVisible" ~> val "0",
        "defaultConfigurationName" ~> val "Debug"
      ],
      "470E2D651287730B0084AE6F" ~> obj [
        "isa" ~> val "XCBuildConfiguration",
        "buildSettings" ~> obj [
          "COPY_PHASE_STRIP" ~> val "NO",
          "GCC_DYNAMIC_NO_PIC" ~> val "NO",
          "GCC_OPTIMIZATION_LEVEL" ~> val "0", 
          "PRODUCT_NAME" ~> val "KitUpdate" 
        ],
        "name" ~> val "Debug"
      ], 
      "470E2D661287730B0084AE6F" ~> obj [
        "isa" ~> val "XCBuildConfiguration",
        "buildSettings" ~> obj [ "PRODUCT_NAME" ~> val "KitUpdate" ],
        "name" ~> val "Release"
      ]]


