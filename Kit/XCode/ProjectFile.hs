{-

  Constant sections of the project file
-}
module Kit.XCode.ProjectFile (projectPbxProj) where
  
  nl a b = a ++ "\n" ++ b 
  
  projectPbxProj bfs fileRefsSection classes headers sources = 
      top `nl` bfs `nl` fileRefsSection `nl` next1 `nl` classes `nl` next2 `nl` headers `nl` next3 `nl` sources `nl` bottom
  
  top = "// !$*UTF8*$!" `nl` 
    "{" `nl`
    " archiveVersion = 1;" `nl`
    " classes = {" `nl`
    " };" `nl`
    " objectVersion = 45;" `nl`
    " objects = {"
  
  next1 = "/* Begin PBXFrameworksBuildPhase section */" `nl`
    		"D2AAC07C0554694100DB518D /* Frameworks */ = {" `nl`
    			"isa = PBXFrameworksBuildPhase;" `nl`
    			"buildActionMask = 2147483647;" `nl`
    			"files = (" `nl`
    				"AACBBE4A0F95108600F1A2B1 /* Foundation.framework in Frameworks */," `nl`
    			");" `nl`
    			"runOnlyForDeploymentPostprocessing = 0;" `nl`
    		"};" `nl`
    "/* End PBXFrameworksBuildPhase section */" `nl`
    "" `nl`
    "/* Begin PBXGroup section */" `nl`
    		"034768DFFF38A50411DB9C8B /* Products */ = {" `nl`
    			"isa = PBXGroup;" `nl`
    			"children = (" `nl`
    				"D2AAC07E0554694100DB518D /* libKitDeps.a */," `nl`
    			");" `nl`
    			"name = Products;" `nl`
    			"sourceTree = \"<group>\";" `nl`
    		"};" `nl`
    		"0867D691FE84028FC02AAC07 /* KitDeps */ = {" `nl`
    			"isa = PBXGroup;" `nl`
    			"children = (" `nl`
    				"08FB77AEFE84172EC02AAC07 /* Classes */," `nl`
    				"32C88DFF0371C24200C91783 /* Other Sources */," `nl`
    				"0867D69AFE84028FC02AAC07 /* Frameworks */," `nl`
    				"034768DFFF38A50411DB9C8B /* Products */," `nl`
    				"4728C52F117C02B10027D7D1 /* Kit.xcconfig */," `nl`
    			");" `nl`
    			"name = KitDeps;" `nl`
    			"sourceTree = \"<group>\";" `nl`
    		"};" `nl`
    		"0867D69AFE84028FC02AAC07 /* Frameworks */ = {" `nl`
    			"isa = PBXGroup;" `nl`
    			"children = (" `nl`
    				"AACBBE490F95108600F1A2B1 /* Foundation.framework */," `nl`
    			");" `nl`
    			"name = Frameworks;" `nl`
    			"sourceTree = \"<group>\";" `nl`
    		"};"
    		
  next2 = "32C88DFF0371C24200C91783 /* Other Sources */ = {" `nl`
        			"isa = PBXGroup;" `nl`
        			"children = (" `nl`
        				"AA747D9E0F9514B9006C5449 /* KitDeps_Prefix.pch */," `nl`
        			");" `nl`
        			"name = \"Other Sources\";" `nl`
        			"sourceTree = \"<group>\";" `nl`
        		"};" `nl`
        "/* End PBXGroup section */"
        
  next3 = "/* Begin PBXNativeTarget section */" `nl`
        		"D2AAC07D0554694100DB518D /* KitDeps */ = {" `nl`
        			"isa = PBXNativeTarget;" `nl`
        			"buildConfigurationList = 1DEB921E08733DC00010E9CD /* Build configuration list for PBXNativeTarget \"KitDeps\" */;" `nl`
        			"buildPhases = (" `nl`
        				"D2AAC07A0554694100DB518D /* Headers */," `nl`
        				"D2AAC07B0554694100DB518D /* Sources */," `nl`
        				"D2AAC07C0554694100DB518D /* Frameworks */," `nl`
        			");" `nl`
              "buildRules = (" `nl`
        			");" `nl`
        			"dependencies = (" `nl`
        			");" `nl`
        			"name = KitDeps;" `nl`
        			"productName = KitDeps;" `nl`
        			"productReference = D2AAC07E0554694100DB518D /* libKitDeps.a */;" `nl`
        			"productType = \"com.apple.product-type.library.static\";" `nl`
        		"};" `nl`
        "/* End PBXNativeTarget section */" `nl`
        "" `nl`
        "/* Begin PBXProject section */" `nl`
        		"0867D690FE84028FC02AAC07 /* Project object */ = {" `nl`
        			"isa = PBXProject;" `nl`
        			"buildConfigurationList = 1DEB922208733DC00010E9CD /* Build configuration list for PBXProject \"KitDepsCustom\" */;" `nl`
        			"compatibilityVersion = \"Xcode 3.1\";" `nl`
        			"hasScannedForEncodings = 1;" `nl`
        			"mainGroup = 0867D691FE84028FC02AAC07 /* KitDeps */;" `nl`
        			"productRefGroup = 034768DFFF38A50411DB9C8B /* Products */;" `nl`
        			"projectDirPath = \"\";" `nl`
        			"projectRoot = \"\";" `nl`
        			"targets = (" `nl`
        				"D2AAC07D0554694100DB518D /* KitDeps */," `nl`
        			");" `nl`
        		"};" `nl`
        "/* End PBXProject section */"
  
  bottom = "/* Begin XCBuildConfiguration section */" `nl`
        		"1DEB921F08733DC00010E9CD /* Debug */ = {" `nl`
        			"isa = XCBuildConfiguration;" `nl`
        			"buildSettings = {" `nl`
        				"ALWAYS_SEARCH_USER_PATHS = NO;" `nl`
        				"ARCHS = \"$(ARCHS_STANDARD_32_BIT)\";" `nl`
        				"COPY_PHASE_STRIP = NO;" `nl`
        				"DSTROOT = /tmp/KitDeps.dst;" `nl`
        				"GCC_DYNAMIC_NO_PIC = NO;" `nl`
        				"GCC_ENABLE_FIX_AND_CONTINUE = YES;" `nl`
        				"GCC_MODEL_TUNING = G5;" `nl`
        				"GCC_OPTIMIZATION_LEVEL = 0;" `nl`
        				"GCC_PRECOMPILE_PREFIX_HEADER = YES;" `nl`
        				"GCC_PREFIX_HEADER = KitDeps_Prefix.pch;" `nl`
        				"INSTALL_PATH = /usr/local/lib;" `nl`
        				"PRODUCT_NAME = KitDeps;" `nl`
        			"};" `nl`
        			"name = Debug;" `nl`
        		"};" `nl`
        		"1DEB922008733DC00010E9CD /* Release */ = {" `nl`
        			"isa = XCBuildConfiguration;" `nl`
        			"buildSettings = {" `nl`
        				"ALWAYS_SEARCH_USER_PATHS = NO;" `nl`
        				"ARCHS = \"$(ARCHS_STANDARD_32_BIT)\";" `nl`
        				"DSTROOT = /tmp/KitDeps.dst;" `nl`
        				"GCC_MODEL_TUNING = G5;" `nl`
        				"GCC_PRECOMPILE_PREFIX_HEADER = YES;" `nl`
        				"GCC_PREFIX_HEADER = KitDeps_Prefix.pch;" `nl`
        				"INSTALL_PATH = /usr/local/lib;" `nl`
        				"PRODUCT_NAME = KitDeps;" `nl`
        			"};" `nl`
        			"name = Release;" `nl`
        		"};" `nl`
        		"1DEB922308733DC00010E9CD /* Debug */ = {" `nl`
        			"isa = XCBuildConfiguration;" `nl`
        			"baseConfigurationReference = 4728C52F117C02B10027D7D1 /* Kit.xcconfig */;" `nl`
        			"buildSettings = {" `nl`
        				"ARCHS = \"$(ARCHS_STANDARD_32_BIT)\";" `nl`
        				"GCC_C_LANGUAGE_STANDARD = c99;" `nl`
        				"GCC_OPTIMIZATION_LEVEL = 0;" `nl`
        				"GCC_WARN_ABOUT_RETURN_TYPE = YES;" `nl`
        				"GCC_WARN_UNUSED_VARIABLE = YES;" `nl`
        				"OTHER_LDFLAGS = \"-ObjC\";" `nl`
        				"PREBINDING = NO;" `nl`
        				"SDKROOT = iphoneos4.0;" `nl`
        			"};" `nl`
        			"name = Debug;" `nl`
        		"};" `nl`
        		"1DEB922408733DC00010E9CD /* Release */ = {" `nl`
        			"isa = XCBuildConfiguration;" `nl`
        			"baseConfigurationReference = 4728C52F117C02B10027D7D1 /* Kit.xcconfig */;" `nl`
        			"buildSettings = {" `nl`
        				"ARCHS = \"$(ARCHS_STANDARD_32_BIT)\";" `nl`
        				"GCC_C_LANGUAGE_STANDARD = c99;" `nl`
        				"GCC_WARN_ABOUT_RETURN_TYPE = YES;" `nl`
        				"GCC_WARN_UNUSED_VARIABLE = YES;" `nl`
        				"OTHER_LDFLAGS = \"-ObjC\";" `nl`
        				"PREBINDING = NO;" `nl`
        				"SDKROOT = iphoneos4.0;" `nl`
        			"};" `nl`
        			"name = Release;" `nl`
        		"};" `nl`
        "/* End XCBuildConfiguration section */" `nl`
        "" `nl`
        "/* Begin XCConfigurationList section */" `nl`
        		"1DEB921E08733DC00010E9CD /* Build configuration list for PBXNativeTarget \"KitDeps\" */ = {" `nl`
        			"isa = XCConfigurationList;" `nl`
        			"buildConfigurations = (" `nl`
        				"1DEB921F08733DC00010E9CD /* Debug */," `nl`
        				"1DEB922008733DC00010E9CD /* Release */," `nl`
        			");" `nl`
        			"defaultConfigurationIsVisible = 0;" `nl`
        			"defaultConfigurationName = Release;" `nl`
        		"};" `nl`
        		"1DEB922208733DC00010E9CD /* Build configuration list for PBXProject \"KitDepsCustom\" */ = {" `nl`
        			"isa = XCConfigurationList;" `nl`
        			"buildConfigurations = (" `nl`
        				"1DEB922308733DC00010E9CD /* Debug */," `nl`
        				"1DEB922408733DC00010E9CD /* Release */," `nl`
        			");" `nl`
        			"defaultConfigurationIsVisible = 0;" `nl`
        			"defaultConfigurationName = Release;" `nl`
        		"};" `nl`
        "/* End XCConfigurationList section */" `nl`
        	"};" `nl`
        	"rootObject = 0867D690FE84028FC02AAC07 /* Project object */;" `nl`
        "}"
  
  
  main = do
    putStrLn $ top `nl` next1 `nl` bottom
    