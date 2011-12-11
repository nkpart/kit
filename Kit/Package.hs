{-# LANGUAGE PackageImports #-}
module Kit.Package (package) where
  import Kit.Spec
  import Kit.Util
  import Data.List

  fileBelongsInPackage :: KitSpec -> FilePath -> Bool
  fileBelongsInPackage config fp = isCore || isProject where
    isCore = elem fp [specResourcesDirectory config, specSourceDirectory config, specTestDirectory config, specLibDirectory config, "KitSpec"]
    isProject = any (`isSuffixOf` fp) ["xcodeproj", "xcconfig", ".pch"] -- TODO this could just check for the defined config and prefi header

  package :: KitSpec -> IO ()
  package spec = do
      contents <- filter (fileBelongsInPackage spec) <$> getDirectoryContents "."
      mkdirP distDir
      sh $ "tar -czf " ++ (distDir </> (kitPath ++ ".tar.gz")) ++ " -s ,^," ++ kitPath ++ "/, " ++ join (intersperse " " contents)
      return ()
    where
      distDir = "dist"
      kitPath = packageFileName spec
      sh c = liftIO $ putStrLn c >> shell c

