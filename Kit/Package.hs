module Kit.Package (package) where
  import Kit.Spec
  import Kit.Util
  import Data.List

  fileBelongsInPackage :: KitSpec -> FilePath -> Bool
  fileBelongsInPackage config fp = isCore || isProject where
    isCore = fp `elem` [specResourcesDirectory config, specSourceDirectory config, specTestDirectory config, specLibDirectory config, specFrameworksDirectory config, "KitSpec"]
    isProject = any (`isSuffixOf` fp) ["xcodeproj", "xcconfig", ".pch"] -- TODO this could just check for the defined config and prefi header

  package :: KitSpec -> IO ()
  package spec = do
      contents <- filter (fileBelongsInPackage spec) <$> getDirectoryContents "."
      mkdirP distDir
      let escapedContents = map (\x -> "\"" ++ x ++ "\"") contents
      sh $ envDontCopy ++ " tar -czhf " ++ (distDir </> (kitPath ++ ".tar.gz")) ++ " -s ,^," ++ kitPath ++ "/, " ++ join (intersperse " " escapedContents)
      return ()
    where
      distDir = "dist"
      kitPath = packageFileName spec
      envDontCopy = "COPYFILE_DISABLE=true "
      sh c = liftIO $ putStrLn c >> shell c

