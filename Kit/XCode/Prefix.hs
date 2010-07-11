module Kit.XCode.Prefix where
  
  import Kit.Util
  import qualified Data.Traversable as T
  
  kitPrefixFile = "Prefix.pch"
  
  generatePrefixHeader :: [FilePath] -> IO String
  generatePrefixHeader kitFileNames = do
      ca <- readMany kitFileNames kitPrefixFile $ \s -> do
        exists <- doesFileExist s
        T.for (justTrue exists s) readFile
      return $ stringJoin "\n" ca