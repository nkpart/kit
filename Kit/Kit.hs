module Kit.Kit where
  import System.FilePath.Posix
    
  data Kit = Kit {
    kitName :: String,
    kitVersion :: String
  } deriving (Eq, Show, Ord, Read)

  kitFileName :: Kit -> String
  kitFileName k = kitName k ++ "-" ++ kitVersion k

  kitConfig :: Kit -> String
  kitConfig kit = kitFileName kit </> (kitName kit ++ ".xcconfig")
