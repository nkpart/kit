module Kit.Kit where
  import Kit.Util
    
  type Version = String
  
  data Kit = Kit {
    kitName :: String,
    kitVersion :: Version
  } deriving (Eq, Show, Ord, Read)

  kitFileName :: Kit -> String
  kitFileName k = kitName k ++ "-" ++ kitVersion k

  kitConfigFile :: Kit -> String
  kitConfigFile kit = kitFileName kit </> (kitName kit ++ ".xcconfig")
