module Kit.Kit where
      
  data Kit = Kit {
    kitName :: String,
    kitVersion :: String
  } deriving (Eq, Show, Ord, Read)

  kitFileName :: Kit -> String
  kitFileName k = kitName k ++ "-" ++ kitVersion k