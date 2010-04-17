module Kit.Spec where
  import Kit.Kit
  
  data KitSpec = KitSpec {
    specKit :: Kit,
    specDependencies :: [Kit] 
  } deriving (Show, Read)
  