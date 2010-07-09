module Kit.Spec where
  import Kit.Kit
  
  data KitSpec = KitSpec {
    specKit :: Kit,
    specConfiguration :: KitConfiguration
  } deriving (Show, Read)
  
  data KitConfiguration = KitConfiguration {
    kitConfigDependencies :: [Kit],
    sourceDir :: FilePath
  } deriving (Show, Read)
  
  specDependencies :: KitSpec -> [Kit]
  specDependencies = kitConfigDependencies . specConfiguration
  
  defaultConfiguration :: KitConfiguration
  defaultConfiguration = KitConfiguration [] "src"