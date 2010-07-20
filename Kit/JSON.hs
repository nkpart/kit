module Kit.JSON where
  import Kit.Kit
  import Kit.Spec
  import Kit.Util
  import Control.Applicative
  import Text.JSON
  
  instance JSON Kit where
      showJSON kit = makeObj
          [ ("name", showJSON $ kitName kit)
          , ("version", showJSON $ kitVersion kit)
          ]

      readJSON (JSObject obj) = Kit <$> f "name" <*> f "version"
        where f x = mLookup x jsonObjAssoc >>= readJSON
              jsonObjAssoc = fromJSObject obj
  
  instance JSON KitSpec where
      showJSON spec = makeObj
          [ ("name", showJSON $ kitName kit)
          , ("version", showJSON $ kitVersion kit)
          , ("dependencies", showJSON $ specDependencies spec)
          ]
          where kit = specKit spec

      readJSON (JSObject obj) = 
          let myKit = Kit <$> f "name" <*> f "version"
              myConfig = (KitConfiguration <$> f "dependencies" <*> (f "sourceDir" <|> pure "src"))
           in KitSpec <$> myKit <*> myConfig
        where f x = mLookup x jsonObjAssoc >>= readJSON
              jsonObjAssoc = fromJSObject obj
  
  mLookup :: Monad m => String -> [(String, b)] -> m b
  mLookup a as = maybe (fail $ "No such element: " ++ a) return (lookup a as)
  