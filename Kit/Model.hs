module Kit.Model where

  import Text.JSON
  import Control.Applicative
  import System.FilePath.Posix

  data KitSpec = KitSpec {
    specKit :: Kit,
    specDependencies :: [Kit],
    specSourceDirectory :: FilePath,
    specTestDirectory :: FilePath,
    specLibDirectory :: FilePath
  } deriving (Show, Read)

  type Version = String

  data Kit = Kit {
    kitName :: String,
    kitVersion :: Version
  } deriving (Eq, Show, Ord, Read)

  kitFileName :: Kit -> String
  kitFileName k = kitName k ++ "-" ++ kitVersion k

  kitConfigFile :: Kit -> String
  kitConfigFile kit = kitFileName kit </> (kitName kit ++ ".xcconfig")

  defaultSpecForKit :: Kit -> KitSpec
  defaultSpecForKit kit = KitSpec kit [] "src" "test" "lib" -- TODO make this and the json reading use the same defaults

  instance JSON Kit where
      showJSON kit = makeObj
          [ ("name", showJSON $ kitName kit)
          , ("version", showJSON $ kitVersion kit)
          ]

      readJSON (JSObject obj) = Kit <$> f "name" <*> f "version"
        where f x = f' obj x 

  instance JSON KitSpec where
      showJSON spec = makeObj
          [ ("name", showJSON $ kitName kit)
          , ("version", showJSON $ kitVersion kit)
          , ("dependencies", showJSON $ specDependencies spec)
          ]
          where kit = specKit spec

      readJSON js@(JSObject obj) =
              KitSpec <$> readJSON js 
                      <*> (f "dependencies" <|> pure []) 
                      <*> (f "source-directory" <|> pure "src")
                      <*> (f "test-directory" <|> pure "test")
                      <*> (f "lib-directory" <|> pure "lib")
        where f x = f' obj x

  f' obj x = mLookup x (fromJSObject obj) >>= readJSON

  mLookup :: Monad m => String -> [(String, b)] -> m b
  mLookup a as = maybe (fail $ "No such element: " ++ a) return (lookup a as)

