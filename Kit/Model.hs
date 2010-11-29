module Kit.Model where

  import Text.JSON
  import Control.Applicative
  import System.FilePath.Posix

  data KitSpec = KitSpec {
    specKit :: Kit,
    specDependencies :: [Kit],
    specSourceDirectory :: FilePath,
    specTestDirectory :: FilePath,
    specLibDirectory :: FilePath,
    specPrefixFile :: FilePath,
    specConfigFile :: FilePath
  } deriving (Show, Read)

  type Version = String

  data Kit = Kit {
    kitName :: String,
    kitVersion :: Version
  } deriving (Eq, Show, Ord, Read)

  kitFileName :: Kit -> String
  kitFileName k = kitName k ++ "-" ++ kitVersion k

  defaultSpecForKit :: Kit -> KitSpec
  defaultSpecForKit kit = KitSpec kit [] "src" "test" "lib" "Prefix.pch" "Config.xcconfig"
  -- TODO make this and the json reading use the same defaults
  -- I suspect that to do this I'll need update functions for each of
  -- fields in the KitSpec record.
  -- Look at the 'lenses' package on haskell.

  instance JSON Kit where
      showJSON kit = makeObj [ ("name", w kitName) , ("version", w kitVersion) ] where w f = showJSON . f $ kit

      readJSON (JSObject obj) = Kit <$> f "name" <*> f "version"
        where f x = f' obj x 

  instance JSON KitSpec where
      showJSON spec = makeObj [
            ("name", showJSON $ kitName kit)
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
                      <*> (f "prefix-header" <|> pure "Prefix.pch")
                      <*> (f "xcconfig" <|> pure "Config.xcconfig")
        where f x = f' obj x

  f' obj x = mLookup x (fromJSObject obj) >>= readJSON

  mLookup :: Monad m => String -> [(String, b)] -> m b
  mLookup a as = maybe (fail $ "No such element: " ++ a) return (lookup a as)

