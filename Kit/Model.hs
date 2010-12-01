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
    specConfigFile :: FilePath,
    specKitDepsXcodeFlags :: Maybe String
  } deriving (Show, Read)

  data Kit = Kit {
    kitName :: String,
    kitVersion :: String
  } deriving (Eq, Show, Ord, Read)

  class Packageable a where
    packageName :: a -> String
    packageVersion :: a -> String

    packageFileName :: a -> String
    packageFileName a = packageName a ++ "-" ++ packageVersion a

  instance Packageable Kit where
    packageName = kitName
    packageVersion = kitVersion

  instance Packageable KitSpec where
    packageName = kitName . specKit
    packageVersion = kitVersion . specKit


  defaultSpec :: String -> String -> KitSpec
  defaultSpec name version = KitSpec (Kit name version) [] "src" "test" "lib" "Prefix.pch" "Config.xcconfig" Nothing
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
                      <*> (Just <$> f "kitdeps-xcode-flags" <|> pure Nothing)
        where f x = f' obj x

  f' obj x = mLookup x (fromJSObject obj) >>= readJSON

  mLookup :: Monad m => String -> [(String, b)] -> m b
  mLookup a as = maybe (fail $ "No such element: " ++ a) return (lookup a as)

