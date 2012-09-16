{-# LANGUAGE OverloadedStrings #-}
module Kit.Spec (
  -- | The Core Kit types
  KitSpec(..),
  Kit(..),
  updateVersion,
  -- | Duck typing the name/version of a Kit/Spec
  Packageable(..),
  packageFileName,
  -- | Utils
  defaultSpec
  ) where

  import Kit.Util
  import Data.Yaml
  import Data.HashMap.Strict as HM (toList) 
  import Data.Text as T (unpack, pack)
  import Data.Maybe (maybeToList)
  import Data.Attoparsec.Number

  data KitSpec = KitSpec {
    specKit :: Kit,
    specDependencies :: [Kit],
    specSourceDirectory :: FilePath,
    specTestDirectory :: FilePath,
    specLibDirectory :: FilePath,
    specResourcesDirectory :: FilePath,
    specPrefixFile :: FilePath,
    specConfigFile :: FilePath,
    specWithARC :: Bool,
    specKitDepsXcodeFlags :: Maybe String
  } deriving (Show, Read, Eq)

  updateVersion :: KitSpec -> (String -> String) -> KitSpec
  updateVersion spec f = spec { specKit = (specKit spec) { kitVersion = f . kitVersion . specKit $ spec } } 

  data Kit = Kit {
    kitName :: String,
    kitVersion :: String
  } deriving (Eq, Show, Ord, Read)

  class Packageable a where
    packageName :: a -> String
    packageVersion :: a -> String

  packageFileName :: Packageable a => a -> String
  packageFileName a = packageName a ++ "-" ++ packageVersion a

  instance Packageable Kit where
    packageName = kitName
    packageVersion = kitVersion

  instance Packageable KitSpec where
    packageName = kitName . specKit
    packageVersion = kitVersion . specKit

  defaultSpec :: String -> String -> KitSpec
  defaultSpec name version = KitSpec (Kit name version) [] "src" "test" "lib" "resources" "Prefix.pch" "Config.xcconfig" False Nothing 
  -- TODO make this and the json reading use the same defaults
  -- I suspect that to do this I'll need update functions for each of
  -- fields in the KitSpec record.
  -- Look at the 'data-lens' package on hackage. (or comonad-transformers)

  instance ToJSON Kit where
    toJSON kit = object ["name" .= kitName kit, "version" .= kitVersion kit]

  showNum :: Number -> String
  showNum l = show l

  instance FromJSON Kit where
    parseJSON (Object obj) = (Kit <$> obj .: "name" <*> (obj .: "version" <|> (showNum <$> obj .: "version"))) <|> case HM.toList obj of
                                                                                                    [(key, String value)] -> pure $ Kit (T.unpack key) (T.unpack value)
                                                                                                    [(key, Number value)] -> pure $ Kit (T.unpack key) (show value)
                                                                                                    x -> error $ "Not a compatible object" ++ show x
    parseJSON x = error $ "NOT A OBJ" ++ show x
                                                                                                    
  -- TODO don't write out default values
  instance ToJSON KitSpec where
    toJSON spec = object ([
         "name" .= (kitName . specKit) spec,
         "version" .= (kitVersion . specKit) spec,
         "dependencies" .= (map makeDep (specDependencies spec)),
         "source-directory" .= specSourceDirectory spec,
         "test-directory" .= specTestDirectory spec,
         "lib-directory" .= specLibDirectory spec,
         "resources-directory" .= specResourcesDirectory spec,
         "prefix-header" .= specPrefixFile spec,
         "with-arc" .= specWithARC spec,
         "xcconfig" .= specConfigFile spec
      ] ++ maybeToList (fmap (("kitdeps-xcode-flags" .=)) (specKitDepsXcodeFlags spec)))
      where makeDep dep = object [(T.pack $ kitName dep, String . T.pack $ kitVersion dep)]

  instance FromJSON KitSpec where
    parseJSON (Object obj) = KitSpec <$> parseJSON (Object obj)
                                    <*> (obj .:? "dependencies" .!= []) -- TODO this should fail if it can't read the format
                                    <*> (obj .:? "source-directory" .!= "src")
                                    <*> (obj .:? "test-directory" .!= "test")
                                    <*> (obj .:? "lib-directory" .!= "lib")
                                    <*> (obj .:? "resources-directory" .!= "resources")
                                    <*> (obj .:? "prefix-header" .!= "Prefix.pch")
                                    <*> (obj .:? "xcconfig" .!= "Config.xcconfig")
                                    <*> (obj .:? "with-arc" .!= False)
                                    <*> (Just <$> obj .:? "kitdeps-xcode-flags") .!= Nothing
    parseJSON _ = fail "Couldn't parse KitSpec"
