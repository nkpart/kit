module Kit.Spec (
  -- | The Core Kit types
  KitSpec(..),
  Kit(..),
  updateVersion,
  -- | Duck typing the name/version of a Kit/Spec
  Packageable(..),
  packageFileName,
  -- | Utils
  defaultSpec,
  -- | Serialisation
  decodeSpec,
  encodeSpec,
  writeSpec
  ) where

  import Control.Applicative
  import Control.Monad.Trans
 
  import Data.Object
  import Kit.Util.IsObject
  import qualified Data.ByteString as BS 
  import qualified Data.Object.Yaml as Y

  data KitSpec = KitSpec {
    specKit :: Kit,
    specDependencies :: [Kit],
    specSourceDirectory :: FilePath,
    specTestDirectory :: FilePath,
    specLibDirectory :: FilePath,
    specResourcesDirectory :: FilePath,
    specPrefixFile :: FilePath,
    specConfigFile :: FilePath,
    specKitDepsXcodeFlags :: Maybe String
  } deriving (Show, Read)

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
  defaultSpec name version = KitSpec (Kit name version) [] "src" "test" "lib" "resources" "Prefix.pch" "Config.xcconfig" Nothing
  -- TODO make this and the json reading use the same defaults
  -- I suspect that to do this I'll need update functions for each of
  -- fields in the KitSpec record.
  -- Look at the 'lenses' package on hackage.

  decodeSpec :: BS.ByteString -> Maybe KitSpec
  decodeSpec s = Y.decode s >>= readObject 

  encodeSpec :: KitSpec -> BS.ByteString
  encodeSpec = Y.encode . showObject

  writeSpec :: MonadIO m => FilePath -> KitSpec -> m ()
  writeSpec fp spec = liftIO $ BS.writeFile fp $ encodeSpec spec

  instance IsObject Kit where
    showObject kit = Mapping [("name", w kitName), ("version", w kitVersion)] where w f = showObject . f $ kit
    readObject x = fromMapping x >>= \obj -> Kit <$> obj #> "name" <*> obj #> "version" 

  instance IsObject KitSpec where
    showObject spec = Mapping [
         "name" ~> (val $ kitName . specKit),
         "version" ~> (val $ kitVersion . specKit),
         "dependencies" ~> showObject (specDependencies spec),
         "source-directory" ~> val specSourceDirectory 
      ] where a ~> b = (a, b)
              val f = Scalar . f $ spec

    readObject x = fromMapping x >>= parser
        where or' a b = a <|> pure b
              parser obj =  KitSpec <$> readObject x
                                    <*> (obj #> "dependencies" `or'` [])
                                    <*> (obj #> "source-directory" `or'` "src")
                                    <*> (obj #> "test-directory" `or'` "test")
                                    <*> (obj #> "lib-directory" `or'` "lib")
                                    <*> (obj #> "resources-directory" `or'` "resources")
                                    <*> (obj #> "prefix-header" `or'` "Prefix.pch")
                                    <*> (obj #> "xcconfig" `or'` "Config.xcconfig")
                                    <*> (Just <$> obj #> "kitdeps-xcode-flags") `or'` Nothing

