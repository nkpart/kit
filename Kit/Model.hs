module Kit.Model where

  import Control.Applicative
 
  import qualified Data.ByteString as BS 
  import Data.Object
  import qualified Data.Object.Yaml as Y

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

  decodeSpec :: BS.ByteString -> Maybe KitSpec
  decodeSpec s = Y.decode s >>= readObject 

  class IsObject x where
    showObject :: x -> StringObject
    readObject :: StringObject -> Maybe x

  instance IsObject Kit where
    showObject kit = Mapping [("name", w kitName), ("version", w kitVersion)] where w f = Scalar . f $ kit
    readObject x = fromMapping x >>= \obj -> Kit <$> lookupScalar "name" obj <*> lookupScalar "version" obj

  instance IsObject KitSpec where
    showObject spec = Mapping [
         "name" ~> (val $ kitName . specKit),
         "version" ~> (val $ kitVersion . specKit),
         "dependencies" ~> seq specDependencies spec
      ] where a ~> b = (a,b)
              val f = Scalar. f $ spec
              seq f = Sequence . (map showObject) . f

    readObject x = fromMapping x >>= parser
        where or a b = a <|> pure b
              parser obj = let f x = lookupScalar x obj in KitSpec <$> readObject x
                                    <*> ((lookupSequence "dependencies" obj >>= mapM readObject) `or` []) 
                                    <*> (f "source-directory" `or` "src")
                                    <*> (f "test-directory" `or` "test")
                                    <*> (f "lib-directory" `or` "lib")
                                    <*> (f "prefix-header" `or` "Prefix.pch")
                                    <*> (f "xcconfig" `or` "Config.xcconfig")
                                    <*> (Just <$> f "kitdeps-xcode-flags") `or` Nothing

