{-# LANGUAGE PackageImports, ScopedTypeVariables #-}
module Tests where

  import Data.Char
  import Data.List
  import Test.QuickCheck
  import Test.HUnit
  import Text.Printf

  import Control.Applicative 
  import System.Directory
  import System.FilePath.Posix
  import "mtl" Control.Monad.Writer

  import qualified Kit.Contents as KC
  import Kit.Spec
  import Kit.Util.FSAction as FSA
  import Kit.Util
  import Kit.Util.IsObject

  import qualified Data.Object.Yaml as Y
  import Debug.Trace
  import Data.Maybe (fromJust)
  import Data.Either (lefts, rights)
  import qualified Data.ByteString.Char8 as BS

  -- Test/Prop helpers
  spec x f = TestLabel x $ TestCase f 
  prop s = quickCheck . label s

  isId f v = v == f v 

  -- My little DSL for writing props and unit tests
  specify l m = do
    let (_, cases) = runWriter m
    putStrLn $ "> " ++ l
    let ps = lefts cases
    let ts = rights cases
    sequence_ ps
    runTestTT $ TestList ts

  satisfy s p = tell [Left $ prop s p]
  test' l t = tell [Right $ spec l t]

  -- Arbitrary instances for Kit Types
  strArb = listOf $ elements "abcdef"

  instance Arbitrary Kit where
    arbitrary = Kit <$> listOf1 (elements "abcdef") <*> strArb

  instance Arbitrary KitSpec where
    arbitrary = KitSpec <$> arbitrary <*> arbitrary <*> strArb <*> strArb <*> strArb <*> strArb <*> strArb <*> strArb <*> (Just <$> strArb)

  -- THE TESTS
  -- Todo
  -- * depsonly.xcconfig must specify SKIP_INSTALL=YES

  contents = "loltents"
  fileA = "fileA"

  main = do
      createDirectoryIfMissing True "test-output"
      setCurrentDirectory "test-output"

      specify "FSAction" $ do
        test' "execute FileCreate" $ do
          runAction $ FileCreate fileA contents 
          assertEqual "file contents" contents =<< readFile fileA
        test' "execute SymLink" $ do
          writeFile fileA contents
          let linkname = "linkfile"
          runAction $ Symlink fileA linkname
          assertEqual "linked contents" contents =<< readFile linkname
        test' "change directory of FileCreate" $ do
          let basedir = "tmpdir"
          runAction $ FSA.within basedir $ FileCreate fileA contents
          assertEqual "rebased file contents" contents =<< readFile (basedir </> fileA)
        test' "change directory of symlink" $ do
          let basedir = "tmpdir"
          let linkname = "linkfile"
          writeFile (basedir </> fileA) contents
          runAction $ FSA.within basedir $ Symlink fileA linkname
          assertEqual "rebased link" contents =<< readFile (basedir </> linkname)

      specify "KitSpec" $ satisfy "yaml show/read == id" (isId (fromJust . readObject . showObject) :: KitSpec -> Bool)

      specify "KitContents" $ test' "check for resource contents" $ do
        createDirectoryIfMissing True "some-kit-0.1/resources"
        let spec = defaultSpec "some-kit" "0.1"
        kc <- KC.readKitContents "some-kit-0.1" spec
        expectedResourceDir <- canonicalizePath "some-kit-0.1/resources"
        assertEqual "resource dir found" (Just expectedResourceDir) (KC.contentResourceDir kc)
        kc <- KC.readKitContents "some-kit-0.1" spec { specResourcesDirectory = "lolburger" }
        assertEqual "resource dir not found" Nothing (KC.contentResourceDir kc)

      return ()
