module Tests where

  import Data.Char
  import Data.List
  import Test.QuickCheck
  import Test.HUnit
  import Text.Printf

  import Control.Applicative 
  import System.Directory
  import System.FilePath.Posix

  import qualified Kit.Contents as KC
  import Kit.Spec
  import Kit.Util.FSAction as FSA
  import Kit.Util
  import Kit.Util.IsObject

  import qualified Data.Object.Yaml as Y
  import Debug.Trace
  import Data.Maybe (fromJust)
  import qualified Data.ByteString.Char8 as BS

  strArb = listOf $ elements "abcdef"

  instance Arbitrary Kit where
    arbitrary = Kit <$> (listOf1 $ elements "abcdef") <*> strArb

  instance Arbitrary KitSpec where
    arbitrary = KitSpec <$> arbitrary <*> arbitrary <*> strArb <*> strArb <*> strArb <*> strArb <*> strArb <*> strArb <*> (Just <$> strArb)

  prop_showReadIdentity :: KitSpec -> Bool
  prop_showReadIdentity s = s == (fromJust $ readObject $ showObject s)

  props  = [("kitspec.show+read/id", quickCheck prop_showReadIdentity)]

  spec x f = TestLabel x $ TestCase f 
  
-- Todo
-- * depsonly.xcconfig should specify SKIP_INSTALL=YES

-- FSAction tests
  contents = "loltents"
  fileA = "fileA"

  fsActionTests = [
        spec "execute FileCreate" $ do
          runAction $ FileCreate fileA contents 
          assertEqual "file contents" contents =<< readFile fileA
      , spec "execute SymLink" $ do
          writeFile fileA contents
          let linkname = "linkfile"
          runAction $ Symlink fileA linkname
          assertEqual "linked contents" contents =<< readFile linkname
      , spec "change directory of FileCreate" $ do
          let basedir = "tmpdir"
          runAction $ FSA.within basedir $ FileCreate fileA contents
          assertEqual "rebased file contents" contents =<< readFile (basedir </> fileA)
      , spec "change directory of symlink" $ do
          let basedir = "tmpdir"
          let linkname = "linkfile"
          writeFile (basedir </> fileA) contents
          runAction $ FSA.within basedir $ Symlink fileA linkname
          assertEqual "rebased link" contents =<< readFile (basedir </> linkname)
    ]

-- KitContents tests
  kitContentsTest = [
      spec "check for resource contents" $ do
        createDirectoryIfMissing True "some-kit-0.1/resources"
        let spec = defaultSpec "some-kit" "0.1"
        kc <- KC.readKitContents' "." (const "some-kit-0.1") spec
        expectedResourceDir <- canonicalizePath "some-kit-0.1/resources"
        assertEqual "resource dir found" (Just expectedResourceDir) (KC.contentResourceDir kc)
        kc <- KC.readKitContents' "." (const "some-kit-0.1") spec { specResourcesDirectory = "lolburger" }
        assertEqual "resource dir not found" (Nothing) (KC.contentResourceDir kc)
    ]

  main = do
      createDirectoryIfMissing True "test-output"
      setCurrentDirectory "test-output"
      runProps
      runTests
      return ()
    where runProps = mapM_ (\(s,a) -> printf "%-25s: " s >> a) props
          runTests = runTestTT (TestList (fsActionTests ++ kitContentsTest))
