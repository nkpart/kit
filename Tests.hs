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

  -- reversing twice a finite list, is the same as identity
  prop_reversereverse s = (reverse . reverse) s == s where _ = s :: [Int]
  
  -- Dropping the "Haq! " string is the same as identity
  prop_haq s = drop (length "Haq! ") (haqify s) == s where haqify s = "Haq! " ++ s

  props  = [("reverse.reverse/id", quickCheck prop_reversereverse)
          ,("drop.haq/id",        quickCheck prop_haq)]

  spec x f = TestLabel x $ TestCase f 
  
-- Todo
-- * depsonly.xcconfig should specifiy SKIP_INSTALL=YES

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
        createDirectoryIfMissing True "Kits/some-kit-0.1/resources"
        let spec = defaultSpec "some-kit" "0.1"
        inDirectory "Kits" $ do
          kc <- KC.readKitContents spec
          assertEqual "resource dir found" (Just "resources") (KC.contentResourceDir kc)
          kc <- KC.readKitContents spec { specResourcesDirectory = "lolburger" }
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
