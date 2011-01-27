module Tests where

  import Data.Char
  import Data.List
  import Test.QuickCheck
  import Test.HUnit
  import Text.Printf

  import Control.Applicative 
  import System.FilePath.Posix

  import Kit.Util.FSAction as FSA

  -- reversing twice a finite list, is the same as identity
  prop_reversereverse s = (reverse . reverse) s == s where _ = s :: [Int]
  
  -- Dropping the "Haq! " string is the same as identity
  prop_haq s = drop (length "Haq! ") (haqify s) == s where haqify s = "Haq! " ++ s

  props  = [("reverse.reverse/id", quickCheck prop_reversereverse)
          ,("drop.haq/id",        quickCheck prop_haq)]

  spec x f = TestLabel x $ TestCase f 
  
  contents = "loltents"
  fileA = "fileA"
  
  tests = TestList [
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

  main = do
      runProps
      runTests
    where runProps = mapM_ (\(s,a) -> printf "%-25s: " s >> a) props
          runTests = runTestTT tests
