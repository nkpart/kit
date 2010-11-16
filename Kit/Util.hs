{-# LANGUAGE TypeSynonymInstances #-}

module Kit.Util(
  module Kit.Util,
  module Control.Applicative,
  module System.FilePath.Posix,
  module System.Directory
  ) where
  import System.FilePath.Posix
  import System.Process
  import System.Directory
  import Data.Maybe
  import Data.List
  import qualified Data.Traversable as T
  import Data.Foldable
  import Control.Applicative

  import Data.Monoid
  import Control.Monad.Error

  import System.FilePath.Glob

  maybeRead :: Read a => String -> Maybe a
  maybeRead = fmap fst . listToMaybe . reads

  justTrue :: Bool -> a -> Maybe a
  justTrue p a = if p then Just a else Nothing

  maybeToRight :: b -> Maybe a -> Either b a
  maybeToRight v = maybe (Left v) Right

  maybeToLeft :: b -> Maybe a -> Either a b
  maybeToLeft v = maybe (Right v) Left

  instance Foldable (Either c) where
    foldr f z = either (const z) (\v -> f v z)

  instance T.Traversable (Either c) where
    sequenceA = either (pure . Left) (Right <$>)

  type KitError = String

  type KitIO a = ErrorT KitError IO a

  maybeToKitIO msg = maybe (throwError msg) return

  mkdir_p :: FilePath -> IO ()
  mkdir_p = createDirectoryIfMissing True

  cleanOrCreate :: FilePath -> IO ()
  cleanOrCreate directory = do
    exists <- doesDirectoryExist directory
    when exists $ removeDirectoryRecursive directory
    mkdir_p directory

  inDirectory :: FilePath -> IO a -> IO a
  inDirectory dir actions = do
    cwd <- getCurrentDirectory
    setCurrentDirectory dir
    v <- actions
    setCurrentDirectory cwd
    return v

  glob :: String -> IO [String]
  glob pattern = globDir1 (compile pattern) ""

  stringJoin :: Monoid a => a -> [a] -> a
  stringJoin x = mconcat . intersperse x

