{-# LANGUAGE TypeSynonymInstances #-}

module Kit.Util(
  module Kit.Util,
  module Control.Applicative,
  module System.FilePath.Posix,
  module System.Directory,
  module Control.Monad
  ) where
  import System.Directory
  import System.FilePath.Posix
  import System.FilePath.Glob

  import Data.Foldable
  import Data.List
  import Data.Maybe
  import Data.Monoid
  import qualified Data.Traversable as T

  import Control.Applicative
  import Control.Monad
  import Control.Monad.Error

  import Data.Map
  import Control.Monad.Writer

  import qualified Control.Monad.State as S

  popS :: S.State [a] a
  popS = do
    (x:t) <- S.get
    S.put t
    return x
  
  puts :: MonadIO m => String -> m ()
  puts a = liftIO $ putStrLn a

  maybeRead :: Read a => String -> Maybe a
  maybeRead = fmap fst . listToMaybe . reads

  justTrue :: Bool -> a -> Maybe a
  justTrue p a = if p then Just a else Nothing

  maybeToRight :: b -> Maybe a -> Either b a
  maybeToRight v = maybe (Left v) Right

  maybeToLeft :: b -> Maybe a -> Either a b
  maybeToLeft v = maybe (Right v) Left

  instance Foldable (Either c) where
    foldr f z = either (const z) (flip f z) 

  instance T.Traversable (Either c) where
    sequenceA = either (pure . Left) (Right <$>)

  type KitError = String

  type KitIO a = ErrorT KitError IO a

  maybeToKitIO :: String -> Maybe a -> KitIO a
  maybeToKitIO msg = maybe (throwError msg) return

  mkdir_p :: MonadIO m => FilePath -> m ()
  mkdir_p = liftIO . createDirectoryIfMissing True

  cleanOrCreate :: FilePath -> IO ()
  cleanOrCreate directory = do
    exists <- doesDirectoryExist directory
    when exists $ removeDirectoryRecursive directory
    mkdir_p directory

  inDirectory :: MonadIO m => FilePath -> m a -> m a
  inDirectory dir actions = do
    cwd <- liftIO getCurrentDirectory
    liftIO $ setCurrentDirectory dir
    v <- actions
    liftIO $ setCurrentDirectory cwd
    return v

  glob :: String -> IO [String]
  glob pattern = globDir1 (compile pattern) ""

  stringJoin :: Monoid a => a -> [a] -> a
  stringJoin x = mconcat . intersperse x
  
