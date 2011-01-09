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

  import Data.List
  import Data.Maybe
  import Data.Monoid

  import Control.Applicative
  import Control.Monad
  import Control.Monad.Error

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

  ifTrue :: MonadPlus m => Bool -> a -> m a
  ifTrue p a = if p then return a else mzero

  maybeToRight :: b -> Maybe a -> Either b a
  maybeToRight v = maybe (Left v) Right

  maybeToLeft :: b -> Maybe a -> Either a b
  maybeToLeft v = maybe (Right v) Left

  type KitIO a = ErrorT String IO a
  
  maybeToKitIO :: String -> Maybe a -> KitIO a
  maybeToKitIO msg = maybe (throwError msg) return

  mkdirP :: MonadIO m => FilePath -> m ()
  mkdirP = liftIO . createDirectoryIfMissing True

  cleanOrCreate :: MonadIO m => FilePath -> m ()
  cleanOrCreate directory = liftIO $ do
    exists <- doesDirectoryExist directory
    when exists $ removeDirectoryRecursive directory
    mkdirP directory

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
  
