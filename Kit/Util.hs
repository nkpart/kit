{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, PackageImports #-}

module Kit.Util(
  module Kit.Util,
  module Control.Applicative,
  module Control.Monad,
  module System.Directory,
  module System.FilePath.Posix,
  module Debug.Trace,
  Color(..),
  (>>>)
  ) where
  import Debug.Trace
  import System.Directory
  import System.FilePath.Posix
  import System.FilePath.Glob

  import Data.List
  import Data.Maybe
  import Data.Monoid
  import Data.Traversable as T

  import Control.Arrow
  import Control.Applicative
  import Control.Monad
  import "mtl" Control.Monad.Error
  import System.Cmd

  import System.Console.ANSI

  import qualified "mtl" Control.Monad.State as S

  popS :: S.State [a] a
  popS = do
    (x:t) <- S.get
    S.put t
    return x

  shell :: String -> IO ()
  shell c = do
    _ <- system c
    return ()

  when' :: Monad m => m Bool -> m () -> m ()
  when' a b = a >>= flip when b

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

  -- Typeclass so that inDirectory can act on effectful values
  class FilePathM a where filePathM :: MonadIO m => a -> m FilePath
  instance FilePathM FilePath where filePathM = return 
  instance FilePathM (IO FilePath) where filePathM = liftIO . id
    
  inDirectory :: (FilePathM p, MonadIO m) => p -> m a -> m a
  inDirectory fp actions = do
    cwd <- liftIO getCurrentDirectory
    dir <- filePathM fp
    liftIO $ setCurrentDirectory dir
    v <- actions
    liftIO $ setCurrentDirectory cwd
    return v

  glob :: String -> IO [String]
  glob pattern = globDir1 (compile pattern) ""

  stringJoin :: Monoid a => a -> [a] -> a
  stringJoin x = mconcat . intersperse x
  
  -- | Lifting bind into a monad. Often denoted /concatMapM/.
  (.=<<.) ::
    (Monad q, Monad m, Traversable m) =>
    (a -> q (m b))
    -> m a
    -> q (m b)
  (.=<<.) f =
    liftM join . T.mapM f
    
  
  say :: MonadIO m => Color -> String -> m ()
  say color msg = do
    liftIO $ setSGR [SetColor Foreground Vivid color]
    puts msg
    liftIO $ setSGR []

  alert :: MonadIO m => String -> m ()
  alert = say Red

