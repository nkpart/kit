module Kit.Util(
  module Kit.Util,
  module Exported,
  Color(..),
  (</>), takeFileName, takeDirectory,
  tryJust
  ) where
  
  import Control.Applicative as Exported
  import Control.Monad.Trans as Exported
  import Control.Monad as Exported
  import System.Directory as Exported

  import System.FilePath.Posix ((</>), takeFileName, takeDirectory)
  import System.FilePath.Glob (globDir1, compile)

  import System.Environment (getEnv)

  import Control.Error

  import Data.List
  import Data.Monoid
  import Data.Traversable as T

  import System.Cmd

  import Kit.AbsolutePath

  import System.Console.ANSI

  import Control.Monad.State as S

  popS :: S.State [a] a
  popS = do
    (x:t) <- S.get
    S.put t
    return x

  shell :: String -> IO ()
  shell = void . system

  when' :: Monad m => m Bool -> m () -> m ()
  when' a b = a >>= flip when b

  puts :: MonadIO m => String -> m ()
  puts = liftIO . putStrLn

  ifTrue :: MonadPlus m => Bool -> a -> m a
  ifTrue p a = if p then return a else mzero

  mkdirP :: MonadIO m => FilePath -> m ()
  mkdirP = liftIO . createDirectoryIfMissing True

  cleanOrCreate :: MonadIO m => FilePath -> m ()
  cleanOrCreate directory = liftIO $ do
    exists <- doesDirectoryExist directory
    when exists $ removeDirectoryRecursive directory
    mkdirP directory
    
  inDirectory :: MonadIO m => FilePath -> m a -> m a
  inDirectory fp actions = do
    cwd <- liftIO getCurrentDirectory
    liftIO $ setCurrentDirectory fp
    v <- actions
    liftIO $ setCurrentDirectory cwd
    return v

  inDirectoryM :: (MonadIO m) => m FilePath -> m a -> m a
  inDirectoryM m a = do
    v <- m
    inDirectory v a

  findFiles :: MonadIO m => FilePath -> FilePath -> String -> m [AbsolutePath]
  findFiles kitDir dir tpe = liftIO $ inDirectory kitDir $ do
                                files <- glob (dir </> "**/*" ++ tpe)
                                -- The glob above wasn't finding folders with the 'tpe' extension
                                -- in the top level directory. This one does:
                                files' <- glob (dir </> "*" ++ tpe)
                                T.mapM absolutePath (nub $ files ++ files')

  glob :: String -> IO [String]
  glob pattern = globDir1 (compile pattern) ""

  stringJoin :: Monoid a => a -> [a] -> a
  stringJoin x = mconcat . intersperse x
  
  -- | Lifting bind into a monad. Often denoted /concatMapM/. From TM
  (.=<<.) ::
    (Monad q, Monad m, Traversable m) =>
    (a -> q (m b))
    -> m a
    -> q (m b)
  (.=<<.) f =
    liftM join . T.mapM f

  getEnv' :: String -> IO (Maybe String)
  getEnv' = fmap hush . runEitherT . tryIO . getEnv

  isSet :: String -> IO Bool
  isSet = fmap isJust . getEnv'
    
  say :: MonadIO m => Color -> String -> m ()
  say color msg = do
    colorize <- liftIO $ isSet "PS1"
    if colorize
      then do
        liftIO $ setSGR [SetColor Foreground Vivid color]
        puts msg
        liftIO $ setSGR []
      else
        puts msg

  sayError :: MonadIO m => String -> m ()
  sayError = say Red

  sayWarn :: MonadIO m => String -> m ()
  sayWarn = say Yellow

  readFile' :: (MonadPlus t, T.Traversable t) => FilePath -> IO (t String)
  readFile' fp = do
      exists <- doesFileExist fp
      T.sequence (fmap readFile $ ifTrue exists fp)

