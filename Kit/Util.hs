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
  import Control.Monad
  import Data.Monoid
  import Control.Monad.Trans
      
  (|>) ma f = fmap f ma
        
  maybeRead :: Read a => String -> Maybe a
  maybeRead = fmap fst . listToMaybe . reads
  
  justTrue :: Bool -> a -> Maybe a
  justTrue True a = Just a
  justTrue False _ = Nothing
  
  maybeToRight :: b -> Maybe a -> Either b a
  maybeToRight _ (Just a) = Right a
  maybeToRight b Nothing = Left b
  
  maybeToLeft :: b -> Maybe a -> Either a b
  maybeToLeft _ (Just a) = Left a
  maybeToLeft b Nothing = Right b
  
  instance Foldable (Either c) where
    foldr f z (Left c) = z
    foldr f z (Right a) = f a z
    
  instance T.Traversable (Either c) where
    sequenceA = either (pure . Left) (Right <$>)
    
  type KitError = String
  
  data KitIO a = KitIO { unKitIO :: (IO (Either [KitError] a)) }
  
  kitError :: KitError -> KitIO a
  kitError e = KitIO . return $ Left [e]
  
  maybeToKitIO msg = maybe (kitError msg) return
  
  instance Monad KitIO where
    (KitIO ioE) >>= f = KitIO $ ioE >>= g
      where g l@(Left v) = return (Left v)
            g (Right v) = unKitIO $ f v
            
    return = KitIO . return . Right 
    
  instance Functor KitIO where
    fmap f kio = kio >>= (return . f)
    
  instance MonadIO KitIO where
    liftIO v = KitIO (fmap Right v)
    
  instance Applicative KitIO where
    pure = return
    mf <*> ma = do
      f <- mf
      a <- ma
      return $ f a
  
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
  glob pattern = fmap lines (readProcess "ruby" ["-e", "puts Dir.glob(\"" ++ pattern ++ "\")"] [])
    
  stringJoin :: Monoid a => a -> [a] -> a
  stringJoin x = mconcat . intersperse x
  
  -- For each directory that exists, execute the function with the given filepath
  readMany :: MonadIO m => [FilePath] -> FilePath -> (String -> m (Maybe a)) -> m [a]
  readMany dirs fileInDir f = do
    directories <- liftIO $ filterM doesDirectoryExist dirs
    let kitSpecFiles = map (</> fileInDir) directories
    kitContents <- mapM f kitSpecFiles
    return $ catMaybes kitContents