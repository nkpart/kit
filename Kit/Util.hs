{-# LANGUAGE TypeSynonymInstances #-}

module Kit.Util where
  import System.Directory
  import Data.Maybe
  import Data.List
  import Data.Traversable
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
    
  instance Traversable (Either c) where
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

  mLookup a as = maybe (fail $ "No such element: " ++ a) return (lookup a as)  
  
  cleanOrCreate :: FilePath -> IO ()
  cleanOrCreate directory = do
    exists <- doesDirectoryExist directory
    when exists $ removeDirectoryRecursive directory
    createDirectoryIfMissing True directory
    
  inDirectory :: FilePath -> IO a -> IO a
  inDirectory dir actions = do
    cwd <- getCurrentDirectory
    setCurrentDirectory dir
    v <- actions
    setCurrentDirectory cwd
    return v
    
  stringJoin :: String -> [String] -> String
  stringJoin x = mconcat . intersperse x