{-# OPTIONS_GHC -XTypeSynonymInstances #-}

module Kit.Util where
  import Data.Maybe
  import Data.List
  import Data.Traversable
  import Data.Foldable
  import Control.Applicative
  import Control.Monad
    
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
  
  type KitIO = IO (Either [KitError] a)
  
  instance Monad (KitIO) where
    ioE >>= f = do
      e <- ioE
      g e
      where g l@(Left _) = return l
            g (Right v) = f v
            
    return v = return . Right $ v
