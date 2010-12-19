{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Kit.Commands where

import Kit.Util
import Kit.Spec
import Kit.Repository
import Kit.Project

import Control.Monad.Error
import Control.Monad.Reader

newtype Command a = Command (ReaderT (KitSpec, KitRepository) (ErrorT String IO) a) deriving Monad

instance MonadIO Command where
  liftIO a = Command $ liftIO a

liftKit :: KitIO a -> Command a
liftKit = Command . ReaderT . const

mySpec :: Command KitSpec
mySpec = Command $ ask >>= (return . fst) 

myRepository :: Command KitRepository
myRepository = Command $ ask >>= (return . snd)

runCommand :: Command a -> IO ()
runCommand (Command cmd) = run $ do
  spec <- readSpec "KitSpec"
  repository <- liftIO defaultLocalRepository
  runReaderT cmd (spec, repository)

myKitSpec :: KitIO KitSpec
myKitSpec = readSpec "KitSpec"

defaultLocalRepoPath :: IO FilePath
defaultLocalRepoPath = getHomeDirectory >>= \h -> return $ h </> ".kit" </> "repository"

defaultLocalRepository :: IO KitRepository
defaultLocalRepository = fileRepo <$> defaultLocalRepoPath

handleFails :: Either String a -> IO ()
handleFails = either (putStrLn . ("kit error: " ++)) (const $ return ())

run :: KitIO a -> IO ()
run f = runErrorT f >>= handleFails





