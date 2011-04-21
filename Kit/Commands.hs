{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Kit.Commands (
  Command,
  liftKit,
  mySpec,
  mySpecFile,
  myRepository,
  runCommand,
  defaultLocalRepoPath,
  defaultLocalRepository
) where

import Kit.Util
import Kit.Spec
import Kit.Repository
import Kit.WorkingCopy

import System.Exit (exitFailure)

import Control.Monad.Error
import Control.Monad.Reader

newtype Command a = Command (ReaderT (WorkingCopy, KitRepository) (ErrorT String IO) a) deriving Monad

instance MonadIO Command where
  liftIO a = Command $ liftIO a

liftKit :: KitIO a -> Command a
liftKit = Command . ReaderT . const

mySpec :: Command KitSpec
mySpec = fmap workingKitSpec myWorkingCopy

myWorkingCopy :: Command WorkingCopy
myWorkingCopy = Command $ fmap fst ask

myRepository :: Command KitRepository
myRepository = Command $ fmap snd ask

mySpecFile :: Command FilePath
mySpecFile = return "KitSpec"

runCommand :: Command a -> IO ()
runCommand (Command cmd) = run $ do
  spec <- currentWorkingCopy
  repository <- liftIO defaultLocalRepository
  runReaderT cmd (spec, repository)
  where run = (handleFails =<<) . runErrorT
        handleFails = either (\msg -> putStrLn ("kit error: " ++ msg) >> exitFailure) (const $ return ())

defaultLocalRepoPath :: IO FilePath
defaultLocalRepoPath = (</> ".kit" </> "repository") <$> getHomeDirectory 

defaultLocalRepository :: IO KitRepository
defaultLocalRepository = KitRepository <$> defaultLocalRepoPath

