{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Kit.Commands (
  Command,
  liftKit,
  mySpec,
  myWorkingCopy,
  myRepository,
  runCommand,
  defaultLocalRepository
) where

import Kit.Util
import Kit.Spec
import Kit.Repository
import Kit.WorkingCopy

import System.Exit (exitFailure)

import Control.Monad.Error
import Control.Monad.Reader

newtype Command a = Command { unCommand :: (ReaderT (WorkingCopy, KitRepository) (ErrorT String IO) a) } deriving (Monad, MonadIO, Functor, Applicative)

liftKit :: KitIO a -> Command a
liftKit = Command . ReaderT . const

mySpec :: Command KitSpec
mySpec = Command $ fmap workingKitSpec (unCommand myWorkingCopy)

myWorkingCopy :: Command WorkingCopy
myWorkingCopy = Command $ fmap fst ask

myRepository :: Command KitRepository
myRepository = Command $ fmap snd ask

runCommand :: Command a -> IO ()
runCommand (Command cmd) = run $ do
  spec <- currentWorkingCopy
  repository <- liftIO defaultLocalRepository
  runReaderT cmd (spec, repository)
  where run = (handleFails =<<) . runErrorT
        handleFails = either (\msg -> putStrLn ("kit error: " ++ msg) >> exitFailure) (const $ return ())

defaultLocalRepository :: IO KitRepository
defaultLocalRepository = makeRepository =<< (</> ".kit" ) <$> getHomeDirectory

