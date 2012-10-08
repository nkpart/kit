{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Kit.Commands (
  Command(),
  liftKit,
  mySpec,
  myWorkingCopy,
  myRepository,
  runCommand
) where

import Kit.Util
import Kit.Spec
import Kit.Repository
import Kit.WorkingCopy
import Control.Monad.Reader
import Control.Error

newtype Command a = Command { 
  unCommand :: ReaderT (WorkingCopy, KitRepository) Script a 
} deriving (Monad, MonadIO, Functor, Applicative)

liftKit :: Script a -> Command a
liftKit = Command . ReaderT . const

mySpec :: Command KitSpec
mySpec = Command $ fmap workingKitSpec (unCommand myWorkingCopy)

myWorkingCopy :: Command WorkingCopy
myWorkingCopy = Command $ fmap fst ask

myRepository :: Command KitRepository
myRepository = Command $ fmap snd ask

runCommand :: Maybe FilePath -> Command a -> IO a
runCommand repository (Command cmd) = runScript $ do
  spec <- currentWorkingCopy
  rep <- liftIO $ maybe defaultLocalRepository makeRepository repository
  runReaderT cmd (spec, rep)

defaultLocalRepository :: IO KitRepository
defaultLocalRepository = makeRepository =<< (</> ".kit" ) <$> getHomeDirectory

