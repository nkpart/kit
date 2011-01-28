{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Kit.Commands (
  Command,
  liftKit,
  mySpec,
  myRepository,
  runCommand,
  defaultLocalRepoPath,
  defaultLocalRepository
) where

import Kit.Util
import Kit.Spec
import Kit.Repository

import qualified Data.ByteString as BS
import System.Exit (exitFailure)

import Control.Monad.Error
import Control.Monad.Reader

newtype Command a = Command (ReaderT (KitSpec, KitRepository) (ErrorT String IO) a) deriving Monad

instance MonadIO Command where
  liftIO a = Command $ liftIO a

liftKit :: KitIO a -> Command a
liftKit = Command . ReaderT . const

mySpec :: Command KitSpec
mySpec = Command $ fmap fst ask

myRepository :: Command KitRepository
myRepository = Command $ fmap snd ask

runCommand :: Command a -> IO ()
runCommand (Command cmd) = run $ do
  spec <- readSpec "KitSpec"
  repository <- liftIO defaultLocalRepository
  runReaderT cmd (spec, repository)
  where run = (handleFails =<<) . runErrorT
        handleFails = either (\msg -> putStrLn ("kit error: " ++ msg) >> exitFailure) (const $ return ())

defaultLocalRepoPath :: IO FilePath
defaultLocalRepoPath = (</> ".kit" </> "repository") <$> getHomeDirectory 

defaultLocalRepository :: IO KitRepository
defaultLocalRepository = KitRepository <$> defaultLocalRepoPath

readSpec :: FilePath -> KitIO KitSpec
readSpec path = checkExists path >>= liftIO . BS.readFile >>= ErrorT . return . parses
  where checkExists pathToSpec = do
          doesExist <- liftIO $ doesFileExist pathToSpec 
          if doesExist then return pathToSpec else throwError $ "Couldn't find the spec at " ++ pathToSpec 
        parses = maybeToRight "Parse error in KitSpec file" . decodeSpec

