module Kit.Client (
  getDeps, 
  myKitSpec) 
    where
      
  import Control.Monad.Trans
  import Control.Monad
  import Control.Applicative
  import Data.Traversable
  import Data.Foldable
  import System.Directory
  import Kit.Kit
  import Kit.Repository
  import Kit.Util
  import Kit.Spec
  
  getDeps :: KitRepository -> Kit -> KitIO [Kit]
  getDeps kr kit = do
    spec <- getKitSpec kr kit
    return $ spec >>= specDependencies
    
  -- x :: KitRepository -> KitSpec -> IO (Either [KitError] [Kit])
  -- x kr ks = let
  --     deps = specDependencies ks
  --     f = getDeps kr
  --   in
  
  myKitSpec :: KitIO KitSpec
  myKitSpec = doRead >>= (\c -> KitIO . return $ parses c)

  -- private!
  specIfExists :: KitIO FilePath
  specIfExists = let kitSpecPath = "KitSpec" in do
    doesExist <- liftIO $ doesFileExist kitSpecPath
    maybeToKitIO "Couldn't find the spec file" (justTrue doesExist kitSpecPath)
  
  doRead :: KitIO String
  doRead = do
    fp <- specIfExists
    contents <- liftIO $ readFile fp
    return contents
  
  parses :: String -> Either [KitError] KitSpec
  parses contents = maybeToRight ["Could not parse spec."] $ maybeRead contents
  
  