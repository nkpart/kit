
-- {-XGeneralizedNewtypeDeriving}

module Kit.Client (
  --getDeps, 
  KitError, 
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
  
  getDeps :: KitRepository -> Kit -> IO (Either KitError [Kit])
  getDeps kr kit = do
    spec <- getKitSpec kr kit
    return $ fmap specDependencies spec
    
  -- x :: KitRepository -> KitSpec -> IO (Either [KitError] [Kit])
  -- x kr ks = let
  --     deps = specDependencies ks
  --     f = getDeps kr
  --   in
  --     
      

  specIfExists :: IO (Either KitError FilePath)
  specIfExists = let kitSpecPath = "KitSpec" in do
    doesExist <- doesFileExist kitSpecPath
    return $ maybeToRight "Couldn't find the spec file" (justTrue doesExist kitSpecPath)
  
  doRead :: IO (Either KitError String)
  doRead = specIfExists >>= traverse readFile
  
  parses :: String -> Either KitError KitSpec
  parses contents = maybeToRight "Could not parse spec." $ maybeRead contents
  
  myKitSpec :: IO (Either KitError KitSpec)
  myKitSpec = do
    contents <- doRead
    return $ contents >>= parses




  
  
  