module Kit.Repository where
  import Kit.Kit
  import Data.List
  import Network.HTTP
  import Network.URI
  import Network.BufferType
  import Data.Maybe
  import Data.Traversable
  import Control.Monad
  import Control.Applicative
  import System.Directory
  import System.FilePath.Posix
  import qualified Data.ByteString as BS

  kitPath :: Kit -> String
  kitPath k = joinS ["kits", kitName k, kitVersion k] "/"
    where joinS xs x = foldl1 (++) $ intersperse x xs 
  
  data KitRepository = KitRepository {
    repoSave :: (String -> FilePath -> IO (Maybe ())),
    repoRead :: (String -> IO (Maybe String))
  }
  
  justTrue :: Bool -> a -> Maybe a
  justTrue True a = Just a
  justTrue False _ = Nothing
  
  getBody :: BufferType a => HStream a => String -> IO (Maybe a)
  getBody path = let
      request = defaultGETRequest_ . fromJust . parseURI
      checkResponse r = justTrue (rspCode r == (2,0,0)) r
      leftMaybe = either (const Nothing) Just 
    in do
      rr <- Network.HTTP.simpleHTTP $ request path
      return $ fmap rspBody $ leftMaybe rr
  
  download :: String -> FilePath -> IO (Maybe ())
  download url destination = do
      body <- getBody url
      sequenceA $ fmap (BS.writeFile destination) body
        
  webRepo :: String -> KitRepository
  webRepo baseUrl = KitRepository save read where
    save = download 
    read = getBody

  fileRepo :: String -> KitRepository
  fileRepo baseDir = KitRepository save read where
    save src destPath = let srcPath = (baseDir </> src) in
      Just <$> copyFile srcPath destPath
    read path = let file = (baseDir </> path) in do
      exists <- doesFileExist file
      sequenceA $ justTrue exists $ readFile file
      
   
   
   
   
   
   