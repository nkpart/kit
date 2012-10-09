{-# LANGUAGE TupleSections #-}
module Kit.Dependency (
    totalSpecDependencies,
    Dependency,
    dependency,
    depSpec,
    isDevDep,
    dependencyTree
  ) where

import Control.Error
import Kit.Util
import Kit.Spec
import Kit.Repository
import Kit.WorkingCopy
import Data.Tree (Tree, levels, unfoldTreeM)
import Data.List (nub)

data Dependency = Dependency { depSpec :: KitSpec, mbKitPath :: Maybe FilePath } deriving (Eq, Show)

-- Fold over a dependency
dependency :: (KitSpec -> a) -> (FilePath -> KitSpec -> a) -> Dependency -> a
dependency repoDepF _ (Dependency spec Nothing) = repoDepF spec
dependency _ f (Dependency spec (Just fp)) = f fp spec

instance Packageable Dependency where 
  packageName = packageName . depSpec
  packageVersion = packageVersion . depSpec
  
isDevDep :: Dependency -> Bool
isDevDep = isJust . mbKitPath

-- todo: check for version ranges :)
totalSpecDependencies :: KitRepository -> WorkingCopy -> Script [Dependency]
totalSpecDependencies repo workingCopy = refineDeps <$> dependencyTree repo workingCopy

-- | Return all the (unique) children of this tree (except the top node), in reverse depth order.
refineDeps :: Eq a => Tree a -> [a]
refineDeps = nub . concat . reverse . drop 1 . levels

dependencyTree :: KitRepository -> WorkingCopy -> Script (Tree Dependency)
dependencyTree repo workingCopy = unfoldTreeM (unfoldDeps repo devPackages) baseSpec
  where devPackages = workingDevPackages workingCopy
        baseSpec = workingKitSpec workingCopy

unfoldDeps :: KitRepository -> DevPackages -> KitSpec -> Script (Dependency, [KitSpec])
unfoldDeps kr devPackages = unfolder (mapM (lookupX kr devPackages) . specDependencies . depSpec) . lookupDependency devPackages 

lookupDependency :: DevPackages -> KitSpec -> Dependency
lookupDependency devPackages ks = maybe (Dependency ks Nothing) (\(devSpec,fp) -> Dependency devSpec (Just fp)) thisDev
      where thisDev = findDevPackage devPackages ks

unfolder :: Functor m => (a -> m b) -> a -> m (a, b)
unfolder f b = (b,) <$> f b

lookupX :: MonadIO m => KitRepository -> DevPackages -> Kit -> EitherT String m KitSpec
lookupX kr devPackages kit = maybe (readKitSpec kr kit) return $ findDevKitSpec devPackages kit 

findDevKitSpec :: DevPackages -> Kit -> Maybe KitSpec
findDevKitSpec devPackages kit = fmap fst $ findDevPackage devPackages kit

