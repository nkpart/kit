{-# LANGUAGE TupleSections #-}
module Kit.Dependency (
    totalSpecDependencies,
    Dependency,
    dependency,
    depSpec,
    isDevDep,
    dependencyTree
  ) where

import Kit.Util
import Kit.Spec
import Kit.Repository
import Kit.WorkingCopy
import Data.Tree (Tree, levels, unfoldTreeM)
import Data.List (nub)
import Data.Maybe (isJust)

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

-- | Return all the (unique) children of this tree (except the top node), in reverse depth order.
refineDeps :: Eq a => Tree a -> [a]
refineDeps = nub . concat . reverse . drop 1 . levels

-- todo: check for version ranges :)
totalSpecDependencies :: KitRepository -> WorkingCopy -> KitIO [Dependency]
totalSpecDependencies repo workingCopy = refineDeps <$> dependencyTree repo workingCopy

dependencyTree :: KitRepository -> WorkingCopy -> KitIO (Tree Dependency)
dependencyTree repo workingCopy = unfoldTreeM (unfoldDeps repo devPackages) baseSpec
  where devPackages = workingDevPackages workingCopy
        baseSpec = workingKitSpec workingCopy

lookupDependency :: DevPackages -> KitSpec -> Dependency
lookupDependency devPackages ks = maybe (Dependency ks Nothing) (\(ks',fp) -> Dependency ks' (Just fp)) thisDev
    where thisDev = findDevPackage devPackages ks

findKitSpec :: DevPackages -> Kit -> Maybe KitSpec
findKitSpec devPackages kit = fmap fst $ findDevPackage devPackages kit

unfoldDeps :: KitRepository -> DevPackages -> KitSpec -> KitIO (Dependency, [KitSpec])
unfoldDeps kr devPackages spec = let rootDep = lookupDependency devPackages spec
                                     lookup' kit = maybe (readKitSpec kr kit) return $ findKitSpec devPackages kit 
                                  in do
                                      children <- mapM lookup' . specDependencies . depSpec $ rootDep
                                      return (rootDep, children)

