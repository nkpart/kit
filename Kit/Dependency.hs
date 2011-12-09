{-# LANGUAGE TupleSections #-}
module Kit.Dependency (
    totalSpecDependencies,
    Dependency,
    dependency,
    depSpec,
    isDevDep,
    dependencyTree,
    devKitDir -- TODO, cleanup main, might not need to export
  ) where

import Kit.Util
import Kit.Spec
import Kit.Repository
import Kit.WorkingCopy
import Data.Tree (Tree, levels, unfoldTreeM)
import Data.List (nub)
import Data.Maybe (isJust)

data Dependency = Dependency { depSpec :: KitSpec, mbKitPath :: (Maybe FilePath) } deriving (Eq, Show)

-- Fold over a dependency
dependency :: (KitSpec -> a) -> (KitSpec -> FilePath -> a) -> Dependency -> a
dependency repoDepF _ (Dependency spec Nothing) = repoDepF spec
dependency _ f (Dependency spec (Just fp)) = f spec fp

instance Packageable Dependency where 
  packageName = packageName . depSpec
  packageVersion = packageVersion . depSpec
  
isDevDep :: Dependency -> Bool
isDevDep = isJust . mbKitPath

-- | Return all the (unique) children of this tree (except the top node), in reverse depth order.
refineDeps :: Eq a => Tree a -> [a]
refineDeps = nub . concat . reverse . drop 1 . levels

-- todo: check for conflicts
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

class KitSpecContainer a where
  lookupKit :: a -> Kit -> KitIO KitSpec

instance KitSpecContainer KitRepository where lookupKit = readKitSpec
instance KitSpecContainer DevPackages where lookupKit dp = maybeToKitIO "Not a dev package" . findKitSpec dp

unfoldDeps :: KitRepository -> DevPackages -> KitSpec -> KitIO (Dependency, [KitSpec])
unfoldDeps kr devPackages spec = let rootDep = lookupDependency devPackages spec
                                     lookup' kit = lookupKit devPackages kit <|> lookupKit kr kit
                                  in do
                                      children <- mapM lookup' . specDependencies . depSpec $ rootDep
                                      return (rootDep, children)

