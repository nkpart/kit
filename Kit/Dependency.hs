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
import Data.Tree
import Data.List

data Location = Repo | Dev FilePath deriving (Eq, Show)
data Dependency = Dependency KitSpec Location deriving (Eq, Show)

dependency :: (KitSpec -> a) -> (KitSpec -> FilePath -> a) -> Dependency -> a
dependency f _ (Dependency spec Repo) = f spec
dependency _ f (Dependency spec (Dev fp)) = f spec fp

depSpec :: Dependency -> KitSpec
depSpec (Dependency k _) = k
  
isDevDep :: Dependency -> Bool
isDevDep (Dependency _ Repo) = False
isDevDep (Dependency _ (Dev _)) = True

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
lookupDependency devPackages ks = maybe (Dependency ks Repo) (\(ks',fp) -> Dependency ks' (Dev fp)) thisDev
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

