{-# LANGUAGE TupleSections #-}
module Kit.Dependency (
    totalSpecDependencies,
    Dependency(..),
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

data Dependency = Dep KitSpec | Dev KitSpec FilePath deriving (Eq, Show)

depSpec (Dep k) = k
depSpec (Dev k _) = k
  
isDevDep :: Dependency -> Bool
isDevDep (Dep _) = False
isDevDep (Dev _ _) = True

-- | Return all the (unique) children of this tree (except the top node), in reverse depth order.
refineDeps :: Eq a => Tree a -> [a]
refineDeps = nub . concat . reverse . drop 1 . levels

-- todo: check for conflicts
-- todo: check for version ranges :)
totalSpecDependencies :: KitRepository -> WorkingCopy -> KitIO [Dependency]
totalSpecDependencies repo workingCopy = refineDeps <$> dependencyTree repo workingCopy

dependencyTree :: KitRepository -> WorkingCopy -> KitIO (Tree Dependency)
dependencyTree repo workingCopy = unfoldTreeM (unfoldDeps repo) (workingKitSpec workingCopy)

unfoldDeps :: KitRepository -> KitSpec -> KitIO (Dependency, [KitSpec])
unfoldDeps kr ks = (Dep ks,) <$> mapM (readKitSpec kr) (specDependencies ks) 

