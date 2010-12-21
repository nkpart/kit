{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Kit.Xcode.Experimental.ProjectBuilder where

import Text.PList
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Map as M

type UUID = String

data UUIDValue = UUIDProject PBXProject | UUIDGroup PBXGroup deriving (Eq, Show, Ord)

data UUIDState = UUIDState {
    unusedUUIDs :: [UUID]
  , knownObjects :: M.Map UUIDValue UUID
}

lookupObject :: UUIDValue -> State UUIDState UUID --UUIDState -> (UUIDState, UUIValue)
lookupObject v = do
    s <- get
    let o = M.lookup v $ knownObjects s
    maybe (insertObject v) return o
     where insertObject v = do
                              s@(UUIDState (u:uuids) objects) <- get
                              put $ UUIDState uuids (M.insert v u objects)
                              return u

data PBXProject = PBXProject {
    projectConfigurationList :: XCConfigurationList
  , projectGroups :: [PBXGroup]
  , projectName :: String
  , targets :: [PBXNativeTarget]
} deriving (Eq, Show, Ord)

data PBXNativeTarget = PBXNativeTarget {
    targetConfigurationList :: XCConfigurationList
  , targetBuildPhases :: [PBXBuildPhase]
} deriving (Eq, Show, Ord)

data PBXBuildPhase = PBXBuildPhase deriving (Eq, Show, Ord)

data XCConfigurationList = XCConfigurationList {
  configurationsList :: [XCBuildConfiguration]
} deriving (Eq, Show, Ord)

data PBXGroup = PBXGroup {
    groupChildren :: [PBXGroup]
  , groupChildrenExtra :: [UUID]
  , groupName :: String
  , groupSourceTree :: String
} deriving (Eq, Show, Ord)

data XCBuildConfiguration = XCBuildConfiguration deriving (Eq, Show, Ord)

instance ProjectPListItem PBXGroup where
  writeEntry group = do
    childUUIDs <- mapM writeEntry (groupChildren group)
    write $ obj [
          "isa" ~> val "PBXGroup"
        , "name" ~> (val . groupName) group
        , "sourceTree" ~> (val . groupSourceTree) group
        , "children" ~> arr $ map val (childUUIDs ++ groupChildrenExtra group)
      ]

type PLObjects = [(UUID, PListType)]
-- Consume UUIDs from state, write out other objects to be added to the plist
newtype PL a = PL { unPL :: WriterT PLObjects (State [UUID]) a }
              deriving (Monad, MonadState [UUID], MonadWriter PLObjects)

class ProjectPListItem a where
  writeEntry :: a -> PL UUID

popS = do
  (x:xs) <- get
  put xs
  return x

write blah = do
  uuid <- popS
  tell [(uuid, blah)]
  return uuid

f :: M.Map UUID a -> a -> (M.Map UUID a, UUID)
f = undefined

instance ProjectPListItem PBXProject where
  writeEntry project = do
    config <- write $ val "TODO"
    productGroupUUID <- writeEntry (PBXGroup [] [] "Products" "<group>")
    mainGroupUUID <- writeEntry (PBXGroup [] [productGroupUUID] (projectName project) "<group>")
    write $ obj [
        "isa" ~> val "PBXProject"
      , "buildConfigurationList" ~> val config
      , "compatibilityVersion" ~> val "Xcode 3.1"
      , "hasScannedForEncodings" ~> val "1"
      , "mainGroup" ~> val mainGroupUUID
      , "productRefGroup" ~> val productGroupUUID
      , "projectDirPath" ~> val ""
      , "projectRoot" ~> val ""
      , "targets" ~> arr []
     ]
