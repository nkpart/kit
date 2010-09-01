{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Kit.XCode.PBXProject where

import Kit.XCode.OldPList
import Control.Monad.State
import Control.Monad.Writer

type UUID = String

data PBXProject = PBXProject {
    projectConfigurationList :: XCConfigurationList
  , projectGroups :: [PBXGroup]
  , projectName :: String
}

data XCConfigurationList = XCConfigurationList {
  configurationsList :: [XCBuildConfiguration]
}

data PBXGroup = PBXGroup {
    groupChildren :: [PBXGroup]
  , groupChildrenExtra :: [UUID]
  , groupName :: String
  , groupSourceTree :: String
}

data XCBuildConfiguration = XCBuildConfiguration

instance ProjectPListItem PBXGroup where
  writeEntry group = do
    childUUIDs <- mapM writeEntry (groupChildren group)
    write $ obj [
          "isa" ~=~ val "PBXGroup"
        , "name" ~=~ (val . groupName) group
        , "sourceTree" ~=~ (val . groupSourceTree) group
        , "children" ~=~ (arr $ map val (childUUIDs ++ groupChildrenExtra group))
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

instance ProjectPListItem PBXProject where
  writeEntry project = do
    config <- write $ val "TODO"
    productGroupUUID <- writeEntry (PBXGroup [] [] "Products" "<group>")
    mainGroupUUID <- writeEntry (PBXGroup [] [productGroupUUID] (projectName project) "<group>")
    write $ obj [
        "isa" ~=~ val "PBXProject"
      , "buildConfigurationList" ~=~ val config
      , "compatibilityVersion" ~=~ val "Xcode 3.1"
      , "hasScannedForEncodings" ~=~ val "1"
      , "mainGroup" ~=~ val mainGroupUUID
      , "productRefGroup" ~=~ val productGroupUUID
      , "projectDirPath" ~=~ val ""
      , "projectRoot" ~=~ val ""
      , "targets" ~=~ arr []
     ]
