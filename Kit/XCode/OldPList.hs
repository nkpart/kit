{-# LANGUAGE TypeSynonymInstances #-}
module Kit.XCode.OldPList where

import Data.List (isInfixOf, isPrefixOf)

-------------------------------------------------------------------------------
-- | A Key/Value pair in a PList Object
data PListObjectItem = PListObjectItem {
    itemKey :: String, -- The key in the object
    itemValue :: PListType -- The value in the object
  } deriving Eq 

lineItem = PListObjectItem 

data PListType = PListValue String
               | PListArray [PListType]
               | PListObject [PListObjectItem]
               deriving Eq

val x = PListValue x 
arr = PListArray
obj = PListObject

infixl 1 ~> 
a ~> b = PListObjectItem a b

quote s = "\"" ++ s ++ "\""
quotable s = or $ map (\x -> x `isInfixOf` s && (not $ "sourcecode" `isPrefixOf` s)) ["-", "<", ">", " ", ".m", ".h"]

instance Show PListObjectItem where
  show (PListObjectItem k v) = k ++ " = " ++ show v ++ "; "

instance Show PListType where
  show (PListValue a) = (if (quotable a) then quote a else a) 
  show (PListArray xs) = "(" ++ (map ((++ ",") . show) xs >>= id) ++ ")"
  show (PListObject kvs) = "{" ++ (kvs >>= show) ++ "}"


data PListFile = PListFile { pListFileCharset :: String, pListFileValue :: PListType }
projectFile = PListFile "!$*UTF8*$!" $ obj [
        "archiveVersion" ~> val "1",
        "classes" ~> obj [],
        "objectVersion" ~> val "45",
        "objects" ~> obj []
    ]

