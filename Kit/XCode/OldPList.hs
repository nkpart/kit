{-# LANGUAGE TypeSynonymInstances #-}
module Kit.XCode.OldPList where

import Data.List (isInfixOf, isPrefixOf)

data PListObjectItem = PListObjectItem {
    itemKey :: String,
    itemComment :: Maybe String,
    itemValue :: PListType
  } deriving Eq 

commentBit = maybe "" (\s -> " /* " ++ s ++ " */") 
printItem :: PListObjectItem -> String
printItem (PListObjectItem k c v) = k ++ (commentBit c) ++ " = " ++ show v ++ "; "

data PListType = PListValue String (Maybe String)
               | PListArray [PListType]
               | PListObject [PListObjectItem]
               deriving Eq

val x = PListValue x Nothing
arr = PListArray
obj = PListObject

data PListFile = PListFile { pListFileCharset :: String, pListFileValue :: PListType }

quote s = "\"" ++ s ++ "\""
quotable s = or $ map (\x -> x `isInfixOf` s && (not $ "sourcecode" `isPrefixOf` s)) ["-", "<", ">", " ", ".m", ".h"]

instance Show PListType where
    show (PListValue a c) = (if (quotable a) then quote a else a) ++ commentBit c 
    show (PListArray xs) = "(" ++ (map ((++ ",") . show) xs >>= id) ++ ")"
    show (PListObject kvs) = "{" ++ (kvs >>= printItem) ++ "}"

a ~> b = PListObjectItem a Nothing b

projectFile = PListFile "!$*UTF8*$!" $ obj [
        "archiveVersion" ~> val "1",
        "classes" ~> obj [],
        "objectVersion" ~> val "45",
        "objects" ~> obj []
    ]

