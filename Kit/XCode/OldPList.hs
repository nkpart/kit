{-# LANGUAGE TypeSynonymInstances #-}
module Kit.XCode.OldPList where

import Data.List (isInfixOf, isPrefixOf)

data PListObjectItem = PListObjectItem {
    itemKey :: String,
    itemComment :: Comment,
    itemValue :: PListType
  } deriving Eq 

-- A comment that can be attached to a value.
newtype Comment = Comment (Maybe String) deriving Eq

comment :: String -> Comment
comment = Comment . Just

noComment :: Comment
noComment = Comment Nothing

instance Show Comment where
  show (Comment m) = maybe "" (\s -> " /* " ++ s ++ " */") m

data PListType = PListValue String Comment
               | PListArray [PListType]
               | PListObject [PListObjectItem]
               deriving Eq

val x = PListValue x noComment 
arr = PListArray
obj = PListObject

infixl 1 ~> 
a ~> b = PListObjectItem a noComment b

infixl 2 /*/
(PListValue a _) /*/ s = PListValue a (comment s)
a /*/ _ = a

quote s = "\"" ++ s ++ "\""
quotable s = or $ map (\x -> x `isInfixOf` s && (not $ "sourcecode" `isPrefixOf` s)) ["-", "<", ">", " ", ".m", ".h"]

instance Show PListObjectItem where
  show (PListObjectItem k c v) = k ++ (show c) ++ " = " ++ show v ++ "; "

instance Show PListType where
  show (PListValue a c) = (if (quotable a) then quote a else a) ++ show c 
  show (PListArray xs) = "(" ++ (map ((++ ",") . show) xs >>= id) ++ ")"
  show (PListObject kvs) = "{" ++ (kvs >>= show) ++ "}"


data PListFile = PListFile { pListFileCharset :: String, pListFileValue :: PListType }
projectFile = PListFile "!$*UTF8*$!" $ obj [
        "archiveVersion" ~> val "1",
        "classes" ~> obj [],
        "objectVersion" ~> val "45",
        "objects" ~> obj []
    ]

