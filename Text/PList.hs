{-# LANGUAGE TypeSynonymInstances #-}
module Text.PList (
    PListType(..), 
    PListObjectItem(..),
    PListFile(..),
    val, 
    arr, 
    obj, 
    plist,
    (~>)) where

-------------------------------------------------------------------------------
data PListFile = PListFile String [PListObjectItem] -- Charset and the object to serialize

-- | A Key/Value pair in a PList Object
data PListObjectItem = PListObjectItem String PListType deriving Eq 

data PListType = PListValue String
               | PListArray [PListType]
               | PListObject Bool [PListObjectItem]
               deriving Eq

plist :: String -> [PListObjectItem] -> PListFile
plist a b = PListFile a b

val :: String -> PListType
val = PListValue

arr :: [PListType] -> PListType
arr = PListArray

obj :: [PListObjectItem] -> PListType
obj = PListObject True

infixl 1 ~> 
(~>) :: String -> PListType -> PListObjectItem
a ~> b = PListObjectItem a b

