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

data PListFile = PListFile String PListType -- Charset and the object to serialize

-- | A Key/Value pair in a PList Object
data PListObjectItem = PListObjectItem String PListType deriving Eq 

data PListType = PListValue String
               | PListArray [PListType]
               | PListObject Bool [PListObjectItem]
               deriving Eq

plist :: String -> PListType -> PListFile
plist = PListFile

val :: String -> PListType
val = PListValue

arr :: [PListType] -> PListType
arr = PListArray

obj :: [PListObjectItem] -> PListType
obj = PListObject True

infixl 1 ~> 
(~>) :: String -> PListType -> PListObjectItem
a ~> b = PListObjectItem a b

