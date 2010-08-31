{-# LANGUAGE TypeSynonymInstances #-}
module Kit.XCode.OldPList where

import Data.List

class AsPListType a where
    asPListType :: a -> PListType

instance AsPListType PListType where
    asPListType = id

data PListType = PListValue String
               | PListArray [PListType]
               | PListObject [(String, PListType)]

val = PListValue
arr = PListArray
obj = PListObject

data PListFile = PListFile { pListFileCharset :: String, pListFileValue :: PListType }

quote s = "\"" ++ s ++ "\""
quotable s = or $ map (\x -> x `isInfixOf` s) ["-", "<", ">", " "]


instance Show PListType where

    show (PListValue a) = if (quotable a) then quote a else a
    show (PListArray xs) = "(" ++ (map ((++ ",") . show) xs >>= id) ++ ")"
    show (PListObject kvs) = "{" ++ (kvs >>= f) ++ "}"
                                where f (k, v) = k ++ " = " ++ show v ++ ";"

(~=~) = (,)


projectFile = PListFile "!$*UTF8*$!" $ PListObject [
        "archiveVersion" ~=~ PListValue "1",
        "classes" ~=~ PListObject [],
        "objectVersion" ~=~ PListValue "45",
        "objects" ~=~ PListObject []
    ]

