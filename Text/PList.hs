{-# LANGUAGE TypeSynonymInstances #-}
module Text.PList (
    PListType, 
    PListObjectItem,
    PListFile,
    val, 
    arr, 
    obj, 
    plist,
    (~>)) where

import Data.List (isInfixOf, isPrefixOf, intersperse)
import Control.Monad

-------------------------------------------------------------------------------
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

quote :: String -> String
quote s = "\"" ++ s ++ "\""

quotable :: String -> Bool
quotable "" = True
quotable s = any (\x -> x `isInfixOf` s && not ("sourcecode" `isPrefixOf` s)) quote_triggers
              where quote_triggers = ["-", "<", ">", " ", ".m", ".h", "_", "$"]

instance Show PListObjectItem where
  show (PListObjectItem k v) = k ++ " = " ++ show v ++ ";\n"

instance Show PListType where
  show (PListValue a) = if quotable a then quote a else a 
  show (PListArray xs) = "(" ++ join (intersperse ", " $ map show xs) ++ ")"
  show (PListObject _ kvs) = "{\n" ++ (kvs >>= (\x -> "  " ++ show x)) ++ "}\n"

data PListFile = PListFile String PListType -- Charset and the object to serialize

instance Show PListFile where
  show (PListFile charset value) = "// " ++ charset ++ "\n" ++ show value ++ "\n"

