{-# LANGUAGE TypeSynonymInstances #-}
module Text.PList where

import Data.List (isInfixOf, isPrefixOf, intersperse)
import Control.Monad

-------------------------------------------------------------------------------
-- | A Key/Value pair in a PList Object
data PListObjectItem = PListObjectItem {
    itemKey :: String, -- The key in the object
    itemValue :: PListType -- The value in the object
  } deriving Eq 

data PListType = PListValue String
               | PListArray [PListType]
               | PListObject [PListObjectItem]
               deriving Eq

val = PListValue
arr = PListArray
obj = PListObject

infixl 1 ~> 
a ~> b = PListObjectItem a b

quote s = "\"" ++ s ++ "\""
quotable "" = True
quotable s = any (\x -> x `isInfixOf` s && not ("sourcecode" `isPrefixOf` s)) quote_triggers
              where quote_triggers = ["-", "<", ">", " ", ".m", ".h", "_", "$"]

instance Show PListObjectItem where
  show (PListObjectItem k v) = k ++ " = " ++ show v ++ ";\n"

instance Show PListType where
  show (PListValue a) = if quotable a then quote a else a 
  show (PListArray xs) = "(" ++ join (intersperse ", " $ map show xs) ++ ")"
  show (PListObject kvs) = "{\n" ++ (kvs >>= (\x -> "  " ++ show x)) ++ "}\n"

data PListFile = PListFile { pListFileCharset :: String, pListFileValue :: PListType }

instance Show PListFile where
  show (PListFile charset value) = "// " ++ charset ++ "\n" ++ show value ++ "\n"

