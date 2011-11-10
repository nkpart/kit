module Text.PList.PrettyPrint (pp) where

import Data.List (intersperse, isInfixOf, isPrefixOf)
import Control.Monad (join)
import Text.PList

pp :: PListFile -> String
pp (PListFile charset value) = "// " ++ charset ++ "\n" ++ printValue value ++ "\n"

printItem :: PListObjectItem -> [Char]
printItem (PListObjectItem k v) = k ++ " = " ++ printValue v ++ ";\n"

printValue :: PListType -> [Char]
printValue (PListValue a) = if quotable a then quote a else a 
printValue (PListArray xs) = "(" ++ join (intersperse ", " $ map printValue xs) ++ ")"
printValue (PListObject _ kvs) = "{\n" ++ (kvs >>= (\x -> "  " ++ printItem x)) ++ "}\n"

quote :: String -> String
quote s = "\"" ++ s ++ "\""

quotable :: String -> Bool
quotable "" = True
quotable s = any (\x -> x `isInfixOf` s && not ("sourcecode" `isPrefixOf` s)) quote_triggers
              where quote_triggers = ["-", "<", ">", " ", ".m", ".h", "_", "$"]


