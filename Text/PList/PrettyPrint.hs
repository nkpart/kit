{-# LANGUAGE TupleSections #-}
module Text.PList.PrettyPrint (pp, ppFlat) where

import Data.List (intersperse, isInfixOf, isPrefixOf)
import Control.Monad (join, (>=>), forM)
import Text.PList

import Kit.Xcode.Common

import Control.Monad.Writer (Writer, runWriter, tell)
import Control.Monad.State (StateT, get, put, runStateT)

pp :: PListFile -> String
pp (PListFile charset root value) = "// " ++ charset ++ "\n" ++ printValue (obj value) ++ "\n"

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

-----

ppFlat :: PListFile -> String
ppFlat (PListFile charset root objects) = "// " ++ charset ++ "\n" ++ printFlat doc ++ "\n"
  where doc = [ ("archiveVersion", FlatStr "1"),
                ("classes", FlatObj []),
                ("objectVersion", FlatStr "46"),
                 -- All Nested objects must end up within "objects", so that is where the flattening happens
                ("objects" , FlatObj (flatten' objects seedKeys)), 
                ("rootObject", FlatStr root)
              ]

seedKeys = map uuid [50000..]

printFlat :: FlatDocument -> String
printFlat = printFlatItem . FlatObj

quoteIf a = if quotable a then quote a else a

xxx (k,v) = k ++ " = " ++ printFlatItem v ++ ";\n"

printFlatItem (FlatStr a) = quoteIf a
printFlatItem (FlatArr xs) = "(" ++ join (intersperse ", " $ map quoteIf xs) ++ ")"
printFlatItem (FlatObj kvs) = "{\n" ++ (kvs >>= (\x -> "  " ++ xxx x)) ++ "}\n"

-- A nested PList document
type Document = [PListObjectItem]

-- A flat flattened document, any internal object/arrays have been referenced and promoted to the top level object
type FlatDocument = [(String, FlatItem)]
data FlatItem = FlatStr String | FlatArr [String] | FlatObj [(String, FlatItem)] deriving (Eq, Show)

flatten' :: Document -> [String] -> FlatDocument
flatten' doc refs = let (a,b) = runWriter $ fmap fst $ runStateT (expandDoc doc) refs
                     in a ++ b

expandDoc :: Document -> ObjectWriter [(String, FlatItem)]
expandDoc doc = forM doc $ \(PListObjectItem key item) -> do
                                            flat <- expand item
                                            return (key, flat)                

type ObjectWriter a = StateT [String] (Writer [(String, FlatItem)]) a

expand :: PListType -> ObjectWriter FlatItem
expand (PListValue s) = return $ FlatStr s
expand (PListArray xs) = fmap FlatArr $ mapM (expand >=> arrExpandFlat) xs
expand (PListObject embed contents) =
  let writeMethod = if embed
            then return 
            else fmap FlatStr . writeObj 
      flatContents = mapM (\(PListObjectItem a b) -> fmap (a,) (expand b)) contents
   in writeMethod . FlatObj =<< flatContents

arrExpandFlat :: FlatItem -> ObjectWriter String
arrExpandFlat (FlatStr s) = return s
arrExpandFlat x@(FlatArr _) = writeObj x
arrExpandFlat o@(FlatObj _) = writeObj o

readRef :: ObjectWriter String
readRef = do
  (x:xs) <- get
  put xs
  return x

writeToRef ref obj = tell [(ref, obj)]

writeObj o = do
  r <- readRef
  writeToRef r o
  return r

