{-# LANGUAGE KindSignatures, TypeSynonymInstances, NoMonomorphismRestriction, OverlappingInstances #-}
module Kit.Util.IsObject where
  import Data.Object

  class ShowObject x where
    showObject :: x -> StringObject

  class ReadObject x where
    readObject :: StringObject -> Maybe x
   
  (#>) :: ReadObject b => [(String, Object String String)] -> String -> Maybe b
  obj #> key = lookupObject key obj >>= readObject

  instance ShowObject a => ShowObject [a] where
    showObject xs = Sequence $ map showObject xs

  instance ReadObject a => ReadObject [a] where
    readObject x = fromSequence x >>= mapM readObject 

  instance ShowObject String where
    showObject = Scalar

  instance ReadObject String where
    readObject = fromScalar

