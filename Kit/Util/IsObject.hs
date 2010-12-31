{-# LANGUAGE KindSignatures, TypeSynonymInstances, NoMonomorphismRestriction, OverlappingInstances #-}
module Kit.Util.IsObject where
  import Data.Object

  class IsObject x where
    showObject :: x -> StringObject
    readObject :: StringObject -> Maybe x
   
  (#>) :: IsObject b => [(String, Object String String)] -> String -> Maybe b
  obj #> key = lookupObject key obj >>= readObject

  instance IsObject a => IsObject [a] where
    showObject xs = Sequence $ map showObject xs
    readObject x = fromSequence x >>= mapM readObject 

  instance IsObject String where
    showObject = Scalar
    readObject = fromScalar

