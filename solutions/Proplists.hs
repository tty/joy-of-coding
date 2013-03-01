module Proplists where

import Data.List
import Data.Char

type Property = (String, PropVal)

type Proplist = [ Property ] 

data PropVal = Int Integer | Str String | Undefined | Obj Proplist | Arr [PropVal]
    deriving (Show, Eq)

testList :: Proplist
testList = [("a",Int 1), ("b", Str "Foo")]

testList2 :: Proplist
testList2 = [("b", Str "Bar"),("c", Undefined)]

fromArr a = Arr a
fromStr s = Str s
fromObj o = Obj o
fromInt i = Int i

-- keys l = [ x | (x,_) <- l ]

-- values l = [ x | (_,x) <- l ]

del :: String -> Proplist -> Proplist
del k l = [(k2,p) | (k2, p) <- l , k /= k2]

set :: String -> PropVal -> Proplist -> Proplist
set k v [] = [ (k,v) ]
set k v l = (k,v):l' 
   where l' = [ (k2,p) | (k2,p) <- del k l ]

get :: String -> Proplist -> PropVal
get k [] = Undefined 
get k ((k2,p):l)  
    | k == k2 = p
    | otherwise = (get k l)   

merge :: Proplist -> Proplist -> Proplist
merge = foldl (\x (k,v) -> set k v x) 

--- PropVal to JSON
toJSON :: PropVal -> String
toJSON (Str s) = "\"" ++ s ++ "\""
toJSON (Int i) = show i 
toJSON (Undefined) = "null"
toJSON (Obj o) =  "{" ++ (concat (props)) ++ "}\n"
    where
      props = intersperse ",\n" [ pJSON p | p <- o ]
      pJSON (k,p) = "\t\"" ++ k ++ "\":" ++ (toJSON p)
toJSON (Arr a) = "[" ++ (concat (props)) ++ "]\n"
    where
      props = intersperse ",\n" [ toJSON p | p <- a ]
