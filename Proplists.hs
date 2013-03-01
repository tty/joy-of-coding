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

