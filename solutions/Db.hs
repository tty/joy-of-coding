module Db where

import JSON
import Proplists 
import Data.List

type DB = [ Proplist ]
type Record = Proplist

create :: DB 
create = []

addRecord :: Record -> DB -> DB
addRecord r db = r:db

-- Usage: readDB (\x -> printDB (select (get "name") (Str "Innovation") x))
readDB :: (DB -> IO b) -> IO b
readDB f = do
    text <- readFile "beers.json"
    res <- (f (fromJSON text))
    return res 

search :: (Record -> Bool) -> DB -> DB
search f db = [ r | r <- db, f r]

select :: (Record -> PropVal) -> PropVal -> DB -> DB
select f v = search (match f v) 
    where
        match f v x = (f x) == v

update :: (Record -> PropVal) -> PropVal -> Record -> DB -> DB
update f v p = map rep
    where
       rep x  
           | (f x) == v = merge x p
           | otherwise = x

printDB :: DB -> IO ()
printDB db = putStr (dbToJSON db)

dbToJSON :: DB -> String
dbToJSON db = toJSON $ fromArr $ (map fromObj db)

fromJSON :: String -> DB
fromJSON s = map f db
    where
       Arr db = parseJSON s 
       f (Obj xs) = xs

