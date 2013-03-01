module Db where

import JSON
import Proplists 
import Data.List

type DB = [ Proplist ]
type Record = Proplist

data Mp3 = Mp3 { song :: String, artist :: String, rating :: Integer } deriving (Show)

mp31 = Mp3 { song = "Street Spirit", artist = "Radiohead", rating = 9}

mp32 = Mp3 { song = "We Will Rock You", artist = "Queen", rating = 3}

mp33 = Mp3 { song = "Bohemian Rhapsody", artist = "Queen", rating = 4}


testDb = addRecord (mp3ToProplist mp31) create
testDb2 = addRecord (mp3ToProplist mp32) testDb
testDb3 = addRecord (mp3ToProplist mp33) testDb2

makeMp3 s a r = mp3
    where
        s' = set "song" (fromStr s) []
        a' = set "artist" (fromStr a) s'
        mp3 = set "rating" (fromInt r) a'

mp3ToProplist (Mp3 {song = s, artist = a, rating = r}) = makeMp3 s a r 

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

printDB :: DB -> IO ()
printDB db = putStr (dbToJSON db)

dbToJSON :: DB -> String
dbToJSON db = toJSON $ fromArr $ (map fromObj db)

fromJSON :: String -> DB
fromJSON s = map f db
    where
       Arr db = parseJSON s 
       f (Obj xs) = xs

