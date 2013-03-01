module Mp3 where

import Db
import Proplists
import JSON

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

ratinghigherthan4 = ratinghigherthan 4
ratinghigherthan n p = i > n
                 where
                  Int i = (get "rating" p)

-- selectByArtist artist db = filterDB (contains "artist" (Str artist) ) db 

artistSelector artist = select artistEq (fromStr artist) 

artistEq = (get "artist")

test1 = printDB $ artistSelector "Queen" testDb3
--   printDB $ update artistEq (Str "Queen") [("rating", (Int 11))] testDb3
test2 = search ratinghigherthan4 testDb3
