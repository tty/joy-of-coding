module JSON(parseJSON) where

import Proplists
import Data.List
import Data.Char
import Text.ParserCombinators.Parsec hiding (token)

token p = do r <- p
             spaces
             return r

comma :: Parser Char
comma = token (char ',')

parseJSON :: String -> PropVal
parseJSON str = case (parse jsonFile "" str) of
               Left  s -> error (show s)
               Right v -> v

jsonFile = do contents <- jsonObject <|> jsonArray
              eof
              return contents

jsonObject = do pairs <- between open close (sepBy jsonPair comma)
                return $ Obj $ pairs
  where
  open  = token (char '{')
  close = token (char '}')

jsonPair = do key   <- token(jsonString)
              token (char ':')
              value <- token(jsonValue)
              return (toString key, value)
  where
 toString (Str s) = s
 toString _ = ""


jsonArray = do values <- between open close (sepBy (token jsonValue) comma)
               return $ Arr values
  where
 open  = token (char '[')
 close = token (char ']')

jsonValue = do spaces
               obj <- token(jsonString 
                             <|> jsonNumber
                             <|> jsonObject
                             <|> jsonArray
                             <|> jsonNull
                             )
               return obj

jsonString = do s <- between (char '"' ) (char '"' ) (many jsonChar)
                return (Str s)

isValidJsonChar ch = (isAscii ch) && (isPrint ch) && (ch /= '\\') && (ch /= '"')

hexToInt s = foldl (\i j -> (16 * i) + j) 0 (map digitToInt s)

jsonChar = satisfy isValidJsonChar
           <|> do char '\\'  -- escaping backslash
                  char '\\'  -- escaped character
                    <|> char '"'
                    <|> char '/'
                    <|> (char 'b' >> return '\b')
                    <|> (char 'f' >> return '\f')
                    <|> (char 'n' >> return '\n')
                    <|> (char 'r' >> return '\r')
                    <|> (char 't' >> return '\t')
                    <|> do char 'u'
                           hex <- count 4 (satisfy isHexDigit)
                           return $ chr (hexToInt hex)

-- JSON Number
jsonNumber = do i    <- int
                frac <- option "" frac
                e    <- option "" expo
                return $ Int (read (i ++ frac ++ e))

int = do sign  <- option "" (string "-")
         value <- (string "0" <|> many1 digit)
         return (sign ++ value)

frac = do char '.'
          digits <- many1 digit
          return ( '.':digits)

expo = do e <- oneOf "eE"
          p <- option '+' (oneOf "+-")
          n <- many1 digit
          return (e : p : n)

jsonNull  = token (string "null")  >> return Undefined 

