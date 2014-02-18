module BasicParser where

import Text.ParserCombinators.Parsec

data Variable = Variable String deriving(Show,Eq) 

{-
Utility function to ignore whitespace around a parser.
-}
whtsp :: Parser a -> Parser a
whtsp parser = do 
    spaces
    a <- parser
    spaces
    return a

-- var ::= A | B | C .... | Y | Z
var :: Parser Variable
var = do
    varName <- upper
    return $ Variable [varName]

varList :: Parser [Variable]
varList = do
    vars <- sepBy var (char ',') 
    return vars

cr :: Parser Char    
cr = char '\n'

str :: Parser String
str = do
    char '"'
    strBody <- many (noneOf "\"")
    char '"'
    return strBody

number :: Parser Int
number = 
    do
        ds <- many1 digit
        return (read ds :: Int)

main :: IO ()
main = do
    return ()