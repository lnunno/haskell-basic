module BasicParser where

import Text.ParserCombinators.Parsec

data Variable = Variable String deriving (Show,Eq)

data Relop = Lt | Gt | Equal deriving (Show,Eq)

{-
Utility function to ignore whitespace following a parser.
-}
tk :: Parser a -> Parser a
tk parser = 
    do 
        a <- parser
        spaces
        return a

-- var ::= A | B | C .... | Y | Z
var :: Parser Variable
var = 
    do
        varName <- upper
        return $ Variable [varName]

varList :: Parser [Variable]
varList = 
    do
        vars <- sepBy var (char ',') 
        return vars

cr :: Parser Char    
cr = char '\n'

str :: Parser String
str = 
    do
        char '"'
        strBody <- many (noneOf "\"")
        char '"'
        return strBody

number :: Parser Int
number = 
    do
        ds <- many1 digit
        return (read ds :: Int)

relop :: Parser Relop
relop = 
    do
        tk $ char '>'
        return Gt
    <|>
    do
        tk $ char '<'
        return Lt
    <|> 
    do
        tk $ char '='
        return Equal

parseBasic p input = parse p "basic" input

printStatement :: Parser String
printStatement = 
    do
        string "PRINT"
        return "Foo"

ifStatement :: Parser String
ifStatement =
    do
        tk $ string "IF"
        tk $ string "THEN"
        return "Foo"

letStatement :: Parser String
letStatement =
    do
        tk $ string "LET"
        tk $ char '='
        return "foo"

goto :: Parser String
goto =
    do
        tk $ string "GOTO"

gosub :: Parser String
gosub =
    do
        tk $ string "GOSUB"
        return "Foo"

ret = tk $ string "RETURN"
clear = tk $ string "CLEAR"
list = tk $ string "LIST"
run = tk $ string "RUN"
end = tk $ string "END"

main :: IO ()
main = do
    return ()