module BasicParser where

import Text.ParserCombinators.Parsec

data Variable = Variable String deriving (Show,Eq)

data Relop = Lt | Gt | Equal deriving (Show,Eq)

data Expression = Expression Int

data Statement = 
    Print [Expression]                       | 
    If Expression Relop Expression Statement | 
    Goto Expression                          |
    Input [Variable]                         | 
    Let Variable Expression                  |
    Gosub Expression                         |
    Return                                   |
    Clear                                    |
    List                                     |
    Run                                      |
    End

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

{-
Statement parsers.
-}
gosub = do {tk $ string "GOSUB";   return Gosub}
ret =   do {tk $ string "RETURN";  return Return}
clear = do {tk $ string "CLEAR";   return Clear}
list =  do {tk $ string "LIST";    return List}
run =   do {tk $ string "RUN";     return Run}
end =   do {tk $ string "END";     return End}

factor = 
    do
        v <- var 
        return "foo"
    <|>
    do
        n <- number
        return "bar"
    <|>
    do
        -- Expression
        return "bleh"

main :: IO ()
main = do
    return ()