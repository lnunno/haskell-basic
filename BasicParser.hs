{-
CS 591: Advanced Declarative Programming
Basic Parser using Parsec
By: Lucas Nunno
-}
module Main where

import System.Environment
import Text.ParserCombinators.Parsec hiding (Line)
import Data.List

data Variable = Variable String deriving (Show,Eq)

data Op = Plus | Minus | Times | Div deriving (Eq)

instance Show Op where
    show Plus   = "+"
    show Minus  = "-"
    show Times  = "*"
    show Div    = "/"

data Line = NumberedLine Int Statement | Line Statement deriving (Show)

data Relop = Lt | Gt | Equal deriving (Eq)

instance Show Relop where
    show Lt = "<"
    show Gt = ">"
    show Equal = "=" 

data Expression = 
    EString String      | 
    EVar Variable       | 
    ENum Int            | 
    EOp  Op             |
    Ne 
    deriving (Show)

data Statement = 
    Print [Expression]                       | 
    If Expression Relop Expression Statement | 
    Goto Expression                          |
    Input [Expression]                       | 
    Let Variable Expression                  |
    Gosub Expression                         |
    Return                                   |
    Clear                                    |
    List                                     |
    Run                                      |
    End

showVarList :: [Variable] -> String
showVarList vars = intercalate ", " (map show vars)

showExprList :: [Expression] -> String
showExprList exprs = intercalate "; " (map show exprs)

instance Show Statement where
    show (Print exprs)      = "PRINT " ++ (showExprList exprs)
    show (If e1 r1 e2 stmt) = "IF " ++ (show e1) ++ " " ++ (show r1) ++ " " ++ (show e2) ++ " THEN " ++ (show stmt)
    show (Goto e)           = "GOTO " ++ (show e)
    show (Input exprs)      = "INPUT " ++ (showExprList exprs)
    show (Let v e)          = "LET " ++ (show v) ++ " = " ++ (show e)
    show (Gosub e)          = "GOSUB " ++ (show e)
    show Return             = "RETURN"
    show Clear              = "CLEAR"
    show List               = "LIST"
    show Run                = "RUN"
    show End                = "END"

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

op :: Parser Op
op =
    do
        tk $ char '+'
        return Plus
    <|>
    do 
        tk $ char '-'
        return Minus
    <|>
    do
        tk $ char '*'
        return Times
    <|>
    do
        tk $ char '/'
        return Div

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

easyParse p input = parse p "basic" input

printStatement :: Parser Statement
printStatement = 
    do
        string "PRINT"
        return $ Print [Ne]

ifStatement :: Parser Statement
ifStatement =
    do
        tk $ string "IF"
        op <- relop
        tk $ string "THEN"
        return $ If Ne op Ne End

letStatement :: Parser Statement
letStatement =
    do
        tk $ string "LET"
        v <- var 
        tk $ char '='
        return $ Let v Ne 

goto :: Parser Statement
goto =
    do
        tk $ string "GOTO"
        return $ Goto Ne

input :: Parser Statement
input = 
    do
        tk $ string "INPUT"
        exprs <- expressionList 
        return $ Input exprs


{-
Statement parsers.
-}
gosub = do {tk $ string "GOSUB";   return $ Gosub Ne}
ret   = do {tk $ string "RETURN";  return Return}
clear = do {tk $ string "CLEAR";   return Clear}
list  = do {tk $ string "LIST";    return List}
run   = do {tk $ string "RUN";     return Run}
end   = do {tk $ string "END";     return End}

statement :: Parser Statement
statement = try printStatement <|> 
            try ifStatement    <|> 
            try goto           <|> 
            try input          <|> 
            try letStatement   <|> 
            try gosub          <|>
            try ret            <|>
            try clear          <|>
            try list           <|>
            try run            <|>
            try end

expression :: Parser Expression
expression = 
    do
        s <- tk $ str
        return $ EString s
    <|>
    do
        v <- tk $ var
        return $ EVar v
    <|>
    do
        n <- tk $ number
        return $ ENum n

expressionList :: Parser [Expression]
expressionList = 
    do
        ls <- tk $ expression `sepBy` (tk $ char ';')
        return ls

line :: Parser Line
line = 
    do
        n <- tk $ number
        s <- tk $ statement
        cr
        return $ NumberedLine n s
    <|>
    do
        s <- statement
        cr
        return $ Line s

basicFile :: Parser [Line]
basicFile = 
    do
        ls <- tk $ many line
        return ls

parseBasic input = parse basicFile "basic" input

getFileLines filePath = 
    do
        content <- readFile filePath
        return $ lines content 

parseBasicFromFilePath fp = 
    do
        fileStr <- readFile fp
        print fileStr
        return $ parseBasic fileStr

main :: IO ()
main = do
    args <- getArgs
    if (length args) > 0 
        then do
            let filePath = head args
            parseResult <- parseBasicFromFilePath filePath
            case parseResult of
                -- Error
                Left a -> print a
                -- Everything is good.
                Right a -> print a
        else do
            print "The first argument should be the path to a basic file."