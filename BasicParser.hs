{-
CS 591: Advanced Declarative Programming
Basic Parser using Parsec
By: Lucas Nunno
-}
module BasicParser where

import Text.ParserCombinators.Parsec hiding (Line)
import Text.ParserCombinators.Parsec.Expr
import Data.List

data Variable = Variable String deriving (Eq,Ord)

instance Show Variable where
    show (Variable s) = s

data Op = Plus | Minus | Times | Div deriving (Eq)

instance Show Op where
    show Plus   = "+"
    show Minus  = "-"
    show Times  = "*"
    show Div    = "/"

data Line = NumberedLine Int Statement | Line Statement

instance Show Line where
    show (NumberedLine n stmt) = (show n) ++ " " ++ (show stmt)
    show (Line stmt)           = show stmt 

instance Eq Line where
    (NumberedLine x _) == (NumberedLine y _) = x == y
    (NumberedLine _ _) == (Line _) = False
    (Line _) == (Line _) = False 

instance Ord Line where
    compare (NumberedLine x _) (NumberedLine y _) = 
        if x > y 
            then GT 
            else 
                if x == y 
                    then EQ
                    else LT
    compare (NumberedLine _ _) (Line _) = LT

data Relop = Lt | Gt | Equal deriving (Eq)

instance Show Relop where
    show Lt = "<"
    show Gt = ">"
    show Equal = "=" 

data Expression = 
    EString String                   | 
    EVar Variable                    | 
    ENum Int                         | 
    Random Int                       |
    ABinary Op Expression Expression 

instance Show Expression where
    show (EString s)        = show s
    show (EVar var)         = show var 
    show (ENum i)           = show i
    show (ABinary op e1 e2) = (show e1) ++ " " ++ (show op) ++ " " ++ (show e2)
    show (Random n)         = "RND(" ++ (show n) ++ ")"

data Statement = 
    Print [Expression]                       | 
    If Expression Relop Expression Statement | 
    ForLoop Expression Expression Expression |
    NextIter Expression                      |
    Goto Expression                          |
    Input [Expression]                       | 
    Array Variable Expression                |
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

showLineList :: [Line] -> String
showLineList lineLs = intercalate "\n" (map show lineLs)

instance Show Statement where
    show (Print exprs)      = "PRINT " ++ (showExprList exprs)
    show (If e1 r1 e2 stmt) = "IF " ++ (show e1) ++ " " ++ (show r1) ++ " " ++ (show e2) ++ " THEN " ++ (show stmt)
    show (ForLoop e1 e2 e3) = "FOR " ++ (show e1) ++ " = " ++ (show e2) ++ " TO " ++ (show e3)
    show (NextIter e)       = "NEXT " ++ (show e)
    show (Goto e)           = "GOTO " ++ (show e)
    show (Input exprs)      = "INPUT " ++ (showExprList exprs)
    show (Array v e)        = "DIM " ++ (show v) ++ " (" ++ (show e) ++ ")"
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
        many (char ' ')
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

cr :: Parser String    
cr = (string "\n") <|> (string "\r\n")

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
        tk $ string "PRINT"
        exprs <- expressionList
        return $ Print exprs

ifStatement :: Parser Statement
ifStatement =
    do
        tk $ string "IF"
        e1 <- tk $ expression
        op <- relop
        e2 <- tk $ expression
        tk $ string "THEN"
        s <- tk $ statement
        return $ If e1 op e2 s

forLoop :: Parser Statement
forLoop =
    do
        tk $ string "FOR"
        e1 <- tk $ expression
        tk $ char '='
        e2 <- tk $ expression
        tk $ string "TO"
        e3 <- tk $ expression
        return $ ForLoop e1 e2 e3

nextIter :: Parser Statement
nextIter = 
    do
        tk $ string "NEXT"
        e <- expression
        return $ NextIter e

parens p = 
    do
        tk $ char '('
        res <- p
        tk $ char ')'
        return res

random :: Parser Expression
random = 
    do
        string "RND"
        n <- parens number
        return $ Random n

arithLet = 
    do
        tk $ string "LET"
        v <- tk $ var 
        tk $ char '='
        e <- tk $ arithExpression
        return $ Let v e 

rndLet =
    do
        tk $ string "LET"
        v <- tk $ var 
        tk $ char '='
        e <- tk $ random
        return $ Let v e         

letStatement :: Parser Statement
letStatement = try rndLet <|> arithLet

array :: Parser Statement
array =
    do
        tk $ string "DIM"
        v <- var 
        e <- tk $ parens expression
        return $ Array v e

goto :: Parser Statement
goto =
    do
        tk $ string "GOTO"
        e <- tk $ expression
        return $ Goto e

input :: Parser Statement
input = 
    do
        tk $ string "INPUT"
        exprs <- expressionList 
        return $ Input exprs


{-
Statement parsers.
-}
gosub = do {tk $ string "GOSUB";  e <- expression; return $ Gosub e}
ret   = do {tk $ string "RETURN";  return Return}
clear = do {tk $ string "CLEAR";   return Clear}
list  = do {tk $ string "LIST";    return List}
run   = do {tk $ string "RUN";     return Run}
end   = do {tk $ string "END";     return End}

statement :: Parser Statement
statement = try printStatement <|> 
            try ifStatement    <|> 
            try forLoop        <|>
            try nextIter       <|>
            try array          <|>
            try goto           <|> 
            try input          <|> 
            try letStatement   <|> 
            try gosub          <|>
            try ret            <|>
            try clear          <|>
            try list           <|>
            try run            <|>
            try end

binaryOp :: Parser Expression
binaryOp =
    do
        f1 <- factor
        operator <- op
        f2 <- factor
        return $ ABinary operator f1 f2

expression :: Parser Expression
expression = 
    do
        binOp <- try binaryOp
        return binOp
    <|>
    do
        f <- factor
        return f
    <|>
    do
        e <- tk $ parens expression
        return e

factor :: Parser Expression
factor =
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
        return $ parseBasic fileStr

arithOperators = [ 
                [Infix  (tk $ char '*'   >> return (ABinary Times)) AssocLeft],
                [Infix  (tk $ char '/'   >> return (ABinary Div))   AssocLeft],
                [Infix  (tk $ char '+'   >> return (ABinary Plus))  AssocLeft],
                [Infix  (tk $ char '-'   >> return (ABinary Minus)) AssocLeft]
               ]

arithExpression :: Parser Expression
arithExpression = buildExpressionParser arithOperators expression