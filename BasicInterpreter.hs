{-
CS 591: Advanced Declarative Programming
Basic Interpreter
By: Lucas Nunno
-}
module BasicInterpreter where

import BasicParser
import Control.Monad.State
import System.Random
import qualified Data.Sequence as Seq
import qualified Data.Map as Map

type LineNumber = Int
type Binding = (Variable,Int)
type VariableMap = Map.Map Variable Int
data LoopBounds = LoopBounds {
    beginLine :: Int,
    upperBound :: Int
} deriving (Show,Eq)

type BasicLineMap = Map.Map LineNumber LineNumber
type StatementMap = Map.Map LineNumber Statement

data BasicContext = BasicContext {
    varMap :: VariableMap,
    instructionPointer :: LineNumber,
    loopStack :: [LoopBounds],
    lineNumMap :: BasicLineMap,
    stmts :: StatementMap
}

lineToMaps :: [Line] -> (BasicLineMap,StatementMap)
lineToMaps lns = (blMap,statementMap)
    where
        rns         = [1 .. length lns]
        nums        = map lineNumber lns
        stmts       = map lineStatement lns
        blMap       = Map.fromList (zip rns nums)
        statementMap= Map.fromList (zip rns stmts)

push :: Eq a => a -> [a] -> [a]
push a ls = a:ls

pop :: Eq a => [a] -> (a,[a])
pop (x:xs) = (x,xs)

addBinding :: Binding -> Basic
addBinding (v,i) = do
    BasicContext vMap ip lStack lm stmts <- get
    put $ BasicContext (Map.insert v i vMap) ip lStack lm stmts

nextStatement :: Basic
nextStatement = do
    BasicContext vMap ip lStack lm stmts <- get
    put $ BasicContext vMap (ip + 1) lStack lm stmts

initialContext = BasicContext Map.empty 0 [] Map.empty Map.empty

type Basic = StateT BasicContext IO ()

readInt :: IO Int
readInt = readLn

-- Print statement
eval :: Statement -> Basic
eval (Print exprs) = do
    BasicContext vMap _ _ _ _ <- get
    let ps = [case e of
                    (EString s) -> do
                        liftIO $ putStr s 
                    (EVar v)    -> do
                        liftIO $ putStr (show $ vMap Map.! v)
                        | e <- exprs] 
    let nl = [(liftIO $ putStrLn "")] :: [Basic]
    sequence_ (ps ++ nl)
    nextStatement

-- Let statement
eval (Let var (ENum i)) = do
    BasicContext vMap ip lStack lm stmts <- get
    put $ BasicContext (Map.insert var i vMap) (ip + 1) lStack lm stmts

eval (Let var (Random i)) = do
    BasicContext vMap ip lStack lm stmts <- get 
    rVal <- liftIO $ randomRIO (0,i)
    put $ BasicContext (Map.insert var rVal vMap) (ip + 1) lStack lm stmts

eval input@(Input exprs) = do
    bindings <- liftIO $ getInputBindings input
    sequence_ [addBinding b | b <- bindings]
    nextStatement

eval (ForLoop (EVar v) (ENum begin) (ENum end)) = do
    addBinding (v,begin)
    BasicContext vMap ip lStack lm stmts <- get 
    let newLStack = push (LoopBounds (ip+1) end) lStack
    put $ BasicContext vMap (ip + 1) newLStack lm stmts

eval (NextIter (EVar v)) = do
    BasicContext vMap ip lStack lm stmts <- get
    let oldVal = vMap Map.! v 
    let newVal = oldVal + 1
    let newVMap = Map.insert v newVal vMap
    let (LoopBounds loopBeginLine end,xs) = pop lStack
    if newVal > end 
        then do
            put $ BasicContext newVMap (ip + 1) xs lm stmts
        else do
            put $ BasicContext newVMap loopBeginLine lStack lm stmts

getInputBindings :: Statement -> IO [Binding]
getInputBindings (Input exprs) = do
    let strInt = 99999 :: Int
    inLs <- sequence [case e of 
                    (EString s) -> do
                        putStr (s ++ " ")
                        return (Variable "fake", strInt)
                    (EVar v)    -> do
                        i <- readInt
                        return (v,i)
                         | e <- exprs]
    return $ filter (\(_,x) -> x /= strInt) inLs

eval' :: [Statement] -> Basic
eval' stmts = do
    sequence_ (map eval stmts)

{-
Tests 
-}
p1 = [Print [EString "Foo ", EString "Bar"]]
vp1 = [
    Let (Variable "X") (ENum 3),
    Let (Variable "Y") (ENum 5),
    Print [
        EString "X = ", 
        EVar (Variable "X"),
        EString " Y = ",
        EVar (Variable "Y")
        ]
    ]
vp2 = [
    Let (Variable "X") (Random 10),
    Let (Variable "Y") (Random 10),
    Print [
        EString "X = ", 
        EVar (Variable "X"),
        EString " Y = ",
        EVar (Variable "Y")
        ]
    ]

in1 = [
    Input [
        EString "What is X?",
        EVar $ Variable "X",
        EString "What is Y?",
        EVar $ Variable "Y"
        ],
    Print [
        EString "You said X is ", 
        EVar (Variable "X"),
        EString "You said Y is ",
        EVar (Variable "Y")
        ]
    ]

tr a = runStateT a initialContext

testPrint = do
    tr $ eval' p1
    tr $ eval' vp1
    tr $ eval' vp2

testInput = do
    tr $ eval' in1

main = do
    testPrint