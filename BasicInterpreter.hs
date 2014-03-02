module BasicInterpreter where

import BasicParser
import Control.Monad.State
import System.Random
import qualified Data.Sequence as Seq
import qualified Data.Map as Map

type LineNumber = Int
type Binding = (Variable,Int)
type VariableMap = Map.Map Variable Int

data BasicContext = BasicContext {
    varMap :: VariableMap,
    instructionPointer :: LineNumber,
    loopStack :: [LineNumber]
}

initialContext = BasicContext Map.empty 0 []

type Basic = StateT BasicContext IO ()

putStrSp a = putStr (a ++ " ") 

-- Print statement
eval :: Statement -> Basic
eval (Print exprs) = do
    BasicContext vMap _ _ <- get
    let ps = [case e of
                    (EString s) -> do
                        liftIO $ putStrSp s 
                    (EVar v)    -> do
                        liftIO $ putStrSp (show $ vMap Map.! v)
                        | e <- exprs] 
    let nl = [(liftIO $ putStrLn "")] :: [Basic]
    sequence_ (ps ++ nl)

-- Let statement
eval (Let var (ENum i)) = do
    BasicContext vMap ip ls <- get
    put $ BasicContext (Map.insert var i vMap) ip ls

eval (Let var (Random i)) = do
    BasicContext vMap ip ls <- get 
    rVal <- liftIO $ randomRIO (0,i)
    put $ BasicContext (Map.insert var rVal vMap) ip ls


eval' :: [Statement] -> Basic
eval' stmts = do
    sequence_ (map eval stmts)

{-
Tests 
-}
p1 = [Print [EString "Foo", EString "Bar"]]
vp1 = [
    Let (Variable "X") (ENum 3),
    Let (Variable "Y") (ENum 5),
    Print [
        EString "X =", 
        EVar (Variable "X"),
        EString "Y =",
        EVar (Variable "Y")
        ]
    ]
vp2 = [
    Let (Variable "X") (Random 10),
    Let (Variable "Y") (Random 10),
    Print [
        EString "X =", 
        EVar (Variable "X"),
        EString "Y =",
        EVar (Variable "Y")
        ]
    ]

tr a = runStateT a initialContext

testPrint = do
    tr $ eval' p1
    tr $ eval' vp1
    tr $ eval' vp2

main = do
    testPrint