{-
CS 591: Advanced Declarative Programming
Basic Interpreter
By: Lucas Nunno
-}
module BasicInterpreter where

import BasicParser
import System.Random
import Control.Monad.State
import qualified Data.Map as Map

type Binding = (Variable,Int)

type Context = Map.Map Variable Int

statefulInsert :: Variable -> Int -> State Context ()  
statefulInsert k v = state $ \xs -> ((),Map.insert k v xs) 

ctxStateEx :: State Context ()
ctxStateEx = do
    ctx <- get
    statefulInsert (Variable "X") 5
    statefulInsert (Variable "Y") 3

evalLet :: Statement -> State Context ()
evalLet (Let var (ENum i)) = do
    statefulInsert var i

evalNext :: Statement -> State Context ()
evalNext (NextIter (EVar v)) = do
    ctx <- get 
    let previousValue = ctx Map.! v
    statefulInsert v (previousValue+1)

evalExpr :: Expression -> Int
evalExpr (ENum i) = i
    
readInt :: IO Int
readInt = readLn

putStrSp a = putStr (a ++ " ") 

evalPrint :: Context -> Statement -> IO ()
evalPrint ctx (Print exprs) = do
    sequence_ [case e of
                    (EString s) -> do
                        putStrSp s 
                    (EVar v)    -> do
                        putStrSp (show $ ctx Map.! v)
                        | e <- exprs]
    putStrLn ""

evalInput :: Statement -> IO [Binding]
evalInput (Input exprs) = do
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

evalRand :: Expression -> IO Int
evalRand (Random i) = randomRIO (0,i)