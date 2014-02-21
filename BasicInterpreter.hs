{-
CS 591: Advanced Declarative Programming
Basic Interpreter
By: Lucas Nunno
-}
module BasicInterpreter where

import BasicParser
import System.Random
import qualified Data.Map as Map

type Binding = (Variable,Int)

type Context = Map.Map Variable Int

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