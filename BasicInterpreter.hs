{-
CS 591: Advanced Declarative Programming
Basic Interpreter
By: Lucas Nunno
-}
module BasicInterpreter where

import BasicParser

type Binding = (Variable,Int)

readInt :: IO Int
readInt = readLn

readInput :: Statement -> IO [Binding]
readInput (Input exprs) = do
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