
module Main where 

import System.Environment
import BasicParser
import BasicInterpreter hiding (main)

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
                Right a -> do
                    putStrLn "Successfully parsed. Parse output:"
                    putStrLn (showLineList a)
                    putStrLn "----------------"
                    interpretBasic a

        else do
            print "The first argument should be the path to a basic file."