module Main where

import System.Environment
import System.IO

import Scanner
import Parser




main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> do
            contents <- readFile filename
            let tokens = scanTokens contents
            let program = parse tokens
            putStrLn (show program)
        _ -> putStrLn "Usage: lox <filename.lox>"
