module Main where
import Scanner
import Parser

-- Define test case
test_leteral = "true; false; 5; 5.111; a; _ab; nil; \"string\";"

main :: IO ()
main = do
    let tokens = scanTokens test_leteral
    let program = parse tokens
    putStrLn (show program)