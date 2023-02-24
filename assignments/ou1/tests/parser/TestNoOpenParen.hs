module Main where
import Scanner
import Parser

-- Define test case
test_missing_open_parentheses = "a+1;)"

main :: IO ()
main = do
    let tokens = scanTokens test_missing_open_parentheses
    let program = parse tokens
    putStrLn (show program)