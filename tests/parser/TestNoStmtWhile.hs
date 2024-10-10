module Main where
import Scanner
import Parser

-- Define test case
test_missing_statement_after_while = "while(1);"

main :: IO ()
main = do
    let tokens = scanTokens test_missing_statement_after_while
    let program = parse tokens
    putStrLn (show program)