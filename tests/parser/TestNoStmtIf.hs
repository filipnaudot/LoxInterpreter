module Main where
import Scanner
import Parser

-- Define test case
test_missing_statement_after_if = "if(1)"

main :: IO ()
main = do
    let tokens = scanTokens test_missing_statement_after_if
    let program = parse tokens
    putStrLn (show program)