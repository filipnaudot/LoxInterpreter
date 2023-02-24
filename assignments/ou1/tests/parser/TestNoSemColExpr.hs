module Main where
import Scanner
import Parser

-- Define test case
test_missing_semicolon_after_expr = "a+1"

main :: IO ()
main = do
    let tokens = scanTokens test_missing_semicolon_after_expr
    let program = parse tokens
    putStrLn (show program)