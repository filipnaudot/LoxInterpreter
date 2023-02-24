module Main where
import Scanner
import Parser

-- Define test case
test_missing_expr_if = "if() return 1;"

main :: IO ()
main = do
    let tokens = scanTokens test_missing_expr_if
    let program = parse tokens
    putStrLn (show program)