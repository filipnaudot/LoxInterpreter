module Main where
import Scanner
import Parser

-- Define test case
test_comparison = "a > b; a >= b; a == b; a < b; a <= b;"

main :: IO ()
main = do
    let tokens = scanTokens test_comparison
    let program = parse tokens
    putStrLn (show program)