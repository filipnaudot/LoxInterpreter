module Main where
import Scanner
import Parser

-- Define test case
test_and = "(a and b); a and b and c and d;"

main :: IO ()
main = do
    let tokens = scanTokens test_and
    let program = parse tokens
    putStrLn (show program)