module Main where
import Scanner
import Parser

-- Define test case
test_double_var = "var var a;"

main :: IO ()
main = do
    let tokens = scanTokens test_double_var
    let program = parse tokens
    putStrLn (show program)