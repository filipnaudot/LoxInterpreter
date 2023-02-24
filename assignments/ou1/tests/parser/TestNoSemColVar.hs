module Main where
import Scanner
import Parser

-- Define test case
test_missing_semicolon_after_vardec = "var a"

main :: IO ()
main = do
    let tokens = scanTokens test_missing_semicolon_after_vardec
    let program = parse tokens
    putStrLn (show program)