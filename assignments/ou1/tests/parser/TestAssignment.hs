module Main where
import Scanner
import Parser

-- Define test case
test_assignment = "variable22 = \"hello\"; a = 5*2; b = true; c = false; d = nil; a == b;"

main :: IO ()
main = do
    let tokens = scanTokens test_assignment
    let program = parse tokens
    putStrLn (show program)