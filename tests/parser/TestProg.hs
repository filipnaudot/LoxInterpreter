module Main where
import Scanner
import Parser

-- Define test case
test_program = "if ( a < 5 ) { print g; 88; } else { if (false) { while (a=5) return; } }"

main :: IO ()
main = do
    let tokens = scanTokens test_program
    let program = parse tokens
    putStrLn (show program)