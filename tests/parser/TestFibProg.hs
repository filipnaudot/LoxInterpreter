module Main where
import Scanner
import Parser

-- Define test case
test_fibonacci_program = "var a = 0; \n var temp; \n var b = 1; \n while (a < 10000) {print a; temp = a; a = b; b = temp + b;}"

main :: IO ()
main = do
    let tokens = scanTokens test_fibonacci_program
    let program = parse tokens
    putStrLn (show program)