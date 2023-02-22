module Main where
import Scanner
import Parser

-- Define test case
test_expr = "5*2; true and false; -2; !true; (2==3); 2+3*5; d=(2+2)+2*2; _e =         2+2+2*2;"

main :: IO ()
main = do
    let tokens = scanTokens test_expr
    let program = parse tokens
    putStrLn (show program)