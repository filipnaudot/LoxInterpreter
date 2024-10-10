module Main where
import Scanner
import Parser

-- Define test case
test_stmt = "if (a) 5*2; if (a) {5*2;} if (a) 5*2; else 99; if (a){5*2;}else{ 99;} { 5; 7; 9; } {} while(1) print a; while(1) {print a;}"

main :: IO ()
main = do
    let tokens = scanTokens test_stmt
    let program = parse tokens
    putStrLn (show program)