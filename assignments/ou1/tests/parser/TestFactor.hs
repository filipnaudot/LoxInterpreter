module Main where
import Scanner
import Parser

-- Define test case
test_factor = "1*2*3*4; 1/2/3/4; (1/2)/3; (1*1)/1; a/b*1; 1/a*b*1/1*b/c*1.5;"

main :: IO ()
main = do
    let tokens = scanTokens test_factor
    let program = parse tokens
    putStrLn (show program)