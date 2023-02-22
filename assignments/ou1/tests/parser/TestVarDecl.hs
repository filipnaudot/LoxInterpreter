module Main where
import Scanner
import Parser

-- Define test case
test_var_declaration = "var a = 1; var b = true; var c = false; var str     = \"hello\"; var d=(2/7); var empty;"

main :: IO ()
main = do
    let tokens = scanTokens test_var_declaration
    let program = parse tokens
    putStrLn (show program)