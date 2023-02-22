module Main where
import Scanner

-- Define test case
test_strings = " \"string\"   \n    \"second row \n third row\"    "

main :: IO ()
main = do
    let results = scanTokens test_strings
    mapM_ (putStrLn . show) results