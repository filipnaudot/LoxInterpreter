module Main where
import Scanner

-- Define test case
test_numbers = "123 123.456 .456 123."

main :: IO ()
main = do
    let results = scanTokens test_numbers
    mapM_ (putStrLn . show) results