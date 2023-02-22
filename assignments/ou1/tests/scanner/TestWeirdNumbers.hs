module Main where
import Scanner

-- Define test case
test_numbers = "1.1.1 1...1 11."

main :: IO ()
main = do
    let results = scanTokens test_numbers
    mapM_ (putStrLn . show) results