module Main where
import Scanner

-- Define test case
test_punctuators = "(){};,+-*!===<=>=!=<>/."

main :: IO ()
main = do
    let results = scanTokens test_punctuators
    mapM_ (putStrLn . show) results