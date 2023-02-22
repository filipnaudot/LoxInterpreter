module Main where
import Scanner

-- Define test case
test_keywords = "and class else false for fun if nil or return super this true var while"

main :: IO ()
main = do
    let results = scanTokens test_keywords
    mapM_ (putStrLn . show) results