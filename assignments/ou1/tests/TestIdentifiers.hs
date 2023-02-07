module Main where
import Scanner

-- Define test case
test_identifiers = "andy formless fo _ _123 _abc ab123 abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_"

main :: IO ()
main = do
    let results = scanTokens test_identifiers
    mapM_ (putStrLn . show) results