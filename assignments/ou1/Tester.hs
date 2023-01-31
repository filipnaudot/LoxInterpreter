module Main where

import Scanner
import Data.List



-- Define test cases
test_identifiers = "andy formless fo _ _123 _abc ab123 abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_"
test_keywords = "and class else false for fun if nil or return super this true var while"
test_numbers = "123 123.456 .456 123."
test_punctuators = "(){};,+-*!===<=>=!=<>/."
test_strings = " \"string\"   \n    \"second row \n third row\"    "

-- Create a list of all test cases
tests = [test_identifiers, test_keywords, test_numbers, test_punctuators, test_strings]

main :: IO ()
main = do
    let results = map scanTokens tests
    mapM_ (putStrLn . show) (concat results)