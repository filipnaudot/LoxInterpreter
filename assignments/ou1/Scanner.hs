module Scanner (scanTokens) where

import Data.Maybe
import Data.Char
import Tokens           -- Import data types.
import Debug.Trace      -- For debug prints.

-- Example of a token list with one token: [TOKEN LEFT_PAREN "test str" (ID "str_id") 1]
scanTokens :: [Char] -> [Token]
scanTokens str = scan str 0

scan :: [Char] -> Int -> [Token]
scan [] _ = []
scan (x:xs) lineNumber =
    case x of
        -- Ignore whitespace.
        ' ' -> scan xs lineNumber
        '\r' -> scan xs lineNumber
        '\t' -> scan xs lineNumber
        '\n' -> scan xs (lineNumber + 1) 

        '(' -> (TOKEN LEFT_PAREN "(" NONE lineNumber) : scan xs (lineNumber)
        ')' -> (TOKEN RIGHT_PAREN ")" NONE lineNumber) : scan xs (lineNumber)
        '{' -> (TOKEN LEFT_BRACE "{" NONE lineNumber) : scan xs (lineNumber)
        '}' -> (TOKEN RIGHT_BRACE "}" NONE lineNumber) : scan xs (lineNumber)
        ',' -> (TOKEN COMMA "," NONE lineNumber) : scan xs (lineNumber)
        '.' -> (TOKEN DOT "." NONE lineNumber) : scan xs (lineNumber)
        '-' -> (TOKEN MINUS "-" NONE lineNumber) : scan xs (lineNumber)
        '+' -> (TOKEN PLUS "+" NONE lineNumber) : scan xs (lineNumber)
        ';' -> (TOKEN SEMICOLON ";" NONE lineNumber) : scan xs (lineNumber)
        '*' -> (TOKEN STAR "*" NONE lineNumber) : scan xs (lineNumber)

        -- Check for "!" and potentially "!="
        --'!' -> if (not (null xs) && not (isWhiteSpace (xs !! 0)))
        '!' -> if (not (null xs) && ((xs !! 0) == '='))
                    then (TOKEN BANG_EQUAL "!=" NONE lineNumber) : scan (tail xs) (lineNumber) 
                    else (TOKEN BANG "!" NONE lineNumber) : scan xs (lineNumber)
        
        -- Check for "=" and potentially "=="
        '=' -> if (not (null xs) && ((xs !! 0) == '='))
                    then (TOKEN EQUAL_EQUAL "==" NONE lineNumber) : scan (tail xs) (lineNumber) 
                    else (TOKEN EQUAL "=" NONE lineNumber) : scan xs (lineNumber) 

        -- Check for "<" and potentially "<="
        '<' -> if (not (null xs) && ((xs !! 0) == '='))
                    then (TOKEN LESS_EQUAL "<=" NONE lineNumber) : scan (tail xs) (lineNumber) 
                    else (TOKEN LESS "<" NONE lineNumber) : scan xs (lineNumber)

        -- Check for ">" and potentially ">="
        '>' -> if (not (null xs) && ((xs !! 0) == '='))
                    then (TOKEN GREATER_EQUAL ">=" NONE lineNumber) : scan (tail xs) (lineNumber) 
                    else (TOKEN GREATER ">" NONE lineNumber) : scan xs (lineNumber)
        
        -- Check for "/" and potentially "//" (comment)
        '/' -> if (not (null xs) && ((xs !! 0) == '/'))
                    then scan (removeComment (tail xs)) (lineNumber + 1)
                    else (TOKEN SLASH "/" NONE lineNumber) : scan xs (lineNumber)
    
        
        _   -> error ("\n\nUnexpected character at line " ++ (show lineNumber) ++ ": " ++ [x] ++ "\n\n")


removeComment :: String -> String
removeComment "" = ""
removeComment (x:xs)
    | x == '\n' = xs
    | otherwise = removeComment xs


-- Checks if a given character is a white-space
-- Returns True if white-space, else false
isWhiteSpace :: Char -> Bool
isWhiteSpace character = character `elem` [' ', '\r', '\t', '\n']