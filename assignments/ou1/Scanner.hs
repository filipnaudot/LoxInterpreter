module Scanner (scanTokens) where

import Data.Maybe
import Data.Char
import Tokens           -- Import data types.
import Debug.Trace      -- For debug prints.




-- scanTokens - Initialize the scanning by running
--              the scan function with the given
--              "lox-string" and line number 0.
-- Input:
--  [Char]    - The "lox-string" to parse.
-- Returns a list of lox lexemes.
scanTokens :: [Char] -> [Token]
scanTokens str = scan str 1


-- scan    - Scans a given "lox-string" character by character
--           recursively and creates a list of tokens.
-- Input:
--  [Char] - The "lox-string" to parse.
--  Int    - The line number to which the character belongs.
-- Returns a list of lox lexemes.
scan :: [Char] -> Int -> [Token]
scan [] lineNumber = [TOKEN EOF "" NONE lineNumber]
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

        _   -> if isDigit x
                then let (token, remaining) = number (x:xs) lineNumber
                    in token : (scan remaining lineNumber)
                else error ("\n\nUnexpected character at line " ++ (show lineNumber) ++ ": " ++ [x] ++ "\n\n")


number :: String -> Int -> (Token, String)
number input lineNumber = 
    let (num, remaining) = span isDigit input
        num' = if not (null remaining) && (head remaining == '.') 
                  then num ++ (takeWhile isDigit (tail remaining))
                  else num
    in ((TOKEN NUMBER (show (read num' :: Double)) NONE lineNumber), remaining)



-- removeComment - Removes a comment from a given string, a comment
--                 goes until the end of the line or string.
-- Input:
--  [Char]       - The string containing the comment.
-- Returns the remaining string after the comment.
removeComment :: [Char] -> [Char]
removeComment "" = ""
removeComment (x:xs)
    | x == '\n' = xs
    | otherwise = removeComment xs


-- isWhiteSpace - Checks if a given character is a white-space.
--
-- Input:
--  Char        - The character to inspect.
-- Returns True if white-space, else false
isWhiteSpace :: Char -> Bool
isWhiteSpace character = character `elem` [' ', '\r', '\t', '\n']