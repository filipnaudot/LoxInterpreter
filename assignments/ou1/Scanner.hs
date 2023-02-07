module Scanner (scanTokens) where

import qualified Data.Map as Map
import Data.Maybe
import Data.Char
import Tokens           -- Import data types.
import Debug.Trace      -- For debug prints.


-- Define a global keyword-map.
keywords = Map.fromList [
                        ("and",    (AND, NONE)),
                        ("class",  (CLASS, NONE)),
                        ("else",   (ELSE, NONE)),
                        ("false",  (FALSE, FALSE_LIT)),
                        ("for",    (FOR, NONE)),
                        ("fun",    (FUN, NONE)),
                        ("if",     (IF, NONE)),
                        ("nil",    (NIL, NIL_LIT)),
                        ("or",     (OR, NONE)),
                        ("print",  (PRINT, NONE)),
                        ("return", (RETURN, NONE)),
                        ("super",  (SUPER, NONE)),
                        ("this",   (THIS, NONE)),
                        ("true",   (TRUE, TRUE_LIT)),
                        ("var",    (VAR, NONE)),
                        ("while",  (WHILE, NONE))]


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
                then let (token, rest) = number (x:xs) lineNumber
                    in token : (scan rest lineNumber)
                else if isValidAlpha x
                    then let (token, rest) = identifier (x:xs) lineNumber
                        in token : (scan rest lineNumber)
                else if x == '"'
                    then let (token, rest, lineNum) = string xs lineNumber
                        in token : (scan rest lineNum)
                else error ("\n\nUnexpected character at line " ++ (show lineNumber) ++ ": " ++ [x] ++ "\n\n")


-- number  - Extracts a number from a given "lox-string".
--
-- Input:
--  [Char] - The "lox-string" containing the number.
--  Int    - The line number to which the input belongs.
-- Returns a tuple containing the token
--         for the Float number and the remaining "lox-string".
number :: [Char] -> Int -> (Token, [Char])
number inputString lineNumber = 
    let (numString, rest) = span (\c -> isDigit c) inputString
        (numStringWithDot, restAfterDot) = if not (null rest) && (head rest) == '.'
                            then let (numString2, rest2) = span (\c -> isDigit c) (tail rest)
                                 in if null numString2
                                    then (numString, rest)
                                    else (numString ++ "." ++ numString2, dropWhile (== '.') rest2)
                            else (numString, rest)
        numValue = read numStringWithDot :: Float
    in (TOKEN NUMBER numStringWithDot (NUM numValue) lineNumber, restAfterDot)




-- identifier  - Extracts an identifier from a given "lox-string".
--               
-- Input:
--  [Char] - The "lox-string" containing the identifier.
--  Int    - The line number to which the input belongs.
-- Returns a tuple containing the token
--         for the identifier and the remaining "lox-string".
identifier :: [Char] -> Int -> (Token, [Char])
identifier inputString lineNumber = 
    -- Check for alphanumeric identifier
    let (charString, rest) = span (\c -> isDigit c || isValidAlpha c) inputString
    -- Check if it is a keyword or IDENTIFIER
    in case Map.lookup charString keywords of
        Just (keywordType, literal) -> (TOKEN keywordType charString literal lineNumber, rest)
        Nothing -> (TOKEN IDENTIFIER charString (ID charString) lineNumber, rest)



-- string  - Extracts a sub-string from a given "lox-string".
--               
-- Input:
--  [Char] - The "lox-string" containing the sub-string.
--  Int    - The line number to which the input belongs.
-- Returns a tuple of 3 values containing the token
--         for the sub-string, the remaining "lox-string" and
--         the updated line number, taking into account any newline
--         characters within the sub-string.
string :: [Char] -> Int -> (Token, [Char], Int)
string inputString lineNumber =
    let (subString, rest) = span (\c -> c /= '"') inputString
        lineNum = lineNumber + length (filter (== '\n') subString)

    in if (null rest) || (head rest == '\0') then
        error ("\n\nUnterminated string on line " ++ (show lineNumber) ++ "\n\n")
    else
        ((TOKEN STRING subString (STR subString) lineNumber), tail rest, lineNum)


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


-- isValidAlpha - Checks if a given character is a valid character in
--                the scope of this program.
--
-- Input:
--  Char        - The character to inspect.
-- Returns True if valid character, else false.
isValidAlpha :: Char -> Bool
isValidAlpha character = 
    isAlpha character || character == '_'