module Scanner (scanTokens) where

import Data.Maybe
import Data.Char
import Tokens           -- Import data types.
import Debug.Trace      -- For debug prints.

-- Example of a token list with one token: [TOKEN LEFT_PAREN "test str" (ID "str_id") 1]
scanTokens :: [Char] -> [Token]
scanTokens str =
    trace "Start" $
    let tokens = scan str 0
    in trace "End" tokens

scan :: [Char] -> Int -> [Token]
scan [] _ = []
scan (x:xs) lineNumber =
    case x of
        '(' -> (TOKEN LEFT_PAREN "(" NONE lineNumber) : scan xs (lineNumber + 1)
        ')' -> (TOKEN RIGHT_PAREN ")" NONE lineNumber) : scan xs (lineNumber + 1)
        '{' -> (TOKEN LEFT_BRACE "{" NONE lineNumber) : scan xs (lineNumber + 1)
        '}' -> (TOKEN RIGHT_BRACE "}" NONE lineNumber) : scan xs (lineNumber + 1)
        ',' -> (TOKEN COMMA "," NONE lineNumber) : scan xs (lineNumber + 1)
        '.' -> (TOKEN DOT "." NONE lineNumber) : scan xs (lineNumber + 1)
        '-' -> (TOKEN MINUS "-" NONE lineNumber) : scan xs (lineNumber + 1)
        '+' -> (TOKEN PLUS "+" NONE lineNumber) : scan xs (lineNumber + 1)
        ';' -> (TOKEN SEMICOLON ";" NONE lineNumber) : scan xs (lineNumber + 1)
        '/' -> (TOKEN SLASH "/" NONE lineNumber) : scan xs (lineNumber + 1)
        '*' -> (TOKEN STAR "*" NONE lineNumber) : scan xs (lineNumber + 1)
        --'!' -> if (head xs) == '=' then (TOKEN BANG_EQUAL "!=" NONE lineNumber) : scan (tail xs) (lineNumber + 1) else (TOKEN BANG "!" NONE lineNumber)
