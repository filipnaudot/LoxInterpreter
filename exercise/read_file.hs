import System.IO

main = do
   contents <- readFile "input.txt"
   let wordsList = words contents
   scan wordsList

-- This function takes a list of tokes from a file
-- and scannes them
scan [] = return () -- Check for empty list
scan (token:tokens) = do
    putStr token -- Print the token
    putStr " - " -- Print deliminator

    if token == "test" 
        then do
            let firstLetter = head token
            putStr " ("
            putChar firstLetter
            putStr ") "
            putStrLn "aha!"
        else putStrLn "Not test"

    scan tokens -- Next element in list