import System.IO

main = do
   contents <- readFile "input.txt"
   let wordsList = words contents
   mapM_ scan_token wordsList
   --scan wordsList


-- scan_token - takes a token and scannes it
scan_token token = do
    putStr token -- Print the token
    putStr " - " -- Print deliminator

    if token == "test"
        then do
            print_error ("Not allowed to write '" ++ token ++ "'. ")
        else putStrLn "OK"


-- print_error - prints a error message
print_error error_message = do
    putStrLn ("(ERROR) " ++ error_message)






-- scan - takes a list of tokes from a file
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