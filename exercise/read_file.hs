import System.IO

main = do
   contents <- readFile "input.txt"
   let file_lines = lines contents
   parse_line file_lines 1


-- scan - takes a list of lines from a file and scannes them
parse_line [] line_num = return () -- Check for empty list
parse_line (line:lines) line_num = do
    putStr ((show line_num) ++ ": ") -- print line number

    let wordsList = words line
    mapM_ (\n -> scan_token n line_num) wordsList

    putStrLn "" -- new line
    parse_line lines (line_num + 1)


-- scan_token - takes a token and scans it
scan_token token line_num = do
    putStr (token ++ " ") -- Print the token

    if token == "test"
        then do
            print_error ("Not allowed to write '" ++ token ++ "'. ") line_num
        else return ()


-- print_error - prints an error message
print_error error_message line_num = do
    putStrLn ("\nError on line " ++ (show line_num) ++ ": " ++ error_message)