import System.IO

main = do
   contents <- readFile "input.txt"
   let wordsList = words contents
   --mapM putStrLn wordsList
   scan wordsList

scan [] = return ()
scan (token:tokens) = do
    putStr token
    putStr " - "
    if token == "test"
        then putStrLn "aha!"
        else putStrLn "Not test"
    scan tokens