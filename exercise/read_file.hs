import System.IO

main = do
   contents <- readFile "input.txt"
   putStrLn contents
