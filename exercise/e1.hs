

main = do
    putStrLn "Enter a number: "
    n <- getLine
    let num = double (read n :: Integer)
    putStrLn "The double is: "
    print num
    


double x = x + x



