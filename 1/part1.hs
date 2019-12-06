main = do
    contents <- readFile "input.txt"
    let values = words contents
        result = sum $ map (toFuel . read) values
    putStrLn $ show result
    where
        toFuel mass = mass `div` 3 - 2
