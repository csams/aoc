main = do
    contents <- readFile "input.txt"
    let values = words contents
        result = sum $ map (sumFuel . read) values
    putStrLn $ show result
    where
        toFuel mass = mass `div` 3 - 2
        fuelList mass = if fuel <= 0 then [0] else fuel : fuelList fuel
            where fuel = toFuel mass
        sumFuel = sum . fuelList
