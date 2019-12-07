alwaysInc (x:y:xs) = if x <= y then alwaysInc (y:xs) else False
alwaysInc _ = True

sameAdj (x:y:xs) = if x == y then True else sameAdj (y:xs)
sameAdj _ = False

main = do
    let possible = show <$> [264793 .. 803935]
    putStrLn $ show $ length [x | x <- possible, alwaysInc x && sameAdj x]
