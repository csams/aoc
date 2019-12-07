import Data.List

alwaysInc (x:y:xs) = if x <= y then alwaysInc (y:xs) else False
alwaysInc _ = True

sameAdj x = 2 `elem` (length <$> group x)

main = do
    let nums = show <$> [264793 .. 803935]
        matches = [x | x <- nums, alwaysInc x && sameAdj x]
    putStrLn $ show $ length matches
