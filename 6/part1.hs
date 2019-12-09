import Data.Map.Strict as M
import Data.List (break)

loadEdges l = let (p, v) = break (==')') l in (tail v, p)

main = do
    contents <- readFile "input.txt"
    let edges = loadEdges <$> lines contents
        catalog = M.fromList edges
        direct = (size catalog)
        indirect = M.foldr (\p acc -> acc + (calcIndirect p)) 0 catalog
        calcIndirect k = cnt k 0
            where
                cnt n acc = 
                    case M.lookup n catalog of
                        Nothing -> acc
                        Just v  -> cnt v (acc + 1)
    putStrLn $ show $ direct + indirect
