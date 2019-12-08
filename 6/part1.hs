import Control.Monad.State
import Data.Map.Strict as M
import Data.List (break, stripPrefix)

loadBody l = let (p, v) = break (==')') l in (tail v, p)

main = do
    contents <- readFile "input.txt"
    let edges = loadBody <$> lines contents
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
