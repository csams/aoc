import Control.Monad.State
import Data.Map.Strict as M
import Data.List (break, stripPrefix)

loadEdges l = let (p, v) = break (==')') l in (tail v, p)

getPath t cat = getParent t []
    where getParent b acc =
            case M.lookup b cat of
                Nothing -> acc
                Just p  -> getParent p (p:acc)

commonPrefix _ [] = []
commonPrefix [] _ = []
commonPrefix (x:xs) (y:ys)
    | x == y    = x : commonPrefix xs ys
    | otherwise = []

main = do
    contents <- readFile "input.txt"
    let edges = loadEdges <$> lines contents
        catalog = M.fromList edges
        santa = getPath "SAN" catalog
        you = getPath "YOU" catalog
        common = length $ commonPrefix santa you
        dist = ((length you) - common) + ((length santa) - common)
    putStrLn $ show dist
