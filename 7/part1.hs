import Data.List (permutations)
import Control.Monad.State
import Computer

runAmp input memory =
    let process = Process memory 0 input []
    in evalStateT runProcess process

runStack phases initial memory = do
    res <- foldM (\inp p -> runAmp ([p] ++ inp) memory) initial phases
    return res

main = do
    contents <- readFile "input.txt"
    let memory = loadCode contents
        allPhases = permutations [0..4]
    vals <- forM allPhases $ (\p -> runStack p [0] memory)
    putStrLn $ show $ maximum vals
