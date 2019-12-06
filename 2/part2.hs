import Data.Sequence (fromList, Seq, index, update)
import Data.Text (pack, splitOn, strip, Text)
import Data.Text.Read (decimal)
import Data.List (find)
import Control.Monad (sequence)
import Control.Monad.State

type Program = Seq Int
type InstPtr = Int
type Process = (Program, InstPtr)

type Noun    = Int
type Verb    = Int
type Target  = Int

data Op = Add Noun Verb Target
        | Mul Noun Verb Target
        | Halt deriving Show

parse contents = fromList <$> result
    where 
        result = sequence $ toInt <$> splitComma contents
        toInt x = fst <$> (decimal . strip) x
        splitComma x = splitOn (pack ",") x

decode prog ip =
    let op = index prog ip in
        case op of
            1  -> Add (load 1) (load 2) (fetch 3)
            2  -> Mul (load 1) (load 2) (fetch 3)
            99 -> Halt
    where load x = let i = fetch x in index prog i
          fetch x = index prog (ip + x)

runComputer = do
    (prog, ip) <- get
    case decode prog ip of
        (Add a b c) -> put (update c (a + b) prog, ip + 4) >> runComputer
        (Mul a b c) -> put (update c (a * b) prog, ip + 4) >> runComputer
        Halt        -> return (index prog 0)

evaluate prog = evalState runComputer startState
    where startState = (prog, 0)

run prog noun verb =
    let t = update 1 noun prog
        p = update 2 verb t
    in
        evaluate p

runSim program trials target = find (\(noun, verb) -> (run program noun verb) == target) trials

main = do
    contents <- readFile "input.txt"
    let program = parse $ pack contents
        trials = [(x,y) | x <- [0..99], y <- [0..99]]
    case program of
        (Left x) -> putStrLn x
        (Right prog) -> putStrLn $ 
            case (runSim prog trials 19690720) of
                Just (noun, verb) -> show (100 * noun + verb)
                Nothing -> "No good combo."
