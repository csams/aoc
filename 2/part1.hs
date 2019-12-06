import Data.Sequence (fromList, Seq, index, update)
import Data.Text (pack, splitOn, strip, Text)
import Data.Text.Read (decimal)
import Control.Monad (sequence)
import Control.Monad.State

type Program = Seq Int
type InstPtr = Int
type Process = (Program, InstPtr)

data Op = Add Int Int Int
        | Mul Int Int Int
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

evaluate (Left x) = x
evaluate (Right prog) = show $ evalState runComputer startState
    where startState = (prog, 0)

main = do
    contents <- readFile "input.txt"
    putStrLn $ evaluate $ parse $ pack contents
