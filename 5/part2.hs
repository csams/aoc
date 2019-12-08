import Control.Monad.State
import Data.List (repeat)
import Data.Sequence (fromList, Seq, index, update)
import Data.Text (pack, splitOn, strip, Text, unpack)

data Mode = Position | Immediate deriving Show
data Param = Param Mode Int deriving Show

type Memory  = Seq Int
type Noun    = Param
type Verb    = Param
type Target  = Param

data Process = Process { memory :: Memory, ip :: Int } deriving Show

data Op = Add Noun Verb Target
        | Mul Noun Verb Target
        | Input Target
        | Output Param
        | JumpIfTrue Param Param
        | JumpIfFalse Param Param
        | LessThan Param Param Target
        | Equals Param Param Target
        | Halt
        | Error String
        deriving Show

toMode '0' = Position
toMode '1' = Immediate

-- parses program text into a sequence of ints
loadCode :: Text -> Memory
loadCode = fromList . (toInt <$>) . splitComma
    where 
        toInt = read . unpack . strip
        splitComma = splitOn $ pack ","

-- fetch a value at the given memory location
fetch :: Int -> StateT Process IO Int
fetch loc = do
    Process memory _ <- get
    return $ index memory loc

-- fetch a parameter's value according to its mode
fetchParam :: Param -> StateT Process IO Int
fetchParam (Param Immediate val) = return val
fetchParam (Param Position val) = fetch val

-- construct num parameters starting at ip + 1 according to
-- the modes in the mode string.
readParams :: Int -> String -> StateT Process IO [Param]
readParams num modes = do
    let flags = take num $ modes ++ repeat '0'
        toParam (f, v) = Param (toMode f) v
    Process _ ip <- get
    vals <- forM [1..num] (\i -> fetch $ ip + i)
    return $ toParam <$> zip flags vals

-- Return the decoded opcode at the current instruction pointer
decode :: StateT Process IO Op
decode = do
    Process _ ip <- get
    inst <- fetch ip
    let (flags, op) = inst `divMod` 100
        modes = reverse $ show flags
    case op of
        99 -> return Halt
        1  -> do { (p0:p1:p2:_) <- readParams 3 modes; return (Add p0 p1 p2) }
        2  -> do { (p0:p1:p2:_) <- readParams 3 modes; return (Mul p0 p1 p2) }
        3  -> do { (p:_)        <- readParams 1 modes; return (Input p) }
        4  -> do { (p:_)        <- readParams 1 modes; return (Output p) }
        5  -> do { (p0:p1:_)    <- readParams 2 modes; return (JumpIfTrue p0 p1) }
        6  -> do { (p0:p1:_)    <- readParams 2 modes; return (JumpIfFalse p0 p1) }
        7  -> do { (p0:p1:p2:_) <- readParams 3 modes; return (LessThan p0 p1 p2) }
        8  -> do { (p0:p1:p2:_) <- readParams 3 modes; return (Equals p0 p1 p2) }
        otherwise  -> return (Error $ show op)

-- opcode interpreters
add :: Param -> Param -> Target -> StateT Process IO ()
add p0 p1 (Param _ loc) = do
    a <- fetchParam p0
    b <- fetchParam p1
    Process prog ip <- get
    put $ Process (update loc (a + b) prog) (ip + 4)

mul :: Param -> Param -> Target -> StateT Process IO ()
mul p0 p1 (Param _ loc) = do
    a <- fetchParam p0
    b <- fetchParam p1
    Process prog ip <- get
    put $ Process (update loc (a * b) prog) (ip + 4)

input :: Target -> StateT Process IO ()
input (Param _ loc) = do
    val <- read <$> liftIO getLine
    Process memory ip <- get
    put $ Process (update loc val memory) (ip + 2)

output :: Param -> StateT Process IO ()
output p = do
    val <- fetchParam p
    liftIO $ putStrLn $ show val
    Process memory ip <- get
    put $ Process memory (ip + 2)

jumpIfTrue :: Param -> Param -> StateT Process IO ()
jumpIfTrue p0 p1 = do
    a <- fetchParam p0
    b <- fetchParam p1
    Process memory ip <- get
    put $ Process memory (if a /= 0 then b else (ip + 3))

jumpIfFalse :: Param -> Param -> StateT Process IO ()
jumpIfFalse p0 p1 = do
    a <- fetchParam p0
    b <- fetchParam p1
    Process memory ip <- get
    put $ Process memory (if a == 0 then b else (ip + 3))

lessThan :: Param -> Param -> Target -> StateT Process IO ()
lessThan p0 p1 (Param _ loc) = do
    a <- fetchParam p0
    b <- fetchParam p1
    Process memory ip <- get
    let res = if a < b then 1 else 0
    put $ Process (update loc res memory) (ip + 4)

equals :: Param -> Param -> Target -> StateT Process IO ()
equals p0 p1 (Param _ loc) = do
    a <- fetchParam p0
    b <- fetchParam p1
    Process memory ip <- get
    let res = if a == b then 1 else 0
    put $ Process (update loc res memory) (ip + 4)

-- run the Process
runProcess :: StateT Process IO ()
runProcess = do
    op <- decode
    case op of
        (Add p0 p1 p2)      -> do { add p0 p1 p2; runProcess }
        (Mul p0 p1 p2)      -> do { mul p0 p1 p2; runProcess }
        (Input p0)          -> do { input p0; runProcess }
        (Output p0)         -> do { output p0; runProcess }
        (JumpIfTrue p0 p1)  -> do { jumpIfTrue p0 p1; runProcess }
        (JumpIfFalse p0 p1) -> do { jumpIfFalse p0 p1; runProcess }
        (LessThan p0 p1 p2) -> do { lessThan p0 p1 p2; runProcess }
        (Equals p0 p1 p2)   -> do { equals  p0 p1 p2; runProcess }
        Halt                -> return ()
        (Error e)           -> do Process _ ip <- get
                                  let msg = "Error with op " ++ e ++ " at " ++ show ip
                                  liftIO $ putStrLn msg

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let memory = loadCode $ pack contents
    evalStateT runProcess $ Process memory 0
