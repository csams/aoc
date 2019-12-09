module Computer where

import Control.Monad.State
import Data.List (repeat)
import Data.Sequence (fromList, Seq, index, update)
import Data.Text (pack, splitOn, strip, unpack)

data Mode = Position | Immediate deriving Show
data Param = Param Mode Int deriving Show

type Memory  = Seq Int
type Noun    = Param
type Verb    = Param
type Target  = Param

data Process = Process { memory :: Memory
                       , ip :: Int 
                       , inBuf :: [Int]
                       , outBuf :: [Int]
                       } deriving Show

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
loadCode :: String -> Memory
loadCode = fromList . (toInt <$>) . splitComma . pack
    where 
        toInt = read . unpack . strip
        splitComma = splitOn $ pack ","

-- fetch a value at the given memory location
fetch :: Int -> StateT Process IO Int
fetch loc = do
    memory <- memory <$> get
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
    ip <- ip <$> get
    vals <- forM [1..num] (\i -> fetch $ ip + i)
    return $ toParam <$> zip flags vals

-- Return the decoded opcode at the current instruction pointer
decode :: StateT Process IO Op
decode = do
    ip <- ip <$> get
    inst <- fetch ip
    let (flags, op) = inst `divMod` 100
        modes = reverse $ show flags
    case op of
        99 -> return Halt
        1  -> do { (p0:p1:p2:_) <- readParams 3 modes; return (Add p0 p1 p2) }
        2  -> do { (p0:p1:p2:_) <- readParams 3 modes; return (Mul p0 p1 p2) }
        3  -> do { (p0:_)        <- readParams 1 modes; return (Input p0) }
        4  -> do { (p0:_)        <- readParams 1 modes; return (Output p0) }
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
    Process memory ip inb outb <- get
    put $ Process (update loc (a + b) memory) (ip + 4) inb outb

mul :: Param -> Param -> Target -> StateT Process IO ()
mul p0 p1 (Param _ loc) = do
    a <- fetchParam p0
    b <- fetchParam p1
    Process memory ip inb outb <- get
    put $ Process (update loc (a * b) memory) (ip + 4) inb outb

input :: Target -> StateT Process IO ()
input (Param _ loc) = do
    Process memory ip (a:as) outb <- get
    put $ Process (update loc a memory) (ip + 2) as outb

output :: Param -> StateT Process IO ()
output p = do
    val <- fetchParam p
    --liftIO $ putStrLn $ show val
    Process memory ip inb outb <- get
    put $ Process memory (ip + 2) inb (val:outb)

jumpIfTrue :: Param -> Param -> StateT Process IO ()
jumpIfTrue p0 p1 = do
    a <- fetchParam p0
    b <- fetchParam p1
    Process memory ip inb outb <- get
    put $ Process memory (if a /= 0 then b else (ip + 3)) inb outb

jumpIfFalse :: Param -> Param -> StateT Process IO ()
jumpIfFalse p0 p1 = do
    a <- fetchParam p0
    b <- fetchParam p1
    Process memory ip inb outb <- get
    put $ Process memory (if a == 0 then b else (ip + 3)) inb outb

lessThan :: Param -> Param -> Target -> StateT Process IO ()
lessThan p0 p1 (Param _ loc) = do
    a <- fetchParam p0
    b <- fetchParam p1
    Process memory ip inb outb <- get
    let res = if a < b then 1 else 0
    put $ Process (update loc res memory) (ip + 4) inb outb

equals :: Param -> Param -> Target -> StateT Process IO ()
equals p0 p1 (Param _ loc) = do
    a <- fetchParam p0
    b <- fetchParam p1
    Process memory ip inb outb <- get
    let res = if a == b then 1 else 0
    put $ Process (update loc res memory) (ip + 4) inb outb

-- run the Process
runProcess :: StateT Process IO [Int]
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
        Halt                -> do { val <- outBuf <$> get; return $ val }
        (Error e)           -> do ip <- ip <$> get
                                  let msg = "Error with op " ++ e ++ " at " ++ show ip
                                  liftIO $ putStrLn msg
                                  return []