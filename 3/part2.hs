import Data.Maybe (catMaybes)
import qualified Data.Map.Strict as M
import Data.Monoid
import Data.Text (pack, splitOn, strip, Text, unpack)

type Delta   = (Sum Int, Sum Int)
type Point   = (Int, Int)
type Path    = [Point]
data Segment = Segment { startPoint :: Point, endPoint :: Point, distance :: Int } deriving Show
type Wire    = [Segment]

-- turn a single instruction into a Delta
toDelta :: Text -> Delta
toDelta i = let (d:xs) = (unpack . strip) i
                zero = Sum 0
                num = read xs
            in case d of
            'U' -> (zero, Sum num)
            'D' -> (zero, Sum (-num))
            'L' -> (Sum (-num), zero)
            'R' -> (Sum num, zero)

-- starting point -> wire description -> list of points
toPath :: Point -> Text -> Path
toPath (x, y) line = reverse points
    where start = [(Sum x, Sum y)]
          splitOnComma = splitOn (pack ",")
          toDeltas = (toDelta <$>) . splitOnComma
          toPointSums = foldl (\all@(a:as) t -> (t<>a):all) start
          points = [(getSum a, getSum b) | (a, b) <- toPointSums $ toDeltas line]

-- calc distance between two points
getDistance :: Point -> Point -> Int        
getDistance a b = abs $ (bx - ax) + (by - ay)
    where (ax, ay)  = a
          (bx, by) =  b

-- starting point -> description of wire -> wire                                                       
-- the zip creates Segments from two adjacent points in the list, so it's
-- a sliding window two points wide. It calculates the total length of the path
-- at each segment.
parseWire :: Point -> Text -> Wire
parseWire start line = reverse segments
    where
        points = toPath start line
        pairs = zip points (tail points)
        dist (s, e) = (s, e, getDistance s e)
        getSegments x = fst $ foldl (\(ps, ac) (s, e, l) -> let d = ac + l in ((Segment s e d):ps, d)) ([], 0) x
        segments = getSegments $ dist <$> pairs

-- Return the intersection of two segments if it exists.  Since lines can only
-- be horizontal or vertical, we can find the point of each segment that's
-- furthest down and to the left. An intersection will be the max of the x and
-- y parts of those points that falls within each segment interval. A wire's
-- total distance to an intersection is the distance to the end of the current
-- segment minus the distance from that end to the intersection.
getIntersection :: Segment -> Segment -> Maybe (Point, Int, Int)
getIntersection s0 s1
    | ix <= min xb xd && iy <= min yb yd = Just (p, d0, d1)
    | otherwise = Nothing
    where
        ((a, b), (c, d)) = ((startPoint s0, endPoint s0), (startPoint s1, endPoint s1))
        ((xa, ya), (xb, yb)) = if a < b then (a, b) else (b, a)
        ((xc, yc), (xd, yd)) = if c < d then (c, d) else (d, c)
        ix = max xa xc
        iy = max ya yc
        p = (ix, iy)
        d0 = (distance s0) - (getDistance p (endPoint s0))
        d1 = (distance s1) - (getDistance p (endPoint s1))

-- get intersections of wires by testing all pairs of segments
-- catMaybes pulls values out of Just's and ignores Nothings
findIntersections w0 w1 = catMaybes [getIntersection s0 s1 | s0 <- w0, s1 <- w1]

main = do
    contents <- readFile "input.txt"
    let 
        (raw1:raw2:xs) = lines contents
        port = (0, 0)
        w1 = parseWire port (pack raw1)
        w2 = parseWire port (pack raw2)
        intersections = [(p, d0, d1) | (p, d0, d1) <- findIntersections w1 w2, p /= port]
        minW1Ints = foldr (\(p, d0, d1) m -> M.insertWith min p d0 m) M.empty intersections
        minW2Ints = foldr (\(p, d0, d1) m -> M.insertWith min p d1 m) M.empty intersections
        distances = [
                let d0 = minW1Ints M.! i
                    d1 = minW2Ints M.! i
                in (d0 + d1) | (i, _, _) <- intersections
            ]
    putStrLn $ show $ minimum distances
