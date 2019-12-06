import Data.List (minimum, sortOn)
import Data.Maybe (catMaybes)
import Data.Monoid
import Data.Text (pack, splitOn, strip, Text, unpack)

type Delta   = (Sum Int, Sum Int)
type Point   = (Int, Int)
type Path    = [Point]
type Segment = (Point, Point)
type Wire    = [Segment]

-- unsafe hack to convert a String to an Int
readInt :: String -> Int
readInt = read

-- turn a single instruction into a Delta
toDelta :: Text -> Delta
toDelta i = let (d:xs) = (unpack . strip) i
           in case d of
            'U' -> (Sum 0, Sum (readInt xs))
            'D' -> (Sum 0, Sum (-(readInt xs)))
            'L' -> (Sum (-(readInt xs)), Sum 0)
            'R' -> (Sum (readInt xs), Sum 0)

-- starting point -> wire description -> list of points
toPath :: Point -> Text -> Path
toPath (x, y) line = reverse points
    where start = [(Sum x, Sum y)]
          splitOnComma = splitOn (pack ",")
          toDeltas t = toDelta <$> splitOnComma t
          toPointSums = foldl (\all@(a:as) t -> (t<>a):all) start
          points = [(getSum a, getSum b) | (a, b) <- toPointSums $ toDeltas line]

-- Return the intersection of two segments if it exists.  Since lines can only
-- be horizontal or vertical, we can find the point of each segment that's
-- furthest down and to the left. An intersection will be the max of the x and
-- y parts of those points that falls within each segment interval.
getIntersection :: Segment -> Segment -> Maybe Point
getIntersection s0 s1
    | ix <= min xb xd && iy <= min yb yd = Just (ix, iy)
    | otherwise = Nothing
    where
        ((a, b), (c, d)) = (s0, s1)
        ((xa, ya), (xb, yb)) = if a < b then (a, b) else (b, a)
        ((xc, yc), (xd, yd)) = if c < d then (c, d) else (d, c)
        ix = max xa xc
        iy = max ya yc

-- get intersections of wires by testing all pairs of segments
-- catMaybes just pulls values out of Just's and ignores Nothings
findIntersections w0 w1 = catMaybes [getIntersection s0 s1 | s0 <- w0, s1 <- w1]

-- starting point -> description of wire -> wire                                                       
-- the zip creates Segments from two adjacent points in the list, so it's
-- a sliding window two points wide.
parseWire :: Point -> Text -> Wire
parseWire start line = zip points (tail points)
    where points = toPath start line

main = do
    contents <- readFile "input.txt"
    let (raw1:raw2:xs) = lines contents
        port = (0, 0)
        w1 = parseWire port (pack raw1)
        w2 = parseWire port (pack raw2)
        intersections = [a | a <- findIntersections w1 w2, a /= port]
        distances = (\(x, y) -> (abs x) + (abs y)) <$> intersections
    putStrLn $ show $ minimum distances
