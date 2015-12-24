
import System.IO (readFile)
import Data.List (nub, sort, permutations)
import Control.Monad (liftM2)
import Data.Maybe (isJust, fromJust)

locations :: Eq a => [((a, a), b)] -> [a]
locations = nub . concat . sequence [map fst, map snd] . map fst

allRoutes :: Eq a => [((a, a), b)] -> [[a]]
allRoutes = permutations . locations

distance :: (Eq a, Num b) => [((a, a), b)] -> a -> a -> Maybe b
distance distances from to = maybe (lookup (to, from) distances) Just $ lookup (from, to) distances

accumulateDistance :: (Eq a, Num b) => [((a, a), b)] -> (a, Maybe b) -> a -> (a, Maybe b)
accumulateDistance distances (from, acc) to = (to, liftM2 (+) acc $ distance distances from to)

totalDistance :: (Eq a, Num b) => [((a, a), b)] -> [a] -> Maybe b
totalDistance distances route = snd $ foldl (accumulateDistance distances) (head route, Just 0) (tail route)

allDistances :: (Eq a, Num b, Ord b) => [((a, a), b)] -> [b]
allDistances distances = map fromJust $ filter isJust $ map (totalDistance distances) $ allRoutes distances

shortestDistance :: (Eq a, Num b, Ord b) => [((a, a), b)] -> b
shortestDistance = head . sort . allDistances

--

longestDistance :: (Eq a, Num b, Ord b) => [((a, a), b)] -> b
longestDistance = last . sort . allDistances

--

parseLine [from, "to", to, "=", dist] = ((from, to), read dist :: Int)

parse = map (parseLine . words) . lines

main = do
	input <- readFile "input09.txt"
	let parsed = parse input
	putStrLn $ show $ shortestDistance $ parsed
	putStrLn $ show $ longestDistance $ parsed
