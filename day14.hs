
import System.IO (readFile)
import Data.List (sortBy, groupBy)
import Data.Ord (compare)

backtrack targetTime ((finalDist, finalTime), (prevSpeed, prevTime))
	= finalDist - (prevSpeed * (finalTime - targetTime))

combine ((accDist, accTime), _) (speed, time) = ((accDist + (speed * time), accTime + time), (speed, time))

distanceAt t = backtrack t . head . dropWhile ((< t) . snd . fst) . scanl combine ((0, 0), (0, 0)) . cycle

winningDistance t = maximum . map (distanceAt t)

--

dist :: Integer -> ([(Integer, Integer)], Integer) -> (([(Integer, Integer)], Integer), Integer)
dist t (reindeer, points) = ((reindeer, points), distanceAt t reindeer)

equalDistance :: (([(Integer, Integer)], Integer), Integer) -> (([(Integer, Integer)], Integer), Integer) -> Bool
equalDistance (_, d1) (_, d2) = d1 == d2

orderByDistance :: (([(Integer, Integer)], Integer), Integer) -> (([(Integer, Integer)], Integer), Integer) -> Ordering
orderByDistance (_, d1) (_, d2) = compare d1 d2

givePoint :: ([(Integer, Integer)], Integer) -> ([(Integer, Integer)], Integer)
givePoint (reindeer, points) = (reindeer, points + 1)

givePointsToLastGroup :: [[([(Integer, Integer)], Integer)]] -> [[([(Integer, Integer)], Integer)]]
givePointsToLastGroup = concat . sequence [init, return . map givePoint . last]

givePoints :: Integer -> [([(Integer, Integer)], Integer)] -> [([(Integer, Integer)], Integer)]
givePoints t = concat . givePointsToLastGroup . map (map fst) . groupBy equalDistance . sortBy orderByDistance . map (dist t)

winningPoints :: Integer -> [[(Integer, Integer)]] -> Integer
winningPoints t r = maximum $ map snd $ foldl (flip givePoints) (zip r $ repeat 0) [1..t]

--

parseLine [_, "can", "fly", v, "km/s", "for", s, "seconds,", "but", "then", "must", "rest", "for", r, "seconds."]
	= [(read v, read s), (0, read r)]

parse = map (parseLine . words) . lines

main = do
	input <- readFile "input14.txt"
	let parsed = parse input
	putStrLn $ show $ winningDistance 2503 parsed
	putStrLn $ show $ winningPoints 2503 parsed
