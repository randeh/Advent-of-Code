
import System.IO (readFile)
import Data.Set (union, (\\), fromList, empty, size)
import Data.List.Split (splitOn)

area ([x1, y1], [x2, y2]) = fromList [[x, y] | x <- [x1..x2], y <- [y1..y2]]

toggle old new = union (old \\ new) (new \\ old)

--

applyCmd on (cmd, pos) = cmd on $ area pos

numLightsOn = size . foldl applyCmd empty

--

coords :: String -> [Int]
coords = map read . splitOn ","

buildCmd c (from:"through":to:[]) = (c, (coords from, coords to))

parseLine ("turn":"on":rest) = buildCmd union $ rest
parseLine ("turn":"off":rest) = buildCmd (\\) rest
parseLine ("toggle":rest) = buildCmd toggle $ rest

parse = map (parseLine . words) . lines

main = do
	input <- readFile "input06.txt"
	let parsed = parse input
	putStrLn $ show $ numLightsOn $ parsed
