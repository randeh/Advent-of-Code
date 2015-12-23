
import System.IO (readFile)
import Data.List (foldl')
import Data.Set (union, (\\), fromList, empty, size)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe, Maybe(Just))
import Data.Map (Map)
import qualified Data.Map as Map

area ([x1, y1], [x2, y2]) = [[x, y] | x <- [x1..x2], y <- [y1..y2]]

areaSet = fromList . area

toggle old new = union (old \\ new) (new \\ old)

--

applyCmd on ((combineCmd, _), pos) = combineCmd on $ areaSet pos

numLightsOn = size . foldl applyCmd empty

--

updateLight u m xy = Map.alter (Just . u . fromMaybe 0) xy m

applyCmd' curr ((_, updateCmd), pos) = foldl' (updateLight updateCmd) curr $ area pos

totalBrightness = Map.fold (+) 0 . foldl applyCmd' Map.empty

--

coords :: String -> [Int]
coords = map read . splitOn ","

buildCmd c (from:"through":to:[]) = (c, (coords from, coords to))

parseLine ("turn":"on":rest) = buildCmd (union, (+1)) rest
parseLine ("turn":"off":rest) = buildCmd ((\\), max 0 . (subtract 1)) rest
parseLine ("toggle":rest) = buildCmd (toggle, (+2)) rest

parse = map (parseLine . words) . lines

main = do
	input <- readFile "input06.txt"
	let parsed = parse input
	putStrLn $ show $ numLightsOn $ parsed
	putStrLn $ show $ totalBrightness $ parsed
