
import System.IO (readFile)
import Data.List (nub)
import GHC.Exts (groupWith)

combine = zipWith (+)

distinctHouses = length . nub

generatePositions = scanl combine [0, 0]

--

singleVisitor = distinctHouses . generatePositions

--

split = map (map snd) . groupWith (odd . fst) . zip [0..]

twoVisitors = distinctHouses . concatMap generatePositions . split

--

delta '^' = [0, 1]
delta 'v' = [0, -1]
delta '<' = [-1, 0]
delta '>' = [1, 0]

parse = map delta

main = do
	input <- readFile "input03.txt"
	let parsed = parse input
	putStrLn $ show $ singleVisitor $ parsed
	putStrLn $ show $ twoVisitors $ parsed
