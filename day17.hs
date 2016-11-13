
import System.IO (readFile)

--

powerset (x:[])
	= [[], [x]]
powerset (x:xs)
	= ps ++ map (x:) ps
		where ps = powerset xs

allCombinations n = filter ((==n) . sum) . powerset

--

numCombinations n = length . allCombinations n

--

numDistinctMinimumCombinations n cs = length $ filter ((==minLen) . length) a
	where
		a = allCombinations n cs
		minLen = minimum $ map length a

--

parse :: String -> [Int]
parse = map read . lines

main = do
	input <- readFile "input17.txt"
	let parsed = parse input
	putStrLn $ show $ numCombinations 150 $ parsed
	putStrLn $ show $ numDistinctMinimumCombinations 150 $ parsed
