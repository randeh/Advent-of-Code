
import System.IO (readFile)
import Data.List (nub, permutations)
import Data.Tuple (swap)
import Data.Maybe (fromMaybe)

pairs l = (last l, head l) : zip l (tail l)

commutativePairs l = pairs l ++ map swap (pairs l)

score p = sum . map (fromMaybe 0 . (flip lookup $ p)) . commutativePairs

family = nub . map (fst . fst)

optimalScore p = maximum . map (score p) . permutations

optimalSeating p = optimalScore p $ family p

--

optimalSeatingWithMe p = optimalScore p $ "Me" : family p

--

delta "gain" = id
delta "lose" = negate

parseLine [a, "would", dir, amt, "happiness", "units", "by", "sitting", "next", "to", b]
  = ((a, b), delta dir $ read amt)

parse = map (parseLine . words . init) . lines

main = do
	input <- readFile "input13.txt"
	let parsed = parse input
	putStrLn $ show $ optimalSeating parsed
	putStrLn $ show $ optimalSeatingWithMe parsed
