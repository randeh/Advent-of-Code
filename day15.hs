
import System.IO (readFile)
import Data.Text (pack, unpack, strip)
import Data.List.Split (splitOn)
import Data.List (transpose)
import Data.Ord (comparing)

trim = unpack . strip . pack

prependListsWith n = map (n:)

listsOfLengthNWithSumS 1 s = [[s]]
listsOfLengthNWithSumS n s = concatMap (listsOfLengthNWithSumSStartingWithX n s) [0..s]

listsOfLengthNWithSumSStartingWithX n s x = prependListsWith x $ listsOfLengthNWithSumS (n-1) (s - x)

score ingredients amounts
	= product $ map (max 0) $ map sum $ transpose $ zipWith map (map (*) amounts) (map init ingredients)

maxScore ingredients
	= maximum . map (score ingredients)

allValidCombinations ingredients
	= listsOfLengthNWithSumS (length ingredients) 100

--

calories ingredients amounts
	= sum $ zipWith (*) (map last ingredients) amounts

fittingCalorieRestrictions ingredients amounts
	= calories ingredients amounts == 500

optimalScoreDiet ingredients
	= maxScore ingredients $ filter (fittingCalorieRestrictions ingredients) $ allValidCombinations ingredients

--

optimalScore ingredients
	= maxScore ingredients $ allValidCombinations ingredients

--

parseProperty :: String -> Int
parseProperty
	= read . last . splitOn " " . trim

parseIngredient
	= map parseProperty . splitOn "," . trim . last . splitOn ":"

parse = map parseIngredient . lines

main = do
	input <- readFile "input15.txt"
	let parsed = parse input
	putStrLn $ show $ optimalScore parsed
	putStrLn $ show $ optimalScoreDiet parsed
