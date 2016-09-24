
import System.IO (readFile)
import Data.Text (pack, unpack, strip)
import Data.List.Split (splitOn)
import Data.List (findIndex)
import Data.Maybe (fromMaybe, fromJust)

dna	= [("children", 3), ("cats", 7), ("samoyeds", 2), ("pomeranians", 3), ("akitas", 0), ("vizslas", 0), ("goldfish", 5), ("trees", 3), ("cars", 2), ("perfumes", 1)]

trim = unpack . strip . pack

--

factMatches fact = elem fact dna

allFactsMatch = all factMatches

identifySue	= fmap (+1) . findIndex allFactsMatch

--

adjustments = [("cats", (>)), ("trees", (>)), ("pomeranians", (<)), ("goldfish", (<))]

comparator attribute = fromMaybe (==) $ lookup attribute adjustments

factMatches' (attribute, amount) = comparator attribute amount $ fromJust $ lookup attribute dna

allFactsMatch' = all factMatches'

identifySue' = fmap (+1) . findIndex allFactsMatch'

--

parseCompound :: [String] -> (String, Int)
parseCompound [compound, amount]
	= (trim compound, read amount)

parseCompounds
	= map (parseCompound . splitOn ": ") . splitOn ","

parseSue 
	= parseCompounds . unwords . drop 2 . words

parse = map parseSue . lines

main = do
	input <- readFile "input16.txt"
	let parsed = parse input
	putStrLn $ show $ identifySue parsed
	putStrLn $ show $ identifySue' parsed
