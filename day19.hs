
import System.IO (readFile)
import Data.List (nub, concatMap, sortBy)
import Data.Ord (comparing)
import qualified Data.Tuple as T (swap)
import Data.Maybe (catMaybes, listToMaybe)

--

startsWith :: String -> String -> Bool
startsWith string substring = and $ zipWith (==) string substring

swap :: String -> String -> String -> String
swap str find replace = replace ++ drop (length find) str

--

generateMolecules' :: String -> String -> (String, String) -> [String]
generateMolecules' prefix [] _ = []
generateMolecules' prefix str (find, replace)
  | str `startsWith` find = (prefix ++ swap str find replace) : rest
  | otherwise = rest
    where
      rest = generateMolecules' (prefix ++ [head str]) (tail str) (find, replace)

generateMolecules :: String -> (String, String) -> [String]
generateMolecules str replacement = generateMolecules' "" str replacement

allPossibleMolecules :: String -> [(String, String)] -> [String]
allPossibleMolecules molecule replacements = concatMap (generateMolecules molecule) replacements

numDistinctMolecules :: String -> [(String, String)] -> Int
numDistinctMolecules molecule replacements = length $ nub $ allPossibleMolecules molecule replacements

--

fabricationCost :: String -> String -> [(String, String)] -> Maybe Int
fabricationCost curr target replacements
  | elem curr allPoss = Just 1
  | otherwise = fmap (+1) $ listToMaybe $ catMaybes $ map (\x -> fabricationCost curr x replacements) allPoss
    where
      allPoss = allPossibleMolecules target $ map T.swap replacements

--

parseRule :: String -> (String, String)
parseRule rule = (from, to)
  where
    [from, _, to] = words rule

parse :: String -> ([(String, String)], String)
parse input = (map parseRule rules, last ls)
  where
    ls = lines input
    rules = takeWhile (/="") ls

main = do
  input <- readFile "input19.txt"
  let (replacements, molecule) = parse input
  putStrLn $ show $ numDistinctMolecules molecule replacements
  putStrLn $ show $ fabricationCost "e" molecule replacements
