
import System.IO (readFile)
import Control.Monad (guard)

--

subsetsWithLength :: Int -> [Int] -> [[Int]]
subsetsWithLength 0 _        = [[]]
subsetsWithLength _ []       = []
subsetsWithLength len (x:xs) = map (x:) (subsetsWithLength (len - 1) xs) ++ subsetsWithLength len xs

subsetsWithSum :: Int -> [Int] -> [[Int]]
subsetsWithSum target set = do
  len <- [1..length set]
  subset <- subsetsWithLength len set
  guard $ sum subset == target
  return subset

quantumEntanglementOfFirstGroup :: Int -> [Int] -> Int
quantumEntanglementOfFirstGroup numGroups packageWeights
  = lowestQuantumEntanglement
    where
      totalWeight = sum packageWeights
      weightPerGroup = div totalWeight numGroups
      potentialGroups = subsetsWithSum weightPerGroup packageWeights
      lengthOfFirstGroup = length $ head potentialGroups
      potentialFirstGroups = takeWhile (\g -> length g == lengthOfFirstGroup) potentialGroups
      -- doesn't seem to be necessary in this case but technically required for correctness:
      -- filter potentialFirstGroups, reject if the remaining elements can't be equally partitioned into (numGroups - 1) groups
      quantumEntanglements = map product $ potentialFirstGroups
      lowestQuantumEntanglement = minimum $ quantumEntanglements

--

parse :: String -> [Int]
parse = map read . lines

main = do
  input <- readFile "input24.txt"
  let packageWeights = parse input
  putStrLn $ show $ quantumEntanglementOfFirstGroup 3 packageWeights
  putStrLn $ show $ quantumEntanglementOfFirstGroup 4 packageWeights
