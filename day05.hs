
import System.IO (readFile)
import Data.List (inits, tails, nub)
import Data.List.Utils (split)

contiguousSubsequences = concatMap inits . tails

contiguousSubsequencesOfLength n = filter ((==n) . length) . contiguousSubsequences

allEqual = (==1) . length . nub

--

threeVowels = (>=3) . length . filter (flip elem $ "aeiou")

doubleLetter = any allEqual . contiguousSubsequencesOfLength 2

doesNotContain sequences = not . any (flip elem $ sequences) . contiguousSubsequences

nice = and . sequence [threeVowels, doubleLetter, doesNotContain ["ab", "cd", "pq", "xy"]]

numNice = length . filter nice

--

repeatedPair s = any ((>2) . length . (flip split $ s)) $ contiguousSubsequencesOfLength 2 s

letterBetweenRepeat = any (allEqual . sequence [head, last]) . contiguousSubsequencesOfLength 3

nice' = and . sequence [repeatedPair, letterBetweenRepeat]

numNice' = length . filter nice'

--

main = do
	input <- readFile "input05.txt"
	let parsed = lines input
	putStrLn $ show $ numNice $ parsed
	putStrLn $ show $ numNice' $ parsed
