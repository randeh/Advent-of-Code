
import System.IO (readFile)
import Data.List (inits, tails, nub)
import Data.List.Split (splitOn)
import Control.Applicative ((<*>))
import Data.Char (ord, chr)

contiguousSubsequences = concatMap inits . tails

contiguousSubsequencesOfLength n = filter ((==n) . length) . contiguousSubsequences

allEqual = (==1) . length . nub

--

inc (x:xs) = if x + 1 < 26 then (x + 1) : xs else 0 : inc xs

encode = (subtract $ ord 'a') . ord

decode = chr . ((+) $ ord 'a')

increment = map decode . reverse . inc . reverse . map encode

sequential (a, b) = (ord b - ord a) == 1

increasing = all sequential . (zip <*> tail)

increasingTriplet = any increasing . contiguousSubsequencesOfLength 3

noBlacklistedLetters = not . any (flip elem $ "iol")

pairs = filter allEqual . contiguousSubsequencesOfLength 2

-- This assumes that the two pairs must be made of different letters, the question is ambiguous
twoPairs str = not $ null $ filter (any (not . null . pairs) . (flip splitOn $ str)) $ pairs str

validPassword = and . sequence [increasingTriplet, noBlacklistedLetters, twoPairs]

nextPassword = until validPassword increment

--

main = do
	input <- readFile "input11.txt"
	putStrLn $ nextPassword input
	putStrLn $ nextPassword $ increment $ nextPassword input
