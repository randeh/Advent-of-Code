
import System.IO (readFile)
import Data.List (group)

say = concatMap (concat . sequence [show . length, return . head])

lookAndSay = say . group

lookAndSayRepeat n str = iterate lookAndSay str !! n

part1 = length . lookAndSayRepeat 40

--

part2 = length . lookAndSayRepeat 50

--

main = do
	input <- readFile "input10.txt"
	putStrLn $ show $ part1 input
	putStrLn $ show $ part2 input
