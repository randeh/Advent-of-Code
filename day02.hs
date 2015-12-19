
import System.IO (readFile)
import Data.List (subsequences, sort)
import Data.List.Split (splitOn)

choose n = filter ((==n) . length) . subsequences

--

smallestSideArea = product . take 2 . sort

surfaceArea = (*2) . sum . map product . choose 2

presentArea = sum . sequence [surfaceArea, smallestSideArea]

area = sum . map presentArea

--

smallestPerimeter = sum . map (*2) . take 2 . sort

bow = product

presentRibbon = sum . sequence [smallestPerimeter, bow]

ribbon = sum . map presentRibbon

--

parseDimensions = map read . splitOn "x"

parse = map parseDimensions . lines

main = do
	input <- readFile "input02.txt"
	let parsed = parse input
	putStrLn $ show $ area $ parsed
	putStrLn $ show $ ribbon $ parsed
