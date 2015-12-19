
import System.IO (readFile)

finalFloor = foldl (+) 0

--

firstBasement = length . takeWhile (/= -1) . scanl (+) 0

--

delta '(' = 1
delta ')' = -1

parse = map delta

main = do
	input <- readFile "input01.txt"
	let parsed = parse input
	putStrLn $ show $ finalFloor $ parsed
	putStrLn $ show $ firstBasement $ parsed
