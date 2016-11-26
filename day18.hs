
import System.IO (readFile)

--

map2 = map . map

count b = length . filter b

count2 b = sum . map (count b)

countTrue = count id

countTrue2 = count2 id

zipWith2 f a b = zipWith (\i j -> zipWith f i j) a b

--

find :: [[a]] -> (Int, Int) -> a
find bs (x, y) = (bs !! y) !! x

neighbourCoords :: (Int, Int) -> [(Int, Int)]
neighbourCoords (x, y) = [(a, b) | a <- [(x-1)..(x+1)], b <- [(y-1)..(y+1)], a >= 0, a < 100, b >= 0, b < 100, a /= x || b /= y]

neighbours :: [[Bool]] -> (Int, Int) -> [Bool]
neighbours bs (x, y) = map (find bs) (neighbourCoords (x, y))

numNeighbours :: [[Bool]] -> (Int, Int) -> Int
numNeighbours bs (x, y) = countTrue $ neighbours bs (x, y)

next :: Bool -> Int -> Bool
next True 2 = True
next True 3 = True
next False 3 = True
next _ _ = False

coords :: [[(Int, Int)]]
coords = [[(x, y) | x <- [0..99]] | y <- [0..99]]

standardLogic :: [[Bool]] -> (Int, Int) -> Bool
standardLogic bs (x, y) = next (find bs (x, y)) (numNeighbours bs (x, y))

modifiedCornerLogic :: [[Bool]] -> (Int, Int) -> Bool
modifiedCornerLogic _ (0, 0) = True
modifiedCornerLogic _ (0, 99) = True
modifiedCornerLogic _ (99, 0) = True
modifiedCornerLogic _ (99, 99) = True
modifiedCornerLogic bs (x, y) = standardLogic bs (x, y)

step :: ([[Bool]] -> (Int, Int) -> Bool) -> [[Bool]] -> [[Bool]]
step strategy bs = map2 (strategy bs) coords

result strategy lights = countTrue2 $ iterate (step strategy) lights !! 100

--

parseChar '#' = True
parseChar '.' = False

parse :: String -> [[Bool]]
parse = map2 parseChar . lines

main = do
	input <- readFile "input18.txt"
	let parsed = parse input
	putStrLn $ show $ result standardLogic parsed
	putStrLn $ show $ result modifiedCornerLogic parsed
