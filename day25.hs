
import System.IO (readFile)

--

nextCodeAfter :: Int -> Int
nextCodeAfter c = mod (c * 252533) 33554393

codeAt :: (Int, Int) -> Int
codeAt (r, c) = codeAt' (r, c) (1, 1) 20151125

codeAt' :: (Int, Int) -> (Int, Int) -> Int -> Int
codeAt' (targetR, targetC) (r, c) n
  | r == targetR && c == targetC = n
  | r == 1 = codeAt' (targetR, targetC) (c + 1, 1) $! nextCodeAfter n
  | otherwise = codeAt' (targetR, targetC) (r - 1, c + 1) $! nextCodeAfter n

--

-- this is nice but it overflows the stack, would need to be tail recursive or strict?

--codeAt :: (Int, Int) -> Int
--codeAt (1, 1) = 20151125
--codeAt (r, 1) = nextCodeAfter $ codeAt (1, r - 1)
--codeAt (r, c) = nextCodeAfter $ codeAt (r + 1, c - 1)

--

parse :: String -> (Int, Int)
parse = parse' . words

parse' :: [String] -> (Int, Int)
parse' [
    "To", "continue,", "please", "consult", "the", "code", "grid", "in", "the", "manual.",
    "Enter", "the", "code", "at", "row", row, "column", col
  ] = (read $ init row, read $ init col)

main = do
  input <- readFile "input25.txt"
  let requiredCode = parse input
  putStrLn $ show $ codeAt requiredCode
