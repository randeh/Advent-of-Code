
import System.IO (readFile)
import Data.Array.ST (newArray, readArray, writeArray, getAssocs, STArray)
import Control.Monad (forM_)
import Control.Monad.ST (runST, ST)
import Data.Foldable (find)

lowestHouseToGetAtLeast :: Int -> Int -> (Int -> Int -> [Int]) -> Int
lowestHouseToGetAtLeast numPresents deliveryAmount deliveryStrategy = house
  where
    -- empirically, it looks like the sum of factors of n is at most 4n
    -- use a slightly more conservative bound (3) to be more confident that the house falls within the range
    -- also make the upper bound at least 10
    upperBound = max (div (div numPresents deliveryAmount) 3) 10
    (Just house) = lowestHouseToGetAtLeast' numPresents deliveryAmount deliveryStrategy upperBound

lowestHouseToGetAtLeast' :: Int -> Int -> (Int -> Int -> [Int]) -> Int -> Maybe Int
lowestHouseToGetAtLeast' numPresents deliveryAmount deliveryStrategy upperBound = runST $ do
  arr <- newArray (1, upperBound) 0 :: ST s (STArray s Int Int)
  let elves = [1..upperBound]
  forM_ elves $ \elf -> do
    let deliveryTargets = deliveryStrategy elf upperBound
    forM_ deliveryTargets $ \deliveryTarget -> do
      currPresents <- readArray arr deliveryTarget
      writeArray arr deliveryTarget (currPresents + (elf * deliveryAmount))
  assocs <- getAssocs arr
  return $ fmap fst $ find (\(h, p) -> p >= numPresents) assocs

allDeliveryTargets :: Int -> Int -> [Int]
allDeliveryTargets elf upperBound = takeWhile (<= upperBound) $ iterate (+ elf) elf

first50DeliveryTargets :: Int -> Int -> [Int]
first50DeliveryTargets elf upperBound = take 50 $ allDeliveryTargets elf upperBound

--

main = do
  input <- readFile "input20.txt"
  let target = (read input :: Int)
  putStrLn $ show $ lowestHouseToGetAtLeast target 10 allDeliveryTargets
  putStrLn $ show $ lowestHouseToGetAtLeast target 11 first50DeliveryTargets
