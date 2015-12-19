
import System.IO (readFile)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Digest.Pure.MD5 (md5)

hash = show . md5 . pack

hashWithPrefix p n = hash $ p ++ show n

startsWithZeroes n = all (=='0') . take n

--

nonce n p = until (startsWithZeroes n . hashWithPrefix p) (+1) 0

--

main = do
	input <- readFile "input04.txt"
	putStrLn $ show $ nonce 5 $ input
	putStrLn $ show $ nonce 6 $ input
