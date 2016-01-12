
import System.IO (readFile)
import Text.Regex
import Data.Aeson
import Data.Maybe (fromJust)
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import Data.String.Conversions (cs)
import Data.Text (pack)

sumNumbers = sum . map (read :: String -> Int) . filter (not . null) . splitRegex (mkRegex "[^-0-9]+")

--

containsRed = any (== String (pack "red")) . HM.elems

sumNonRed (Object o) = if containsRed o then 0 else (sum $ HM.elems $ HM.map sumNonRed o)
sumNonRed (Array a) = V.sum $ V.map sumNonRed a
sumNonRed (String _) = 0
sumNonRed (Number n) = n
sumNonRed (Bool _) = 0
sumNonRed (Null) = 0

--

main = do
	input <- readFile "input12.txt"
	putStrLn $ show $ sumNumbers input
	putStrLn $ show $ sumNonRed $ fromJust (decode (cs input) :: Maybe Value)
