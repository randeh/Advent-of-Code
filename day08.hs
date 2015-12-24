
import System.IO (readFile)
import Data.Composition ((.:))

--

literalSize = length

unquotedSize ('\\':'\\':rest) = 1 + unquotedSize rest
unquotedSize ('\\':'"':rest) = 1 + unquotedSize rest
unquotedSize ('\\':'x':_:_:rest) = 1 + unquotedSize rest
unquotedSize (_:rest) = 1 + unquotedSize rest
unquotedSize [] = 0

memorySize = unquotedSize . init . tail

total = sum .: map

sizeDifference strs = total literalSize strs - total memorySize strs

--

escape ('"':rest) = '\\':'"':escape rest
escape ('\\':rest) = '\\':'\\':escape rest
escape (c:rest) = c:escape rest
escape "" = ""

encode str = "\"" ++ escape str ++ "\""

encodedSize = length . encode

encodedSizeDifference strs = total encodedSize strs - total literalSize strs

--

parse = lines

main = do
	input <- readFile "input08.txt"
	let parsed = parse input
	putStrLn $ show $ sizeDifference $ parsed
	putStrLn $ show $ encodedSizeDifference $ parsed
