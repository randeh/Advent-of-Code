
import System.IO (readFile)
import Data.List (foldl')
import Control.Applicative ((<*>))
import Data.Bits ((.&.), (.|.), shiftL, shiftR, complement)
import Data.Char (isDigit)
import Data.Word (Word16)
import Data.Map (Map)
import qualified Data.Map as Map

fromJust (Just x) = x

--

binaryOp :: String -> Word16 -> Word16 -> Word16
binaryOp "AND" = (.&.)
binaryOp "OR" = (.|.)
binaryOp "LSHIFT" = flip $ flip shiftL . fromIntegral
binaryOp "RSHIFT" = flip $ flip shiftR . fromIntegral

unaryOp :: String -> Word16 -> Word16
unaryOp "NOT" = complement

evaluateOperand :: Map String Word16 -> Operand -> Maybe Word16
evaluateOperand values (Wire wire) = Map.lookup wire values
evaluateOperand values (Value num) = Just num

evaluateExpr :: Map String Word16 -> Expr -> Maybe Word16
evaluateExpr values (BinaryOp op l r) = fmap (binaryOp op) (evaluateOperand values l) <*> (evaluateOperand values r)
evaluateExpr values (UnaryOp op o) = fmap (unaryOp op) (evaluateOperand values o)
evaluateExpr values (Const o) = evaluateOperand values o

calculateNextValue :: String -> Expr -> Map String Word16 -> Map String Word16
calculateNextValue wire expr values = Map.alter (const $ evaluateExpr values expr) wire values

calculateNextValues :: Map String Expr -> Map String Word16 -> Map String Word16
calculateNextValues wirings values = Map.foldrWithKey calculateNextValue values wirings

signalAt :: String -> Map String Expr -> Word16
signalAt wire wirings = fromJust $ Map.lookup wire $ until (Map.member wire) (calculateNextValues wirings) Map.empty

--

rewiredSignal wirings = signalAt "a" $ Map.insert "b" (Const $ Value $ signalAt "a" wirings) wirings

--

data Operand = Value Word16
	| Wire String
	deriving Show

data Expr = BinaryOp String Operand Operand
	| UnaryOp String Operand
	| Const Operand
	deriving Show

parseOperand o = if all isDigit o then Value $ read o else Wire o

parseLine wirings [l, op, r, "->", wire] = Map.insert wire (BinaryOp op (parseOperand l) (parseOperand r)) wirings
parseLine wirings [op, o, "->", wire] = Map.insert wire (UnaryOp op $ parseOperand o) wirings
parseLine wirings [o, "->", wire] = Map.insert wire (Const $ parseOperand o) wirings

parse = foldl' (parseLine) Map.empty . map words . lines

main = do
	input <- readFile "input07.txt"
	let parsed = parse input
	putStrLn $ show $ signalAt "a" $ parsed
	putStrLn $ show $ rewiredSignal $ parsed
