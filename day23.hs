
import System.IO (readFile)
import Data.List.Split (splitOn)
import Data.Array (Array, listArray, bounds, (!))

--

data Register = A | B

instance Show Register where
  show A = "a"
  show B = "b"

type RegisterValues = (Int, Int)

getRegister :: Register -> RegisterValues -> Int
getRegister A = fst
getRegister B = snd

setRegister :: Register -> Int -> RegisterValues -> RegisterValues
setRegister A n (_, b) = (n, b)
setRegister B n (a, _) = (a, n)

type Offset = Int

showOffset o = (if o < 0 then '-' else '+') : show o

data Instruction = HLF Register
                 | TPL Register
                 | INC Register
                 | JMP Offset
                 | JIE Register Offset
                 | JIO Register Offset

instance Show Instruction where
  show (HLF r) = "hlf " ++ show r
  show (TPL r) = "tpl " ++ show r
  show (INC r) = "inc " ++ show r
  show (JMP o) = "jmp " ++ showOffset o
  show (JIE r o) = "jie " ++ show r ++ ", " ++ showOffset o
  show (JIO r o) = "jio " ++ show r ++ ", " ++ showOffset o

type Program = Array Int Instruction

--

execute :: Program -> RegisterValues
execute p = execute' p 0 (0, 0)

execute' :: Program -> Int -> RegisterValues -> RegisterValues
execute' p ip rv = if ip < lowerBound || ip > upperBound then rv else execute' p ip' rv'
  where
    (lowerBound, upperBound) = bounds p
    instruction = p ! ip
    (rv', o) = executeInstruction instruction rv
    ip' = ip + o

executeInstruction :: Instruction -> RegisterValues -> (RegisterValues, Offset)
executeInstruction (HLF r) rv = (setRegister r (div (getRegister r rv) 2) rv, 1)
executeInstruction (TPL r) rv = (setRegister r (getRegister r rv * 3) rv, 1)
executeInstruction (INC r) rv = (setRegister r (getRegister r rv + 1) rv, 1)
executeInstruction (JMP o) rv = (rv, o)
executeInstruction (JIE r o) rv = (rv, if mod (getRegister r rv) 2 == 0 then o else 1)
executeInstruction (JIO r o) rv = (rv, if getRegister r rv == 1 then o else 1)

--

parseRegister :: String -> Register
parseRegister "a" = A
parseRegister "b" = B

parseOffset :: String -> Offset
parseOffset ('+':n) = read n
parseOffset ('-':n) = negate $ read n

parseInstruction :: String -> Instruction
parseInstruction instruction = parseInstruction' (opcode:argList)
  where
    opcode = head $ words instruction
    argListCommaSeparated = unwords $ tail $ words instruction
    argList = splitOn ", " argListCommaSeparated

parseInstruction' :: [String] -> Instruction
parseInstruction' ["hlf", r] = HLF (parseRegister r)
parseInstruction' ["tpl", r] = TPL (parseRegister r)
parseInstruction' ["inc", r] = INC (parseRegister r)
parseInstruction' ["jmp", o] = JMP (parseOffset o)
parseInstruction' ["jie", r, o] = JIE (parseRegister r) (parseOffset o)
parseInstruction' ["jio", r, o] = JIO (parseRegister r) (parseOffset o)

parse :: String -> Program
parse input = listArray (0, length instructions - 1) instructions
  where
    instructions = map parseInstruction $ lines input

main = do
  input <- readFile "input23.txt"
  let program = parse input
  putStrLn $ show $ snd $ execute program
  putStrLn $ show $ snd $ execute' program 0 (1, 0)
