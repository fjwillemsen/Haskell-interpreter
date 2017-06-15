module Main (main) where

-- Imports
import ParseSVM
import Text.Parsec.Error
import Data.Sequence
import Data.Foldable (toList)
import System.Environment
import Control.Exception
import Data.Time



-- Additional Types

-- list of Labels, each label has a string, list of instructions and the program counter
type Labels = [(String, [Instruction], Int)]

-- Tuple of remaining program, memory, registery, program counter and labels
type InstructionArgs = ([Instruction], [Literal], [Literal], Int, Labels)
type InstructionResult = ([Instruction], [Literal], [Literal], Int, Labels)





-- Main Function

-- reads arguments and the SVM-file, initializes memory and registers and starts execution unless Parse Errors
main = do
  start <- getCurrentTime -- get starttime
  -- read from stdin and parse text-based SVM file
  input <- getContents
  memsize <- getArgs
  let result = parse program "(standard input)" input

  -- initializing the memory of size N where N is the command line argument
  let memorysize = (read (memsize!!0) :: Int)
  let memory = initMem memorysize
  -- initializing the register
  let registers = initReg

  -- unwrap Either: prog of type Program or err of type ParseError
  putStrLn ("Start of program")
  case result of
    Left err -> printError err
    Right prog -> run (prog, memory, registers, 1, (prerun (prog, memory, registers, 1, []))) -- use "run" or "debug"
  end <- getCurrentTime -- get endtime
  putStrLn ("Execution Duration: " ++ (show (diffUTCTime end start)))





-- Run Functions

-- recursive function for preprocessing the list of labels
prerun :: InstructionArgs -> Labels
prerun ([], mem, reg, pc, labels) = labels
prerun (((Label string):list), mem, reg, pc, labels) = prerun (list, mem, reg, (pc+1), (labels ++ [(string, list, pc)]))
prerun ((instruction:list), mem, reg, pc, labels) = prerun (list, mem, reg, (pc+1), labels)

-- recursive function for running the program
run :: InstructionArgs -> IO()
run ([], mem, reg, pc, labels) = do
  putStrLn ("End of program at PC " ++ (show pc))
  prettyPrintMem mem False
run ((instruction:list), mem, reg, pc, labels) = do
  putStrLn "Current Storage State:"
  prettyPrintReg reg True
  prettyPrintMem mem False
  putStrLn "\n\n\n"
  putStrLn (multiplyCharRec '-' "" 0 100)
  putStrLn ("\n\nInstruction '" ++ (show instruction) ++ "'")
  putStrLn ("ProgramCounter " ++ (show pc))                                           -- displays the programcounter
  case (execute instruction (list, mem, reg, pc, labels)) of
    Left err -> putStrLn err                                                          -- if error, halt program and report
    Right (rlist, rmem, rreg, rpc, rlabels) -> run (rlist, rmem, rreg, rpc, rlabels)  -- if result, go on with recursion






-- Execute Statement Function

-- function for pattern matching and executing an instruction, example: "execute instruction mem"
execute :: Instruction -> InstructionArgs -> Either error InstructionResult
-- /func /mem /name /arg1 /arg2 /statement
execute (Nop)               (list, mem, reg, pc, labels) = nop (list, mem, reg, pc, labels)
execute (Mov    loc val)    (list, mem, reg, pc, labels) = mov loc val (list, mem, reg, pc, labels)
execute (And    rg val)    (list, mem, reg, pc, labels) = andI rg val (list, mem, reg, pc, labels)
execute (Or     rg val)    (list, mem, reg, pc, labels) = orI rg val (list, mem, reg, pc, labels)
execute (Not    rg)        (list, mem, reg, pc, labels) = notI rg (list, mem, reg, pc, labels)
execute (Mod    rg val)    (list, mem, reg, pc, labels) = modI rg val (list, mem, reg, pc, labels)
execute (Add    rg val)    (list, mem, reg, pc, labels) = add rg val (list, mem, reg, pc, labels)
execute (Sub    rg val)    (list, mem, reg, pc, labels) = sub rg val (list, mem, reg, pc, labels)
execute (Mul    rg val)    (list, mem, reg, pc, labels) = mul rg val (list, mem, reg, pc, labels)
execute (Div    rg val)    (list, mem, reg, pc, labels)  = divI rg val (list, mem, reg, pc, labels)
execute (Cmp    rg val)     (list, mem, reg, pc, labels) = cmp rg val (list, mem, reg, pc, labels)
execute (Label  string)     (list, mem, reg, pc, labels) = nop (list, mem, reg, pc, labels)
execute (Jmp    string)     (list, mem, reg, pc, labels) = jmp string (list, mem, reg, pc, labels)
execute (Jc     string rg)  (list, mem, reg, pc, labels) = jc string rg (list, mem, reg, pc, labels)
execute (Jeq    string rg)  (list, mem, reg, pc, labels) = jeq string rg (list, mem, reg, pc, labels)
-- error in case of unmatched instruction
execute instr (list, mem, reg, pc, labels) = Left (error ("Runtime Error: invalid statement '" ++ (show instr) ++ "'"))









-- Instruction Functions

-- function that doesn't do anything and leaves the state of the SVM unchanged
nop :: InstructionArgs -> Either error InstructionResult
nop (list, mem, reg, pc, labels) = Right (list, mem, reg, pc, labels)

-- function that copies the content or value of Arg2 into Arg1. If the memory address is out of range it will throw a runtime exception.
mov :: Location -> Value -> InstructionArgs -> Either error InstructionResult
mov (Address adr) (Location loc)   (list, mem, reg, pc, labels) = case (locationRead loc mem reg) of
                                                                    Left err -> Left err
                                                                    Right value -> case (writeMem mem (addressValue adr reg) value) of
                                                                      Left err -> Left err
                                                                      Right rmem -> Right (list, rmem, reg, (pc+1), labels)
mov (Address adr) (Literal lit)    (list, mem, reg, pc, labels) = case (writeMem mem (addressValue adr reg) (lit)) of
                                                                    Left err -> Left err
                                                                    Right rmem -> Right (list, rmem, reg, (pc+1), labels)
mov (Register rg) (Location loc)   (list, mem, reg, pc, labels) = case (locationRead loc mem reg) of
                                                                    Left err -> Left err
                                                                    Right value -> Right (list, mem, (writeReg (rg) reg (value)), (pc+1), labels)
mov (Register rg) (Literal lit)    (list, mem, reg, pc, labels) = Right (list, mem, (writeReg (rg) reg (lit)), (pc+1), labels)

-- function that stores 1 into Arg1 if both arguments are >= 0, otherwise - 1. It accepts only integer numbers, otherwise it raises a runtime exception.
andI :: Register -> Value -> InstructionArgs -> Either error InstructionResult
andI rg (Location loc) (list, mem, reg, pc, labels) = case ((locationRead loc mem reg)) of
                                                      Left err -> Left err
                                                      Right lit -> case (andArgs (readReg rg reg) lit) of
                                                        Left err -> Left err
                                                        Right result -> Right (list, mem, (writeReg rg reg result), (pc+1), labels)
andI rg (Literal lit) (list, mem, reg, pc, labels) = case (andArgs (readReg rg reg) lit) of
                                                      Left err -> Left err
                                                      Right result -> Right (list, mem, (writeReg rg reg result), (pc+1), labels)

-- function that stores -1 into Arg1 if both arguments are < 0, otherwise 1. It accepts only integer numbers, otherwise it raises a runtime exception.
orI :: Register -> Value -> InstructionArgs -> Either error InstructionResult
orI rg (Location loc) (list, mem, reg, pc, labels) = case ((locationRead loc mem reg)) of
                                                      Left err -> Left err
                                                      Right lit -> case (orArgs (readReg rg reg) lit) of
                                                        Left err -> Left err
                                                        Right result -> Right (list, mem, (writeReg rg reg result), (pc+1), labels)
orI rg (Literal lit) (list, mem, reg, pc, labels) = case (orArgs (readReg rg reg) lit) of
                                                      Left err -> Left err
                                                      Right result -> Right (list, mem, (writeReg rg reg result), (pc+1), labels)

-- function that stores -1 in Arg1 if the argument is non-negative, otherwise it stores 0. It accepts only an integer number, otherwise it raises a runtime exception.
notI :: Register -> InstructionArgs -> Either error InstructionResult
notI rg (list, mem, reg, pc, labels) = case (notArgs (readReg rg reg)) of
                                         Left err -> Left err
                                         Right result -> Right (list, mem, (writeReg rg reg result), (pc+1), labels)

-- function that computes the modulus operation (remainder of the integer division) with Arg1 and Arg2 and stores the result in Arg1. It accepts only numerical arguments (integer or float), otherwise it raises a runtime exception.
modI :: Register -> Value -> InstructionArgs -> Either error InstructionResult
modI rg val (list, mem, reg, pc, labels) = case (getLits rg val (list, mem, reg, pc, labels)) of
                                            Left err -> Left err
                                            Right (arg1, arg2) -> case (modArgs arg1 arg2) of
                                              Left err -> Left err
                                              Right result -> Right (list, mem, (writeReg rg reg result), (pc+1), labels)

-- function that computes the sum operation with Arg1 and Arg2 and stores the result in Arg1. It accepts only numerical arguments (integer or float), otherwise it raises a runtime exception.
add :: Register -> Value -> InstructionArgs -> Either error InstructionResult
add rg val (list, mem, reg, pc, labels) = case (getLits rg val (list, mem, reg, pc, labels)) of
                                            Left err -> Left err
                                            Right (arg1, arg2) -> case (addArgs arg1 arg2) of
                                              Left err -> Left err
                                              Right result -> Right (list, mem, (writeReg rg reg result), (pc+1), labels)


-- function that computes the difference operation with Arg1 and Arg2 and stores the result in Arg1. It accepts only numerical arguments (integer or float), otherwise it raises a runtime exception.
sub :: Register -> Value -> InstructionArgs -> Either error InstructionResult
sub rg val (list, mem, reg, pc, labels) = case (getLits rg val (list, mem, reg, pc, labels)) of
                                            Left err -> Left err
                                            Right (arg1, arg2) -> case (subArgs arg1 arg2) of
                                              Left err -> Left err
                                              Right result -> Right (list, mem, (writeReg rg reg result), (pc+1), labels)

-- function that computes the multiplication operation with Arg1 and Arg2 and stores the result in Arg1. It accepts only numerical arguments (integer or float), otherwise it raises a runtime exception.
mul :: Register -> Value -> InstructionArgs -> Either error InstructionResult
mul rg val (list, mem, reg, pc, labels) = case (getLits rg val (list, mem, reg, pc, labels)) of
                                            Left err -> Left err
                                            Right (arg1, arg2) -> case (mulArgs arg1 arg2) of
                                              Left err -> Left err
                                              Right result -> Right (list, mem, (writeReg rg reg result), (pc+1), labels)

-- function that computes the division operation with Arg1 and Arg2 and stores the result in Arg1. Note that with integers you do the integer division and with floats the floating point division. It accepts only numerical arguments (integer or float), otherwise it raises a runtime exception.
divI :: Register -> Value -> InstructionArgs -> Either error InstructionResult
divI rg val (list, mem, reg, pc, labels) = case (getLits rg val (list, mem, reg, pc, labels)) of
                                            Left err -> Left err
                                            Right (arg1, arg2) -> case (divArgs arg1 arg2) of
                                              Left err -> Left err
                                              Right result -> Right (list, mem, (writeReg rg reg result), (pc+1), labels)

-- function that compares the two arguments, returning -1 if Arg1 < Arg2, 0 if they are equal, 1 if Arg1 > Arg2. The result is stored in Arg1. It accepts only numerical values otherwise it raises a runtime exception.
cmp :: Register -> Value -> InstructionArgs -> Either error InstructionResult
cmp rg val (list, mem, reg, pc, labels) = case (getLits rg val (list, mem, reg, pc, labels)) of
                                            Left err -> Left err
                                            Right (arg1, arg2) -> case (compareArgs arg1 arg2) of
                                              Left err -> Left err
                                              Right result -> Right (list, mem, (writeReg rg reg result), (pc+1), labels)

--  function that jumps to the point of the program where the label with the name in Arg1 is defined. This instruction modifies the program counter to do so.
jmp :: String -> InstructionArgs -> Either error InstructionResult
jmp string (list, mem, reg, pc, labels) = case (labelReturnRec string labels) of
                                            Left err -> Left err
                                            Right (rstring, rlist, rpc) -> Right (rlist, mem, reg, rpc, labels)

--  function that jumps at the point of the program where the label with the name in Arg1 is defined, if Arg2 is >= 0. This instruction modifies the program counter to do so.
jc :: String -> Register -> InstructionArgs -> Either error InstructionResult
jc string rg (list, mem, reg, pc, labels) = case (compareArgs (readReg rg reg) (Integer (-1))) of
                                              Left err -> Left err
                                              Right result -> if (result == (Integer 1))
                                                              then jmp string (list, mem, reg, (pc+1), labels)
                                                              else Right (list, mem, reg, (pc+1), labels)

--  function that jumps at the point of the program where the label with the name in Arg1 is defined, if Arg2 is 0. This instruction modifies the program counter to do so.
jeq :: String -> Register -> InstructionArgs -> Either error InstructionResult
jeq string rg (list, mem, reg, pc, labels) = case (compareArgs (readReg rg reg) (Integer 0)) of
                                              Left err -> Left err
                                              Right result -> if (result == (Integer 0))
                                                              then jmp string (list, mem, reg, (pc+1), labels)
                                                              else Right (list, mem, reg, (pc+1), labels)


-- operator that defines a label with the name specified in Arg1. It is not allowed to define labels with the same name, so in this case a runtime error should be raised.
label :: String -> InstructionArgs -> Either error InstructionResult
label string (list, mem, reg, pc, labels)
  | (labelExistsRec string labels) = Left (error ("Label " ++ string ++ " already defined.")) -- checks if the label exists
  | otherwise = Right (list, mem, reg, (pc+1), labels ++ [(string, list, pc)]) -- adds the label to the list of labels




-- Instruction Utility Functions
-- functions commonly used by the instructions defined above

-- function that compares two Literals, returning -1 if Arg1 < Arg2, 0 if they are equal, 1 if Arg1 > Arg2.
compareArgs :: Literal -> Literal -> Either error Literal
compareArgs (Integer arg1) (Integer arg2)
  | (arg1 < arg2)  = Right (Integer (-1))
  | (arg1 == arg2) = Right (Integer 0)
  | (arg1 > arg2)  = Right (Integer 1)
compareArgs (Integer arg1) (Float arg2)
  | ((fromIntegral arg1 :: Double) < arg2)  = Right (Integer (-1))
  | ((fromIntegral arg1 :: Double) == arg2) = Right (Integer 0)
  | ((fromIntegral arg1 :: Double) > arg2)  = Right (Integer 1)
compareArgs (Float arg1) (Integer arg2)
  | (arg1 < (fromIntegral arg2 :: Double))  = Right (Integer (-1))
  | (arg1 == (fromIntegral arg2 :: Double)) = Right (Integer 0)
  | (arg1 > (fromIntegral arg2 :: Double))  = Right (Integer 1)
compareArgs (Float arg1) (Float arg2)
  | (arg1 < arg2)  = Right (Integer (-1))
  | (arg1 == arg2) = Right (Integer 0)
  | (arg1 > arg2)  = Right (Integer 1)
compareArgs _ _ = Left (error ("Values are not numbers"))

-- function for adding two Literals
addArgs :: Literal -> Literal -> Either error Literal
addArgs (Integer arg1) (Integer arg2) = Right (Integer (arg1 + arg2))
addArgs (Integer arg1) (Float arg2) = Right (Float ((fromIntegral arg1 :: Double) + arg2))
addArgs (Float arg1) (Integer arg2) = Right (Float (arg1 + (fromIntegral arg2 :: Double)))
addArgs (Float arg1) (Float arg2) = Right (Float (arg1 + arg2))
addArgs _ _ = Left (error ("Values are not numbers"))

-- function for subtracting two Literals
subArgs :: Literal -> Literal -> Either error Literal
subArgs (Integer arg1) (Integer arg2) = Right (Integer (arg1 - arg2))
subArgs (Integer arg1) (Float arg2) = Right (Float ((fromIntegral arg1 :: Double) - arg2))
subArgs (Float arg1) (Integer arg2) = Right (Float (arg1 - (fromIntegral arg2 :: Double)))
subArgs (Float arg1) (Float arg2) = Right (Float (arg1 - arg2))
subArgs _ _ = Left (error ("Values are not numbers"))

-- function for multiplying two Literals
mulArgs :: Literal -> Literal -> Either error Literal
mulArgs (Integer arg1) (Integer arg2) = Right (Integer (arg1 * arg2))
mulArgs (Integer arg1) (Float arg2) = Right (Float ((fromIntegral arg1 :: Double) * arg2))
mulArgs (Float arg1) (Integer arg2) = Right (Float (arg1 * (fromIntegral arg2 :: Double)))
mulArgs (Float arg1) (Float arg2) = Right (Float (arg1 * arg2))
mulArgs _ _ = Left (error ("Values are not numbers"))

-- function for dividing two Literals
divArgs :: Literal -> Literal -> Either error Literal
divArgs (Integer arg1) (Integer arg2) = Right (Integer (arg1 `div` arg2))
divArgs (Integer arg1) (Float arg2) = Right (Float ((fromIntegral arg1 :: Double) / arg2))
divArgs (Float arg1) (Integer arg2) = Right (Float (arg1 / (fromIntegral arg2 :: Double)))
divArgs (Float arg1) (Float arg2) = Right (Float (arg1 / arg2))
divArgs _ _ = Left (error ("Values are not numbers"))

-- function for returning the modulo over two Literals
modArgs :: Literal -> Literal -> Either error Literal
modArgs (Integer arg1) (Integer arg2) = Right (Integer (arg1 `mod` arg2))
modArgs (Integer arg1) (Float arg2) = Right (Integer (arg1 `mod` (round arg2)))
modArgs (Float arg1) (Integer arg2) = Right (Integer ((round arg1) `mod` arg2))
modArgs (Float arg1) (Float arg2) = Right (Integer ((round arg1) `mod` (round arg2)))
modArgs _ _ = Left (error ("Values are not numbers"))

-- function that returns -1 if both Literals are less than 0, 1 otherwise
andArgs :: Literal -> Literal -> Either error Literal
andArgs (Integer arg1) (Integer arg2)
  | (arg1 >= 0 && arg2 >= 0) = Right (Integer 1)
  | otherwise = Right (Integer (-1))
andArgs (Integer arg1) (Float arg2)
  | (arg1 >= 0 && arg2 >= 0) = Right (Integer 1)
  | otherwise = Right (Integer (-1))
andArgs (Float arg1) (Integer arg2)
  | (arg1 >= 0 && arg2 >= 0) = Right (Integer 1)
  | otherwise = Right (Integer (-1))
andArgs (Float arg1) (Float arg2)
  | (arg1 >= 0 && arg2 >= 0) = Right (Integer 1)
  | otherwise = Right (Integer (-1))
andArgs _ _ = Left (error "Values are not numbers")

-- function that returns -1 if both Literals are less than 0, 1 otherwise
orArgs :: Literal -> Literal -> Either error Literal
orArgs (Integer arg1) (Integer arg2)
  | (arg1 < 0 && arg2 < 0) = Right (Integer (-1))
  | otherwise = Right (Integer 1)
orArgs (Integer arg1) (Float arg2)
  | (arg1 < 0 && arg2 < 0) = Right (Integer (-1))
  | otherwise = Right (Integer 1)
orArgs (Float arg1) (Integer arg2)
  | (arg1 < 0 && arg2 < 0) = Right (Integer (-1))
  | otherwise = Right (Integer 1)
orArgs (Float arg1) (Float arg2)
  | (arg1 < 0 && arg2 < 0) = Right (Integer (-1))
  | otherwise = Right (Integer 1)
orArgs _ _ = Left (error "Values are not numbers")

-- function that returns -1 if the Literal Integer is non-negative, 0 otherwise
notArgs :: Literal -> Either error Literal
notArgs (Integer x)
  | (x < 0) = Right (Integer 0)
  | otherwise = Right (Integer (-1))
notArgs x = Left (error ("Value " ++ (show x) ++ " is not an Integer"))








-- Value Readers

-- function for reading the value of an Address type
addressValue :: Address -> [Literal] -> Literal
addressValue (Direct int)  reg = (Integer int)
addressValue (Indirect rv) reg = readReg rv reg

-- function for reading the value of a Location type
locationValue :: Location -> [Literal] -> Literal
locationValue (Address adr) reg = addressValue adr reg
locationValue (Register rv) reg = readReg rv reg

-- function for reading the value of a location, in memory or in register
locationRead :: Location -> [Literal] -> [Literal] -> Either error Literal
locationRead (Address adr) mem reg = readMem mem (addressValue adr reg)
locationRead (Register rv) mem reg = Right (readReg rv reg)


-- Literal Utility Functions

-- function for retrieving Literal values from Register and Value
getLits :: Register -> Value -> InstructionArgs -> Either error (Literal, Literal)
getLits rg (Location loc) (list, mem, reg, pc, labels) = case (locationRead loc mem reg) of
                                                          Left err -> Left err
                                                          Right val -> Right (((readReg rg reg), val))
getLits rg (Literal val) (list, mem, reg, pc, labels)  = Right (((readReg rg reg), val))

-- function for reading the value of a Literal type
literalToString :: Literal -> String
literalToString (Integer x) = show x
literalToString (Float x)   = show x
literalToString (String x)  = x

-- function for calculating the number of spaces required to keep the prettyPrinters in columns
literalSpacePrint :: Literal -> String
literalSpacePrint lit = (multiplyCharRec ' ' "" 0 (5 - (Prelude.length (literalToString lit)))) ++ "|"

-- function for unwrapping an Integer Literal and converting it to Int
litToInt :: Literal -> Int
litToInt (Integer x) = fromInteger x







-- Register Implementation
--    Register consists of a one dimensional list of Literals
--    Using functions below, register can be initialized, read, written to and printed

-- function for initializing the register
initReg :: [Literal]
initReg = [ (Integer 0) :: Literal | j <- [1..4] ]

-- function for reading from the register
readReg :: Register -> [Literal] -> Literal
readReg (Reg1) reg = reg!!0
readReg (Reg2) reg = reg!!1
readReg (Reg3) reg = reg!!2
readReg (Reg4) reg = reg!!3

-- function for writing to the register
writeReg ::  Register -> [Literal] -> Literal -> [Literal]
writeReg (Reg1) reg value = toList (update 0 value (fromList reg))
writeReg (Reg2) reg value = toList (update 1 value (fromList reg))
writeReg (Reg3) reg value = toList (update 2 value (fromList reg))
writeReg (Reg4) reg value = toList (update 3 value (fromList reg))

-- function for printing the register in a pretty way
prettyPrintReg :: [Literal]-> Bool -> IO()
prettyPrintReg reg printtype = do
  putStrLn "\n\n - Register -"
  putStrLn ("size: " ++  (show (Prelude.length reg)) ++ "\n")
  if (printtype)
  then putStrLn (printMemRecType reg "")
  else putStrLn (printPrettyMemRec reg "" 0 19)






-- Memory Implementation
--    Memory consists of a one dimensional list of Literals
--    Using functions below, memory can be initialized, read, written to and printed

-- function for initializing the memory using integer size to set the size
initMem :: Int -> [Literal]
initMem size = [ (Integer 0) :: Literal | j <- [1..size] ]

-- function for safely reading a memory value in a safe way, will return error if outside bounds
readMem :: [Literal] -> Literal -> Either error Literal
readMem mem (Integer index)
  | (Prelude.length mem) > (fromInteger index) = Right (mem!!(fromInteger index))
  | otherwise = Left (error ("Memory read failed, index " ++ (show index) ++ " out of bounds"))

-- function for safely writing a memory value
writeMem :: [Literal] -> Literal -> Literal -> Either error [Literal]
writeMem mem (Integer index) value
  | (Prelude.length mem) <= (fromInteger index) = Left (error ("Memory write failed, index " ++ (show index) ++ " out of bounds"))
  | otherwise = Right (toList (update (fromInteger index) value (fromList mem)))

-- function for printing the memory in a pretty way
prettyPrintMem :: [Literal]-> Bool -> IO()
prettyPrintMem mem printtype = do
  putStrLn "\n\n - MEMORY -"
  putStrLn ("size: " ++  (show (Prelude.length mem)) ++ "\n")
  if (printtype)
  then putStrLn (printMemRecType mem "")
  else putStrLn (printPrettyMemRec mem "" 0 19)

-- recursive function for concatenating the types and values in an array
printMemRecType :: [Literal] -> String -> String
printMemRecType []        string = string
printMemRecType (x:list)  string = printMemRecType list (string ++ (show x) ++ (literalSpacePrint x))

-- recursive function for concatenating the values in an array in a pretty way
printPrettyMemRec :: [Literal] -> String -> Int -> Int -> String
printPrettyMemRec []        string index break = string
printPrettyMemRec (x:list)  string index break
  | (index `mod` break == (break-1)) = printPrettyMemRec list (string ++ (literalToString x) ++ "\n") (index+1) break
  | otherwise = printPrettyMemRec list (string ++ (literalToString x) ++ (literalSpacePrint x)) (index+1) break








-- Utility Functions

-- function for printing errors and messages, example "printError x"
printError :: ParseError -> IO()
printError err = print err

-- function for printing errors and messages, example: "printError "ParseError Message " (show x)"
printErrorMessage :: String -> String -> IO()
printErrorMessage msg err = print (msg ++ err)

-- function for printing all instructions in a list, example: "mapM_ printT prog" where prog is a list of instructions
printT :: Instruction -> IO()
printT instruction = print instruction

-- recursive function for creating a concatenated string out of a specified number of chars
multiplyCharRec :: Char -> String -> Int -> Int -> String
multiplyCharRec char string current target
  | (current == target) = string
  | (current < target) = multiplyCharRec char (string ++ [char]) (current+1) target
  | otherwise = string

-- recursive function for safely reading a value at the specified index of a list
atIndex :: [a] -> Int -> Either error a
atIndex [] _ = Left (error "Index out of bounds")
atIndex (x:xs) index
  | index <= 0 = Right x
  | otherwise = atIndex xs (index-1)

-- recursive function for checking if a label exists
labelExistsRec :: String -> Labels -> Bool
labelExistsRec string [] = False
labelExistsRec string ((lstring, x, _):list)
  | (string == lstring) = True
  | otherwise = labelExistsRec string list

-- recursive function for returning the label values if it exists
labelReturnRec :: String -> Labels -> Either error (String, [Instruction], Int)
labelReturnRec string [] = Left (error (string ++ " is not defined as a label"))
labelReturnRec string ((lstring, llist, pc):list)
  | (string == lstring) = Right (string, llist, pc)
  | otherwise = labelReturnRec string list
