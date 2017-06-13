module Main (main) where

import ParseSVM
import Text.Parsec.Error
-- import Data.Typeable
import Data.Sequence
import Data.Foldable (toList)
import System.Environment





-- main function
main = do
  -- read from stdin and parse text-based SVM file
  input <- getContents
  memsize <- getArgs
  let result = parse program "(standard input)" input

  -- initializing the memory of size N where N is the command line argument
  let memorysize = (read (memsize!!0) :: Int)
  let memory = initMem memorysize
  let registers = initReg

  -- unwrap Either: prog of type Program or err of type ParseError
  case result of
    Left err -> printError err
    Right prog -> debug prog memory registers
    -- Right prog -> run prog memory registers



-- Run Function

-- recursive function for running the program
run :: [Instruction] -> [Literal] -> [Literal] -> IO()
run [] mem reg = putStrLn "End of program"
run (instruction:list) mem reg = do
  print instruction
  prettyPrintMem mem
  case (execute instruction mem reg) of
    Left err -> putStrLn err  -- if error, halt program and report
    Right (rmem, rreg) -> run list rmem rreg -- if result, go on with recursion






-- Execute Statement Function

-- function for pattern matching and executing an instruction, example: "execute instruction mem"
execute :: Instruction -> [Literal] -> [Literal] -> Either error ([Literal], [Literal])
-- /func /mem /name /arg1 /arg2 /statement
execute (Nop)               mem reg = nop mem reg
execute (Mov    loc val)    mem reg = mov loc val mem reg
-- execute (And    reg val)    mem reg = print reg
-- execute (Or     reg val)    mem reg = print reg
-- execute (Not    reg)        mem reg = print reg
-- execute (Mod    reg val)    mem reg = print reg
-- execute (Add    reg val)    mem reg = print reg
-- execute (Sub    reg val)    mem reg = print reg
-- execute (Mul    reg val)    mem reg = print reg
-- execute (Div    reg val)    mem reg = print reg
-- execute (Cmp    reg val)    mem reg = print reg
-- execute (Label  string)     mem reg = print string
-- execute (Jmp    string)     mem reg = print string
-- execute (Jc     string reg) mem reg = print string
-- execute (Jeq    string reg) mem reg = print string
-- error in case of unmatched instruction
execute x mem reg = Left (error ("Runtime Error: invalid statement '" ++ (show x) ++ "'"))









-- -- Instruction Functions

-- function that doesn't do anything and leaves the state of the SVM unchanged
nop :: [Literal] -> [Literal] -> Either error ([Literal], [Literal])
nop mem reg = Right (mem, reg)

-- function that copies the content or value of Arg2 into Arg1. If the memory address is out of range it will throw a runtime exception.
mov :: Location -> Value -> [Literal] -> [Literal] -> Either error ([Literal], [Literal])
mov (Address adr) (Location loc)   mem reg = case (writeMem mem (addressValue adr reg) (locationValue loc reg)) of
                                              Left err -> Left err
                                              Right rmem -> Right (rmem, reg)
mov (Address adr) (Literal lit)    mem reg = case (writeMem mem (addressValue adr reg) (lit)) of
                                              Left err -> Left err
                                              Right rmem -> Right (rmem, reg)
mov (Register rg) (Location loc)   mem reg = case (writeMem mem (registerValue rg reg) (locationValue loc reg)) of
                                              Left err -> Left err
                                              Right rmem -> Right (rmem, reg)
mov (Register rg) (Literal lit)    mem reg = case (writeMem mem (registerValue rg reg) (lit)) of
                                              Left err -> Left err
                                              Right rmem -> Right (rmem, reg)
mov x y mem reg = Left (error ("Function not executed properly. " ++ (show x) ++ "\n" ++ (show y)))

-- -- function that stores 1 into Arg1 if both arguments are >= 0, otherwise - 1. It accepts only integer numbers, otherwise it raises a runtime exception.
-- andI :: Register -> Either Register (Either Address constant) -> IO ()
-- andI arg1 arg2 = print (arg1 ++ arg2)
--
-- -- function that stores -1 into Arg1 if both arguments are < 0, otherwise 1. It accepts only integer numbers, otherwise it raises a runtime exception.
-- orI :: Register -> Either Register (Either Address constant) -> IO ()
-- orI arg1 arg2 = print (arg1 ++ arg2)
-- -- if arg1 < 0 and arg2 < 0
-- --  arg1 = -1
-- -- else
-- --  arg1 = 1
--
-- -- function that stores -1 in Arg1 if the argument is non-negative, otherwise it stores 0. It accepts only an integer number, otherwise it raises a runtime exception.
-- notI :: Register -> IO ()
-- notI arg1 = print arg1
-- -- if arg1 > 0
-- --  arg1 = -1
-- -- else
-- --  arg1 = 0
--
-- -- function that computes the modulus operation (remainder of the integer division) with Arg1 and Arg2 and stores the result in Arg1. It accepts only numerical arguments (integer or float), otherwise it raises a runtime exception.
-- modI :: Register -> Either Register (Either Address constant) -> IO () -- change output from IO () to () to make it void instead of print
-- modI arg1 arg2 = print (arg1 ++ arg2)
--
-- -- function that computes the sum operation with Arg1 and Arg2 and stores the result in Arg1. It accepts only numerical arguments (inte- ger or float), otherwise it raises a runtime exception.
-- addI :: Register -> Either Register (Either Address constant) -> IO ()
-- addI arg1 arg2 = print (arg1 ++ arg2)
--
-- -- function that computes the difference operation with Arg1 and Arg2 and stores the result in Arg1. It accepts only numerical arguments (integer or float), otherwise it raises a runtime exception.
-- subI :: Register -> Either Register (Either Address constant) -> IO ()
-- subI arg1 arg2 = print (arg1 ++ arg2)
--
-- -- function that computes the multiplication operation with Arg1 and Arg2 and stores the result in Arg1. It accepts only numerical arguments (integer or float), otherwise it raises a runtime exception.
-- mulI :: Register -> Either Register (Either Address constant) -> IO ()
-- mulI arg1 arg2 = print (arg1 ++ arg2)
--
-- -- function that computes the division operation with Arg1 and Arg2 and stores the result in Arg1. Note that with integers you do the integer division and with floats the floating point division. It accepts only numerical arguments (integer or float), otherwise it raises a runtime exception.
-- divI :: Register -> Either Register (Either Address constant) -> IO ()
-- divI arg1 arg2 = print (arg1 ++ arg2)
--
-- -- function that compares the two arguments, returning -1 if Arg1 < Arg2, 0 if they are equal, 1 if Arg1 > Arg2. The result is stored in Arg1. It accepts only numerical values otherwise it raises a runtime exception.
-- cmpI :: Register -> Either Register (Either Address constant) -> IO ()
-- cmpI arg1 arg2 = print (arg1 ++ arg2)
--
-- --  function that jumps to the point of the program where the label with the name in Arg1 is defined. This instruction modifies the program counter to do so.
-- jmpI :: String -> IO ()
-- jmpI id = print id
--
-- --  function that jumps at the point of the program where the label with the name in Arg1 is defined, if Arg2 is >= 0. This instruction modifies the program counter to do so.
-- jcI :: String -> Register -> IO ()
-- jcI id arg2 = print (id ++ arg2)
--
-- --  function that jumps at the point of the program where the label with the name in Arg1 is defined, if Arg2 is 0. This instruction modifies the program counter to do so.
-- jeqI :: String -> Register -> IO ()
-- jeqI id arg2 = print (id ++ arg2)
--
-- --  operator that defines a label with the name specified in Arg1. It is not allowed to define labels with the same name, so in this case a runtime error should be raised.
-- labelI :: String -> IO ()
-- labelI id = print id



-- Data Readers
-- functions for getting the values out of Address, Value and Location

-- getAddress :: Address -> [Int] -> Either Integer Register
-- getAddress (Direct int) mem = Left int
-- getAddress (Indirect reg) mem = Right reg
--
-- getLocation :: Location -> [Int] -> Either Register Address
-- getLocation (Register reg) mem = Left reg
-- getLocation (Address adr) mem = Right adr
--
-- getValue :: Value -> [Int] -> Either Literal Location
-- getValue (Literal lit)  mem = Left lit
-- getValue (Location loc) mem = Right loc
--
-- getLiteral :: Literal -> [Int] -> Either Integer (Either Double String)
-- getLiteral (Integer int) mem = Left int
-- getLiteral (Float flt)   mem = Right (Left flt)
-- getLiteral (String str)  mem = Right (Right str)

-- function for unwrapping values
--   case (getValue val mem) of
--     Left liter -> case (getLiteral liter mem) of
--                     Left int -> print int
--                     Right dblstr -> print dblstr
--     Right locat -> case (getLocation locat mem) of
--                     Left register -> print register
--                     Right address -> case (getAddress address mem) of
--                                       Left int -> print int
--                                       Right reg -> print reg



-- Value Readers

-- function for reading the value of an Address type
addressValue :: Address -> [Literal] -> Literal
addressValue (Direct int)  reg = (Integer int)
addressValue (Indirect rv) reg = registerValue rv reg

-- function for reading the value of a Location type
locationValue :: Location -> [Literal] -> Literal
locationValue (Address adr) reg = addressValue adr reg
locationValue (Register rv) reg = registerValue rv reg



-- Register Implementation
--    Register consists of a one dimensional list of Literals
--    Using functions below, register can be initialized and read

-- function for initializing the register
initReg :: [Literal]
initReg = [ (Integer 0) :: Literal | j <- [1..4] ]

-- function for reading from the register
registerValue :: Register -> [Literal] -> Literal
registerValue (Reg1) reg = reg!!0
registerValue (Reg2) reg = reg!!1
registerValue (Reg3) reg = reg!!2
registerValue (Reg4) reg = reg!!3





-- Memory Implementation
--    Memory consists of a one dimensional list of Literals
--    Using functions below, memory can be initialized, read, written to and printed

-- function for initializing the memory using integer size to set the size
initMem :: Int -> [Literal]
initMem size = [ (Integer 0) :: Literal | j <- [1..size] ]

-- function for safely reading a memory value in a safe way, will return error if outside bounds
readMem :: [Literal] -> Int -> Either error Literal
readMem mem index
  | (Prelude.length mem) > index = Right (mem!!index)
  | otherwise = Left (error ("Memory read failed, index " ++ (show index) ++ " out of bounds"))

-- function for safely writing a memory value
writeMem :: [Literal] -> Literal -> Literal -> Either error [Literal]
writeMem mem (Integer index) value
  | (Prelude.length mem) <= (fromInteger index) = Left (error ("Memory write failed, index " ++ (show index) ++ " out of bounds"))
  | otherwise = Right (toList (update (fromInteger index) value (fromList mem)))

-- function for printing the memory in a pretty way
prettyPrintMem :: [Literal] -> IO()
prettyPrintMem mem = do
  putStrLn "\n\n - MEMORY - \n"
  putStrLn (prettyPrintMemRec mem 1 0 "")

-- recursive function for concatenating the elements of a list in a pretty string
prettyPrintMemRec :: [Literal] -> Int -> Int -> String -> String
-- absIndex is equal to index divided by 2 as the real index also counts ',' as elements, hence the +2
prettyPrintMemRec mem index absIndex string
  -- if index too large, end the recursion:
  | ((Prelude.length mem) * 2) < (index) = string ++ "\n   memory size: " ++ (show absIndex) ++ "\n\n"
  -- break the line if the index is divisible by 20 (every 20 elements):
  | (absIndex > 0) && (absIndex `mod` 20 == 19) = prettyPrintMemRec mem (index + 2) (absIndex+1) (string ++ ([show mem!!index]) ++ "\n")
  -- add the value of the index to the string:
  | ((show mem!!index) /= '[') && ((show mem!!index) /= ']') && ((show mem!!index) /= ',') = prettyPrintMemRec mem (index + 2) (absIndex+1) (string ++ ([show mem!!index]) ++ " ")
  -- no matching value, increment index and continue recursion:
  | otherwise = prettyPrintMemRec mem (index+2) (absIndex+1) string





-- Debug Functions
-- Debugging allows for instruction-functions to be printed
-- Warning: initial memory and register state is always used as a side effect

-- recursive function for debugging the program
debug :: [Instruction] -> [Literal] -> [Literal] -> IO ()
debug [] mem reg = putStrLn "End of program"
debug (instruction:list) mem reg = do
  putStrLn ("DEBUG: " ++ (show instruction))
  prettyPrintMem mem
  debugEx instruction mem reg
  debug list mem reg

-- function for debugging executable functions, example "debug instruction mem"
debugEx :: Instruction -> [Literal] -> [Literal] -> IO ()
-- /func /mem /name /arg1 /arg2 /statement
debugEx (Nop)               mem reg = print mem
debugEx (Mov    loc val)    mem reg = debugmov loc val mem reg
-- error in case of unmatched instruction
debugEx x mem reg = putStrLn (error ("Runtime Debugging Error: invalid statement '" ++ (show x) ++ "'"))



debugmov :: Location -> Value -> [Literal] -> [Literal] -> IO ()
debugmov (Address adr) (Location loc)   mem reg = print ((show (addressValue adr reg)) ++ (show loc))
debugmov (Address adr) (Literal lit)    mem reg = print ((show (addressValue adr reg)) ++ (show lit))
debugmov (Register rg) (Location loc)   mem reg = print ((show (registerValue rg reg)) ++ (show loc))
debugmov (Register rg) (Literal lit)    mem reg = print ((show (registerValue rg reg)) ++ (show lit))




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

-- recursive function for safely reading a value at the specified index of a list
atIndex :: [a] -> Int -> Either error a
atIndex [] _ = Left (error "Index out of bounds")
atIndex (x:xs) index
  | index <= 0 = Right x
  | otherwise = atIndex xs (index-1)
