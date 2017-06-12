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

  -- unwrap Either: prog of type Program or err of type ParseError
  case result of
    Left err -> printError err
    Right prog -> run prog memory



-- Run Function

-- recursive function for running the program
run :: [Instruction] -> [Int] -> IO()
run [] mem = putStrLn "End of program"
run (instruction:list) mem = do
  prettyPrintMem mem
  print instruction
  case execute instruction mem of
    Left err -> putStrLn err  -- if error, halt program and report
    Right res -> run list res -- if result, go on with recursion




-- Execute Statement Function

-- function for executing all statements in a list, example: "mapM_ execute prog" where prog is a list of instructions
execute :: Instruction -> [Int] -> Either error [Int]
-- /func /mem /name /arg1 /arg2 /statement
execute (Nop)               mem = nopI mem
execute (Mov    loc val)    mem = movI loc val mem
-- execute (And    reg val)    mem = print reg
-- execute (Or     reg val)    mem = print reg
-- execute (Not    reg)        mem = print reg
-- execute (Mod    reg val)    mem = print reg
-- execute (Add    reg val)    mem = print reg
-- execute (Sub    reg val)    mem = print reg
-- execute (Mul    reg val)    mem = print reg
-- execute (Div    reg val)    mem = print reg
-- execute (Cmp    reg val)    mem = print reg
-- execute (Label  string)     mem = print string
-- execute (Jmp    string)     mem = print string
-- execute (Jc     string reg) mem = print string
-- execute (Jeq    string reg) mem = print string
-- -- error in case of unmatched instruction
execute x mem = Left (error ("Runtime Error: invalid statement '" ++ (show x) ++ "'"))
-- execute x mem = putStrLn ("Runtime Error: invalid statement '" ++ (show x) ++ "'")






-- -- Instruction Functions

-- function that doesn't do anything and leaves the state of the SVM unchanged
nopI :: [Int] -> Either error [Int]
nopI mem = Right mem

-- function that copies the content or value of Arg2 into Arg1. If the memory address is out of range then it throws a runtime exception.
movI :: Location -> Value -> [Int] -> Either error [Int]
-- movI loc val mem = writeMem mem 100 10
movI loc val mem = do
  -- print ((show loc) ++ " & " ++ (show val))
  writeMem mem 2 3

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







-- Memory Implementation
--    Memory consists of a one dimensional list of Integers
--    Using functions below, memory can be initialized, read, written to and printed

-- function for initializing the memory using integer size to set the size
initMem :: Int -> [Int]
initMem size = [ 0 :: Int | j <- [1..size] ]

-- function for safely reading a memory value in a safe way, will return error if outside bounds
readMem :: [Int] -> Int -> Either error Int
readMem mem index
  | (Prelude.length mem) > index = Right (mem!!index)
  | otherwise = Left (error ("Memory read failed, index " ++ (show index) ++ " out of bounds"))

-- function for safely writing a memory value
writeMem :: [Int] -> Int -> Int -> Either error [Int]
writeMem mem index value
  | (Prelude.length mem) <= index = Left (error ("Memory write failed, index " ++ (show index) ++ " out of bounds"))
  | otherwise = Right (toList (update index value (fromList mem)))

-- function for printing the memory in a pretty way
prettyPrintMem :: [Int] -> IO()
prettyPrintMem mem = do
  putStrLn "\n\n - MEMORY - \n"
  putStrLn (prettyPrintMemRec mem 1 0 "")

-- recursive function for concatenating the elements of a list in a pretty string
prettyPrintMemRec :: [Int] -> Int -> Int -> String -> String
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
