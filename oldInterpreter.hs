module Main (main) where

import ParseSVM
import Text.Parsec.Error
import Data.Typeable




-- main function
main = do
  -- read from stdin and parse text-based SVM file
  input <- getContents
  let result = parse program "(standard input)" input

  -- unwrap Either: prog of type Program or err of type ParseError
  case result of
    Left err -> printError err;
    Right prog -> mapM_ execute prog;


execute :: Instruction -> IO ()
execute (Jmp arg1) = print arg1
execute (Mov arg1 arg2) = print arg1
execute stmnt = putStrLn ("Runtime Error: invalid statement '" ++ stmnt ++ "'")



-- function that casts the Instruction to a [Char] and passes it to the executer
executeInstruction :: Instruction -> IO ()
executeInstruction instuction = execute (show instuction) instuction




-- Execute Statement Function

-- function for executing all statements in a list, example: "mapM_ execute prog" where prog is a list of instructions
-- execute :: String -> Instruction -> IO ()
-- -- execute ('N':'o':'p':' ': args) = print "Nothing"
-- -- execute ('M':'o':'v':' ': args) instr = print (typeOf instr)
-- execute ('M':'o':'v':' ':'(': args) instr = print Reg1
-- -- execute ('A':'n':'d':' ': args) = andI args
-- -- execute ('O':'r':' ': args)     = orI args
-- -- execute ('N':'o':'t':' ': args) = notI args
-- -- execute ('M':'o':'d':' ': args) = modI args
-- -- execute ('A':'d':'d':' ': args) = addI args
-- -- execute ('S':'u':'b':' ': args) = subI args
-- -- execute ('M':'u':'l':' ': args) = mulI args
-- -- execute ('D':'i':'v':' ': args) = divI args
-- -- execute ('C':'m':'p':' ': args) = cmpI args
-- -- execute ('J':'m':'p':' ': args) = jmpI args
-- -- execute ('J':'c':' ': args)     = jcI args
-- -- execute ('J':'e':'q':' ': args) = jeqI args
-- execute ('L':'a':'b':'e':'l':' ': args) instr = print instr
-- execute args instr = putStrLn ("Runtime Error: invalid statement '" ++ args ++ "'")




-- TODO: TAKE THE ARGUMENTS OUT OF THE ABOVE INSTRUCTIONS ("args") AND PASS THEM ON

-- movArgs :: String -> Pair Location Value
-- parseLocation :: String -> Register
-- parseLocation ('R':'e':'g':'i':'s':'t':'e':'r':' ':'R':'e':'g':reg:')':' ':'(':arg2) = read "Register Reg1" :: Register
-- parseLocation ('A':'d':'d':'r':'e':'s':'s':' ':'R':'e':'g':reg:')':' ':'(':arg2) = read ("Reg1") :: Location

-- parseValue :: String -> IO()
-- parseValue ('L':'o':'c':'a':'t':'i':'o':'n')


-- -- Instruction Functions
--
-- -- function that doesn't do anything and leaves the state of the SVM unchanged
-- nopI :: IO ()
-- nopI = print '%'
--
-- -- function that copies the content or value of Arg2 into Arg1. If the memory address is out of range then it throws a runtime exception.
-- movI :: Either Address Register -> Either Register (Either Address constant) -> IO ()
-- movI arg1 arg2 = print (arg1 ++ arg2)
--
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
