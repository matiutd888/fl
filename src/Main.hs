import StatementChecker
import Interpreter
import TestProgram
import System.IO

main :: IO ()

main = do
  case runTypeChecker p of
    Left m -> putStr m
    _ -> 
      do 
        putStrLn "TYPECHECK OK" 
        case runInterpreter p of
          Left m -> putStr m
          right -> putStrLn "No error in interpreter"