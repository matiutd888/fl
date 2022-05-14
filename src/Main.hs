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
        interpreterOutput <- runInterpreter p
        case interpreterOutput of
          Left m -> putStr m
          right -> putStrLn "No error in interpreter"