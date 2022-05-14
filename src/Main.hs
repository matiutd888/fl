import StatementChecker
import Interpreter
import TestProgram
import System.IO

main :: IO ()

main = do
  case runTypeChecker p of
    Left m -> putStr $ m
    _ -> putStrLn "TYPECHECK OK" >> (putStr (show $ runInterpreter p))
    