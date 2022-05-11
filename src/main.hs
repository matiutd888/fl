import StatementChecker
import TestProgram
import System.IO

main :: IO ()

main = do
	putStr $ show $ runTypeChecker p
