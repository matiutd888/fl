module Interpreter where

import AbsGramatyka as A
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Either as DE
import qualified Data.Map as M
import qualified Data.Set as S

type Loc = Int

data FunctionData = FunctionData {
  env :: Env,
  stmt :: A.Stmt
}

data Env = Env {
  variables :: M.Map A.Ident Loc,
  -- No need to check levels - we already checked them in type checking.
  -- No need to store loc - functions cannot be changed in block, they are always the same.
  -- funct
  functions :: M.Map A.Ident FunctionData
}

-- TODO think what will be stored in function map, what in variables map and how it will change what we store in the memoryCell.
data Primary = Str String | Int Int | Bool Bool
type Tuple = [TupleElem]
data TupleElem = TupleElem Primary | TupleRec Tuple
data Function = Function FunctionData

data Data = PData Primary | TData Tuple | FData Function

-- Maybe we will need to store also something else in the future.
data Store = Store {
  memory :: M.Map Int Data
}

type StmtEval a = ReaderT Env (StateT Store (ExceptT String Identity)) a
