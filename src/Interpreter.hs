{-# LANGUAGE FlexibleContexts #-}

module Interpreter where

import qualified AbsGramatyka as A
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Either as DE
import qualified Data.Map as M
import qualified Data.Set as S
import Errors
import PrintGramatyka (printTree)

-- TODO przemyśleć return.
type Loc = Integer

-- TODO tutaj można się zastanowić co z returnem. 
-- Sofar pomysłem jest dodać własny znacznik, który będzie wskazywał gdize w storze umieścić return. (-1)?
retLoc :: Loc
retLoc = -1

data FunctionData =
  FunctionData
    { env :: Env
    , stmt :: A.Stmt
    , signature :: [A.Arg]
    }
  deriving (Prelude.Eq, Prelude.Ord, Prelude.Show)

data Env =
  Env
    { variables :: M.Map A.Ident Loc
  -- No need to check levels - we already checked them in type checking.
  -- No need to store loc - functions cannot be changed in block, they are always the same.
  -- funct
    , functions :: M.Map A.Ident FunctionData
    }
  deriving (Prelude.Eq, Prelude.Ord, Prelude.Show)
    -- ~ = Int a
    -- ~ | Str a
    -- ~ | Bool a
    -- ~ | Void a
    -- ~ | Tuple a [Type' a]
    -- ~ | Function a (Type' a) [ArgType' a]
  -- ~ deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

-- TODO think what will be stored in function map, what in variables map and how it will change what we store in the memoryCell.
-- ~ type Type = Type' BNFC'Position
-- ~ data Type' a
data Data
  = Int Integer
  | Str String
  | Bool Bool
  | Tuple [Data]
  | Function FunctionData
  deriving (Prelude.Eq, Prelude.Ord, Prelude.Show)

-- Maybe we will need to store also something else in the future.
data Store =
  Store
    { memory :: M.Map Int Data
    }

type EvalT a = ReaderT Env (StateT Store (ExceptT String Identity)) a
    -- ~ = ETuple a [Expr' a]
    -- ~ | EVar a Ident
    -- ~ | ELitInt a Integer DONE
    -- ~ | ELitTrue a DONE
    -- ~ | ELitFalse a DONE
    -- ~ | ELambda a (Lambda' a)
    -- ~ | EApp a (Callee' a) [Expr' a]
    -- ~ | EString a String DONE
    -- ~ | Neg a (Expr' a) DONE
    -- ~ | Not a (Expr' a) DONE
    -- ~ | EMul a (Expr' a) (MulOp' a) (Expr' a) DONE
    -- ~ | EAdd a (Expr' a) (AddOp' a) (Expr' a) DONE
    -- ~ | ERel a (Expr' a) (RelOp' a) (Expr' a)
    -- ~ | EAnd a (Expr' a) (Expr' a) DONE
    -- ~ | EOr a (Expr' a) (Expr' a) DONE
  -- ~ deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

-- ~ type Expr = Expr' BNFC'Position
-- ~ data Expr' a
-- ~ fromInt :: MonadError String m => A.BNFC'Position -> Data -> m Int
fromInt :: A.BNFC'Position -> Data -> EvalT Integer
fromInt pos (Int i) = return i
fromInt pos d =
  throwError $
  "TYPECHECKER ERROR " ++
  showPosition pos ++ "expected data of type int, received " ++ show d

-- ~ fromBool :: MonadError String m => A.BNFC'Position -> Data -> m Bool
fromBool :: A.BNFC'Position -> Data -> EvalT Bool
fromBool pos (Bool b) = return b
fromBool pos d =
  throwError $
  "TYPECHECKER ERROR " ++
  showPosition pos ++ "expected data of type bool, received " ++ show d

evalExpr :: A.Expr -> EvalT Data
evalExpr (A.ELitTrue _) = return $ Bool True
evalExpr (A.ELitFalse _) = return $ Bool False
evalExpr (A.ELitInt _ n) = return $ Int n
evalExpr (A.EString _ s) = return $ Str s
evalExpr (A.Neg _ e) = do
  n <- evalExpr e >>= fromInt (A.hasPosition e)
  return $ Int $ -n
evalExpr (A.Not _ e) = do
  b <- evalExpr e >>= fromBool (A.hasPosition e)
  return $ Bool $ not b
evalExpr (A.EMul pos e1 (A.Times _) e2) = do
  n1 <- evalExpr e1 >>= fromInt (A.hasPosition e1)
  n2 <- evalExpr e2 >>= fromInt (A.hasPosition e2)
  return $ Int $ n1 * n2
evalExpr (A.EMul pos e1 (A.Div _) e2) = do
  n1 <- evalExpr e1 >>= fromInt (A.hasPosition e1)
  n2 <- evalExpr e2 >>= fromInt (A.hasPosition e2)
  assertM (n2 /= 0) $
    showPosition pos ++
    "Attemtpting to perform division by zero, " ++
    printTree e2 ++ " is equal to zero"
  return $ Int $ div n1 n2
evalExpr (A.EMul pos e1 (A.Mod _) e2) = do
  n1 <- evalExpr e1 >>= fromInt (A.hasPosition e1)
  n2 <- evalExpr e2 >>= fromInt (A.hasPosition e2)
  assertM (n2 /= 0) $
    showPosition pos ++
    "Attemtpting to perform modulo by zero, " ++
    printTree e2 ++ " is equal to zero"
  return $ Int $ mod n1 n2
evalExpr (A.EAdd pos e1 (A.Plus _) e2) = do
  n1 <- evalExpr e1 >>= fromInt (A.hasPosition e1)
  n2 <- evalExpr e2 >>= fromInt (A.hasPosition e2)
  return $ Int $ n1 + n2
evalExpr (A.EAdd pos e1 (A.Minus _) e2) = do
  n1 <- evalExpr e1 >>= fromInt (A.hasPosition e1)
  n2 <- evalExpr e2 >>= fromInt (A.hasPosition e2)
  return $ Int $ n1 - n2
evalExpr (A.EAnd pos e1 e2) = do
  n1 <- evalExpr e1 >>= fromBool (A.hasPosition e1)
  n2 <- evalExpr e2 >>= fromBool (A.hasPosition e2)
  return $ Bool $ n1 && n2
evalExpr (A.EOr pos e1 e2) = do
  n1 <- evalExpr e1 >>= fromBool (A.hasPosition e1)
  n2 <- evalExpr e2 >>= fromBool (A.hasPosition e2)
  return $ Bool $ n1 || n2
