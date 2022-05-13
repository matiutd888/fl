{-# LANGUAGE FlexibleContexts #-}

module Interpreter where

import qualified AbsGramatyka as A
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Either as DE
import qualified Data.Map as M
import qualified Data.Maybe as DM
import qualified Data.Set as S
import Errors
import PrintGramatyka (printTree)

-- TODO think about return.
-- A flag will suffice. I think it should be saved in the environment.
type Loc = Integer

-- TODO tutaj można się zastanowić co z returnem. 
-- Sofar pomysłem jest dodać własny znacznik, który będzie wskazywał gdize w storze umieścić return. (-1)?
retLoc :: Loc
retLoc = -1

-- Should functions be always passed by copy or by reference? Maybe both?
-- My idea is that by reference. 
-- Why? Function has evironment. To pass function by value I would have to change environment so that
-- all local variables in function (Yes, there can be local variables in function because of closures)
-- Won't be affected.
-- I have to typecheck ref function.
-- TODO.
-- That's why I cannot allow passing functions by reference - 
-- I need to typecheck it - throw error if "ref" is near function type.
-- For now I won't worry about it though - I want to write passing by reference variables.
data FunctionData =
  FunctionData
    { env :: Env
    , stmt :: A.Stmt
    , arguments :: [A.Arg]
    , retType :: A.Type
    }
  deriving (Prelude.Eq, Prelude.Ord, Prelude.Show)

data Env =
  Env
    { variables :: M.Map A.Ident Loc
  -- No need to check levels - we already checked them in type checking.
  -- No need to store loc - functions cannot be changed in block, they are always the same.
  -- funct
    , hasReturn :: Bool -- It will be important in checking whether return has occured.
    , functions :: M.Map A.Ident FunctionData
    }
  deriving (Prelude.Eq, Prelude.Ord, Prelude.Show)

-- TODO think what will be stored in function map, what in variables map and how it will change what we store in the memoryCell.
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
    { memory :: M.Map Loc Data
    }

type EvalT a = ReaderT Env (StateT Store (ExceptT String Identity)) a
    -- ~ = ETuple a [Expr' a] DONE
    -- ~ | EVar a Ident DONE
    -- ~ | ELitInt a Integer DONE
    -- ~ | ELitTrue a DONE
    -- ~ | ELitFalse a DONE
    -- ~ | ELambda a (Lambda' a) DONE
    -- ~ | EApp a (Callee' a) [Expr' a]
    -- ~ | EString a String DONE
    -- ~ | Neg a (Expr' a) DONE
    -- ~ | Not a (Expr' a) DONE
    -- ~ | EMul a (Expr' a) (MulOp' a) (Expr' a) DONE
    -- ~ | EAdd a (Expr' a) (AddOp' a) (Expr' a) DONE
    -- ~ | ERel a (Expr' a) (RelOp' a) (Expr' a) DONE
    -- ~ | EAnd a (Expr' a) (Expr' a) DONE
    -- ~ | EOr a (Expr' a) (Expr' a) DONE
  -- ~ deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

typeCheckerError :: String
typeCheckerError = "TYPECHECKER ERROR "

-- ~ type Expr = Expr' BNFC'Position
-- ~ data Expr' a
-- ~ fromInt :: MonadError String m => A.BNFC'Position -> Data -> m Int
fromInt :: A.BNFC'Position -> Data -> EvalT Integer
fromInt pos (Int i) = return i
fromInt pos d =
  throwError $
  typeCheckerError ++
  showPosition pos ++ "expected data of type int, received " ++ show d

-- ~ fromBool :: MonadError String m => A.BNFC'Position -> Data -> m Bool
fromBool :: A.BNFC'Position -> Data -> EvalT Bool
fromBool pos (Bool b) = return b
fromBool pos d =
  throwError $
  typeCheckerError ++
  showPosition pos ++ "expected data of type bool, received " ++ show d

fromFunction :: A.BNFC'Position -> Data -> EvalT FunctionData
fromFunction pos (Function f) = return f
fromFunction pos d =
  throwError $
  typeCheckerError ++
  showPosition pos ++ "expected data of type function, received " ++ show d

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
evalExpr (A.ERel pos e1 relop e2) = do
  n1 <- evalExpr e1 >>= fromInt (A.hasPosition e1)
  n2 <- evalExpr e2 >>= fromInt (A.hasPosition e2)
  return $ Bool $ (operation relop) n1 n2
evalExpr (A.EVar pos ident) = do
  env <- ask
  s <- get
  case M.lookup ident $ variables env of
    Nothing -> do
      case M.lookup ident $ functions env of
        Nothing ->
          throwError $ typeCheckerError ++ (undefinedReferenceMessage ident pos)
        Just f -> return $ Function f
    Just l -> return $ DM.fromJust (M.lookup l $ memory s)
evalExpr (A.ETuple pos expressions) = do
  values <- mapM evalExpr expressions
  return $ Tuple values
evalExpr (A.ELambda pos (A.Lambda _ args rT block)) = do
  e <- ask
  return $
    Function $
    FunctionData
      { env = e
      , stmt = A.BStmt (A.hasPosition block) block
      , retType = rT
      , arguments = args
      }
evalExpr (A.EApp pos (A.LambdaCallee p l) exprs) = do
  function <- evalExpr (A.ELambda p l) >>= fromFunction p
  handleFunction function exprs
evalExpr (A.EApp pos (A.IdentCallee p ident) exprs) = do
  function <- evalExpr (A.EVar p ident) >>= fromFunction p
  handleFunction function exprs

evalWithLoc :: A.Expr -> EvalT Loc
evalWithLoc e@(A.EVar pos ident) = do
  env <- ask
  case M.lookup ident (variables env) of
    Just l -> return l
    Nothing ->
      throwError $
      typeCheckerError ++ showPosition pos ++ "evar variable not found"
evalWithLoc e =
  throwError $ typeCheckerError ++ showPositionOf e ++ "not a variable"

handleFunction :: FunctionData -> [A.Expr] -> handle
handleFunction = undefined

-- ~ type Lambda = Lambda' BNFC'Position
-- ~ data Lambda' a = Lambda a [Arg' a] (Type' a) (Block' a)
operation :: A.RelOp -> (Integer -> Integer -> Bool)
operation (A.LTH _) = (<)
operation (A.LE _) = (<=)
operation (A.GTH _) = (>)
operation (A.GE _) = (>=)
operation (A.EQU _) = (==)
operation (A.NE _) = (/=)
    -- ~ LTH p -> p
    -- ~ LE p -> p
    -- ~ GTH p -> p
    -- ~ GE p -> p
    -- ~ EQU p -> p
    -- ~ NE p -> p
