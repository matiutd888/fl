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

propagateFlags :: Env -> EvalT Env
propagateFlags newEnv = ask >>= \env -> return $ env {hasReturn = hasReturn newEnv}

newLoc :: MonadState Store m => m Loc
newLoc = do
  s <- get
  let l = newloc s
  put $ s {newloc = l + 1}
  return l

putData :: Loc -> Data -> EvalT ()
putData l d = do
  s <- get
  put $ s {memory = M.insert l d (memory s)}
  return ()

safeLookup :: Ord b => String -> b -> M.Map b a -> EvalT a
safeLookup errorMsg b m = do
  case M.lookup b m of
    Nothing -> throwError $ "SAFELOOKUP ERROR " ++ errorMsg
    Just s -> return s

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
  | Void
  | NODATA
  deriving (Prelude.Eq, Prelude.Ord, Prelude.Show)

-- Maybe we will need to store also something else in the future.
data Store =
  Store
    { memory :: M.Map Loc Data
    , newloc :: Loc
    }

type EvalT a = ReaderT Env (StateT Store (ExceptT String Identity)) a

typeCheckerError :: String
typeCheckerError = "TYPECHECKER ERROR "

runTimeError :: String
runTimeError = "Runtime error: "

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
    Just l -> case DM.fromJust (M.lookup l $ memory s) of
                NODATA -> throwError $ runTimeError ++ showPosition pos ++ " attempting to get the value of uinitialized value " ++ printTree ident
                x -> return x

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

-- Wywołuje funkcje function
handleFunction :: FunctionData -> [A.Expr] -> EvalT Data
handleFunction f@(FunctionData env stmt arguments retType) exprs = undefined

-- I'll do it later.
-- Tutaj będę iterował się po argumentach
-- Jezeli będzie typ referencyjny to wezmę lokację expression i zapiszę pod daną zmienną
-- jeżeli będzie normalny to zewaluuję wyrażenie, zalokuję nową zmienną i do stora
-- w storze trzymając wartość tego wyrażenia.
-- ~ -- Ewaluacja wyrażenia jest w innym środowisku niż 
handleArg :: A.Arg -> A.Expr -> Env -> EvalT Env
handleArg (A.Arg _ (A.ArgT pos t) ident) expr envToAdd = do
  d <- evalExpr expr
  l <- newLoc
  putData l d
  return envToAdd {variables = M.insert ident l (variables envToAdd)}
handleArg (A.Arg _ (A.ArgRef pos t) ident) expr envToAdd = do
  l <- evalWithLoc expr
  return envToAdd {variables = M.insert ident l (variables envToAdd)}

-- Typing statements
evalStmt :: A.Stmt -> EvalT Env
evalStmt (A.Empty pos) = ask >>= return
evalStmt (A.Ass pos ident expr) = do
  env <- ask
  d <- evalExpr expr
  l <- safeLookup (showPosition pos ++ typeCheckerError) ident (variables env)
  putData l d
  ask >>= return
evalStmt (A.Cond pos expr stmt) = do
  b <- evalExpr expr >>= fromBool pos
  if b
    then evalStmt stmt >>= propagateFlags >>= return
    else ask >>= return
evalStmt (A.CondElse pos e s1 s2) = do
  b <- evalExpr e >>= fromBool pos
  (if b
     then evalStmt s1
     else evalStmt s2) >>= propagateFlags >>= return
evalStmt s@(A.While pos e stmt) = do
  b <- evalExpr e >>= fromBool pos
  if b
    then do
      e <- evalStmt stmt >>= propagateFlags 
      if (hasReturn e) then return e else evalStmt s
    else ask >>= return
evalStmt (A.SExp pos e) = do
  evalExpr e
  ask >>= return
evalStmt (A.Ret pos e) = do
  d <- evalExpr e
  putData retLoc d
  e <- ask
  return $ e {hasReturn = True}
evalStmt (A.VRet pos) = do
  putData retLoc Void
  e <- ask
  return $ e {hasReturn = True}
evalStmt (A.BStmt _ (A.Block _ stmts)) = do
  env <- ask
  foldM handleStmtWithNewEnv env stmts >>= propagateFlags >>= return
  where
    handleStmtWithNewEnv :: Env -> A.Stmt -> EvalT Env
    handleStmtWithNewEnv e s = do
      if (hasReturn e)
        then return e
        else local (\_ -> e) (evalStmt s)
evalStmt (A.DeclStmt pos (A.Decl _ t items)) = do
  env <- ask 
  foldM handleItem env items
  where    
    handleItem :: Env -> A.Item -> EvalT Env
    handleItem newEnv (A.Init pos ident e) = do
      d <- local (\_ -> newEnv) $ evalExpr e
      l <- newLoc
      putData l d
      return newEnv { variables = M.insert ident l (variables newEnv) }

    handleItem newEnv (A.NoInit pos ident) = do
      l <- newLoc
      putData l NODATA
      return newEnv { variables = M.insert ident l (variables newEnv) }
evalStmt (A.DeclStmt _ (A.FDecl pos retType ident args b)) = do
  en <- ask
  return $ en {functions = M.insert ident (fix (phi en)) (functions en)}
  -- Alternative, without fixpoint.
  -- let newEnv =
  --       en
  --         { functions =
  --             M.insert
  --               ident
  --               (FunctionData newEnv (A.BStmt (A.hasPosition b) b) args retType)
  --               (functions env)
  --         }
  where
    phi :: Env -> FunctionData -> FunctionData
    phi e f =
      FunctionData
        (e {functions = M.insert ident f (functions e)})
        (A.BStmt (A.hasPosition b) b)
        args
        retType
evalStmt (TupleAss a [TupleIdent' a] (Expr' a))


-- type Item = Item' BNFC'Position
-- data Item' a = NoInit a Ident | Init a Ident (Expr' a)
--   deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

-- data FunctionData =
--   FunctionData
--     { env :: Env
--     , stmt :: A.Stmt
--     , arguments :: [A.Arg]
--     , retType :: A.Type
--     }
-- type Decl = Decl' BNFC'Position
-- data Decl' a
--     = Decl a (Type' a) [Item' a]
--     | FDecl a (Type' a) Ident [Arg' a] (Block' a)
-- evalStmt
-- type Stmt = Stmt' BNFC'Position
-- data Stmt' a
--     = Empty a DONE
--     | BStmt a (Block' a) DONE
--     | DeclStmt a (Decl' a)  DONE
--     | TupleAss a [TupleIdent' a] (Expr' a)
--     | Ass a Ident (Expr' a) DONE
--     | Ret a (Expr' a) DONE
--     | VRet a DONE
--     | Cond a (Expr' a) (Stmt' a) DONE
--     | CondElse a (Expr' a) (Stmt' a) (Stmt' a) DONE
--     | While a (Expr' a) (Stmt' a) DONE
--     | SExp a (Expr' a) DONE
--   deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)
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
