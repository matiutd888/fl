{-# LANGUAGE FlexibleContexts #-}

module Interpreter where

import qualified AbsGramatyka as A
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Either as DE
import qualified Data.List as DL
import qualified Data.Map as M
import qualified Data.Maybe as DM
import qualified Data.Set as S
import Errors
import PrintGramatyka (printTree)
import System.IO
import Utils (assertM, isType, noPos)

type Loc = Integer

retLoc :: Loc
retLoc = -1

data FunctionArg
  = ArgCopy Data
  | ArgRef Loc

data FunctionData =
  FunctionData
    { functionEnvironment :: Env
    , functionEval :: A.BNFC'Position -> [FunctionArg] -> EvalT Data
    , arguments :: [A.Arg]
    , returnType :: A.Type
    }

compose :: a -> [a -> EvalT a] -> EvalT a
compose a l = composeHelp a (reverse l)
  where
    composeHelp y (x:xs) = composeHelp y xs >>= x
    composeHelp y [] = return y

propagateFlags :: Env -> EvalT Env
propagateFlags newEnv =
  ask >>= \env -> return $ env {hasReturn = hasReturn newEnv}

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

initEnv :: Env
initEnv = Env M.empty False M.empty

data Data
  = Int Integer
  | Str String
  | Bool Bool
  | Tuple [Data]
  | Function FunctionData
  | Void
  | NODATA

data Store =
  Store
    { memory :: M.Map Loc Data
    , newloc :: Loc
    }

initStore :: Store
initStore = Store M.empty 0

type EvalT a = ReaderT Env (StateT Store (ExceptT String IO)) a

runEvalT :: Env -> Store -> EvalT a -> IO (Either String (a, Store))
runEvalT env st e = runExceptT (runStateT (runReaderT e env) st)

typeCheckerError :: String
typeCheckerError = "TYPECHECKER ERROR "

runTimeError :: String
runTimeError = "Runtime error\n"

fromInt :: A.BNFC'Position -> Data -> EvalT Integer
fromInt _ (Int i) = return i
fromInt pos _ =
  throwError $
  typeCheckerError ++ showPosition pos ++ "expected data of type int"

-- ~ fromBool :: MonadError String m => A.BNFC'Position -> Data -> m Bool
fromBool :: A.BNFC'Position -> Data -> EvalT Bool
fromBool _ (Bool b) = return b
fromBool pos _ =
  throwError $
  typeCheckerError ++ showPosition pos ++ "expected data of type bool"

fromFunction :: A.BNFC'Position -> Data -> EvalT FunctionData
fromFunction _ (Function f) = return f
fromFunction pos _ =
  throwError $
  typeCheckerError ++ showPosition pos ++ "expected data of type function"

fromTuple :: A.BNFC'Position -> Data -> EvalT [Data]
fromTuple _ (Tuple d) = return d
fromTuple pos _ =
  throwError $
  typeCheckerError ++ showPosition pos ++ "expected data of type tuple"

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
evalExpr (A.EMul _ e1 (A.Times _) e2) = do
  n1 <- evalExpr e1 >>= fromInt (A.hasPosition e1)
  n2 <- evalExpr e2 >>= fromInt (A.hasPosition e2)
  return $ Int $ n1 * n2
evalExpr (A.EMul pos e1 (A.Div _) e2) = do
  n1 <- evalExpr e1 >>= fromInt (A.hasPosition e1)
  n2 <- evalExpr e2 >>= fromInt (A.hasPosition e2)
  assertM (n2 /= 0) $
    runTimeError ++
    showPosition pos ++
    "attempting to perform division by zero, " ++
    printTree e2 ++ " is equal to zero"
  return $ Int $ div n1 n2
evalExpr (A.EMul pos e1 (A.Mod _) e2) = do
  n1 <- evalExpr e1 >>= fromInt (A.hasPosition e1)
  n2 <- evalExpr e2 >>= fromInt (A.hasPosition e2)
  assertM (n2 /= 0) $
    showPosition pos ++
    "attempting to perform modulo by zero, " ++
    printTree e2 ++ " is equal to zero"
  return $ Int $ mod n1 n2
evalExpr (A.EAdd _ e1 (A.Plus _) e2) = do
  n1 <- evalExpr e1 >>= fromInt (A.hasPosition e1)
  n2 <- evalExpr e2 >>= fromInt (A.hasPosition e2)
  return $ Int $ n1 + n2
evalExpr (A.EAdd _ e1 (A.Minus _) e2) = do
  n1 <- evalExpr e1 >>= fromInt (A.hasPosition e1)
  n2 <- evalExpr e2 >>= fromInt (A.hasPosition e2)
  return $ Int $ n1 - n2
evalExpr (A.EAnd _ e1 e2) = do
  n1 <- evalExpr e1 >>= fromBool (A.hasPosition e1)
  if not n1
    then return (Bool False)
    else Bool <$> (evalExpr e2 >>= fromBool (A.hasPosition e2))
evalExpr (A.EOr _ e1 e2) = do
  n1 <- evalExpr e1 >>= fromBool (A.hasPosition e1)
  if n1
    then return (Bool True)
    else Bool <$> (evalExpr e2 >>= fromBool (A.hasPosition e2))
evalExpr (A.ERel pos e1 relop e2) =
  handleDoubleExpression
    (operation relop)
    (operation relop)
    (operation relop)
    pos
    e1
    e2
  where
    compareData ::
         MonadError String m
      => A.BNFC'Position
      -> (Integer -> Integer -> Bool)
      -> (String -> String -> Bool)
      -> (Bool -> Bool -> Bool)
      -> Data
      -> Data
      -> m Bool
    compareData _ cInt _ _ (Int i1) (Int i2) = return $ i1 `cInt` i2
    compareData _ _ _ cBool (Bool b1) (Bool b2) = return $ b1 `cBool` b2
    compareData _ _ cString _ (Str s1) (Str s2) = return $ s1 `cString` s2
    compareData p cInt cString cBool (Tuple d1) (Tuple d2) =
      and <$> zipWithM (compareData p cInt cString cBool) d1 d2
    compareData p _ _ _ _ _ =
      throwError $ showPosition p ++ typeCheckerError ++ " comparision error"
    handleDoubleExpression ::
         (Integer -> Integer -> Bool)
      -> (String -> String -> Bool)
      -> (Bool -> Bool -> Bool)
      -> A.BNFC'Position
      -> A.Expr
      -> A.Expr
      -> EvalT Data
    handleDoubleExpression f g h position expr1 expr2 = do
      d1 <- evalExpr expr1
      d2 <- evalExpr expr2
      b <- lift $ compareData position f g h d1 d2
      return $ Bool b
evalExpr (A.EVar pos ident) = do
  env <- ask
  s <- get
  case M.lookup ident $ variables env of
    Nothing -> do
      case M.lookup ident $ functions env of
        Nothing ->
          throwError $ typeCheckerError ++ (undefinedReferenceMessage ident pos)
        Just f -> return $ Function f
    Just l ->
      case DM.fromJust (M.lookup l $ memory s) of
        NODATA ->
          throwError $
          runTimeError ++
          showPosition pos ++
          " attempting to read the value of uinitialized value " ++
          printTree ident
        x -> return x
evalExpr (A.ETuple _ expressions) = do
  values <- mapM evalExpr expressions
  return $ Tuple values
evalExpr (A.ELambda _ (A.Lambda _ args rT block)) = do
  e <- ask
  return $
    Function $
    functionFromSyntax e (A.BStmt (A.hasPosition block) block) args rT
evalExpr (A.EApp pos (A.LambdaCallee p l) exprs) = do
  function <- evalExpr (A.ELambda p l) >>= fromFunction p
  handleFunction pos function exprs
evalExpr (A.EApp pos (A.IdentCallee p ident) exprs) = do
  function <- evalExpr (A.EVar p ident) >>= fromFunction p
  handleFunction pos function exprs

functionFromSyntax :: Env -> A.Stmt -> [A.Arg] -> A.Type -> FunctionData
functionFromSyntax fenv stmt args retType = do
  FunctionData fenv eval args retType
  where
    eval pos functionArgs = do
      newFunctionEnv <- compose fenv $ zipWith handleArg args functionArgs
      retEnv <- local (\_ -> newFunctionEnv) $ evalStmt stmt
      if (hasReturn retEnv)
        then do
          store <- get
          safeLookup "No return value found" retLoc (memory store)
        else do
          assertM
            (isType retType A.Void)
            (runTimeError ++
             showPosition pos ++
             "function returned no value, shoud return value of type " ++
             printTree retType)
          return $ Void
    handleArg :: A.Arg -> FunctionArg -> Env -> EvalT Env
    handleArg (A.Arg _ (A.ArgT _ _) ident) (ArgCopy d) envToAdd = do
      l <- newLoc
      putData l d
      return envToAdd {variables = M.insert ident l (variables envToAdd)}
    handleArg (A.Arg _ (A.ArgRef _ _) ident) (ArgRef l) envToAdd = do
      return envToAdd {variables = M.insert ident l (variables envToAdd)}
    handleArg _ _ _ = throwError typeCheckerError

handleFunction :: A.BNFC'Position -> FunctionData -> [A.Expr] -> EvalT Data
handleFunction pos f exprs = do
  args <- zipWithM handleArg (arguments f) exprs
  (functionEval f) pos args
  where
    handleArg :: A.Arg -> A.Expr -> EvalT FunctionArg
    handleArg (A.Arg _ (A.ArgT _ _) _) expr =
      evalExpr expr >>= \d -> return $ ArgCopy d
    handleArg (A.Arg _ (A.ArgRef _ _) _) expr =
      evalWithLoc expr >>= \l -> return $ ArgRef l

evalWithLoc :: A.Expr -> EvalT Loc
evalWithLoc (A.EVar pos ident) = do
  env <- ask
  case M.lookup ident (variables env) of
    Just l -> return l
    Nothing ->
      throwError $
      typeCheckerError ++ showPosition pos ++ "evar variable not found"
evalWithLoc e =
  throwError $ typeCheckerError ++ showPositionOf e ++ "not a variable"

-- Statement evaluation
evalStmt :: A.Stmt -> EvalT Env
evalStmt (A.Empty _) = ask
evalStmt (A.Ass pos ident expr) = do
  env <- ask
  d <- evalExpr expr
  l <- safeLookup (showPosition pos ++ typeCheckerError) ident (variables env)
  putData l d
  ask
evalStmt (A.Cond pos expr stmt) = do
  b <- evalExpr expr >>= fromBool pos
  if b
    then evalStmt stmt >>= propagateFlags
    else ask
evalStmt (A.CondElse pos e s1 s2) = do
  b <- evalExpr e >>= fromBool pos
  (if b
     then evalStmt s1
     else evalStmt s2) >>=
    propagateFlags
evalStmt s@(A.While pos expr stmt) = do
  b <- evalExpr expr >>= fromBool pos
  if b
    then do
      e <- evalStmt stmt >>= propagateFlags
      if (hasReturn e)
        then return e
        else evalStmt s
    else ask
evalStmt (A.SExp _ e) = do
  evalExpr e >> ask
evalStmt (A.Ret _ e) = do
  d <- evalExpr e
  putData retLoc d
  env <- ask
  return $ env {hasReturn = True}
evalStmt (A.VRet _) = do
  putData retLoc Void
  e <- ask
  return $ e {hasReturn = True}
evalStmt (A.BStmt _ (A.Block _ stmts)) = do
  env <- ask
  foldM handleStmtWithNewEnv env stmts >>= propagateFlags
  where
    handleStmtWithNewEnv :: Env -> A.Stmt -> EvalT Env
    handleStmtWithNewEnv e s = do
      if (hasReturn e)
        then return e
        else local (\_ -> e) (evalStmt s)
evalStmt (A.DeclStmt _ (A.Decl _ _ items)) = do
  env <- ask
  foldM handleItem env items
  where
    handleItem :: Env -> A.Item -> EvalT Env
    handleItem newEnv (A.Init _ ident e) = do
      d <- local (\_ -> newEnv) $ evalExpr e
      l <- newLoc
      putData l d
      return newEnv {variables = M.insert ident l (variables newEnv)}
    handleItem newEnv (A.NoInit _ ident) = do
      l <- newLoc
      putData l NODATA
      return newEnv {variables = M.insert ident l (variables newEnv)}
evalStmt (A.DeclStmt _ (A.FDecl _ retType ident args b)) = do
  en <- ask
  return $ en {functions = M.insert ident (fix (phi en)) (functions en)}
  where
    phi :: Env -> FunctionData -> FunctionData
    phi e f =
      functionFromSyntax
        (e {functions = M.insert ident f (functions e)})
        (A.BStmt (A.hasPosition b) b)
        args
        retType
-- We can ignore the output.
-- Env does not change during assignment.
evalStmt (A.TupleAss p tupleIdents expr) = do
  d <- evalExpr expr >>= fromTuple p
  zipWithM handleIdent tupleIdents d
  ask
  where
    handleIdent :: A.TupleIdent -> Data -> EvalT Env
    handleIdent (A.TupleNoIdent _) _ = ask
    handleIdent (A.TupleIdent pos ident) d = do
      env <- ask
      l <-
        safeLookup (showPosition pos ++ typeCheckerError) ident (variables env)
      putData l d >> ask
    handleIdent (A.TupleRec _ idents) (Tuple datas) = do
      env <- ask
      compose env $ zipWith propagateEnv idents datas
        -- Here I propagate env, although it's not neccessary as env does not change during assignment.
      where
        propagateEnv :: A.TupleIdent -> Data -> Env -> EvalT Env
        propagateEnv ident d newEnv = local (\_ -> newEnv) $ handleIdent ident d
    handleIdent (A.TupleRec pos _) _ =
      throwError $
      typeCheckerError ++ showPosition pos ++ " error unpacking tuple"

evalTopDef :: A.TopDef -> Env -> EvalT Env
evalTopDef (A.FnDef pos retType ident args body) newEnv =
  local (\_ -> newEnv) $
  evalStmt (A.DeclStmt pos (A.FDecl pos retType ident args body))

evalProgram :: A.Program -> EvalT Integer
evalProgram (A.Program _ funcs) = do
  env <- ask
  envWithFunctions <- compose env $ map evalTopDef funcs
  let mainCall = A.EApp noPos (A.IdentCallee noPos (A.Ident "main")) []
  local (\_ -> envWithFunctions) (evalExpr mainCall) >>= fromInt noPos

printBool :: FunctionData
printBool =
  FunctionData
    initEnv
    evalPrint
    [A.Arg noPos (A.ArgT noPos (A.Bool noPos)) (A.Ident "x")]
    (A.Void noPos)
  where
    evalPrint _ [ArgCopy (Bool b)] = liftIO (putStrLn $ show b) >> return Void
    evalPrint p _ = throwError $ showPosition p ++ "wrong use of printBool"

printInt :: FunctionData
printInt =
  FunctionData
    initEnv
    evalPrint
    [A.Arg noPos (A.ArgT noPos (A.Int noPos)) (A.Ident "x")]
    (A.Void noPos)
  where
    evalPrint _ [ArgCopy (Int n)] = liftIO (putStrLn $ show n) >> return Void
    evalPrint p _ = throwError $ showPosition p ++ "wrong use of printInt"

printString :: FunctionData
printString =
  FunctionData
    initEnv
    evalPrint
    [A.Arg noPos (A.ArgT noPos (A.Str noPos)) (A.Ident "x")]
    (A.Void noPos)
  where
    evalPrint _ [ArgCopy (Str s)] = liftIO (putStrLn s) >> return Void
    evalPrint p _ = throwError $ showPosition p ++ "wrong use of printString"

assert :: FunctionData
assert =
  FunctionData
    initEnv
    evalAssert
    [A.Arg noPos (A.ArgT noPos (A.Bool noPos)) (A.Ident "x")]
    (A.Void noPos)
  where
    evalAssert p [ArgCopy (Bool b)] =
      assertM b (showPosition p ++ "assertion failed") >> return Void
    evalAssert p _ = throwError $ showPosition p ++ "wrong use of assert"

addFunctions :: Env -> Env
addFunctions e = e {functions = x (functions e)}
  where
    x =
      M.insert (A.Ident "printInt") printInt .
      M.insert (A.Ident "printBool") printBool .
      M.insert (A.Ident "printString") printString .
      M.insert (A.Ident "assert") assert

runInterpreter :: A.Program -> IO (Either String (Integer, Store))
runInterpreter p = runEvalT (addFunctions initEnv) initStore $ evalProgram p

operation :: Ord a => A.RelOp -> (a -> a -> Bool)
operation (A.LTH _) = (<)
operation (A.LE _) = (<=)
operation (A.GTH _) = (>)
operation (A.GE _) = (>=)
operation (A.EQU _) = (==)
operation (A.NE _) = (/=)
