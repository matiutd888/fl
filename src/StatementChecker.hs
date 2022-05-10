-- TODO 
module StatementChecker where

import AbsGramatyka as A
import PrintGramatyka (printTree)
import Errors
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Data.Text
import qualified Data.Either as DE
import qualified Data.Map as M
import qualified Data.Set as S
import TypeChecker (assertM, typesEq, ExprEnv (ExprEnv), typeOfExpr, runExprTEval, getArgType, isType)
-- ~ type Stmt = Stmt' BNFC'Position
-- ~ data Stmt' a
    -- ~ = Empty a DONE
    -- ~ | BStmt a (Block' a) DONE
    -- ~ | DeclStmt a (Decl' a) DONE
    -- ~ | TupleAss a [TupleIdent' a] (Expr' a) DONE
    -- ~ | Ass a Ident (Expr' a) DONE
    -- ~ | Ret a (Expr' a) DONE
    -- ~ | VRet a DONE
    -- ~ | Cond a (Expr' a) (Stmt' a) DONE
    -- ~ | CondElse a (Expr' a) (Stmt' a) (Stmt' a) DONE
    -- ~ | While a (Expr' a) (Stmt' a) DONE
    -- ~ | SExp a (Expr' a) DONE
  -- ~ deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

-- Poziom bloku (liczymy od zera).

-- Variables (zawiera zmienne do których mogę przypisać).
-- Functions (holds function declarations, you can't assign to such functions. This map doesn't have info about
--  functions that are lambdas, variables or are function parameters).
-- Levels (zawiera informację o tym, na którym poziomie zadeklarowana została dana zmienna / funkcja).
data Env = Env { variables :: M.Map A.Ident A.Type, 
                variableLevels :: M.Map A.Ident Int, 
                functions :: M.Map A.Ident A.Type,  
                functionLevels :: M.Map A.Ident Int,
                level :: Int,
                functionType :: A.Type } deriving Show

type StmtTEval a = StateT Env (ExceptT String Identity) a

toExprEnv :: Env -> ExprEnv
toExprEnv env = ExprEnv (variables env) 
						(variableLevels env) 
						(functions env)
						(level env)
						(functionType env)


incrementBlockLevel :: Env -> Env
incrementBlockLevel env = env { level = (+1) $ level env }


typeStmt :: A.Stmt -> StmtTEval ()
typeStmt (A.Empty _) = return ()
-- ~ typeStmt (BStmt pos (Block' a))
typeStmt (A.Cond pos expr stmt) = do
  checkExpressionType (A.Bool A.BNFC'NoPosition) expr
  env <- get
  put $ incrementBlockLevel env
  typeStmt stmt
  put env
typeStmt (A.CondElse pos expr stmt1 stmt2) = do
  checkExpressionType (A.Bool A.BNFC'NoPosition) expr
  env <- get
  put $ incrementBlockLevel env
  typeStmt stmt1
  put $ incrementBlockLevel env
  typeStmt stmt2
  put env
typeStmt (A.SExp pos expr) = do
  env <- get
  liftEither $ runExprTEval (toExprEnv env) (typeOfExpr expr)
  return ()
typeStmt (A.While pos expr stmt) = do
  checkExpressionType (A.Bool A.BNFC'NoPosition) expr
  env <- get
  put $ incrementBlockLevel env
  typeStmt stmt
  put env
typeStmt (A.DeclStmt _ (A.Decl pos t items)) = do
  mapM_ (handleItem t) items
  return ()
typeStmt (A.DeclStmt _ (A.FDecl pos retType ident params body)) = do
  env <- get
  let newFunctions = M.insert ident (A.Function pos retType (Prelude.map getArgType params)) (functions env)
  let newFunctionLevels = M.insert ident (level env) (functionLevels env)
  let envWithAddedParams = Prelude.foldr (handleArg (level env + 1)) env params
  put $ env {functions = newFunctions,
            variableLevels = variableLevels envWithAddedParams,
            variables = variables envWithAddedParams,
            functionLevels = newFunctionLevels,
            functionType = retType,
            level = level env + 1}
  typeStmt $ A.BStmt (hasPosition body) body
  put $ env {functions = newFunctions,
            functionLevels = newFunctionLevels}
typeStmt (A.Ret pos expr) = do
  env <- get
  checkExpressionType (functionType env) expr
  return ()
typeStmt (A.VRet pos) = do
  funcT <- liftM functionType get 
  let voidType = A.Void pos
  assertM (typesEq voidType funcT) $ errorMessageWrongType pos voidType funcT
typeStmt (A.Ass pos ident expr) = do
  variables <- liftM variables get
  case M.lookup ident variables of
    Nothing -> throwError $ showPosition pos ++ "attempting to assign to an undeclared variable"
    Just t -> checkExpressionType t expr
typeStmt (A.TupleAss pos tupleIdents expr) = do
  env <- get
  typeOfExpr <- liftEither $ runExprTEval (toExprEnv env) (typeOfExpr expr)
  case typeOfExpr of
    A.Tuple p types -> do
      assertM (Prelude.length types == Prelude.length tupleIdents) $ showPosition pos ++ "error unpacking tuple"
      zipWithM handleTupleIdent tupleIdents types
      return ()
    t -> throwError $ showPosition pos ++ "attempting to unpack tuple with expression that is not a tuple"
typeStmt (BStmt _ (Block pos stmts)) = do
  env <- get
  mapM_ typeStmt stmts
  put env
  return ()
  
-- ~ Tuple a [Type' a]

-- ~ TupleAss a [TupleIdent' a] (Expr' a)

-- ~ type TupleIdent = TupleIdent' BNFC'Position
-- ~ data TupleIdent' a
    -- ~ = TupleIdent a Ident | TupleRec a [TupleIdent' a]

handleTupleIdent :: A.TupleIdent -> A.Type -> StmtTEval ()
handleTupleIdent (A.TupleIdent pos ident) t = do
  env <- get
  put $ env { variables = M.insert ident t (variables env), variableLevels = M.insert ident (level env) (variableLevels env) }
handleTupleIdent (A.TupleRec pos tupleIdents) t = do
  env <- get
  case t of
    A.Tuple p types -> do
      assertM (Prelude.length types == Prelude.length tupleIdents) $ showPosition pos ++ "error unpacking tuple"
      zipWithM handleTupleIdent tupleIdents types
      return ()
    wrongType -> throwError $ showPosition pos ++ "attempting to unpack tuple with expression that is not a tuple"
  
-- Check if variable ident can be declared at the given level.
checkVariableLevel :: BNFC'Position -> Ident -> StmtTEval ()
checkVariableLevel pos ident = do
  env <- get
  let lvl = level env
  case M.lookup ident (variableLevels env) of
    Nothing -> return ()
    Just varLevel -> assertM (varLevel /= lvl) $ showPosition pos ++ "variable " ++ printTree ident ++ "was already declared at this level"
  
checkFunctionLevel :: BNFC'Position -> Ident -> StmtTEval ()
checkFunctionLevel pos ident = do
  env <- get
  let lvl = level env
  case M.lookup ident (functionLevels env) of
    Nothing -> return ()
    Just varLevel -> assertM (varLevel /= lvl) $ showPosition pos ++ "function " ++ printTree ident ++ "was already declared at this level"
  

-- TODO check if level is this is second declaration at the same level.
handleItem :: A.Type -> A.Item -> StmtTEval ()
handleItem t (A.NoInit pos ident) = do
  env <- get
  checkVariableLevel pos ident
  put $ env { variables = M.insert ident t (variables env), 
              variableLevels = M.insert ident (level env) (variableLevels env) }
  return ()
handleItem t (A.Init pos ident expr) = do
  checkExpressionType t expr
  checkVariableLevel pos ident
  env <- get
  put $ env { variables = M.insert ident t (variables env), 
              variableLevels = M.insert ident (level env) (variableLevels env) }
  return ()
  
checkExpressionType :: A.Type -> A.Expr -> StmtTEval ()
checkExpressionType t expr = do
  env <- get
  exprType <- liftEither $ runExprTEval (toExprEnv env) (typeOfExpr expr)
  assertM (typesEq exprType t) $ errorMessageWrongType (hasPosition expr) exprType t
  return ()

-- ~ type Program = Program' BNFC'Position
-- ~ data Program' a = Program a [TopDef' a]
  -- ~ deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

-- ~ type TopDef = TopDef' BNFC'Position
-- ~ data TopDef' a = FnDef a (Type' a) Ident [Arg' a] (Block' a)
  -- ~ deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)
typeProgram :: A.Program -> StmtTEval ()
typeProgram (A.Program pos functions) = do
  assertM (Prelude.any checkIfMainDef functions) $ "no main function!"
  mapM_ handleTopDef functions
  return ()

handleTopDef :: A.TopDef -> StmtTEval ()
handleTopDef (A.FnDef pos retType ident args body) = typeStmt (A.DeclStmt pos (A.FDecl pos retType ident args body))

checkIfMainDef :: A.TopDef -> Bool
checkIfMainDef (A.FnDef pos retType ident args body) = ident == A.Ident "main"
                                                      && isType retType A.Int
                                                      && args == []

runStmtTEval :: Env -> StmtTEval a -> Either String (a, Env)
runStmtTEval env e = runIdentity (runExceptT (runStateT e env))


noPos :: A.BNFC'Position
noPos = A.BNFC'NoPosition

printInt :: A.TopDef
printInt = FnDef noPos (A.Void noPos) (A.Ident "printInt") [A.Arg noPos (A.ArgT noPos (A.Int noPos)) (A.Ident "x")] (A.Block noPos [])

printBool :: A.TopDef
printBool = FnDef noPos (A.Void noPos) (A.Ident "printBool") [A.Arg noPos (A.ArgT noPos (A.Bool noPos)) (A.Ident "x")] (A.Block noPos [])

printString :: A.TopDef
printString = FnDef noPos (A.Void noPos) (A.Ident "printString") [A.Arg noPos (A.ArgT noPos (A.Str noPos)) (A.Ident "x")] (A.Block noPos [])

addPrintFunctions :: Env -> Env
addPrintFunctions e = snd $ DE.fromRight ((), initEnv) $ runStmtTEval e x
	where x = do
		handleTopDef printInt
		handleTopDef printBool
		handleTopDef printString


initEnv :: Env
initEnv = addPrintFunctions $ Env { variables = M.empty,
                variableLevels = M.empty,
                functions = M.empty,
                functionLevels = M.empty,
                level = 0,
                functionType = A.Void A.BNFC'NoPosition } -- won't be used anyway.


runTypeChecker :: A.Program -> Either String ((), Env)
runTypeChecker p = runStmtTEval initEnv (typeProgram p)

-- ads arg to variables map and levels map
handleArg :: Int -> Arg -> Env -> Env
handleArg newLevel (Arg _ (ArgT _ t) ident) env = env { variables = M.insert ident t (variables env),
                                               variableLevels = M.insert ident newLevel(variableLevels env) }
handleArg newLevel (Arg _ (ArgRef _ t) ident) env = env { variables = M.insert ident t (variables env),
                                               variableLevels = M.insert ident newLevel (variableLevels env) }

