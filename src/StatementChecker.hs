-- TODO 
module StatementChecker where

import AbsGramatyka as A
import Errors
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Data.Text
import qualified Data.Map as M
import qualified Data.Set as S
import TypeChecker (assertM, typesEq, ExprEnv (ExprEnv), typeOfExpr, runExprTEval, getArgType)
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
-- Functions (zawiera deklaracje funkcji, do nich nie mogę przypisać).
-- Levels (zawiera informację o tym, na którym poziomie zadeklarowana została dana zmienna / funkcja).
-- Jak na razie nie rozpatruję przypadku w której zmienne się pokrywają nazwą.
data Env = Env { variables :: M.Map A.Ident A.Type, 
                levelMap :: M.Map A.Ident Int, 
                functions :: M.Map A.Ident A.Type, 
                level :: Int, 
                functionType :: A.Type }

type StmtTEval a = StateT Env (ExceptT String Identity) a

toExprEnv :: Env -> ExprEnv
toExprEnv env = ExprEnv (variables env) (functions env)


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
  let newLevelMap = M.insert ident (level env) (levelMap env)
  put $ env {functions = newFunctions,
            levelMap = newLevelMap,
            functionType = retType,
            level = level env + 1}
  typeStmt $ A.BStmt (hasPosition body) body
  put $ env {variables = newFunctions,
            levelMap = newLevelMap}
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
handleTupleIdent (TupleIdent pos ident) t = do
  env <- get
  put $ env { variables = M.insert ident t (variables env), levelMap = M.insert ident (level env) (levelMap env) }
handleTupleIdent (TupleRec pos tupleIdents) t = do
  env <- get
  case t of
    A.Tuple p types -> do
      assertM (Prelude.length types == Prelude.length tupleIdents) $ showPosition pos ++ "error unpacking tuple"
      zipWithM handleTupleIdent tupleIdents types
      return ()
    wrongType -> throwError $ showPosition pos ++ "attempting to unpack tuple with expression that is not a tuple"
    
-- TODO check if level is this is second declaration at the same level.
handleItem :: A.Type -> A.Item -> StmtTEval ()
handleItem t (A.NoInit pos ident) = do
  env <- get
  put $ env { variables = M.insert ident t (variables env), levelMap = M.insert ident (level env) (levelMap env) }
  return ()
handleItem t (A.Init pos ident expr) = do
  checkExpressionType t expr
  env <- get
  put $ env { variables = M.insert ident t (variables env), levelMap = M.insert ident (level env) (levelMap env) }
  return ()
  
checkExpressionType :: A.Type -> A.Expr -> StmtTEval ()
checkExpressionType t expr = do
  env <- get
  exprType <- liftEither $ runExprTEval (toExprEnv env) (typeOfExpr expr)
  assertM (typesEq exprType t) $ errorMessageWrongType (hasPosition expr) exprType t
  return ()
