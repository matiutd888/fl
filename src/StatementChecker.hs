-- TODO 
module StatementChecker where

import AbsGramatyka as A
import Errors
import TypeChecker

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Data.Text
import qualified Data.Map as M

-- ~ type Stmt = Stmt' BNFC'Position
-- ~ data Stmt' a
    -- ~ = Empty a DONE
    -- ~ | BStmt a (Block' a)
    -- ~ | DeclStmt a (Decl' a)
    -- ~ | TupleAss a [TupleIdent' a] (Expr' a)
    -- ~ | Ass a Ident (Expr' a)
    -- ~ | Ret a (Expr' a)
    -- ~ | VRet a
    -- ~ | Cond a (Expr' a) (Stmt' a) DONE
    -- ~ | CondElse a (Expr' a) (Stmt' a) (Stmt' a) DONE
    -- ~ | While a (Expr' a) (Stmt' a) DONE
    -- ~ | SExp a (Expr' a) DONE
  -- ~ deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

-- Poziom bloku (liczymy od zera).
data Env = Env { typeMap :: M.Map A.Ident A.Type, levelMap :: M.Map A.Ident Int, level :: Int, functionType :: A.Type, isConst :: M.Map A.Ident Bool }

type StmtTEval a = StateT Env (ExceptT String Identity) a

incrementBlockLevel :: Env -> Env
incrementBlockLevel (Env a b c d e) = Env a b (c + 1) d e

typeStmt :: A.Stmt -> StmtTEval ()
typeStmt (A.Empty _) = return ()
-- ~ typeStmt (BStmt pos (Block' a))
typeStmt (Cond pos expr stmt) = do
  env <- get
  exprType <- liftEither $ runExprTEval (typeMap env) (typeOfExpr expr)
  assertM (isType exprType A.Bool) (errorMessageWrongType (hasPosition expr) exprType (A.Bool A.BNFC'NoPosition))
  put (incrementBlockLevel env)
  typeStmt stmt
typeStmt (CondElse pos expr stmt1 stmt2) = do
  env <- get
  exprType <- liftEither $ runExprTEval (typeMap env) (typeOfExpr expr)
  assertM (isType exprType A.Bool) (errorMessageWrongType (hasPosition expr) exprType (A.Bool A.BNFC'NoPosition))
  put (incrementBlockLevel env)
  typeStmt stmt1
  put (incrementBlockLevel env)
  typeStmt stmt2
typeStmt (SExp pos expr) = do
  env <- get
  liftEither $ runExprTEval (typeMap env) (typeOfExpr expr)
  return ()
typeStmt (While pos expr stmt) = do
  env <- get
  exprType <- liftEither $ runExprTEval (typeMap env) (typeOfExpr expr)
  assertM (isType exprType A.Bool) (errorMessageWrongType (hasPosition expr) exprType (A.Bool A.BNFC'NoPosition))
  put (incrementBlockLevel env)
  typeStmt stmt

typeStmt (DeclStmt _ (Decl pos t items)) = undefined
typeStmt (DeclStmt _ (ConstDecl pos t items)) = undefined
typeStmt (DeclStmt _ (FDecl pos retType ident params body)) = undefined

-- Put item in typeMap (and in const map if it is necessary). 
handleItem :: Bool -> Item -> StmtTEval ()
handleItem b (NoInit pos ident) = undefined
handleItem b (Init pos ident expr) = undefined
