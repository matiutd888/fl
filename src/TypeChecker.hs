{-# LANGUAGE FlexibleContexts #-}

module TypeChecker (ExprEnv (ExprEnv), typesEq, assertM, typeOfExpr, ExprTEval, runExprTEval, getArgType, isType) where

import AbsGramatyka as A
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import PrintGramatyka
import Errors

import qualified Data.Map as M
import Data.Text


data ExprEnv = ExprEnv { variables :: M.Map A.Ident A.Type, 
                levelMap :: M.Map A.Ident Int, 
                functions :: M.Map A.Ident A.Type, 
                level :: Int, 
                functionType :: A.Type } deriving Show

type ExprTEval a = ReaderT ExprEnv (ExceptT String Identity) a

runExprTEval :: ExprEnv -> ExprTEval a -> Either String a
runExprTEval env e = runIdentity (runExceptT (runReaderT e env))

-- Zawiera pozycję z którego się wzięło wyrażenie.
typeOfExpr :: A.Expr -> ExprTEval A.Type
typeOfExpr (A.EVar pos ident) = do
  env <- ask
  case M.lookup ident $ variables env of
    Nothing -> case M.lookup ident $ functions env of
                Nothing -> throwError $ (undefinedReferenceMessage ident pos) ++ "\n"
                Just t -> return t
    Just t -> return t
  
-- Literals.
typeOfExpr (A.ELitTrue pos) = return $ A.Bool pos
typeOfExpr (A.ELitFalse pos) = return $ A.Bool pos
typeOfExpr (A.ELitInt pos _) = return $ A.Int pos
typeOfExpr (A.EString pos _) = return $ A.Str pos

-- Binary operator expressions.
typeOfExpr (A.EAnd pos e1 e2) = typeOfBinOp A.Bool A.Bool pos e1 e2
typeOfExpr (A.EOr pos e1 e2) = typeOfBinOp A.Bool A.Bool pos e1 e2
typeOfExpr (A.EAdd pos e1 _ e2) = typeOfBinOp A.Int A.Int pos e1 e2
typeOfExpr (A.EMul pos e1 _ e2) = typeOfBinOp A.Int A.Int pos e1 e2
typeOfExpr (A.ERel pos e1 _ e2) = typeOfBinOp A.Int A.Bool pos e1 e2

-- Unary operator expressions.
typeOfExpr (A.Not pos e) = do
  typeOfExpr e >>= checkForType A.Bool pos
  return $ A.Bool pos

typeOfExpr (A.Neg pos e) = do
  typeOfExpr e >>= checkForType A.Int pos
  return $ A.Int pos

typeOfExpr (A.ETuple pos l)
  -- foldr (liftM2 (:)) (pure []) changes list of monads to monad of list.
  -- http://learnyouahaskell.com/functors-applicative-functors-and-monoids
 = do
  listOfTypes <- Prelude.foldr (liftM2 (:)) (pure []) $ typeOfExpr <$> l
  return $ A.Tuple pos listOfTypes
  
typeOfExpr (A.ELambda pos (A.Lambda _ arguments retType body))
  -- TODO check if block returns good type!
 = do
  return $ Function pos retType $ getArgType <$> arguments
  
typeOfExpr (A.EApp pos callee args) =
  case callee of
    A.LambdaCallee p l -> do
      t <- typeOfExpr (A.ELambda p l)
      handleFunction pos t args
    A.IdentCallee p ident -> do
      t <- typeOfExpr (A.EVar p ident)
      -- Evar
      -- 1. Will check if there exists variable of given iden, if so it returs its type.
      -- 2. If not, checks if there exists function of given iden, if so, it returns its type.
      -- What is left to handle is the case when both function and variable of given iden exist and 
      -- variable was not of a suitable type.
      catchError (handleFunction pos t args) (\_ -> do 
          env <- ask
          case M.lookup ident (functions env) of
            Just t -> handleFunction pos t args
            Nothing -> throwError $ showPosition pos ++ "no function " ++ printTree ident ++ " found")
        

-- Checks if function application is performed correctly. If so, returns its return type.
handleFunction :: BNFC'Position -> A.Type -> [A.Expr] -> ExprTEval A.Type
handleFunction pos f args = case f of
    A.Function _ retType params -> (checkArgsCorrectness params args) >>= (\_ -> return retType)
    _ -> throwError $ notAFunctionMessage pos f

checkArgsCorrectness :: [A.ArgType] -> [A.Expr] -> ExprTEval ()
checkArgsCorrectness params args = do 
  zipWithM checkArgCorrectness args params
  return ()
  
checkArgCorrectness :: A.Expr -> A.ArgType -> ExprTEval ()
checkArgCorrectness arg param =
  case param of
    A.ArgRef pos ttype ->
      case arg of
        A.EVar _ ident -> do
          varType <- typeOfExpr arg
          let paramType = (getTypeFromArgType param)
          assertM (typesEq varType paramType) (errorMessageWrongType (hasPosition arg) varType paramType) 
        _ -> throwError $ errorWrongArgumentPassedByReference arg param
    _ ->  do
      argType <- typeOfExpr arg
      let paramType = (getTypeFromArgType param)
      assertM (typesEq argType paramType) (errorMessageWrongType (hasPosition arg) argType paramType) 
    

assertM :: MonadError String m => Bool -> String -> m ()
assertM b s = if b then return () else throwError s

getArgType :: A.Arg -> A.ArgType
getArgType (A.Arg _ t _) = t

getTypeFromArgType :: A.ArgType -> A.Type
getTypeFromArgType (A.ArgRef _ t) = t
getTypeFromArgType (A.ArgT _ t) = t

typeOfBinOp ::
  (BNFC'Position -> A.Type)
  -> (BNFC'Position -> A.Type)
  -> A.BNFC'Position
  -> A.Expr
  -> A.Expr
  -> ExprTEval A.Type
typeOfBinOp typeConstructor retTypeConstructor pos e1 e2 = do
  typeOfExpr e1 >>= checkForType typeConstructor (hasPosition e1)
  typeOfExpr e2 >>= checkForType typeConstructor (hasPosition e2)
  return $ retTypeConstructor pos

checkForType :: MonadError String m => (BNFC'Position -> A.Type) -> BNFC'Position -> A.Type -> m ()
checkForType typeConstructor pos t = assertM (isType t typeConstructor) (errorMessageWrongType pos t $ typeConstructor pos)
    
isType :: A.Type -> (BNFC'Position -> A.Type) -> Bool
isType t1 t2 = typesEq t1 $ t2 BNFC'NoPosition

typesEq :: A.Type -> A.Type -> Bool
typesEq (A.Int _) (A.Int _) = True
typesEq (A.Str _) (A.Str _) = True
typesEq (A.Bool _) (A.Bool _) = True
typesEq (A.Void _) (A.Void _) = True
-- typesEq (A.NoType _) (A.NoType _) = True
typesEq (A.Tuple _ types1) (A.Tuple _ types2) =
  and $ Prelude.zipWith typesEq types1 types2
typesEq (A.Function _ ret1 args1) (A.Function _ ret2 args2) =
  typesEq ret1 ret2 && and (Prelude.zipWith paramTypesEqual args1 args2)
typesEq _ _ = False

-- TODO użyc tego gdzieś.
paramTypesEqual :: A.ArgType -> A.ArgType -> Bool
paramTypesEqual (A.ArgRef _ t1) (A.ArgRef _ t2) = typesEq t1 t2
paramTypesEqual (A.ArgT _ t1) (A.ArgT _ t2) = typesEq t1 t2
paramTypesEqual _ _ = False
