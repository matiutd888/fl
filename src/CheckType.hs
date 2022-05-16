{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BlockArguments #-}

module CheckType where

import AbsGramatyka as A
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Either as DE
import qualified Data.Map as M
import Data.Text
import Errors
import Prelude as P
import PrintGramatyka
import Utils

-- Variables (holds variables of any type).
-- Functions (holds function declarations, you can't assign to such functions. This map doesn't have info about
--  functions that are lambdas, variables or are function parameters).
-- Levels (holds info about level on which function or variable was declared - useful to check whether there are two declarations on the same block).
data Env =
  Env
    { variables :: M.Map A.Ident A.Type
    , variableLevels :: M.Map A.Ident Int
    , functions :: M.Map A.Ident A.Type
    , functionLevels :: M.Map A.Ident Int
    , level :: Int
    , functionType :: A.Type
    }
  deriving (Show)

type ExprTEval a = ReaderT Env (ExceptT String Identity) a

runExprTEval :: Env -> ExprTEval a -> Either String a
runExprTEval env e = runIdentity (runExceptT (runReaderT e env))

-- Type contains also information about position of the expression that has the type returned.
typeOfExpr :: A.Expr -> ExprTEval A.Type
typeOfExpr (A.EVar pos ident) = do
  env <- ask
  case M.lookup ident $ variables env of
    Nothing ->
      case M.lookup ident $ functions env of
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
typeOfExpr (A.ERel pos e1 relop e2) = do
  t1 <- typeOfExpr e1
  t2 <- typeOfExpr e2
  assertM (typesEq t1 t2) $
    showPosition pos ++
    "comparing values of diferent type, " ++
    printTree t1 ++ " and " ++ printTree t2
  checkComparable t1
  return $ A.Bool pos
  where
    checkComparable :: A.Type -> ExprTEval ()
    checkComparable (A.Tuple _ listOfTypes) = mapM_ checkComparable listOfTypes
    checkComparable f@(A.Function _ _ _) =
      throwError $ showPosition pos ++ printTree f ++ " type is not comparable"
    checkComparable _ = return ()
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
  listOfTypes <- P.foldr (liftM2 (:)) (pure []) $ typeOfExpr <$> l
  return $ A.Tuple pos listOfTypes
typeOfExpr (A.ELambda pos (A.Lambda _ arguments retType body)) = do
  env <- ask
  let envWithAddedParams = P.foldr (addArgToEnv (level env + 1)) env arguments
  let blockEnv =
        env
          { functionType = retType
          , variableLevels = variableLevels envWithAddedParams
          , variables = variables envWithAddedParams
      -- We do NOT add lambda to function environment (recursion not possible).
          }
  liftEither $
    runStmtTEval blockEnv $ typeStmt $ A.BStmt (hasPosition body) body
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
      -- Maybe right now I should not do it.
      handleFunction pos t args
      -- ~ catchError
        -- ~ (handleFunction pos t args)
        -- ~ (\_ -> do
           -- ~ env <- ask
           -- ~ case M.lookup ident (functions env) of
             -- ~ Just t -> handleFunction pos t args
             -- ~ Nothing ->
               -- ~ throwError $
               -- ~ showPosition pos ++ "no function " ++ printTree ident ++ " found")

-- Checks if function application is performed correctly. If so, returns its return type.
handleFunction :: BNFC'Position -> A.Type -> [A.Expr] -> ExprTEval A.Type
handleFunction pos f args =
  case f of
    A.Function _ retType params ->
      (checkArgsCorrectness pos params args) >>= (\_ -> return retType)
    _ -> throwError $ notAFunctionMessage pos f

checkArgsCorrectness ::
     A.BNFC'Position -> [A.ArgType] -> [A.Expr] -> ExprTEval ()
checkArgsCorrectness pos params args = do
  assertM (P.length params == P.length args) $
    showPosition pos ++
    "function expected " ++
    show (P.length params) ++ " argument(s), received " ++ show (P.length args)
  zipWithM checkArgCorrectness args params
  return ()

checkArgCorrectness :: A.Expr -> A.ArgType -> ExprTEval ()
checkArgCorrectness arg param =
  case param of
    A.ArgRef pos ttype ->
      case arg of
        A.EVar _ ident
          -- We evaluate varType by hand because we don't want functions environment to be checked.
         -> do
          varType <-
            do env <- ask
               case M.lookup ident (variables env) of
                 Just t -> return t
                 Nothing ->
                   throwError $ errorWrongArgumentPassedByReference arg param
          let paramType = (getTypeFromArgType param)
          assertM
            (typesEq varType paramType)
            (errorMessageWrongType (hasPosition arg) varType paramType)
        _ -> throwError $ errorWrongArgumentPassedByReference arg param
    _ -> do
      argType <- typeOfExpr arg
      let paramType = (getTypeFromArgType param)
      assertM
        (typesEq argType paramType)
        (errorMessageWrongType (hasPosition arg) argType paramType)

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

checkForType ::
     MonadError String m
  => (BNFC'Position -> A.Type)
  -> BNFC'Position
  -> A.Type
  -> m ()
checkForType typeConstructor pos t =
  assertM
    (isType t typeConstructor)
    (errorMessageWrongType pos t $ typeConstructor pos)

-- Statement typechecker

type StmtTEval a = StateT Env (ExceptT String Identity) a

incrementBlockLevel :: Env -> Env
incrementBlockLevel env = env {level = (+ 1) $ level env}

typeStmt :: A.Stmt -> StmtTEval ()
typeStmt (A.Empty _) = return ()
-- ~ typeStmt (BStmt pos (Block' a))
typeStmt (A.Cond pos expr stmt) = do
  checkExpressionType (A.Bool A.BNFC'NoPosition) expr
  env <- get
  put (incrementBlockLevel env) >> typeStmt stmt
  put env
typeStmt (A.CondElse pos expr stmt1 stmt2) = do
  checkExpressionType (A.Bool A.BNFC'NoPosition) expr
  env <- get
  put (incrementBlockLevel env) >> typeStmt stmt1
  put (incrementBlockLevel env) >> typeStmt stmt2
  put env
typeStmt (A.SExp pos expr) = do
  env <- get
  liftEither $ runExprTEval env (typeOfExpr expr)
  return ()
typeStmt (A.While pos expr stmt) = do
  checkExpressionType (A.Bool A.BNFC'NoPosition) expr
  env <- get
  put (incrementBlockLevel env) >> typeStmt stmt
  put env
typeStmt (A.DeclStmt _ (A.Decl pos t items)) = do
  mapM_ (addItemToEnv t) items
  return ()
  where
    addItemToEnv :: A.Type -> A.Item -> StmtTEval ()
    addItemToEnv t (A.NoInit pos ident) = do
      env <- get
      checkVariableLevel pos ident
      put $
        env
          { variables = M.insert ident t (variables env)
          , variableLevels = M.insert ident (level env) (variableLevels env)
          }
      return ()
    addItemToEnv t (A.Init pos ident expr) = do
      checkExpressionType t expr
      checkVariableLevel pos ident
      env <- get
      put $
        env
          { variables = M.insert ident t (variables env)
          , variableLevels = M.insert ident (level env) (variableLevels env)
          }
      return ()
typeStmt (A.DeclStmt _ (A.FDecl pos retType ident params body)) = do
  env <- get
  checkFunctionLevel pos ident
  let newFunctions =
        M.insert
          ident
          (A.Function pos retType (P.map getArgType params))
          (functions env)
  let newFunctionLevels = M.insert ident (level env) (functionLevels env)
  let envWithAddedParams = P.foldr (addArgToEnv (level env + 1)) env params
  put $
    env
      { functions = newFunctions
      , variableLevels = variableLevels envWithAddedParams
      , variables = variables envWithAddedParams
      , functionLevels = newFunctionLevels
      , functionType = retType
      }
  typeStmt $ A.BStmt (hasPosition body) body
  put $ env {functions = newFunctions, functionLevels = newFunctionLevels}
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
    Nothing ->
      throwError $
      showPosition pos ++ "attempting to assign to an undeclared variable"
    Just t -> checkExpressionType t expr
typeStmt (A.TupleAss pos tupleIdents expr) = do
  env <- get
  typeOfExpr <- liftEither $ runExprTEval env (typeOfExpr expr)
  case typeOfExpr of
    A.Tuple p types -> do
      assertM (P.length types == P.length tupleIdents) $
        showPosition pos ++
        "error unpacking tuple, argument numbers don't match"
      zipWithM handleTupleIdent tupleIdents types
      return ()
    t ->
      throwError $
      showPosition pos ++
      "attempting to unpack tuple with expression that is not a tuple"
  where
    handleTupleIdent :: A.TupleIdent -> A.Type -> StmtTEval ()
    handleTupleIdent (A.TupleNoIdent pos) _ = return ()
    handleTupleIdent (A.TupleIdent pos ident) t
      -- Same code as during assignment, only don't check the type of expression 
      -- (as we know the type from typing the tuple).
     = do
      variables <- liftM variables get
      case M.lookup ident variables of
        Nothing ->
          throwError $
          showPosition pos ++ "attempting to assign to an undeclared variable"
        Just varType ->
          assertM (typesEq varType t) $
          showPosition pos ++
          "attempting to assign expression of type " ++
          printTree t ++ " to a variable of type " ++ printTree varType
    handleTupleIdent (A.TupleRec pos tupleIdents) t = do
      env <- get
      case t of
        A.Tuple p types -> do
          assertM (P.length types == P.length tupleIdents) $
            showPosition pos ++
            "error unpacking tuple, argument numbers don't match"
          zipWithM handleTupleIdent tupleIdents types
          return ()
        wrongType ->
          throwError $
          showPosition pos ++
          "attempting to unpack tuple with expression that is not a tuple"
typeStmt (BStmt _ (Block pos stmts)) = do
  env <- get
  put env {level = level env + 1}
  mapM_ typeStmt stmts
  put env
  return ()

-- Check if variable ident can be declared at the given level.
checkVariableLevel :: BNFC'Position -> Ident -> StmtTEval ()
checkVariableLevel pos ident = do
  env <- get
  let lvl = level env
  case M.lookup ident (variableLevels env) of
    Nothing -> return ()
    Just varLevel ->
      assertM (varLevel /= lvl) $
      showPosition pos ++
      "variable " ++ printTree ident ++ " was already declared at this level"

checkFunctionLevel :: BNFC'Position -> Ident -> StmtTEval ()
checkFunctionLevel pos ident = do
  env <- get
  let lvl = level env
  case M.lookup ident (functionLevels env) of
    Nothing -> return ()
    Just varLevel ->
      assertM (varLevel /= lvl) $
      showPosition pos ++
      "function " ++ printTree ident ++ " was already declared at this level"

checkExpressionType :: A.Type -> A.Expr -> StmtTEval ()
checkExpressionType t expr = do
  env <- get
  exprType <- liftEither $ runExprTEval env (typeOfExpr expr)
  assertM (typesEq exprType t) $
    errorMessageWrongType (hasPosition expr) exprType t
  return ()

typeProgram :: A.Program -> StmtTEval ()
typeProgram (A.Program pos functions) = do
  assertM (P.any checkIfMainDef functions) $ "no main function"
  mapM_ typeTopDef functions
  return ()

typeTopDef :: A.TopDef -> StmtTEval ()
typeTopDef (A.FnDef pos retType ident args body) =
  typeStmt (A.DeclStmt pos (A.FDecl pos retType ident args body))

runStmtTEval :: Env -> StmtTEval a -> Either String (a, Env)
runStmtTEval env e = runIdentity (runExceptT (runStateT e env))

addFunctions :: Env -> Env
addFunctions e = snd $ DE.fromRight ((), initEnv) $ runStmtTEval e x
  where
    x = do
      typeTopDef printInt
      typeTopDef printBool
      typeTopDef Utils.printString
      typeTopDef assert

initEnv :: Env
initEnv =
  addFunctions $
  Env
    { variables = M.empty
    , variableLevels = M.empty
    , functions = M.empty
    , functionLevels = M.empty
    , level = 0
    , functionType = A.Void A.BNFC'NoPosition -- won't be used anyway.
    }

runTypeChecker :: A.Program -> Either String ((), Env)
runTypeChecker p = runStmtTEval initEnv (typeProgram p)

-- Adds arg to variables map and levels map.
addArgToEnv :: Int -> Arg -> Env -> Env
addArgToEnv newLevel (Arg _ (ArgT _ t) ident) env =
  env
    { variables = M.insert ident t (variables env)
    , variableLevels = M.insert ident newLevel (variableLevels env)
    }
addArgToEnv newLevel (Arg _ (ArgRef _ t) ident) env =
  env
    { variables = M.insert ident t (variables env)
    , variableLevels = M.insert ident newLevel (variableLevels env)
    }
