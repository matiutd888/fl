{-# LANGUAGE FlexibleContexts #-}

import AbsGramatyka as A
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State

import qualified Data.Map as M
import Data.Text
import PrintGramatyka

type TypeMap = M.Map A.Ident A.Type

type ExprTEval a = ReaderT TypeMap (ExceptT String Identity) a

-- throwIf :: A.Bool -> String -> Except String Identity
-- throwIf b str = if b then throwError str else ()
typeOfExpr :: A.Expr -> ExprTEval A.Type
typeOfExpr (A.EVar pos ident) = do
  env <- ask
  case M.lookup ident env of
    Nothing -> throwError $ (undefinedReferenceMessage ident pos) ++ "\n"
    Just t -> return t
-- Literals.
typeOfExpr (A.ELitTrue pos) = return $ A.Bool pos
typeOfExpr (A.ELitFalse pos) = return $ A.Bool pos
typeOfExpr (A.ELitInt pos _) = return $ A.Int pos
typeOfExpr (A.EString pos _) = return $ A.Str pos
-- Binary operator expressions.
typeOfExpr (A.EAnd pos e1 e2) = typeOfBinOp checkForBool A.Bool pos e1 e2
typeOfExpr (A.EOr pos e1 e2) = typeOfBinOp checkForBool A.Bool pos e1 e2
typeOfExpr (A.EAdd pos e1 _ e2) = typeOfBinOp checkForInt A.Int pos e1 e2
typeOfExpr (A.EMul pos e1 _ e2) = typeOfBinOp checkForInt A.Int pos e1 e2
typeOfExpr (A.ERel pos e1 _ e2) = typeOfBinOp checkForInt A.Bool pos e1 e2
-- Unary operator expressions.
typeOfExpr (A.Not pos e) = do
  typeOfExpr e >>= checkForBool
  return $ A.Bool pos
typeOfExpr (A.Neg pos e) = do
  typeOfExpr e >>= checkForBool
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
  -- LambdaCallee a (Lambda' a) | IdentCallee a Ident
  case callee of
    -- TODO retType pos should be changed to `pos`.
	A.LambdaCallee p l -> do
		t <- typeOfExpr (A.ELambda p l)
		handleFunction t args
	A.IdentCallee p ident -> do
		t <- typeOfExpr (A.EVar p ident)
		handleFunction t args

handleFunction :: A.Type -> [A.Expr] -> ExprTEval A.Type
handleFunction f args = case f of
    A.Function _ retType params -> (checkArgsCorrectness params args) >>= (\_ -> return retType)
    _ -> throwError $ notAFunctionMessage f

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
          assertM (typesEq varType paramType) (showPositionOf arg ++ errorMessageWrongType paramType varType) 
        _ -> throwError $ errorWrongArgumentPassedByReference arg param
    _ ->  do
			argType <- typeOfExpr arg
			let paramType = (getTypeFromArgType param)
			assertM (typesEq argType paramType) (showPositionOf arg ++ errorMessageWrongType paramType argType) 
		

assertM :: MonadError String m => Bool -> String -> m ()
assertM b s = if b then return () else throwError s

getArgType :: A.Arg -> A.ArgType
getArgType (A.Arg _ t _) = t

getTypeFromArgType :: A.ArgType -> A.Type
getTypeFromArgType (A.ArgRef _ t) = t
getTypeFromArgType (A.ArgT _ t) = t

typeOfBinOp ::
     (Type -> ExprTEval ())
  -> (BNFC'Position -> A.Type)
  -> A.BNFC'Position
  -> A.Expr
  -> A.Expr
  -> ExprTEval A.Type
typeOfBinOp checkFunction typeConstructor pos e1 e2 = do
  typeOfExpr e1 >>= checkFunction
  typeOfExpr e2 >>= checkFunction
  return $ typeConstructor pos

-- TODO jak to przerobić?
checkForBool :: MonadError String m => A.Type -> m ()
checkForBool t =
  case t of
    A.Bool _ -> return ()
    _ -> throwError (errorMessageWrongType t $ A.Bool $ A.hasPosition t)

checkForInt :: MonadError String m => A.Type -> m ()
checkForInt t =
  case t of
    A.Int _ -> return ()
    _ -> throwError (errorMessageWrongType t $ A.Int $ A.hasPosition t)

{-- checkForVar :: MonadError String m => A.Type -> m ()
checkForVar t =
  case t of
    A.EVar _ -> return ()
    _ -> throwError (errorMessageWrongType t $ A.EVar $ A.hasPosition t) --}

errorMessageWrongType :: A.Type -> A.Type -> String
errorMessageWrongType received expected =
  unexpectedTypeMessage received ++ ", " ++ (expectedTypeMessage expected)

showPositionOf :: A.HasPosition a => a -> String
showPositionOf x = (show $ A.hasPosition x) ++ ": "

showPosition :: BNFC'Position -> String
showPosition pos = show pos ++ ": "

unexpectedTypeMessage :: Type -> String
unexpectedTypeMessage t = showPositionOf t ++ "unexpected type " ++ printTree t

expectedTypeMessage :: Type -> String
expectedTypeMessage t = "expected type " ++ printTree t

undefinedReferenceMessage :: A.Ident -> BNFC'Position -> String
undefinedReferenceMessage ident pos =
  showPosition pos ++ "undefined reference " ++ show ident

notAFunctionMessage :: A.Type -> String
notAFunctionMessage expr =
  showPositionOf expr ++
  " applying argument to expression that is not a function!"

errorWrongArgumentPassedByReference :: A.Expr -> ArgType -> String
errorWrongArgumentPassedByReference expr arg =
	showPositionOf expr ++ " passing " ++ show expr ++ " as an reference argument "
	++ show arg ++ ", expected variable type"

isType :: A.Type -> (BNFC'Position -> A.Type) -> Bool
isType t1 t2 = typesEq t1 $ t2 BNFC'NoPosition

typesEq :: A.Type -> A.Type -> Bool
typesEq (A.Int _) (A.Int _) = True
typesEq (A.Str _) (A.Str _) = True
typesEq (A.Bool _) (A.Bool _) = True
typesEq (A.Void _) (A.Void _) = True
typesEq (A.Tuple _ types1) (A.Tuple _ types2) =
  and $ Prelude.zipWith typesEq types1 types2
typesEq (A.Function _ ret1 args1) (A.Function _ ret2 args2) =
  typesEq ret1 ret2 && and (Prelude.zipWith paramTypesEqual args1 args2)
typesEq _ _ = False

paramTypesEqual :: A.ArgType -> A.ArgType -> Bool
paramTypesEqual (A.ArgRef _ t1) (A.ArgRef _ t2) = typesEq t1 t2
paramTypesEqual (A.ArgT _ t1) (A.ArgT _ t2) = typesEq t1 t2
paramTypesEqual _ _ = False
