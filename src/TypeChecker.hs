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
typeOfExpr (A.EApp pos callee args) = do
  case typeOfExpr callee
    -- TODO retType pos should be changed to `pos`.
        of
    A.Function _ retType params ->
      (checkArgsCorrectness params args) >>= (\_ -> return retType)
    _ -> throwError $ notAFunctionMessage callee

checkArgsCorrectness :: MonadError String m => [ArgType] -> [Expr] -> m ()
checkArgsCorrectness params args = undefined

-- TODO tutaj skończyłem
checkArgCorrectness arg param =
  case param of
    A.ArgRef pos ttype -> error
    _ -> error--  otherwise = undefined

getArgType :: A.Arg -> A.ArgType
getArgType (A.Arg _ t _) = t

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
    _ -> throwError (errorMessage1 t $ A.Bool $ A.hasPosition t)

checkForInt :: MonadError String m => A.Type -> m ()
checkForInt t =
  case t of
    A.Int _ -> return ()
    _ -> throwError (errorMessage1 t $ A.Int $ A.hasPosition t)

checkForVar :: MonadError String m => A.Type -> m ()
checkForVar t =
  case t of
    A.EVar _ -> return ()
    _ -> throwError (errorMessage1 t $ A.Bool $ A.hasPosition t)

errorMessage1 :: A.Type -> A.Type -> String
errorMessage1 received expected =
  unexpectedTypeMessage received ++ ", " ++ (expectedTypeMessage expected)

showPositionOf :: A.HasPosition a => a -> String
showPositionOf x = (show $ A.hasPosition x) ++ ": "

showPosition :: BNFC'Position -> String
showPosition pos = show pos ++ ": "

unexpectedTypeMessage :: Type -> String
unexpectedTypeMessage t = showPositionOf t ++ "unexpected type " ++ printTree t

expectedTypeMessage :: Type -> String
expectedTypeMessage t = "expected type " ++ printTree t

undefinedReferenceMessage :: Ident -> BNFC'Position -> String
undefinedReferenceMessage ident pos =
  showPosition pos ++ "undefined reference " ++ show ident

notAFunctionMessage :: Expr -> String
notAFunctionMessage expr =
  showPositionOf expr ++
  " applying argument to expression that is not a function!"

isType :: A.Type -> (BNFC'Position -> A.Type) -> Bool
isType t1 t2 = typesEq t1 $ t2 BNFC'Position

typesEq :: A.Type -> A.Type -> Bool
typesEq (A.Int _) (A.Int _) = True
typesEq (A.Str _) (A.Str _) = True
typesEq (A.Bool _) (A.Bool _) = True
typesEq (A.Void _) (A.Void _) = True
typesEq (A.Tuple _ types1) (A.Tuple _ types2) =
  and $ zipWith typesEq types1 types2
typesEq (A.Function _ ret1 args1) (A.Function _ ret2 args2) =
  typesEq ret1 ret2 && and zipWith paramTypesEqual args1 args2
typesEq _ _ = False

paramTypesEqual :: A.ArgType -> A.ArgType -> Bool
paramTypesEqual (A.ArgRef _ t1) (A.ArgRef _ t2) = typesEq t1 t2
paramTypesEqual (A.ArgT _ t1) (A.ArgT _ t2) = typesEq t1 t2
paramTypesEqual _ _ = False
