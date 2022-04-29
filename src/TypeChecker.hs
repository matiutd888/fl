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
typeOfExpr (A.EAppIdent pos Ident)
getArgType :: A.Arg -> ArgType
getArgType (Arg _ t _) = t

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
checkForBool :: MonadError String m => Type -> m ()
checkForBool t =
  case t of
    A.Bool _ -> return ()
    t -> throwError (errorMessage1 t $ A.Bool $ A.hasPosition t)

checkForInt :: MonadError String m => Type -> m ()
checkForInt t =
  case t of
    A.Int _ -> return ()
    t -> throwError (errorMessage1 t $ A.Int $ A.hasPosition t)

errorMessage1 :: Type -> Type -> String
errorMessage1 received expected =
  unexpectedTypeMessage received ++ ", " ++ (expectedTypeMessage expected)

unexpectedTypeMessage :: Type -> String
unexpectedTypeMessage t =
  "unexpected type " ++
  printTree t ++ " on position " ++ (show $ A.hasPosition t)

expectedTypeMessage :: Type -> String
expectedTypeMessage t = "expected type " ++ printTree t

undefinedReferenceMessage :: Ident -> BNFC'Position -> String
undefinedReferenceMessage ident pos =
  "undefined reference " ++ show ident ++ " at " ++ (show pos)
