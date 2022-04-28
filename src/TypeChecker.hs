-# LANGUAGE FlexibleContexts #-}

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

-- TODO sprawdzić czy to działa
-- TODO ujednolicić te cztery funkcje
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
  "Unexpected type " ++
  printTree t ++ " on position " ++ (show $ A.hasPosition t)

expectedTypeMessage :: Type -> String
expectedTypeMessage t = "expected type " ++ printTree t
