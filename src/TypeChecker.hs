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
-- Boolean operator expression expressions.
typeOfExpr (A.EAnd pos e1 e2) = typeOfBoolOpExp pos e1 e2
typeOfExpr (A.EOr pos e1 e2) = typeOfBoolOpExp pos e1 e2

-- TODO sprawdzić czy to działa
typeOfBoolOpExp pos e1 e2 = do
  t1 <- typeOfExpr e1 >>= checkForBool
  t2 <- typeOfExpr e2 >>= checkForBool
  return $ A.Bool pos

typeOfExpr _ = undefined

checkForBool :: Type -> ExprTEval ()
checkForBool t =
  case t of
    A.Bool _ -> return ()
    t -> throwError (errorMessage1 t $ A.Bool $ A.hasPosition t)

errorMessage1 :: Type -> Type -> String
errorMessage1 received expected =
  unexpectedTypeMessage received ++ ", " ++ (expectedTypeMessage expected)

unexpectedTypeMessage :: Type -> String
unexpectedTypeMessage t =
  "Unexpected type " ++
  printTree t ++ " on position " ++ (show $ A.hasPosition t)

expectedTypeMessage :: Type -> String
expectedTypeMessage t = "expected type " ++ printTree t
