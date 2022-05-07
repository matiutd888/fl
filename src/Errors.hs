module Errors where

import AbsGramatyka as A
import PrintGramatyka

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
