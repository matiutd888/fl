module Errors where

import AbsGramatyka as A
import PrintGramatyka

errorMessageWrongType :: BNFC'Position -> A.Type -> A.Type -> String
errorMessageWrongType pos received expected =
  showPosition pos ++ unexpectedTypeMessage received ++ ", " ++ (expectedTypeMessage expected)

showPositionOf :: A.HasPosition a => a -> String
showPositionOf = showPosition . A.hasPosition

showPosition :: BNFC'Position -> String
showPosition Nothing = "NoPos: "
showPosition (Just x) = show x ++ ": "

unexpectedTypeMessage :: Type -> String
unexpectedTypeMessage t = "unexpected type " ++ printTree t

expectedTypeMessage :: Type -> String
expectedTypeMessage t = "expected type " ++ printTree t

undefinedReferenceMessage :: A.Ident -> BNFC'Position -> String
undefinedReferenceMessage (Ident x) pos =
  showPosition pos ++ "undefined reference " ++ show x

notAFunctionMessage :: A.Type -> String
notAFunctionMessage expr =
  showPositionOf expr ++
  " applying argument to expression that is not a function!"

errorWrongArgumentPassedByReference :: A.Expr -> ArgType -> String
errorWrongArgumentPassedByReference expr arg =
    showPositionOf expr ++ " passing " ++ show expr ++ " as an reference argument "
    ++ show arg ++ ", expected variable type"
