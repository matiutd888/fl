{-# LANGUAGE FlexibleContexts #-}

module Errors where

import AbsGramatyka as A
import PrintGramatyka

errorMessageWrongType :: BNFC'Position -> A.Type -> A.Type -> String
errorMessageWrongType pos received expected =
  showPosition pos ++
  unexpectedTypeMessage received ++ ", " ++ (expectedTypeMessage expected)

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

notAFunctionMessage :: BNFC'Position -> A.Type -> String
notAFunctionMessage pos typeOfExpr =
  showPosition pos ++
  " applying argument to expression of type " ++
  printTree typeOfExpr ++ ", that is not a function"

errorWrongArgumentPassedByReference :: A.Expr -> ArgType -> String
errorWrongArgumentPassedByReference expr arg =
  showPositionOf expr ++
  " passing " ++
  printTree expr ++
  " as an reference argument " ++ printTree arg ++ ", expected variable type"
