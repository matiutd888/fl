{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BlockArguments #-}

module Utils where

import AbsGramatyka as A
import Control.Monad.Except

checkIfMainDef :: A.TopDef -> Bool
checkIfMainDef (A.FnDef pos retType ident args body) =
  ident == A.Ident "main" && isType retType A.Int && args == []

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

paramTypesEqual :: A.ArgType -> A.ArgType -> Bool
paramTypesEqual (A.ArgRef _ t1) (A.ArgRef _ t2) = typesEq t1 t2
paramTypesEqual (A.ArgT _ t1) (A.ArgT _ t2) = typesEq t1 t2
paramTypesEqual _ _ = False

noPos :: A.BNFC'Position
noPos = A.BNFC'NoPosition

assertM :: MonadError String m => Bool -> String -> m ()
assertM b s =
  if b
    then return ()
    else throwError s
