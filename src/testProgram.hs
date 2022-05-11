module TestProgram where
import AbsGramatyka
p::Program
p=Program (Just (1,1)) [FnDef (Just (1,1)) (Void (Just (1,1))) (Ident "main") [] (Block (Just (1,13)) [DeclStmt (Just (2,5)) (FDecl (Just (2,5)) (Int (Just (2,5))) (Ident "f") [] (Block (Just (2,13)) [Ret (Just (3,9)) (EApp (Just (3,16)) (IdentCallee (Just (3,16)) (Ident "f")) [])]))])]
