----------------------------------------------------------------------------
-- LABORATORIO DE PROGRAMACION FUNCIONAL 2023
-- Módulo de generación de código C
--
-- Se debe implementar la función genProgram,
-- que dado un AST que representa un programa válido
-- genera el código C correspondiente.
----------------------------------------------------------------------------

module Generator where

import Syntax
-- se pueden agregar mas importaciones 
-- en caso de ser necesario

import Data.List

-- CODE GENERATOR

-- Para evitar posibles conflictos de nombres con palabras reservadas de C, los
-- identificadores del programa FUN debe traducirse prefij´andoles el caracter
-- ‘ ’. Por ejemplo, el identificador x se traduce como _x

genProgram :: Program -> String
genProgram program = "#include <stdio.h>\n" ++ (genFunction program) ++ "int main () { \n printf(\"%d\\n\", " ++ (genExpr program) ++ "); }"

genFunction :: Program -> String
genFunction (Program defs expr) = concat (map genFunDef defs)

genFunDef :: FunDef -> String
genFunDef (FunDef (name, sig) args expr) = (genType (snd sig)) ++ " " ++ (genName name) ++ "(" ++ (genArgs args) ++ ") { \n return " ++ (genExpr expr) ++ "; \n } \n"

genArgs :: [Name] -> String
genArgs [] = ""
genArgs (x:xs) = (genName x) ++ (genArgs xs)

genExpr :: Program -> String
genExpr (Program defs expr) = genExpr expr

genExpr (Var name) = genName name
genExpr (IntLit int) = show int
genExpr (BoolLit bool) = if bool then "1" else "0"
genExpr (Infix op expr1 expr2) = "(" ++ (genExpr expr1) ++ (genOp op) ++ (genExpr expr2) ++ ")"
genExpr (If expr1 expr2 expr3) = "(" ++ (genExpr expr1) ++ " ? " ++ (genExpr expr2) ++ " : " ++ (genExpr expr3) ++ ")"
genExpr (Let (name, _) expr1 expr2) = "(" ++ (genExpr expr1) ++ " ? " ++ (genExpr expr2) ++ ")"
genExpr (App name exprs) = (genName name) ++ "(" ++ (genExprs exprs) ++ ")"
genExpr _ = error "Error en genExpr"

genExprs :: [Expr] -> String
genExprs [] = ""
genExprs (x:xs) = (genExpr x) ++ (genExprs xs)

genOp :: Op -> String
genOp Add = " + "
genOp Sub = " - "
genOp Mult = " * "
genOp Div = " / "
genOp Eq = " == "
genOp NEq = " != "
genOp GTh = " > "
genOp LTh = " < "
genOp GEq = " >= "
genOp LEq = " <= "
genOp _ = error "Error en genOp"

genType :: Type -> String
genType TyInt = "int"
genType TyBool = "Bool"

genName :: Name -> String
genName name = "_" ++ name

-- Para evitar posibles conflictos de nombres con palabras reservadas de C, los
-- identificadores del programa FUN debe traducirse prefij´andoles el caracter
-- ‘ ’. Por ejemplo, el identificador x se traduce como _x


