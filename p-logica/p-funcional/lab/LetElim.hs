----------------------------------------------------------------------------
-- LABORATORIO DE PROGRAMACION FUNCIONAL 2023
-- Módulo de eliminación de LETs
--
-- Un LET (let x = e1 in e2) es eliminado si e1 es 
-- un literal entero o booleano. En ese caso se 
-- sustituyen las ocurrencias de x en e2 por e1, 
-- o sea, e2[e1/x]. 
----------------------------------------------------------------------------

module LetElim where

import Syntax
import Data.List

-- Luego de superar de forma exitosa la fase de chequeos y antes de generar c´odigo
-- el compilador va a aplicar una optimizaci´on sobre el programa fuente en la que
-- se van a eliminar aquellos lets en los que se liga una variable a un literal. Conc-
-- retamente, todo let de la forma let x :: t = c in e, donde c es un literal
-- entero o booleano, va a ser transformado en una nueva expresi´on, que denota-
-- mos como e[c/x], que resulta de sustituir por el literal c todas las ocurrencias
-- libres de la variable x en la expresi´on e.
-- Una variable x se dice que ocurre libre en una expresi´on e cuando no aparece
-- en el cuerpo de un let que liga esa misma variable. Caso contrario se dice que
-- la variable ocurre ligada. Por ejemplo, x ocurre libre en la expresi´on x + 2. En
-- cambio, en la expresi´on let x :: Int = 4 * x in x + 2 la variable x tiene
-- una ocurrencia libre (la que aparece en la expresi´on 4 * x) y una ligada (la que
-- aparece en la expresi´on x + 2).
-- En los siguientes ejemplos escribiremos e =⇒ e’ para significar que e se
-- transforma en e’ y eliminaremos la anotaci´on del tipo de la variable ligada de
-- un let con el fin de mejorar la lectura.
-- let x = 3 in x + 2 * x =⇒ 3 + 2 * 3
-- let x = 3 * 4 in x + 2 =⇒ let x = 3 * 4 in x + 2
-- let x = 3 in let x = 4 in x + 2 =⇒ let x = 4 in x + 2
-- =⇒ 4 + 2
-- let x = 3 in let y = x in y + 2 =⇒ let y = 3 in y + 2
-- =⇒ 3 + 2
-- let x = 3 in let x = 4 + x in x + 2 =⇒ let x = 4 + 3 in x + 2
-- Hay casos en que la eliminaci´on de un let dentro de la expresi´on ligada puede
-- generar una oportunidad de eliminaci´on sobre el let externo. Por ejemplo,
-- let x = (let y = 4 in y) in x + 2 =⇒ let x = 4 in x + 2
-- =⇒ 4 + 2
-- En cambio, hay otros casos en que la eliminaci´on de un let interno puede que
-- no tenga efecto en la eliminaci´on del let externo.
-- let x = 5 + (let y = 4 in y) in x + 2 =⇒ let x = 5 + 4 in x + 2
-- let x = (let y = 4 in y + 3) in x + 2 =⇒ let x = 4 + 3 in x + 2
-- La implementaci´on de esta optimizaci´on se debe realizar mediante la definici´on
-- de la funci´on
-- ELIMINACION DE LETs

letElimP :: Program -> Program 
letElimP (Program defs expr) = Program (letElimD defs) (letElimE expr)

letElimD :: Defs -> Defs
letElimD [] = []
letElimD (x:xs) = (letElimF x) : (letElimD xs)

letElimF :: FunDef -> FunDef
letElimF (FunDef (name, sig) args expr) = FunDef (name, sig) args (letElimE expr)

letElimE :: Expr -> Expr
letElimE (Var name) = Var name
letElimE (IntLit int) = IntLit int
letElimE (BoolLit bool) = BoolLit bool
letElimE (Infix op expr1 expr2) = Infix op (letElimE expr1) (letElimE expr2)
letElimE (If expr1 expr2 expr3) = If (letElimE expr1) (letElimE expr2) (letElimE expr3)
letElimE (Let (name, _) expr1 expr2) = letElimE (subst expr2 name expr1)
letElimE (App name exprs) = App name (letElimEs exprs)
letElimE _ = error "Error en letElimE"

letElimEs :: [Expr] -> [Expr]
letElimEs [] = []
letElimEs (x:xs) = (letElimE x) : (letElimEs xs)

subst :: Expr -> Name -> Expr -> Expr
subst (Var name) name' expr = if name == name' then expr else (Var name)
subst (IntLit int) _ _ = IntLit int
subst (BoolLit bool) _ _ = BoolLit bool
subst (Infix op expr1 expr2) name expr = Infix op (subst expr1 name expr) (subst expr2 name expr)
subst (If expr1 expr2 expr3) name expr = If (subst expr1 name expr) (subst expr2 name expr) (subst expr3 name expr)
subst (Let (name', _) expr1 expr2) name expr = if name == name' then (Let (name', TyInt) (subst expr1 name expr) expr2) else (Let (name', TyInt) (subst expr1 name expr) (subst expr2 name expr))
subst (App name exprs) name' expr = App name (substEs exprs name expr)
subst _ _ _ = error "Error en subst"

substEs :: [Expr] -> Name -> Expr -> [Expr]
substEs [] _ _ = []
substEs (x:xs) name expr = (subst x name expr) : (substEs xs name expr)

-- que recibe un programa y devuelve un programa equivalente en el que se han
-- eliminado todos los lets que ligan una variable a un literal. Para ello se debe
-- definir la funci´on
-- ELIMINACION DE LETs




