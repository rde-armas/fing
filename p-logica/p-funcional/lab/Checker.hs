----------------------------------------------------------------------------
-- LABORATORIO DE PROGRAMACION FUNCIONAL 2023
-- Módulo de chequeo
--
-- Se debe implementar la funcion checkProgram que, dado un AST
-- que representa un programa, retorna Ok en caso de no encontrar errores, 
-- o la listInta de errores encontrados en otro caso.   
----------------------------------------------------------------------------


module Checker where

import Syntax
-- se pueden agregar mas importaciones 
import Debug.Trace

-- en caso de ser necesario
import Data.Map (Map)
import qualified Data.Map as Map

import Data.List
import Data.Maybe

-- CHECKER

data Checked = Ok | Wrong [Error]

data Error = Duplicated      Name
           | Undefined       Name
           | ArgNumDef       Name Int Int
           | ArgNumApp       Name Int Int
           | Expected        Type Type
            
instance Show Error where
 show (Duplicated      n)  = "Duplicated declaration: " ++ n
 show (Undefined       n)  = "Undefined: " ++ n
 show (ArgNumDef   n s d)
   = "The number of parameters in the definition of "++ n ++
     " doesn't match the signature ("++ show d ++ " vs " ++ show s ++ ")"
 show (ArgNumApp   n s d)
   = "The number of arguments in the application of: " ++ n ++
     " doesn't match the signature ("++ show d ++ " vs " ++ show s ++ ")"
 show (Expected    ty ty')
   = "Expected: " ++ show ty ++ " Actual: " ++ show ty'

checkProgram :: Program -> Checked
checkProgram program =
  let errorsFunctions = nameFunctionDuplicate program
      errorsPrarams = nameParametersDuplicate program
      errorsNumberOfParameters = numberOfParameters program 
      errorsUndefined = undifinidPrameters program
      errorsTypes = checkTypes program
      errors = errorsFunctions ++ errorsPrarams ++ errorsNumberOfParameters ++ errorsUndefined ++ errorsTypes
  in case length errors of
    0 -> Ok
    _ -> Wrong errors

--------------------- Check function declarations and parameters ------------------------
buildError :: Name -> Error
buildError name  = Duplicated name

mainFuntion :: Program -> Expr
mainFuntion (Program _ expr) = expr

collectionNameFunctionDeclarations :: Program -> Defs
collectionNameFunctionDeclarations (Program defs _) = defs

sortAndRemoveFirst :: [Name] -> Int -> [Name]
sortAndRemoveFirst xs pos 
  | length xs > pos =
    if elem x xs' then 
      [x] ++ sortAndRemoveFirst xs (pos + 1)
    else 
      sortAndRemoveFirst xs (pos + 1)
  | otherwise =  []
  where xs' = getPrefix xs pos
        x  = last (getPrefix xs (pos + 1))

getPrefix :: [Name] -> Int -> [Name]
getPrefix _ 0 = []
getPrefix [] _ = []
getPrefix (x:xs) n = x : getPrefix xs (n-1)

-- Name function declarations
nameFunctionDuplicate :: Program -> [Error]
nameFunctionDuplicate program = 
  let defs = collectionNameFunctionDeclarations program
      names = map (\(FunDef (name, _) _ _) -> name) defs
      namesDuplicate = filter (\name -> checkMultipleFunctionDeclarations names name) names
      sortName = sortAndRemoveFirst namesDuplicate 0
      errors = map (\name -> buildError name) sortName
  in errors

-- Parametros duplicados
nameParametersDuplicate :: Program -> [Error]
nameParametersDuplicate program = 
  let defs = collectionNameFunctionDeclarations program
      params = map (\(FunDef _ params _) -> params) defs
      parametersDuplicate = duplicatedParameterName params
      errors = map (\name -> buildError name) parametersDuplicate
  in errors

duplicatedParameterName :: [[Name]] -> [Name]
duplicatedParameterName [] = []
duplicatedParameterName (x:xs) = 
  sortAndRemoveFirst (filter(\x' -> checkMultipleFunctionDeclarations x x') x) 0 ++ duplicatedParameterName xs
  where x' = if length x > 0 then head x else  []

checkMultipleFunctionDeclarations :: [Name] -> Name -> Bool
checkMultipleFunctionDeclarations names name = any (== name) (tail (dropWhile (/= name) (names)))

-- numberOfParameters
numberOfParameters :: Program -> [Error]
numberOfParameters program =
  let defs = collectionNameFunctionDeclarations program
      parametersNumberDeclarationsDiff = map (\(FunDef (name, Sig params' _) params _) -> (name, (checkNumberOfParameters name params params'))) defs
      temp = filter (\(name ,(x, y)) -> x /= y) parametersNumberDeclarationsDiff
      errorsNumberOfParametersDeclaration = map (\(name, (x, y)) -> ArgNumDef name x y) temp
      listIntaApp = map (\(FunDef (name, Sig _ _) _ bdy) -> (name, findApps bdy)) defs
      main = mainFuntion program
      -- filtrar las f quue solo estan bien definidas
      filterMain = filter (\(App name _) -> not (elem name (map fst temp))) (findApps main)
      listIntaAppMain' = [("main", filterMain)] ++ listIntaApp
      -- eliminar todas las funciones no definidas
      listIntaAppMain = filter (\(name, _) -> not (elem name (map fst temp))) listIntaAppMain'
      listIntaAppType = map (\(nameFInBody, expr) ->  compareExprDeclaration nameFInBody expr defs) listIntaAppMain
      errorsApplications = concat listIntaAppType
      errors = errorsNumberOfParametersDeclaration -- ++ errorsApplications
  in errors

checkNumberOfParameters :: Name -> [Name] -> [Type] -> (Int, Int)
checkNumberOfParameters name params params' = 
  if length params == length params' then
    (1,1)
  else
    (length params', length params)

findApps :: Expr -> [Expr]
findApps (App name exprs) = [App name exprs]
findApps (Infix _ expr1 expr2) = findApps expr1 ++ findApps expr2
findApps (If expr1 expr2 expr3) = findApps expr1 ++ findApps expr2 ++ findApps expr3
findApps (Let _ expr1 expr2) = findApps expr1 ++ findApps expr2
findApps _ = []

compareExprDeclaration :: Name -> [Expr] -> Defs -> [Error]
compareExprDeclaration name exprCall defs =
  if length exprCall == 0 then
    []
  else
  let nameCall = if name == "main"
                then case head exprCall of
                    App name _ -> name
                else name
      functionDeclaration = map (\(FunDef (namef,_) params _) -> (namef, params)) defs
      coincidence = filter (\(nameF, _) -> nameF == nameCall) functionDeclaration
      exprCall' = concatMap (\(App _ exprs) -> exprs) exprCall
      coincidenceCall = filter (\(name', varLis) -> ((name' == nameCall) && (length varLis == length exprCall'))) coincidence
      
  in if (length coincidenceCall == 0) && (length coincidence /= 0) && (length exprCall' /= 0) then
          [ArgNumApp nameCall (length (snd (head coincidence))) (length exprCall')]
      else []

checkTypesParameters :: Name -> [Name] -> [Type] -> Error
checkTypesParameters name params params' = Expected TyBool TyBool
  -- if all (\(x, y) -> x == y) (zip params params') then
  --   Expected TyInt TyInt
  -- else
  --   Expected TyBool TyBool

-- undifinidPrameters
undifinidPrameters :: Program -> [Error]
undifinidPrameters program = 
  let defs = collectionNameFunctionDeclarations program
      functionDeclaration = map (\(FunDef (namef,_) params _) -> (namef, params)) defs
      functionlistIntVarAndAppFree = map (\(FunDef (name, Sig _ _) _ bdy) ->  (name, findVars bdy ++ findVarFreeInApps bdy )) defs
      main = mainFuntion program
      listIntaAppMainFree = findVarFreeInApps main ++ findVars main ++ findApps main
      funcionlistInt =  filter (\name -> elem name (map fst functionDeclaration)) (map fst functionlistIntVarAndAppFree)
      undifinidVar =  filter (\x -> not (elem x (concat (map snd functionDeclaration)))) (concat (map forVarAndApp (map snd functionlistIntVarAndAppFree)))
      undifinidFuncionVar = filter (\x -> not (elem x funcionlistInt)) undifinidVar
      tempMain =  forVarAndApp [main]
      undifinidFuncionVarMain = filter (\x -> not (elem x funcionlistInt)) tempMain
      temp = nub (undifinidFuncionVar ++ undifinidFuncionVarMain)
      errors =  map (\name -> Undefined name) temp
  in errors
   
forVarAndApp :: [Expr] -> [Name]
forVarAndApp [] = []
forVarAndApp (x:xs) = 
  case x of
    Var name -> [name] ++ forVarAndApp xs
    App name expr -> [name] ++ forVarAndApp xs  ++ forVarAndApp expr
    Let x expr1 expr2 -> forVarAndApp (findVarNonInFunction x expr1) ++ forVarAndApp (findVarNonInFunction x expr2) ++ forVarAndApp xs
    _ ->  forVarAndApp xs

findVars :: Expr -> [Expr]
findVars (Var name) = [Var name]
findVars (Infix _ expr1 expr2) = findVars expr1 ++ findVars expr2
findVars (If expr1 expr2 expr3) = findVars expr1 ++ findVars expr2 ++ findVars expr3
findVars (Let x expr1 expr2) = (findVarNonInFunction x  expr1) ++ (findVarNonInFunction x expr2)
findVars _ = [] --- queda definir el alcance le los let dentro de las funciones
-- (Let (vn,t) e b))todas las aplicaciones de vn sobre b estan bien definidas

findVarNonInFunction :: TypedVar -> Expr -> [Expr]
findVarNonInFunction (name, _) (Var name') = if name == name' then [] else [Var name']
findVarNonInFunction (name, _) (Infix _ expr1 expr2) = findVarNonInFunction (name, TyInt) expr1 ++ findVarNonInFunction (name, TyInt) expr2
findVarNonInFunction (name, _) (If expr1 expr2 expr3) = findVarNonInFunction (name, TyInt) expr1 ++ findVarNonInFunction (name, TyInt) expr2 ++ findVarNonInFunction (name, TyInt) expr3
findVarNonInFunction (name, _) (Let x expr1 expr2) = []--(findVarNonInFunction x  expr1) ++ (findVarNonInFunction x expr2)
findVarNonInFunction (name, _) (App _ exprs) = concatMap (\expr -> findVarNonInFunction (name, TyInt) expr) exprs
findVarNonInFunction _ _ = []

findVarFreeInApps :: Expr -> [Expr]
findVarFreeInApps (App name exprs) = [App name (concatMap findVars exprs)]
findVarFreeInApps (Infix _ expr1 expr2) = findVarFreeInApps expr1 ++ findVarFreeInApps expr2
findVarFreeInApps (If expr1 expr2 expr3) = findVarFreeInApps expr1 ++ findVarFreeInApps expr2 ++ findVarFreeInApps expr3
findVarFreeInApps (Let x expr1 expr2) = (findVarNonInFunction x  expr1) ++ (findVarNonInFunction x expr2)
findVarFreeInApps _ = []

--chequear que las expresiones que ocurren en el programa tienen el tipo correcto
checkTypes :: Program -> [Error]
checkTypes program = 
  let defs = collectionNameFunctionDeclarations program
      functionTypes = map (\(FunDef (namef, types) _ _) -> (namef, types)) defs
      functionExpr = map (\(FunDef (namef,_) _ expr) -> (namef, expr)) defs
      functionDeclaration = map (\(FunDef (namef,_) params _) -> (namef, params)) defs
      typesForExpr = map (\(name, expr) -> typeForExpr expr functionTypes) functionExpr
      exprsType = zip typesForExpr functionExpr
      main = mainFuntion program
      mainType = typeForExpr main functionTypes
      mainExp =  exprsType ++ [(mainType, ("main", main))]
      e = map (\(ty, (name, expr)) -> checkTypesForExpr name ty expr functionTypes functionDeclaration functionExpr ) mainExp

      --errorTypes = map (\(name, expr) -> typeForExpr expr) function
      -- despues para main  lo mismo que antes
  in trace ("Busco todo" ++ show  functionExpr) concat e


checkTypesForExpr :: Name -> String -> Expr -> [(Name, Sig)] -> [(Name, [Name])] -> [(Name, Expr)] -> [Error]
checkTypesForExpr name ty expr functionTypes functionDeclaration functionExpr = 
  let errors = case expr of
        (Var name) -> []
        (IntLit _) -> if ty == "Int" then [] else [Expected TyBool TyInt]
        (BoolLit _) -> if ty == "Bool" then [] else [Expected TyInt TyBool]
        (Infix Add expr1 expr2) -> errorInts name ty expr1 expr2 functionTypes functionDeclaration functionExpr
        (Infix Sub expr1 expr2) -> errorInts name ty expr1 expr2 functionTypes functionDeclaration functionExpr
        (Infix Mult expr1 expr2) -> errorInts name ty expr1 expr2 functionTypes functionDeclaration functionExpr
        (Infix Div expr1 expr2) -> errorInts name ty expr1 expr2 functionTypes functionDeclaration functionExpr
        (Infix Eq expr1 expr2) -> errorBool name ty expr1 expr2 functionTypes functionDeclaration functionExpr
        (Infix NEq expr1 expr2) -> errorBool name ty expr1 expr2 functionTypes functionDeclaration functionExpr
        (Infix GTh expr1 expr2) -> errorBool name ty expr1 expr2 functionTypes functionDeclaration functionExpr
        (Infix LTh expr1 expr2) -> errorBool name ty expr1 expr2 functionTypes functionDeclaration functionExpr
        (Infix GEq expr1 expr2) -> errorBool name ty expr1 expr2 functionTypes functionDeclaration functionExpr
        (Infix LEq expr1 expr2) -> errorBool name ty expr1 expr2 functionTypes functionDeclaration functionExpr
        (If expr1 expr2 expr3) ->  case finTypef name functionTypes of
                                      "Int" -> if (typeForExpr expr2 functionTypes) == "Int" then
                                                 [] ++ checkTypesForExpr name (typeForExpr expr1 functionTypes) expr1 functionTypes functionDeclaration functionExpr
                                                else [Expected TyInt TyBool] ++ checkTypesForExpr name (typeForExpr expr1 functionTypes) expr1 functionTypes functionDeclaration functionExpr
                                      "Bool" -> if (typeForExpr expr2 functionTypes) == "Bool" then
                                                  [] ++ checkTypesForExpr name (typeForExpr expr1 functionTypes) expr1 functionTypes functionDeclaration functionExpr
                                                else [Expected TyBool TyInt] ++ checkTypesForExpr name (typeForExpr expr1 functionTypes) expr1 functionTypes functionDeclaration functionExpr
                                      ++
                                   case (typeForExpr expr1 functionTypes) of
                                        "Bool" -> case ty of
                                                    "Int" -> --(checkTypesForExpr name ty expr1 functionTypes functionDeclaration ) ++ 
                                                              if (typeForExpr expr2 functionTypes ) == "Int" then
                                                                if (typeForExpr expr3 functionTypes) == "Int" then
                                                                  [] ++ list23
                                                                else [Expected TyInt TyBool] ++ list23
                                                              else [Expected TyInt TyBool] ++ list23
                                                    "Bool" -> if (typeForExpr expr2 functionTypes) == "Int" then
                                                                if (typeForExpr expr3 functionTypes) == "Int" then
                                                                  [] ++ list23
                                                                else [Expected TyBool TyInt] ++ list23
                                                              else [Expected TyBool TyInt] ++ list23
                                                    x -> if (finTypevar name x functionTypes functionDeclaration) == finTypef name functionTypes then
                                                            [] ++ list23
                                                          else
                                                            let esperado =  if (finTypef name functionTypes) == "Int" then TyInt else TyBool
                                                                actual = if (finTypevar name x functionTypes functionDeclaration) == "Int" then TyInt else TyBool
                                                            in [Expected esperado actual] ++ (checkTypesForExpr name ty expr3 functionTypes functionDeclaration functionExpr ) 
                                        "Int" -> [Expected TyBool TyInt] ++ list23
                                        _ ->  (findTypeVarBool name (typeForExpr expr1 functionTypes) functionTypes functionDeclaration functionExpr) ++ list23
                                    where list23 = checkTypesForExpr name (typeForExpr expr2 functionTypes) expr2 functionTypes functionDeclaration functionExpr ++ checkTypesForExpr name (typeForExpr expr2 functionTypes) expr3 functionTypes functionDeclaration functionExpr
                                          lista12 = checkTypesForExpr name (typeForExpr expr1 functionTypes) expr1 functionTypes functionDeclaration functionExpr ++ checkTypesForExpr name (typeForExpr expr2 functionTypes) expr2 functionTypes functionDeclaration functionExpr 
        (Let x expr1 expr2) -> trace ("tipo de w" ++ show x) $case ty of 
                                  "Int" -> if (typeForExpr expr1 functionTypes) == "Int" then
                                              checkTypesForExpr name (typeForExpr expr2 functionTypes) expr2 functionTypes functionDeclaration functionExpr
                                            else [Expected TyInt TyBool] ++ checkTypesForExpr name ty expr2 functionTypes functionDeclaration functionExpr
                                  "Bool" -> if (typeForExpr expr1 functionTypes)== "Bool" then
                                              checkTypesForExpr name (typeForExpr expr2 functionTypes) expr2 functionTypes functionDeclaration functionExpr
                                            else [Expected TyBool TyInt] ++ checkTypesForExpr name ty expr2 functionTypes functionDeclaration functionExpr
                                  _ -> (findTypeVarBool name (typeForExpr expr1 functionTypes) functionTypes functionDeclaration functionExpr) ++ checkTypesForExpr name ty expr2 functionTypes functionDeclaration functionExpr
        (App name' exprs) -> let typesF = filter (\(name'', Sig _ _) -> name'' == name') functionTypes
                                 funcionforIndef = filter (\(name'', _) -> name'' == name') functionDeclaration
                                 nameF = concatMap (\(name'', _) -> name'') funcionforIndef
                                 temp = concatMap (\(_ ,(Sig types _)) -> types) typesF
                                 callF = concatMap (\(App _ exprs) -> exprs) [expr]
                                 tempType = map (\(_ ,(Sig _ types)) -> types) typesF
                                 auxTypes = head (map (\x -> if x == TyInt then "Int" else "Bool") tempType)
                                 errors = if length temp == length callF then
                                            let temp' = map (\x -> if x == TyInt then "Int" else "Bool") temp
                                            in if auxTypes == ty then
                                              [] ++ concatMap (\(expr, ty) -> checkTypesForExpr nameF ty expr functionTypes functionDeclaration functionExpr) (zip callF temp')
                                            else [Expected (if ty == "Int" then TyInt else TyBool) (if auxTypes == "Int" then TyInt else TyBool) ] ++ concatMap (\(expr, ty) -> checkTypesForExpr nameF ty expr functionTypes functionDeclaration functionExpr) (zip callF temp')
                                     else
                                        let temp' = map (\x -> if x == TyInt then "Int" else "Bool") temp
                                            in if auxTypes == ty then
                                              [ArgNumApp nameF (length temp) (length callF)]  ++ concatMap (\(expr, ty) -> checkTypesForExpr nameF ty expr functionTypes functionDeclaration functionExpr) (zip callF temp')
                                            else [Expected (if ty == "Int" then TyInt else TyBool) (if auxTypes == "Int" then TyInt else TyBool) ] ++ [ArgNumApp nameF (length temp) (length callF)]  ++ concatMap (\(expr, ty) -> checkTypesForExpr nameF ty expr functionTypes functionDeclaration functionExpr) (zip callF temp')
                              
                             in errors 
  in errors 

findTypeVarBool :: Name -> String -> [(Name, Sig)] -> [(Name, [Name])] -> [(Name, Expr)] -> [Error]
findTypeVarBool name ty functionTypes functionDeclaration functionExpr = 
  let typesF = filter (\(name', Sig _ _) -> name' == name) functionTypes
      funcionforIndef = filter (\(name', _) -> name' == name) functionDeclaration
      index = elemIndex ty (snd (head funcionforIndef))
      --Si el incide es vacio busco en lo letç
      typeX = if index == Nothing then
            let expresionF = filter (\(name',_) -> name' == name) functionExpr
                expr = map (\(_ ,expr) -> expr) expresionF
                z = map (\expr -> findTypeLet name expr) expr
                typeX = head z
            in trace ("busco el let :" ++ show (typeX )) "" -- $ if typeX == "Int" then [Expected TyBool TyInt] else []
          else  let temp = map (\(_ ,(Sig types _)) -> types) typesF
                    aux = head temp !! fromJust index
                in if aux == TyInt then "Int" else "Bool"

      error = if typeX == "Int" then [Expected TyBool TyInt] else []
  in trace ("Me llama :" ++ show name ++ show typeX) []--error

findTypeLet :: Name -> Expr -> String
findTypeLet name (Let x _ expr) = case x of
                              (name, TyInt) -> "Int"
                              (name, TyBool) -> "Bool"
                              _ -> "" -- Otros casos
findTypeLet name (Infix _ expr1 expr2) = findTypeLet name expr1
findTypeLet name (If expr1 expr2 expr3) = findTypeLet name expr1
findTypeLet name (App name' exprs) = ""
findTypeLet name _ = "" 


-- finTypeLet (Let x _ expr) = case x of
--                               (name, TyInt) -> "Int"
--                               (name, TyBool) -> "Bool"
--                               _ -> "" -- Otros casos
-- finTypeLet (Infix _ expr1 expr2) = finTypeLet expr1
-- finTypeLet (If expr1 expr2 expr3) = finTypeLet expr1
-- finTypeLet (App name exprs) = ""
-- finTypeLet _ = ""




findTypeVarInt :: Name -> String -> [(Name, Sig)] -> [(Name, [Name])] -> [(Name, Expr)] -> [Error]
findTypeVarInt name ty functionTypes functionDeclaration functionExpr = 
  let typesF = filter (\(name', Sig _ _) -> name' == name) functionTypes
      funcionforIndef = filter (\(name', _) -> name' == name) functionDeclaration
      index = elemIndex ty (snd (head funcionforIndef))
      temp = map (\(_ ,(Sig types _)) -> types) typesF
      typeX = head temp !! fromJust index
      error = if typeX == TyInt then [] else [Expected TyInt TyBool]
  in error

finTypevar :: Name -> String -> [(Name, Sig)] -> [(Name, [Name])] -> String
finTypevar name x functionTypes functionDeclaration  = 
  let typesF = filter (\(name', Sig _ _) -> name' == name) functionTypes
      funcionforIndef = filter (\(name', _) -> name' == name) functionDeclaration
      index = elemIndex x (snd (head funcionforIndef))
      temp = map (\(_ ,(Sig types _)) -> types) typesF
      typeX = head temp !! fromJust index
      typeFinal = if typeX == TyInt then "Int" else "Bool"
  in typeFinal

typeForExpr ::  Expr -> [(Name, Sig)] ->String
typeForExpr (IntLit _) functionTypes = "Int"
typeForExpr (BoolLit _) functionTypes = "Bool"
typeForExpr (Var x) functionTypes = x
typeForExpr (Infix Add _ _) functionTypes = "Int"
typeForExpr (Infix Sub _ _) functionTypes = "Int"
typeForExpr (Infix Mult _ _) functionTypes = "Int"
typeForExpr (Infix Div _ _) functionTypes = "Int"
typeForExpr (Infix Eq _ _) functionTypes =  "Bool"
typeForExpr (Infix NEq _ _) functionTypes =  "Bool"
typeForExpr (Infix GTh _ _) functionTypes =  "Bool"
typeForExpr (Infix LTh _ _) functionTypes =  "Bool"
typeForExpr (Infix GEq _ _) functionTypes =  "Bool"
typeForExpr (Infix LEq _ _) functionTypes =  "Bool"
typeForExpr (If _ expr _) functionTypes = typeForExpr expr functionTypes
typeForExpr (Let _ _ expr) functionTypes = typeForExpr expr functionTypes
typeForExpr (App name _) functionTypes = finTypef name functionTypes

finTypef :: Name -> [(Name, Sig)] -> String
finTypef name functionTypes = 
  let typesF = filter (\(name'', Sig _ _) -> name'' == name) functionTypes
      tempType = map (\(_ ,(Sig _ types)) -> types) typesF
      auxTypes = head (map (\x -> if x == TyInt then "Int" else "Bool") tempType)
  in auxTypes

errorInts :: Name -> String -> Expr -> Expr -> [(Name, Sig)] -> [(Name, [Name])] -> [(Name, Expr)] -> [Error]
errorInts name ty expr1 expr2  functionTypes functionDeclaration functionExpr =
  case ty of   
    "Int" -> checkTypesForExpr name ty expr1 functionTypes functionDeclaration functionExpr ++ checkTypesForExpr name ty expr2 functionTypes functionDeclaration functionExpr
    "Bool" -> [Expected TyInt TyBool] ++ checkTypesForExpr name ty expr1 functionTypes functionDeclaration functionExpr ++ checkTypesForExpr name ty expr2 functionTypes functionDeclaration functionExpr 
    _ ->  (findTypeVarInt name (typeForExpr expr1 functionTypes) functionTypes functionDeclaration functionExpr) ++ checkTypesForExpr name ty expr1 functionTypes functionDeclaration functionExpr ++ checkTypesForExpr name ty expr2 functionTypes functionDeclaration functionExpr

errorBool :: Name -> String -> Expr -> Expr -> [(Name, Sig)] -> [(Name, [Name])] -> [(Name, Expr)] -> [Error]
errorBool name ty expr1 expr2  functionTypes functionDeclaration functionExpr = 
  case ty of
    "Bool" -> checkTypesForExpr name ty expr1 functionTypes functionDeclaration functionExpr ++ checkTypesForExpr name ty expr2 functionTypes functionDeclaration functionExpr 
    "Int" -> [Expected TyInt TyBool] ++ checkTypesForExpr name ty expr1 functionTypes functionDeclaration functionExpr ++ checkTypesForExpr name ty expr2 functionTypes functionDeclaration functionExpr  
    _ -> (findTypeVarBool name (typeForExpr expr1 functionTypes) functionTypes functionDeclaration functionExpr) ++ checkTypesForExpr name ty expr1 functionTypes functionDeclaration functionExpr ++ checkTypesForExpr name ty expr2 functionTypes functionDeclaration functionExpr