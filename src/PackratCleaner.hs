module PackratCleaner where

import qualified KoakPackrat as KP

type Name = String

data Expr = 
    ExprFor (Name, Expr) Expr Expr [Expr]
  | ExprIf Expr [Expr] [Expr]
  | ExprWhile Expr [Expr]
  | ExprFloat Double
  | ExprBinaryOperation KP.BinOp Expr Expr
  | ExprUnaryOperation KP.UnOp Expr
  | ExprVar String
  | ExprCall Name [Expr]
  | ExprFunction Name [Expr] [Expr]
  | ExprExtern Name [Expr]
  deriving (Show)

getPrimary :: KP.Primary -> Expr
getPrimary (KP.Identifier s) = ExprVar s
getPrimary (KP.PrimaryLiteral (KP.LiteralInt i)) = ExprFloat (fromIntegral i :: Double)
getPrimary (KP.PrimaryLiteral (KP.LiteralFloat d)) = ExprFloat d
getPrimary (KP.PrimaryExpressions exprs) = getExpression exprs

getUnaryPostfix :: KP.UnaryPostfix -> Expr
getUnaryPostfix (KP.Unary op unary) = ExprUnaryOperation op (getUnaryPostfix unary)
getUnaryPostfix (KP.Postfix primary Nothing) = getPrimary primary
getUnaryPostfix (KP.Postfix (KP.Identifier name) (Just callExpr)) = ExprCall name (map getExpression callExpr) 

getUnaryExpr :: KP.UnaryExpr -> Expr
getUnaryExpr (KP.UnaryExprUnary unaryPostfix) = getUnaryPostfix unaryPostfix
getUnaryExpr (KP.UnaryExprExpression expr) = getExpression expr

getBinops :: KP.BinaryOperation -> Expr -> Expr
getBinops (KP.BinaryOperation binop rightExpr) leftExpr = ExprBinaryOperation binop leftExpr (getUnaryExpr rightExpr)

getBinopsArray :: Expr -> [KP.BinaryOperation] -> Expr
getBinopsArray leftExpr [] = leftExpr
getBinopsArray leftExpr (x:xs) = getBinopsArray (getBinops x leftExpr) xs

getExpression :: KP.Expression -> Expr
getExpression (KP.Expression exprUnary []) = getUnaryPostfix exprUnary
getExpression (KP.Expression exprUnary binops) = getBinopsArray (getUnaryPostfix exprUnary) binops

getExpressions :: KP.Expressions -> [Expr]
getExpressions (KP.ForExpr init condition increment exprs) = [ExprFor forInit cond incr expr]
  where
    forInit = (\ ((KP.Identifier s), expr) -> (s, getExpression expr)) init
    cond    = getExpression condition
    incr    = getExpression increment
    expr    = getExpressions exprs
getExpressions (KP.IfExpr condIf exprs elseExprs) = [ExprIf cond expr elseExpr]
  where
    cond = getExpression condIf
    expr = getExpressions exprs
    elseExpr = case elseExprs of
      (Just exprs) -> getExpressions exprs
      (Nothing) -> []
getExpressions (KP.WhileExpr condWhile exprs) = [ExprWhile cond expr]
  where
    cond = getExpression condWhile
    expr = getExpressions exprs
getExpressions (KP.Expr exprs) = map getExpression exprs

getDefs :: KP.Prototype -> KP.Expressions -> Expr
getDefs (KP.Prototype name args return) exprs = ExprFunction name fnArgs (getExpressions exprs)
  where
    fnArgs = map (\(KP.Identifier x, _) -> ExprVar x) args

getExtern :: String -> [(KP.Primary, KP.Type)] -> KP.Type -> Expr
getExtern name args return = ExprExtern name fnArgs
  where
    fnArgs = map (\(KP.Identifier x, _) -> ExprVar x) args

getKdefs :: [KP.Kdefs] -> [Expr]
getKdefs [] = []
getKdefs ((KP.Defs proto exprs):xs) = (getDefs proto exprs):(getKdefs xs)
getKdefs ((KP.Extern name args return):xs) = (getExtern name args return):(getKdefs xs)
getKdefs ((KP.KExpressions exprs):xs) = (getExpressions exprs) ++ (getKdefs xs)

filterGlobalExpr :: Expr -> Bool
filterGlobalExpr (ExprFunction _ _ _) = True
filterGlobalExpr (ExprExtern _ _) = True
filterGlobalExpr _ = False

cleanPackrat :: KP.Stmt -> [Expr]
cleanPackrat (KP.Stmt kdefs) = res
  where
    allExpr = getKdefs kdefs
    globalExpr = filter filterGlobalExpr allExpr
    mainExprs = filter (\a -> not (filterGlobalExpr a)) allExpr
    mainExpr = case length mainExprs of
      0 -> []
      _ -> [ExprFunction "main" [] mainExprs]
    res = globalExpr ++ mainExpr