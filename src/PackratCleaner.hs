module PackratCleaner where

import qualified KoakPackrat as KP

type Name = String

data Expr = 
    ExprFor (Name, Expr) Expr Expr Expr
  | ExprIf Expr Expr (Maybe Expr)
  | ExprWhile Expr Expr
  | ExprFloat Double
  | ExprBinaryOperation KP.BinOp Expr Expr
  | ExprUnaryOperation KP.UnOp Expr
  | ExprBinaryDef Name [Expr] Expr
  | ExprUnaryDef Name [Expr] Expr
  | ExprVar String
  | ExprCall Name [Expr]
  | ExprFunction Name [Expr] [Expr]
  | ExprExtern Name [Expr]
  deriving (Show)

{- Special Operators -}

notExpr :: Expr -> Expr
notExpr a = ExprIf (ExprBinaryOperation "!=" (ExprFloat 0) a) (ExprFloat 0) (Just $ ExprFloat 1)

andExpr :: Expr -> Expr -> Expr
andExpr leftExpr rightExpr = ExprIf (notExpr leftExpr) (ExprFloat 0) (Just (notExpr (notExpr rightExpr)))

orExpr :: Expr -> Expr -> Expr
orExpr leftExpr rightExpr = ExprIf leftExpr (ExprFloat 1) (Just (ExprIf rightExpr (ExprFloat 1) (Just (ExprFloat 0))))

andFn :: Expr
andFn = ExprBinaryDef "&" [(ExprVar "LHS"), (ExprVar "RHS")] (andExpr (ExprVar "LHS") (ExprVar "RHS"))

orFn :: Expr
orFn = ExprBinaryDef "|" [(ExprVar "LHS"), (ExprVar "RHS")] (orExpr (ExprVar "LHS") (ExprVar "RHS"))

utilsFn :: [Expr]
utilsFn = [andFn, orFn]

{- -}

getPrimary :: KP.Primary -> Expr
getPrimary (KP.Identifier s) = ExprVar s
getPrimary (KP.PrimaryLiteral (KP.LiteralInt i)) = ExprFloat (fromIntegral i :: Double)
getPrimary (KP.PrimaryLiteral (KP.LiteralFloat d)) = ExprFloat d
getPrimary (KP.PrimaryExpressions exprs) = getExpression exprs

getUnaryPostfix :: KP.UnaryPostfix -> Expr
getUnaryPostfix (KP.Unary "!" unary) = notExpr (getUnaryPostfix unary)
getUnaryPostfix (KP.Unary op unary) = ExprUnaryOperation op (getUnaryPostfix unary)
getUnaryPostfix (KP.Postfix primary Nothing) = getPrimary primary
getUnaryPostfix (KP.Postfix (KP.Identifier name) (Just callExpr)) = ExprCall name (map getExpression callExpr) 

getUnaryExpr :: KP.UnaryExpr -> Expr
getUnaryExpr (KP.UnaryExprUnary unaryPostfix) = getUnaryPostfix unaryPostfix
getUnaryExpr (KP.UnaryExprExpression expr) = getExpression expr

getBinops :: KP.BinaryOperation -> Expr -> Expr
-- getBinops (KP.BinaryOperation "&" rightExpr) leftExpr = andExpr leftExpr (getUnaryExpr rightExpr)
-- getBinops (KP.BinaryOperation "|" rightExpr) leftExpr = andExpr leftExpr (getUnaryExpr rightExpr)
getBinops (KP.BinaryOperation binop rightExpr) leftExpr = ExprBinaryOperation binop leftExpr (getUnaryExpr rightExpr)

getBinopsArray :: Expr -> [KP.BinaryOperation] -> Expr
getBinopsArray leftExpr [] = leftExpr
getBinopsArray leftExpr (x:xs) = getBinopsArray (getBinops x leftExpr) xs

getExpression :: KP.Expression -> Expr
getExpression (KP.Expression exprUnary []) = getUnaryPostfix exprUnary
getExpression (KP.Expression exprUnary binops) = getBinopsArray (getUnaryPostfix exprUnary) binops

getExpressions :: KP.Expressions -> Expr
getExpressions (KP.ForExpr init condition increment exprs) = ExprFor forInit cond incr expr
  where
    forInit = (\ ((KP.Identifier s), expr) -> (s, getExpression expr)) init
    cond    = getExpression condition
    incr    = getExpression increment
    expr    = getExpressions exprs
getExpressions (KP.IfExpr condIf exprs elseExprs) = ExprIf cond expr elseExpr
  where
    cond = getExpression condIf
    expr = getExpressions exprs
    elseExpr = case elseExprs of
      (Just exprs) -> (Just $ getExpressions exprs)
      (Nothing) -> Nothing
getExpressions (KP.WhileExpr condWhile exprs) = ExprWhile cond expr
  where
    cond = getExpression condWhile
    expr = getExpressions exprs
getExpressions (KP.Expr exprs) = getExpression exprs
getExpressions (KP.ExprConcat exprA exprB) = ExprBinaryOperation ":" (getExpressions exprA) (getExpressions exprB)

getDefs :: KP.Prototype -> KP.Expressions -> Expr
getDefs (KP.Prototype KP.Function _ name args return) exprs = ExprFunction name fnArgs [(getExpressions exprs)]
  where
    fnArgs = map (\(KP.Identifier x, _) -> ExprVar x) args
getDefs (KP.Prototype KP.UnaryFn prec name args return) exprs = ExprUnaryDef name fnArgs (getExpressions exprs)
  where
    fnArgs = map (\(KP.Identifier x, _) -> ExprVar x) args
getDefs (KP.Prototype KP.BinaryFn prec name args return) exprs = ExprBinaryDef name fnArgs (getExpressions exprs)
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
getKdefs ((KP.KExpressions exprs):xs) = (getExpressions exprs):(getKdefs xs)

filterGlobalExpr :: Expr -> Bool
filterGlobalExpr (ExprFunction _ _ _) = True
filterGlobalExpr (ExprExtern _ _) = True
filterGlobalExpr (ExprBinaryDef _ _ _) = True
filterGlobalExpr (ExprUnaryDef _ _ _) = True
filterGlobalExpr _ = False

cleanPackrat :: KP.Stmt -> [Expr]
cleanPackrat (KP.Stmt kdefs) = res
  where
    allExpr = getKdefs kdefs ++ utilsFn
    globalExpr = filter filterGlobalExpr allExpr
    mainExprs = filter (\a -> not (filterGlobalExpr a)) allExpr
    mainExpr = case length mainExprs of
      0 -> []
      _ -> [ExprFunction "main" [] mainExprs]
    res = globalExpr ++ mainExpr