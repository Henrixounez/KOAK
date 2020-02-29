{-# LANGUAGE OverloadedStrings #-}

module KoakAST where

import KoakPackrat

import LLVM.AST
import qualified LLVM.AST as AST
import LLVM.AST.Global
import LLVM.Context
import LLVM.Module

import Control.Monad.Except
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Short as SB
import Data.ByteString.Short.Internal

toShortString :: String -> Data.ByteString.Short.Internal.ShortByteString
toShortString s = SB.toShort (UTF8.fromString s)

toType :: KoakPackrat.Type -> AST.Type
toType kType = case kType of
    Int -> int
    Double -> double
    -- TODO void

int :: LLVM.AST.Type
int = IntegerType 32

double :: LLVM.AST.Type
double = FloatingPointType DoubleFP

consumeBinop :: Int -> Primary -> BinOp -> UnaryExpr -> (Named Instruction, Primary)
consumeBinop id (Identifier var) (Addition) (UnaryExprUnary (Postfix (Identifier var2) _)) = (instructions, Identifier var2)
  where
    instructions = Name (toShortString $ show id) :=
              Add False
                  False
                  (LocalReference int (Name (toShortString var)))
                  (LocalReference int (Name (toShortString var2)))
                  []

consumeBinops :: UnaryPostfix -> [BinaryOperation] -> [Named Instruction]
consumeBinops (Unary op un) binops = undefined -- TODO
consumeBinops (Postfix primary (Just callExpr)) binops = undefined --TODO
consumeBinops (Postfix primary Nothing) ((BinaryOperation binop unexpr):xs) = undefined
-- consumeBinops (Postfix primary Nothing) ((BinaryOperation binop unexpr):xs) = (fst binopRes):(consumeBinops (Postfix (snd binopRes) Nothing) xs)
--   where
--     binopRes = (consumeBinop primary binop unexpr)
consumeBinops _ [] = []

consumeExpression :: Expression -> [Named Instruction]
consumeExpression (Expression un binops) = consumeBinops un binops

consumeExpressionArray :: [Expression] -> [Named Instruction]
consumeExpressionArray [] = []
consumeExpressionArray (x:xs) = (consumeExpression x) ++ (consumeExpressionArray xs)

consumeExpressions :: Expressions -> [BasicBlock]
consumeExpressions (ForExpr init condition increment expressions) = undefined -- TODO
-- consumeExpressions (IfExpr conditionIf expressions elseExpressions) = [BasicBlock
--                                     (Name "entry")
--                                     ([])
--                                     (Do $ CondBr (Just 
--                                       (LocalReference int (Name "result")))
--                                  
--
--                                       []
                                      
--                                     )] -- TODO
consumeExpressions (WhileExpr conditionWhile expressions) = undefined -- TODO
consumeExpressions (Expr exprs) = [BasicBlock
                                    (Name "entry")
                                    (consumeExpressionArray exprs)
                                    (Do $ Ret (Just (LocalReference int (Name "result"))) [])]

consumeDefArgs :: [(Primary, KoakPackrat.Type)] -> [Parameter]
consumeDefArgs [] = []
consumeDefArgs ((Identifier name, argType):xs) = (Parameter argTypeLLVM (Name (toShortString name)) []):(consumeDefArgs xs)
  where
    argTypeLLVM = toType argType

consumeStmt :: [Kdefs] -> [Definition]
consumeStmt [] = []
consumeStmt ((Defs proto defExpr):xs) = (GlobalDefinition functionDefaults
  {
    LLVM.AST.Global.name = Name (toShortString defName),
    parameters = (defParams, False),
    returnType = toType (KoakPackrat.return proto),
    basicBlocks = body
  }):(consumeStmt xs)
  where
    defName = (KoakPackrat.name proto)
    defParams = consumeDefArgs (KoakPackrat.args proto)
    body = consumeExpressions defExpr
    -- body = BasicBlock
    --   (Name "entry")
    --   [ Name "result" :=
    --       Add False  -- no signed wrap
    --           False  -- no unsigned wrap
    --           (LocalReference int (Name "x"))
    --           (LocalReference int (Name "y"))
    --           []]
    --   (Do $ Ret (Just (LocalReference int (Name "result"))) [])
consumeStmt ((KExpressions exprs):xs) = consumeStmt xs -- TODO
consumeStmt ((Extern name args ret):xs) = consumeStmt xs -- TODO

-- defAdd :: Definition
-- defAdd = GlobalDefinition functionDefaults
--   { LLVM.AST.Global.name = Name "add"
--   , parameters =
--       ( [ Parameter int (Name "a") []
--         , Parameter int (Name "b") [] ]
--       , False )
--   , returnType = int
--   , basicBlocks = [body]
--   }
--   where
--     body = BasicBlock
--         (Name "entry")
--         [ Name "result" :=
--             Add False  -- no signed wrap
--                 False  -- no unsigned wrap
--                 (LocalReference int (Name "a"))
--                 (LocalReference int (Name "b"))
--                 []]
--         (Do $ Ret (Just (LocalReference int (Name "result"))) [])

-- genModule :: Stmt ->  AST.Module
-- genModule stmt = defaultModule
--   {
--     moduleName = "basic",
--     moduleDefinitions = [defAdd]
--   }

genModule :: Stmt ->  AST.Module
genModule stmt = defaultModule
  {
    moduleName = "basic",
    moduleDefinitions = consumeStmt (kdefs stmt)
  }

toLLVM :: AST.Module -> IO ()
toLLVM mod = withContext $ \ctx -> do
  llvm <- withModuleFromAST ctx mod moduleLLVMAssembly
  BS.putStrLn llvm
