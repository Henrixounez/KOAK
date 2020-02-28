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
    basicBlocks = [body]
  }):(consumeStmt xs)
  where
    defName = (KoakPackrat.name proto)
    defParams = consumeDefArgs (KoakPackrat.args proto)
    body = BasicBlock
      (Name "entry")
      [ Name "result" :=
          Add False  -- no signed wrap
              False  -- no unsigned wrap
              (LocalReference int (Name "x"))
              (LocalReference int (Name "x"))
              []]
      (Do $ Ret (Just (LocalReference int (Name "result"))) [])
consumeStmt (x:xs) = consumeStmt xs

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
