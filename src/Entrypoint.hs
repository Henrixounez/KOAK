module Entrypoint where

import qualified LLVM.AST as AST
import Debug.Trace

import Codegen
import KoakPackrat
import PackratCleaner

import qualified LLVM.Target as T
import qualified LLVM.Relocation as Reloc
import qualified LLVM.CodeModel as CodeModel
import qualified LLVM.CodeGenOpt as CodeGenOpt
import LLVM.Context
import LLVM.Module

generateName :: String -> String -> String
generateName srcName ext = name ++ ext
    where
        (name, _) = break (== '.') srcName

toObj :: AST.Module -> String -> IO ()
toObj mod input = T.withHostTargetMachine $ \tgt ->
  withContext $ \ctx ->
  withModuleFromAST ctx mod $ \m -> do
    writeObjectToFile tgt (File (generateName input ".so")) m

toIR :: AST.Module -> String -> IO ()
toIR mod input = withContext $ \ctx ->
  withModuleFromAST ctx mod $ \m -> do
    writeLLVMAssemblyToFile (File (generateName input ".ll")) m

initModule :: AST.Module
initModule = emptyModule "Koak Compiler"

toAST :: Stmt -> AST.Module
toAST stmt = codegen initModule (cleanPackrat stmt)

toJIT :: AST.Module -> String -> IO ()
toJIT ast name = Prelude.return ()

toExe :: AST.Module -> String -> IO ()
toExe ast name = Prelude.return ()

getHandler :: String -> (AST.Module -> String -> IO ())
getHandler flag
    | flag == "to_so" = toObj
    | flag == "to_jit" = toJIT
    | flag == "to_ir" = toIR
    | otherwise = toExe

entrypoint :: Stmt -> (String, String) -> IO ()
entrypoint ast (name, flag) = handler (toAST ast) name
    where handler = getHandler flag
    