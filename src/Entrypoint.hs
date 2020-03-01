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

generateName :: String -> String -> String
generateName srcName ext = name ++ ext
    where
        (name, _) = break (== '.') srcName

toObj :: AST.Module -> String -> IO ()
toObj mod input = T.withHostTargetMachine Reloc.PIC CodeModel.Default CodeGenOpt.Default $ \tgt ->
  withContext $ \ctx ->
  withModuleFromAST ctx mod $ \m -> do
    writeObjectToFile tgt (File outputName) m
    where
        outputName = (File (generateName input ".so"))

toIR :: AST.Module -> String -> IO ()
toIR mod input = withContext $ \ctx ->
    withModuleFromAST context mod $ \m -> do
        writeLLVMAssemblyToFile outputname m
        where
            outputName = (File (generateName input ".ll"))

initModule :: AST.Module
initModule = emptyModule "Koak Compiler"
    
toIR :: Stmt :: -> IO ()
toIR ast = codegen initModule (trace (show $ cleanPackrat koakAST) (cleanPackrat koakAST))

toSOFile :: Stmt :: IO ()
toSOFile ast = return ()

toJIT :: Stmt :: IO ()
toJIT ast = return ()

toExe :: Stmt :: IO ()
toExe ast = return ()

getHandler :: String -> (Stmt :: -> IO ())
getHandler flag
    | flag == "to_so" = toSOFile
    | flag == "to_jit" = toJIT
    | flag == "to_ir" = toIR
    | otherwise = toExe

entrypoint :: Stmt -> (String, String) -> IO ()
entrypoint ast (name, flag) = handler ast
    where handler = getHandler flag
    