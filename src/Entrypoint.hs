module Entrypoint where

import qualified LLVM.AST as AST
import Debug.Trace

import Codegen
import KoakPackrat
import PackratCleaner

import Foreign.Ptr ( FunPtr, castFunPtr )

import qualified LLVM.Target as T
import qualified LLVM.Relocation as Reloc
import qualified LLVM.CodeModel as CodeModel
import qualified LLVM.CodeGenOpt as CodeGenOpt
import qualified LLVM.ExecutionEngine as EE
import LLVM.Context
import LLVM.Module
import LLVM.PassManager

import qualified Data.ByteString.UTF8 as UTF8

foreign import ccall "dynamic" haskFun :: FunPtr (IO Double) -> (IO Double)

run :: FunPtr a -> IO Double
run fn = haskFun (castFunPtr fn :: FunPtr (IO Double))

jit :: Context -> (EE.MCJIT -> IO a) -> IO a
jit c = EE.withMCJIT c optlevel model ptrelim fastins
  where
    optlevel = Just 0  -- optimization level
    model    = Nothing -- code model ( Default )
    ptrelim  = Nothing -- frame pointer elimination
    fastins  = Nothing -- fast instruction selection

passes :: PassSetSpec
passes = defaultCuratedPassSetSpec { optLevel = Just 3 }

generateName :: String -> String -> String
generateName srcName ext = name ++ ext
    where
        (name, _) = break (== '.') srcName

toObj :: AST.Module -> String -> IO ()
toObj mod input = T.withHostTargetMachine $ \tgt ->
  withContext $ \ctx ->
  withModuleFromAST ctx mod $ \m -> do
    writeObjectToFile tgt (File (generateName input ".so")) m

toS :: AST.Module -> String -> IO ()
toS mod input = T.withHostTargetMachine $ \tgt ->
  withContext $ \ctx ->
  withModuleFromAST ctx mod $ \m -> do
    writeTargetAssemblyToFile tgt (File (generateName input ".s")) m


toIR :: AST.Module -> String -> IO ()
toIR mod input = withContext $ \ctx ->
  withModuleFromAST ctx mod $ \m -> do
    writeLLVMAssemblyToFile (File (generateName input ".ll")) m

initModule :: AST.Module
initModule = emptyModule "Koak Compiler"

toAST :: Stmt -> AST.Module
toAST stmt = codegen initModule (cleanPackrat stmt)

toJIT :: AST.Module -> String -> IO ()
toJIT ast name = withContext $ \ctx ->
  jit ctx $ \execEngine -> withModuleFromAST ctx ast $ \m ->
    withPassManager passes $ \pm -> do
      optmod <- moduleAST m
      s <- moduleLLVMAssembly m
      EE.withModuleInEngine execEngine m $ \ee -> do
        mainfn <- EE.getFunction ee (AST.Name "main")
        case mainfn of
          Just fn -> do
            res <- run fn
            putStrLn $ show res
          Nothing -> Prelude.return ()
      Prelude.return ()

toBC :: AST.Module -> String -> IO ()
toBC mod input = withContext $ \ctx ->
  withModuleFromAST ctx mod $ \m -> do
    writeBitcodeToFile (File (generateName input ".bc")) m

getHandler :: String -> (AST.Module -> String -> IO ())
getHandler flag
    | flag == "to_so" = toObj
    | flag == "to_jit" = toJIT
    | flag == "to_ir" = toIR
    | flag == "to_bc" = toBC
    | flag == "to_s" = toS
    | otherwise = toJIT

entrypoint :: Stmt -> (String, String) -> IO ()
entrypoint ast (name, flag) = handler (toAST ast) name
    where handler = getHandler flag
    