module Codegen where

import qualified KoakPackrat as KP
import qualified PackratCleaner as PC

import Debug.Trace
import Data.Word
import Data.String
import Data.List
import Data.Function
import qualified Data.Map as Map

import Control.Monad.State
import Control.Applicative

import LLVM.AST
import LLVM.AST.Global
import qualified LLVM.AST as AST

import qualified LLVM.AST.Linkage as L
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Attribute as A
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.FloatingPointPredicate as FP
import qualified LLVM.AST.Instruction as I
import qualified LLVM.AST.Float as F
import LLVM.AST.AddrSpace
import LLVM.Context
import LLVM.Module

import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Short as SB
import Data.ByteString.Short.Internal

toShortString :: String -> Data.ByteString.Short.Internal.ShortByteString
toShortString s = SB.toShort (UTF8.fromString s)

-------------------------------------------------------------------------------
-- Module Level
-------------------------------------------------------------------------------

newtype LLVM a = LLVM (State AST.Module a)
  deriving (Functor, Applicative, Monad, MonadState AST.Module )

runLLVM :: AST.Module -> LLVM a -> AST.Module
runLLVM mod (LLVM m) = execState m mod

emptyModule :: String -> AST.Module
emptyModule label = defaultModule { moduleName = (toShortString label) }

addDefn :: Definition -> LLVM ()
addDefn d = do
  defs <- gets moduleDefinitions
  modify $ \s -> s { moduleDefinitions = defs ++ [d] }

define ::  Type -> String -> [(Type, Name)] -> [BasicBlock] -> LLVM ()
define retty label argtys body = addDefn $
  GlobalDefinition $ functionDefaults {
    name        = Name (toShortString label)
  , parameters  = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
  , returnType  = retty
  , basicBlocks = body
  }

external ::  Type -> String -> [(Type, Name)] -> LLVM ()
external retty label argtys = addDefn $
  GlobalDefinition $ functionDefaults {
    name        = Name (toShortString label)
  , linkage     = L.External
  , parameters  = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
  , returnType  = retty
  , basicBlocks = []
  }

---------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

int :: LLVM.AST.Type
int = IntegerType 32

double :: LLVM.AST.Type
double = FloatingPointType DoubleFP

-------------------------------------------------------------------------------
-- Names
-------------------------------------------------------------------------------

type Names = Map.Map String Int

uniqueName :: String -> Names -> (String, Names)
uniqueName nm ns =
  case Map.lookup nm ns of
    Nothing -> (nm,  Map.insert nm 1 ns)
    Just ix -> (nm ++ show ix, Map.insert nm (ix+1) ns)

-------------------------------------------------------------------------------
-- Codegen State
-------------------------------------------------------------------------------

type SymbolTable = [(String, Operand)]

data CodegenState
  = CodegenState {
    currentBlock :: Name                     -- Name of the active block to append to
  , blocks       :: Map.Map Name BlockState  -- Blocks for function
  , symtab       :: SymbolTable              -- Function scope symbol table
  , blockCount   :: Int                      -- Count of basic blocks
  , count        :: Word                     -- Count of unnamed instructions
  , names        :: Names                    -- Name Supply
  } deriving Show

data BlockState
  = BlockState {
    idx   :: Int                            -- Block index
  , stack :: [Named Instruction]            -- Stack of instructions
  , term  :: Maybe (Named Terminator)       -- Block terminator
  } deriving Show

-------------------------------------------------------------------------------
-- Codegen Operations
-------------------------------------------------------------------------------

newtype Codegen a = Codegen { runCodegen :: State CodegenState a }
  deriving (Functor, Applicative, Monad, MonadState CodegenState )

sortBlocks :: [(Name, BlockState)] -> [(Name, BlockState)]
sortBlocks = sortBy (compare `on` (idx . snd))

createBlocks :: CodegenState -> [BasicBlock]
createBlocks m = map makeBlock $ sortBlocks $ Map.toList (blocks m)

makeBlock :: (Name, BlockState) -> BasicBlock
makeBlock (l, (BlockState _ s t)) = BasicBlock l (reverse s) (maketerm t)
  where
    maketerm (Just x) = x
    maketerm Nothing = error $ "Block has no terminator: " ++ (show l)

entryBlockName :: String
entryBlockName = "entry"

emptyBlock :: Int -> BlockState
emptyBlock i = BlockState i [] Nothing

emptyCodegen :: CodegenState
emptyCodegen = CodegenState (Name (toShortString entryBlockName)) Map.empty [] 1 0 Map.empty

execCodegen :: Codegen a -> CodegenState
execCodegen m = execState (runCodegen m) emptyCodegen

fresh :: Codegen Word
fresh = do
  i <- gets count
  modify $ \s -> s { count = 1 + i }
  return $ i + 1

instr :: Instruction -> Codegen (Operand)
instr ins = do
  n <- fresh
  let ref = (UnName n)
  blk <- current
  let i = stack blk
  modifyBlock (blk { stack = (ref := ins) : i } )
  return $ local ref

instrNoSave :: Instruction -> Codegen (Operand)
instrNoSave ins = do
  n <- fresh
  let ref = (UnName n)
  blk <- current
  let i = stack blk
  modifyBlock (blk { stack = (Do ins) : i})
  return $ local ref

terminator :: Named Terminator -> Codegen (Named Terminator)
terminator trm = do
  blk <- current
  modifyBlock (blk { term = Just trm })
  return trm

-------------------------------------------------------------------------------
-- Block Stack
-------------------------------------------------------------------------------

entry :: Codegen Name
entry = gets currentBlock

addBlock :: String -> Codegen Name
addBlock bname = do
  bls <- gets blocks
  ix  <- gets blockCount
  nms <- gets names

  let new = emptyBlock ix
      (qname, supply) = uniqueName bname nms

  modify $ \s -> s { blocks = Map.insert (Name (toShortString qname)) new bls
                   , blockCount = ix + 1
                   , names = supply
                   }
  return (Name (toShortString qname))

setBlock :: Name -> Codegen Name
setBlock bname = do
  modify $ \s -> s { currentBlock = bname }
  return bname

getBlock :: Codegen Name
getBlock = gets currentBlock

modifyBlock :: BlockState -> Codegen ()
modifyBlock new = do
  active <- gets currentBlock
  modify $ \s -> s { blocks = Map.insert active new (blocks s) }

current :: Codegen BlockState
current = do
  c <- gets currentBlock
  blks <- gets blocks
  case Map.lookup c blks of
    Just x -> return x
    Nothing -> error $ "No such block: " ++ show c

-------------------------------------------------------------------------------
-- Symbol Table
-------------------------------------------------------------------------------

assign :: String -> Operand -> Codegen ()
assign var x = do
  lcls <- gets symtab
  modify $ \s -> s { symtab = [(var, x)] ++ lcls }

getvar :: String -> Codegen Operand
getvar var = do
  syms <- gets symtab
  case lookup var syms of
    Just x  -> return x
    Nothing -> error $ "Local variable not in scope: " ++ show var

mgetvar :: String -> Codegen (Maybe Operand)
mgetvar var = do
  syms <- gets symtab
  return (lookup var syms)

-------------------------------------------------------------------------------

-- References
local ::  Name -> Operand
local = LocalReference double

global ::  Name -> C.Constant
global = C.GlobalReference double

-- Arithmetic and Constants

binops = Map.fromList [
    ("+", fadd)
  , ("-", fsub)
  , ("*", fmul)
  , ("/", fdiv)
  , ("<", lt)
  , (">", gt)
  , ("==", eq)
  , ("!=", neq)
  , (":", sequ)
  -- , ("|", fOr)
  -- , ("&", fAnd)
  , ("^", fXor)
  ]

unops = Map.fromList [
   ("-", minus)
  ]

one = cons $ C.Float (F.Double 1.0)
zero = cons $ C.Float (F.Double 0.0)
false = zero
true = one

fadd :: Operand -> Operand -> Codegen Operand
fadd a b = instr $ FAdd I.noFastMathFlags a b []

fsub :: Operand -> Operand -> Codegen Operand
fsub a b = instr $ FSub I.noFastMathFlags a b []

fmul :: Operand -> Operand -> Codegen Operand
fmul a b = instr $ FMul I.noFastMathFlags a b []

fdiv :: Operand -> Operand -> Codegen Operand
fdiv a b = instr $ FDiv I.noFastMathFlags a b []

fcmp :: FP.FloatingPointPredicate -> Operand -> Operand -> Codegen Operand
fcmp cond a b = instr $ FCmp cond a b []

lt :: Operand -> Operand -> Codegen Operand
lt a b = do
  test <- fcmp FP.ULT a b
  uitofp double test

gt :: Operand -> Operand -> Codegen Operand
gt a b = do
  test <- fcmp FP.UGT a b
  uitofp double test

eq :: Operand -> Operand -> Codegen Operand
eq a b = do
  test <- fcmp FP.UEQ a b
  uitofp double test

neq :: Operand -> Operand -> Codegen Operand
neq a b = do
  test <- fcmp FP.UNE a b
  uitofp double test

sequ :: Operand -> Operand -> Codegen Operand
sequ a b = do
  return b

fOr :: Operand -> Operand -> Codegen Operand
fOr a b = instr $ Or a b []

fAnd :: Operand -> Operand -> Codegen Operand
fAnd a b = instr $ And a b []

fXor :: Operand -> Operand -> Codegen Operand
fXor a b = instr $ Xor a b []

minus :: Operand -> Codegen Operand
minus a = instr $ FSub I.noFastMathFlags zero a []

--------

cons :: C.Constant -> Operand
cons = ConstantOperand

uitofp :: Type -> Operand -> Codegen Operand
uitofp ty a = instr $ UIToFP a ty []

toArgs :: [Operand] -> [(Operand, [A.ParameterAttribute])]
toArgs = map (\x -> (x, []))

-- Effects
call :: Operand -> [Operand] -> Codegen Operand
call fn args = instr $ Call Nothing CC.C [] (Right fn) (toArgs args) [] []

alloca :: Type -> Codegen Operand
alloca ty = instr $ Alloca ty Nothing 0 []

store :: Operand -> Operand -> Codegen Operand
store ptr val = instrNoSave $ Store False ptr val Nothing 0 []

load :: Operand -> Codegen Operand
load ptr = instr $ Load False ptr Nothing 0 []

-- Control Flow
br :: Name -> Codegen (Named Terminator)
br val = terminator $ Do $ Br val []

cbr :: Operand -> Name -> Name -> Codegen (Named Terminator)
cbr cond tr fl = terminator $ Do $ CondBr cond tr fl []

ret :: Operand -> Codegen (Named Terminator)
ret val = terminator $ Do $ Ret (Just val) []

--------------

toType :: KP.Type -> AST.Type
toType kType = case kType of
    KP.Int -> int
    KP.Double -> double
    -- TODO void

codegenTop :: PC.Expr -> LLVM ()
codegenTop (PC.ExprFunction name args body) = do
  define double name fnArgs bls
    where
      fnArgs = toSig args
      bls = createBlocks $ execCodegen $ do
        entry <- addBlock entryBlockName
        setBlock entry
        forM args $ \(PC.ExprVar a) -> do
          var <- alloca double
          store var (local (AST.Name (toShortString a)))
          assign a var
        mapM cgen (init body)
        cgen (last body) >>= ret
codegenTop (PC.ExprBinaryDef name args body) =
  codegenTop $ PC.ExprFunction ("binary" ++ name) args [body]
codegenTop (PC.ExprUnaryDef name args body) =
  codegenTop $ PC.ExprFunction ("unary" ++ name) args [body]
codegenTop (PC.ExprExtern name args) = do
  external double name fnArgs
    where
      fnArgs = toSig args
codegenTop exp = do
  define double "main" [] blks
    where
      blks = createBlocks $ execCodegen $ do
        entry <- addBlock entryBlockName
        setBlock entry
        cgen exp >>= ret

toSig :: [PC.Expr] -> [(AST.Type, AST.Name)]
toSig = map (\(PC.ExprVar x) -> (double, AST.Name (toShortString x)))

cgen :: PC.Expr -> Codegen AST.Operand
cgen (PC.ExprFloat n) = return $ cons $ C.Float (F.Double n)
cgen (PC.ExprVar x) = getvar x >>= load
cgen (PC.ExprCall fn args) = do
  largs <- mapM cgen args
  call (externf (AST.Name (toShortString fn))) largs
  where
    externf = ConstantOperand . C.GlobalReference pointerType
    pointerType = PointerType {pointerReferent = FunctionType {resultType = FloatingPointType {floatingPointType = DoubleFP}, argumentTypes = argsList, isVarArg = False}, pointerAddrSpace = AddrSpace 0}
    argsList = [FloatingPointType {floatingPointType = DoubleFP} | i <- [0..((Data.List.length args) - 1)]]
cgen (PC.ExprBinaryOperation "=" (PC.ExprVar var) val) = do
  a <- mgetvar var
  case a of
    (Just a) -> do
      cval <- cgen val
      store a cval
      return cval
    Nothing -> do
      cur <- gets currentBlock
      i <- alloca double
      cval <- cgen val
      assign var i
      store i cval
      load i
cgen (PC.ExprBinaryOperation op a b) = do
  case Map.lookup op binops of
    Just f -> do
      ca <- cgen a
      cb <- cgen b
      f ca cb
    Nothing -> cgen (PC.ExprCall ("binary" ++ op) [a, b])
cgen (PC.ExprUnaryOperation op a) = do
  case Map.lookup op unops of
    Just f -> do
      ca <- cgen a
      f ca
    Nothing -> cgen (PC.ExprCall ("unary" ++ op) [a])
cgen (PC.ExprIf cond th Nothing) = do
  ifThen <- addBlock "if.then"
  ifElse <- addBlock "if.else"
  ifExit <- addBlock "if.exit"
  
  cond <- cgen cond
  test <- fcmp FP.ONE false cond
  cbr test ifThen ifElse

  setBlock ifThen
  thVal <- cgen th
  br ifExit
  ifThen <- getBlock

  setBlock ifElse
  elVal <- cgen (PC.ExprFloat 0.0)
  br ifExit
  ifElse <- getBlock

  setBlock ifExit
  instr $ LLVM.AST.Phi double [(thVal, ifThen), (elVal, ifElse)] []
cgen (PC.ExprIf cond th (Just el)) = do
  ifThen <- addBlock "if.then"
  ifElse <- addBlock "if.else"
  ifExit <- addBlock "if.exit"
  
  cond <- cgen cond
  test <- fcmp FP.ONE false cond
  cbr test ifThen ifElse

  setBlock ifThen
  thVal <- cgen th
  br ifExit
  ifThen <- getBlock

  setBlock ifElse
  elVal <- cgen el
  br ifExit
  ifElse <- getBlock

  setBlock ifExit
  instr $ LLVM.AST.Phi double [(thVal, ifThen), (elVal, ifElse)] []
cgen (PC.ExprFor (ivar, start) cond step body) = do
  forLoop <- addBlock "for.loop"
  forExit <- addBlock "for.exit"

  i <- alloca double
  iStart <- cgen start
  stepVal <- cgen step
  store i iStart
  assign ivar i
  br forLoop

  setBlock forLoop
  cgen body
  iVal <- load i
  iNext <- fadd iVal stepVal
  store i iNext

  cond <- cgen cond
  test <- fcmp FP.ONE false cond
  cbr test forLoop forExit

  setBlock forExit
  return zero
cgen (PC.ExprWhile cond body) = do
  whileLoop <- addBlock "while.loop"
  whileExit <- addBlock "while.exit"

  br whileLoop

  setBlock whileLoop
  cgen body
  
  cond <- cgen cond
  test <- fcmp FP.ONE false cond
  cbr test whileLoop whileExit

  setBlock whileExit
  return zero
cgen e = trace (show e) undefined

codegen :: AST.Module -> [PC.Expr] -> AST.Module
codegen mod fns = newast
  where
    modn = mapM codegenTop fns
    newast = runLLVM mod modn