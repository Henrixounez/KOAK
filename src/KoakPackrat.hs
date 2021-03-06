module KoakPackrat where

import Data.Either
import Data.Char
import Debug.Trace

{--
stmt                := _* kdefs* #eof
kdefs               := ( defStr defs | extern | expressions ) ';' _*
defStr              := 'def'
defs                := prototype _* expressions
prototype           := _* prototype_start _* prototype_args
prototype_start     := ( unaryStr . decimal_const? operator ) |
                       ( binaryStr . decimal_const? operator ) |
                       identifier
unaryStr            := 'unary'
binaryStr           := 'binary'
prototype_args      := '(' args_list ')' _* ':' type
args_list           := (identifier ':' type)*
type                := _* (intStr | doubleStr | voidStr) _*

extern              := externStr identifier prototype_args

intStr              := 'int'
doubleStr           := 'double'
voidStr             := 'void'
externStr           := 'extern'

expressions         := _* '('? expressionContent ')'? exprConcat
expressionsContent  := (for_expr
                     | if_expr
                     | while_expr
                     | expression ) exprConcat
exprConcat          := (_* ':' expressions)?
for_expr            := forStr identifier '=' expression ','
                       expression ','
                       expression inStr expressions
if_expr             := ifStr expression thenStr expressions else_expr?
else_expr           := elseStr expressions
while_expr          := whileStr expression doStr expressions

forStr              := 'for'
inStr               := 'in'
ifStr               := 'if'
thenStr             := 'then'
elseStr             := 'else'
whileStr            := 'while'
doStr               := 'do'

expression          := _* unary binaryOperations _*
binaryOperations    := _* (#binop (unary | expression))*
unary               := _* #unop unary | postfix
postfix             := _* primary _* call_expr?
call_expr           := '(' _* (expression exprList)? ')'
exprList            := (',' expression)*
primary             := identifier
                     | literal
                     | '(' expression ')'

identifier          := _* ([a-zA-Z] identifierContent) _*
identifierContent   := [a-zA-Z0-9]*
dot                 := '.' !'.'
decimal_const       := [0-9]+
double_const        := (decimal_const dot [0-9]* | dot decimal_const)
literal             := decimal_const | double_const
whitespace          := (' ' | '\n' | '\t' | comment)*
comment             := '#' .* '\n'
operator            := _* (![a-zA-Z0-9_;] operatorContent) _*
operatorContent     := ![a-zA-Z0-9_;]*
#unop               := '!' | '-' | operator
#binop              := '*' | '/' | '+' | '-' | '<' | '>' | '==' | '!=' | '=' | operator
char                := .
--}

data Type = Int | Double | Void
data FunctionType = Function | UnaryFn | BinaryFn deriving (Eq, Ord)

data Stmt = Stmt {
  kdefs           :: [Kdefs]
}

data Kdefs = Defs {
  prototype       :: Prototype,
  defExpressions  :: Expressions
} | Extern {
  externName      :: String,
  externArgs      :: [(Primary, Type)],
  externReturn    :: Type
} | KExpressions Expressions

data Prototype = Prototype {
  ptype           :: FunctionType,
  precendence     :: Int,
  name            :: String,
  args            :: [(Primary, Type)],
  return          :: Type
}

data PrototypeStart = PrototypeStart {
  pptype           :: FunctionType,
  pname           :: String,
  pprecedence     :: Int
}

data Expressions = ForExpr {
  init            :: (Primary, Expression),
  condition       :: Expression,
  increment       :: Expression,
  expressions     :: Expressions
} | IfExpr {
  conditionIf     :: Expression,
  expressions     :: Expressions,
  elseExpressions :: Maybe Expressions
} | WhileExpr {
  conditionWhile  :: Expression,
  expressions     :: Expressions
} | ExprConcat {
  expr            :: Expressions,
  expressions     :: Expressions
} | Expr Expression

data Expression = Expression {
  exprUnary       :: UnaryPostfix,
  binaryOps       :: [BinaryOperation]
}

data BinaryOperation = BinaryOperation {
  binop           :: BinOp,
  unaryExpr       :: UnaryExpr
}

data UnaryExpr = UnaryExprUnary UnaryPostfix | UnaryExprExpression Expression

data UnaryPostfix = Unary {
  op              :: UnOp,
  unary           :: UnaryPostfix
} | Postfix {
  primary         :: Primary,
  callExpr        :: Maybe [Expression]
}

data Primary = Identifier String | PrimaryLiteral Literal | PrimaryExpressions Expression

data Literal = LiteralInt Int | LiteralFloat Double

type BinOp = String
type UnOp = String

instance Show Stmt where
  show (Stmt kdefs) = "file:\n" ++ show kdefs

instance Show Kdefs where
  show (Defs proto exprs) = "def:\n" ++ show proto ++ "\ninstructions:\n" ++ show exprs ++ "\n"
  show (Extern name args return) = "extern: " ++ show name ++ "\n\targs: " ++ show args ++ "\n\ttype: " ++ show return
  show (KExpressions exprs) = "instructions:\n" ++ show exprs ++ "\n"

instance Show Prototype where
  show (Prototype _ _ name args return) = show name ++ ":\n\targs: " ++ show args ++ "\n\ttype: " ++ show return

instance Show Expressions where
  show (ForExpr init condition increment expressions) = "for:\n\tinit: " ++ show init ++ "\n\tcondition: " ++ show condition ++ "\n\tincrement: " ++ show increment ++ "\n\tinstructions: " ++ show expressions ++ "\n"
  show (IfExpr cond exprs elseExprs) = "if:\n\tcondition: " ++ show cond ++ "\n\tinstructions: " ++ show exprs ++ "\n\telse_instructions: " ++ show elseExprs ++ "\n"
  show (WhileExpr cond exprs) = "while:\n\tcondition: " ++ show cond ++ "\n\tinstructions: " ++ show exprs ++ "\n"
  show (Expr exprs) = show exprs ++ "\n"
  show (ExprConcat exprs exprsC) = show exprs ++ ", " ++ show exprsC ++ "\n"

instance Show Expression where
  show (Expression (Postfix primary _) ((BinaryOperation binop (UnaryExprUnary (Postfix primaryOne _))):[])) = show binop ++ ": (" ++ show primaryOne ++ ", " ++ show primary ++ ")"
  show (Expression exprUnary []) = show exprUnary
  show (Expression exprUnary binops) = show exprUnary ++ show binops 

instance Show BinaryOperation where
  show (BinaryOperation binop (UnaryExprUnary (Postfix primary _))) = show binop ++ ": " ++ show primary
  show (BinaryOperation binop unaryExpr) = show binop ++ ": " ++ show unaryExpr

instance Show UnaryExpr where
  show (UnaryExprUnary unary) = show unary
  show (UnaryExprExpression expr) = show expr

instance Show UnaryPostfix where
  show (Unary op unary) = "(" ++ show op ++ ", " ++ show unary ++ ")"
  show (Postfix primary (Just callExpr)) = show primary ++ ": " ++ show callExpr
  show (Postfix primary Nothing) = show primary

instance Show Primary where
  show (Identifier str) = show str
  show (PrimaryLiteral lit) = show lit
  show (PrimaryExpressions exp) = show exp

instance Show Literal where
  show (LiteralInt i) = show i
  show (LiteralFloat f) = show f

instance Show Type where
  show Int = "int"
  show Double = "double"
  show Void = "void"

{--                   --}

data Result v = Parsed v Derivs
              | NoParse (Maybe String)

data Derivs = Derivs {
  dvStmt                :: Result [Kdefs],
  dvKdefs               :: Result Kdefs,
  dvDefStr              :: Result String,
  dvDefs                :: Result Kdefs,
  dvPrototype           :: Result Prototype,
  dvPrototypeStart      :: Result PrototypeStart,
  dvUnaryStr            :: Result String,
  dvBinaryStr           :: Result String,
  dvPrototypeArgs       :: Result ([(Primary, Type)], Type),
  dvArgsList            :: Result [(Primary, Type)],
  dvType                :: Result Type,
  dvExtern              :: Result Kdefs,
  dvIntStr              :: Result String,
  dvDoubleStr           :: Result String,
  dvVoidStr             :: Result String,
  dvExternStr           :: Result String,
  dvExpressions         :: Result Expressions,
  dvExpressionsContent  :: Result Expressions,
  dvExprConcat          :: Result (Maybe Expressions),
  dvForExpr             :: Result Expressions,
  dvIfExpr              :: Result Expressions,
  dvElseExpr            :: Result (Maybe Expressions),
  dvWhileExpr           :: Result Expressions,
  dvForStr              :: Result String,
  dvInStr               :: Result String,
  dvIfStr               :: Result String,
  dvThenStr             :: Result String,
  dvElseStr             :: Result String,
  dvWhileStr            :: Result String,
  dvDoStr               :: Result String,
  dvExpression          :: Result Expression,
  dvBinaryOperations    :: Result [BinaryOperation],
  dvUnary               :: Result UnaryPostfix,
  dvPostfix             :: Result UnaryPostfix,
  dvCallExpr            :: Result [Expression],
  dvExprList            :: Result [Expression],
  dvPrimary             :: Result Primary,
  dvIdentifier          :: Result Primary,
  dvIdentifierContent   :: Result String,
  dvDot                 :: Result String,
  dvDecimalConst        :: Result Int,
  dvDecimalConstContent :: Result String,
  dvDoubleConst         :: Result Double,
  dvLiteral             :: Result Literal,
  dvWhitespace          :: Result (),
  dvComment             :: Result (),
  dvOperator            :: Result String,
  dvOperatorContent     :: Result String,
  dvUnop                :: Result UnOp,
  dvBinop               :: Result BinOp,
  dvChar                :: Result Char
}

{--                   --}

testFct :: String -> IO (String)
testFct filePath = do
  file <- readFile filePath
  case eval file of
    (Right file) -> Prelude.return ("FILE OK")
    (Left strErr) -> case strErr of
      (Just str) -> Prelude.return ("Error while parsing : " ++ str)
      Nothing -> Prelude.return ("Error while parsing\n")

eval :: String -> Either (Maybe String) Stmt
eval s = case dvStmt (parse s) of
  Parsed v rem -> (Right (Stmt v))
  NoParse str -> Left str

parse :: String -> Derivs
parse s = d where
    d             = Derivs stmt kdefs defStr defs prototype prototypeS unaryStr binaryStr prototypeArgs argsList types extern intStr doubleStr voidStr externStr expressions expressionsC exprConcat forExpr ifExpr elseExpr whileExpr forStr inStr ifStr thenStr elseStr whileStr doStr expression binaryOps unary postFix callExpr exprList primary identifier identifierC dot decimalConst decimalConstC doubleConst literal whitespace comment op opContent unop binop char
    stmt          = pStmt d
    kdefs         = pKdefs d
    defStr        = pDefStr d
    defs          = pDefs d
    prototype     = pPrototype d
    prototypeS    = pPrototypeStart d
    unaryStr      = pUnaryStr d
    binaryStr     = pBinaryStr d
    prototypeArgs = pPrototypeArgs d
    argsList      = pArgsList d
    types         = pType d
    extern        = pExtern d
    intStr        = pIntStr d
    doubleStr     = pDoubleStr d
    voidStr       = pVoidStr d
    externStr     = pExternStr d
    expressions   = pExpressions d
    expressionsC  = pExpressionsContent d
    exprConcat    = pExprConcat d
    forExpr       = pForExpr d
    ifExpr        = pIfExpr d
    elseExpr      = pElseExpr d
    whileExpr     = pWhileExpr d
    forStr        = pForStr d
    inStr         = pInStr d
    ifStr         = pIfStr d
    thenStr       = pThenStr d
    elseStr       = pElseStr d
    whileStr      = pWhileStr d
    doStr         = pDoStr d
    expression    = pExpression d
    binaryOps     = pBinaryOperations d
    unary         = pUnary d
    postFix       = pPostfix d
    callExpr      = pCallExpr d
    exprList      = pExprList d
    primary       = pPrimary d
    identifier    = pIdentifier d
    identifierC   = pIdentifierContent d
    dot           = pDot d
    decimalConst  = pDecimalConst d
    decimalConstC = pDecimalConstContent d
    doubleConst   = pDoubleConst d
    literal       = pLiteral d
    whitespace    = pWhitespace d
    comment       = pComment d
    op            = pOperator d
    opContent     = pOperatorContent d
    unop          = pUnop d
    binop         = pBinop d
    char          = case s of
                    (c:s') -> Parsed c (parse s')
                    [] -> NoParse Nothing

{--                   --}

pStmt :: Derivs -> Result [Kdefs]
pStmt d = case dvWhitespace d of
  Parsed _ d1 -> case dvKdefs d1 of
    Parsed kdef d2 -> case dvChar d2 of
      NoParse str -> Parsed [kdef] d2
      _ -> case dvStmt d2 of
        Parsed stmt d3 -> Parsed (kdef:stmt) d3
        NoParse str -> NoParse str
    NoParse str -> NoParse str


pKdefs :: Derivs -> Result Kdefs
pKdefs d = case dvDefStr d of
  Parsed _ d1 -> case dvDefs d1 of
    Parsed defs d2 -> case dvChar d2 of
      Parsed ';' d3 -> case dvWhitespace d3 of
        Parsed _ d4 -> Parsed defs d4
      _ -> NoParse (Just "Syntax error: missing ';'")
    _ -> NoParse (Just "syntax error: error on 'def' definition")
  _ -> case dvExtern d of
    Parsed extern d1 -> case dvChar d1 of
      Parsed ';' d2 -> case dvWhitespace d2 of
        Parsed _ d3 -> Parsed extern d3
      _ -> NoParse Nothing
    _ -> case dvExpressions d of
      Parsed exprs d1 -> case dvChar d1 of
        Parsed ';' d2 -> case dvWhitespace d2 of
          Parsed _ d3 -> Parsed (KExpressions exprs) d3
        _ -> NoParse (Just "syntax error: Error on 'extern' definition")
      _ -> NoParse Nothing

pDefStr :: Derivs -> Result String
pDefStr d = case dvChar d of
  Parsed 'd' d1 -> case dvChar d1 of
    Parsed 'e' d2 -> case dvChar d2 of
      Parsed 'f' d3 -> Parsed "def" d3
      _ -> NoParse Nothing
    _ -> NoParse Nothing
  _ -> NoParse Nothing

pDefs :: Derivs -> Result Kdefs
pDefs d = case dvPrototype d of
  Parsed proto d1 -> case dvWhitespace d1 of
    Parsed _ d2 -> case dvExpressions d2 of
      Parsed exprs d3 -> Parsed (Defs proto exprs) d3
      _ -> NoParse Nothing
    _ -> NoParse Nothing
  _ -> NoParse Nothing

pPrototype :: Derivs -> Result Prototype
pPrototype d = case dvWhitespace d of
  Parsed _ d1 -> case dvPrototypeStart d1 of
    Parsed (PrototypeStart typ name prec) d2 -> case dvWhitespace d2 of
      Parsed _ d3 -> case dvPrototypeArgs d3 of
        Parsed (args, return) d4 -> Parsed (Prototype typ prec name args return) d4
        _ -> NoParse Nothing
      _ -> NoParse Nothing
    _ -> NoParse Nothing

pPrototypeStart :: Derivs -> Result PrototypeStart
pPrototypeStart d = case dvUnaryStr d of
  Parsed un d1 -> case dvDecimalConst d1 of
    Parsed dc d2 -> case dvWhitespace d2 of
      Parsed _ d3 -> case dvOperator d3 of
        Parsed id d4 -> Parsed (PrototypeStart UnaryFn id dc) d4
        _ -> NoParse Nothing
      _ -> NoParse Nothing
    _ -> NoParse Nothing
  _ -> case dvBinaryStr d of
    Parsed bn d1 -> case dvDecimalConst d1 of
      Parsed dc d2 -> case dvWhitespace d2 of
        Parsed _ d3 -> case dvOperator d3 of
          Parsed id d4 -> Parsed (PrototypeStart BinaryFn id dc) d4
          _ -> NoParse Nothing
        _ -> NoParse Nothing
      _ -> NoParse Nothing
    _ -> case dvIdentifier d of
      Parsed (Identifier id) d1 -> Parsed (PrototypeStart Function id 0) d1
      _ -> NoParse Nothing

pUnaryStr :: Derivs -> Result String
pUnaryStr d = case dvChar d of
  Parsed 'u' d1 -> case dvChar d1 of
    Parsed 'n' d2 -> case dvChar d2 of
      Parsed 'a' d3 -> case dvChar d3 of
        Parsed 'r' d4 -> case dvChar d4 of
          Parsed 'y' d5 -> case dvWhitespace d5 of
            Parsed _ d6 -> Parsed "unary" d6
          _ -> NoParse Nothing
        _ -> NoParse Nothing
      _ -> NoParse Nothing
    _ -> NoParse Nothing
  _ -> NoParse Nothing

pBinaryStr :: Derivs -> Result String
pBinaryStr d = case dvChar d of
  Parsed 'b' d1 -> case dvChar d1 of
    Parsed 'i' d2 -> case dvChar d2 of
      Parsed 'n' d3 -> case dvChar d3 of
        Parsed 'a' d4 -> case dvChar d4 of
          Parsed 'r' d5 -> case dvChar d5 of
            Parsed 'y' d6 -> case dvWhitespace d6 of
              Parsed _ d7 -> Parsed "binary" d7
            _ -> NoParse Nothing
          _ -> NoParse Nothing
        _ -> NoParse Nothing
      _ -> NoParse Nothing
    _ -> NoParse Nothing
  _ -> NoParse Nothing 

pPrototypeArgs :: Derivs -> Result ([(Primary, Type)], Type)
pPrototypeArgs d = case dvChar d of
  Parsed '(' d1 -> case dvArgsList d1 of
    Parsed args d2 -> case dvChar d2 of
      Parsed ')' d3 -> case dvWhitespace d3 of
        Parsed _ d4 -> case dvChar d4 of
          Parsed ':' d5 -> case dvType d5 of
            Parsed typ d6 -> Parsed (args, typ) d6
            _ -> NoParse  Nothing
          _ -> NoParse  Nothing
      _ -> NoParse  Nothing
    _ -> NoParse  Nothing
  _ -> NoParse  Nothing

pArgsList :: Derivs -> Result [(Primary, Type)]
pArgsList d = case dvIdentifier d of
  Parsed id d1 -> case dvChar d1 of
    Parsed ':' d2 -> case dvType d2 of
      Parsed typ d3 -> case dvArgsList d3 of
        Parsed [] d4 -> Parsed [(id, typ)] d3
        Parsed lst d4 -> Parsed ((id, typ):lst) d4 
        _ -> NoParse Nothing
      _ -> NoParse Nothing
    _ -> NoParse Nothing
  _ -> Parsed [] d

pType :: Derivs -> Result Type
pType d = case dvWhitespace d of
  Parsed _ d1 -> case dvIntStr d1 of
    Parsed _ d2 -> case dvWhitespace d2 of
      Parsed _ d3 -> Parsed Int d3
    _ -> case dvDoubleStr d1 of
      Parsed _ d2 -> case dvWhitespace d2 of
        Parsed _ d3 -> Parsed Double d3
      _ -> case dvVoidStr d1 of
        Parsed _ d2 -> case dvWhitespace d2 of
          Parsed _ d3 -> Parsed Void d3
        _ -> NoParse Nothing

pExtern :: Derivs -> Result Kdefs
pExtern d = case dvExternStr d of
  Parsed _ d1 -> case dvIdentifier d1 of
    Parsed (Identifier id) d2 -> case dvPrototypeArgs d2 of
      Parsed (args, return) d3 -> Parsed (Extern id args return) d3
      _ -> NoParse  Nothing
    _ -> NoParse  Nothing
  _ -> NoParse  Nothing

pIntStr :: Derivs -> Result String
pIntStr d = case dvChar d of
  Parsed 'i' d1 -> case dvChar d1 of
    Parsed 'n' d2 -> case dvChar d2 of
      Parsed 't' d3 -> Parsed "int" d3
      _ -> NoParse Nothing
    _ -> NoParse Nothing
  _ -> NoParse Nothing

pDoubleStr :: Derivs -> Result String
pDoubleStr d = case dvChar d of
  Parsed 'd' d1 -> case dvChar d1 of
    Parsed 'o' d2 -> case dvChar d2 of
      Parsed 'u' d3 -> case dvChar d3 of
        Parsed 'b' d4 -> case dvChar d4 of
          Parsed 'l' d5 -> case dvChar d5 of
            Parsed 'e' d6 -> Parsed "double" d6
            _ -> NoParse Nothing
          _ -> NoParse Nothing
        _ -> NoParse Nothing
      _ -> NoParse Nothing
    _ -> NoParse Nothing
  _ -> NoParse Nothing 

pVoidStr :: Derivs -> Result String
pVoidStr d = case dvChar d of
  Parsed 'v' d1 -> case dvChar d1 of
    Parsed 'o' d2 -> case dvChar d2 of
      Parsed 'i' d3 -> case dvChar d3 of
        Parsed 'd' d4 -> Parsed "void" d4
        _ -> NoParse Nothing
      _ -> NoParse Nothing
    _ -> NoParse Nothing
  _ -> NoParse Nothing

pExternStr :: Derivs -> Result String
pExternStr d = case dvChar d of
  Parsed 'e' d1 -> case dvChar d1 of
    Parsed 'x' d2 -> case dvChar d2 of
      Parsed 't' d3 -> case dvChar d3 of
        Parsed 'e' d4 -> case dvChar d4 of
          Parsed 'r' d5 -> case dvChar d5 of
            Parsed 'n' d6 -> Parsed "extern" d6
            _ -> NoParse Nothing
          _ -> NoParse Nothing
        _ -> NoParse Nothing
      _ -> NoParse Nothing
    _ -> NoParse Nothing
  _ -> NoParse Nothing

pExpressions :: Derivs -> Result Expressions
pExpressions d = case dvWhitespace d of
  Parsed _ d1 -> case dvChar d1 of
    Parsed '(' d2 -> case dvExpressionsContent d2 of
      Parsed exprs d3 -> case dvChar d3 of
        Parsed ')' d4 -> case dvExprConcat d4 of
          Parsed (Just exprsC) d5 -> Parsed (ExprConcat exprs exprsC) d5
          Parsed (Nothing) d5 -> Parsed exprs d4
          _ -> NoParse Nothing
        _ -> NoParse Nothing
      _ -> NoParse Nothing
    Parsed c d2 -> case dvExpressionsContent d1 of
      Parsed exprs d2 -> case dvExprConcat d2 of
        Parsed (Just exprsC) d3 -> Parsed (ExprConcat exprs exprsC) d3
        Parsed (Nothing) d3 -> Parsed exprs d2
        _ -> NoParse Nothing
      _ -> NoParse Nothing
    _ -> NoParse Nothing

pExpressionsContent :: Derivs -> Result Expressions
pExpressionsContent d = case dvWhitespace d of
  Parsed _ d1 -> case dvForExpr d1 of
    Parsed expr d2 -> Parsed expr d2
    _ -> case dvIfExpr d1 of
      Parsed expr d2 -> Parsed expr d2
      _ -> case dvWhileExpr d1 of
        Parsed expr d2 -> Parsed expr d2
        _ -> case dvExpression d1 of
          Parsed expr d2 -> Parsed (Expr expr) d2
          _ -> NoParse Nothing
        
pExprConcat :: Derivs -> Result (Maybe Expressions)
pExprConcat d = case dvWhitespace d of
  Parsed _ d1 -> case dvChar d1 of
    Parsed ':' d2 -> case dvExpressions d2 of
      Parsed ex d3 -> Parsed (Just ex) d3
      _ -> NoParse Nothing
    Parsed c d -> Parsed Nothing d1
    _ -> NoParse Nothing
  _ -> NoParse Nothing

pForExpr :: Derivs -> Result Expressions
pForExpr d = case dvForStr d of
  Parsed _ d1 -> case dvIdentifier d1 of
    Parsed id d2 -> case dvChar d2 of
      Parsed '=' d3 -> case dvExpression d3 of
        Parsed init d4 -> case dvChar d4 of
          Parsed ',' d5 -> case dvExpression d5 of
            Parsed cond d6 -> case dvChar d6 of
              Parsed ',' d7 -> case dvExpression d7 of
                Parsed increment d8 -> case dvInStr d8 of
                  Parsed _ d9 -> case dvExpressions d9 of
                    Parsed exprs d10 -> Parsed (ForExpr (id, init) cond increment exprs) d10
                    _ -> NoParse Nothing
                  _ -> NoParse Nothing
                _ -> NoParse Nothing
              _ -> NoParse Nothing
            _ -> NoParse Nothing
          _ -> NoParse Nothing
        _ -> NoParse Nothing
      _ -> NoParse Nothing
    _ -> NoParse Nothing
  _ -> NoParse Nothing

pIfExpr :: Derivs -> Result Expressions
pIfExpr d = case dvIfStr d of
  Parsed _ d1 -> case dvExpression d1 of
    Parsed cond d2 -> case dvThenStr d2 of
      Parsed _ d3 -> case dvExpressions d3 of
        Parsed exprs d4 -> case dvElseExpr d4 of
          Parsed elseExprs d5 -> Parsed (IfExpr cond exprs elseExprs) d5
          _ -> NoParse Nothing
        _ -> NoParse Nothing
      _ -> NoParse Nothing
    _ -> NoParse Nothing
  _ -> NoParse Nothing

pElseExpr :: Derivs -> Result (Maybe Expressions)
pElseExpr d = case dvElseStr d of
  Parsed _ d1 -> case dvExpressions d1 of
    Parsed exprs d2 -> Parsed (Just exprs) d2
    _ -> NoParse Nothing
  _ -> Parsed Nothing d

pWhileExpr :: Derivs -> Result Expressions
pWhileExpr d = case dvWhileStr d of
  Parsed _ d1 -> case dvExpression d1 of
    Parsed cond d2 -> case dvDoStr d2 of
      Parsed _ d3 -> case dvExpressions d3 of
        Parsed exprs d4 -> Parsed (WhileExpr cond exprs) d4
        _ -> NoParse Nothing
      _ -> NoParse Nothing
    _ -> NoParse Nothing
  _ -> NoParse Nothing

pForStr :: Derivs -> Result String
pForStr d = case dvChar d of
  Parsed 'f' d1 -> case dvChar d1 of
    Parsed 'o' d2 -> case dvChar d2 of
      Parsed 'r' d3 -> Parsed "for" d3
      _ -> NoParse Nothing
    _ -> NoParse Nothing
  _ -> NoParse Nothing

pInStr :: Derivs -> Result String
pInStr d = case dvChar d of
  Parsed 'i' d1 -> case dvChar d1 of
    Parsed 'n' d2 -> Parsed "in" d2
    _ -> NoParse Nothing
  _ -> NoParse Nothing

pIfStr :: Derivs -> Result String
pIfStr d = case dvChar d of
  Parsed 'i' d1 -> case dvChar d1 of
    Parsed 'f' d2 -> Parsed "if" d2
    _ -> NoParse Nothing
  _ -> NoParse Nothing

pThenStr :: Derivs -> Result String
pThenStr d = case dvChar d of
  Parsed 't' d1 -> case dvChar d1 of
    Parsed 'h' d2 -> case dvChar d2 of
      Parsed 'e' d3 -> case dvChar d3 of
        Parsed 'n' d4 -> Parsed "then" d4
        _ -> NoParse Nothing
      _ -> NoParse Nothing
    _ -> NoParse Nothing
  _ -> NoParse Nothing

pElseStr :: Derivs -> Result String
pElseStr d = case dvChar d of
  Parsed 'e' d1 -> case dvChar d1 of
    Parsed 'l' d2 -> case dvChar d2 of
      Parsed 's' d3 -> case dvChar d3 of
        Parsed 'e' d4 -> Parsed "else" d4
        _ -> NoParse Nothing
      _ -> NoParse Nothing
    _ -> NoParse Nothing
  _ -> NoParse Nothing

pWhileStr :: Derivs -> Result String
pWhileStr d = case dvChar d of
  Parsed 'w' d1 -> case dvChar d1 of
    Parsed 'h' d2 -> case dvChar d2 of
      Parsed 'i' d3 -> case dvChar d3 of
        Parsed 'l' d4 -> case dvChar d4 of
          Parsed 'e' d5 -> Parsed "while" d5
          _ -> NoParse Nothing
        _ -> NoParse Nothing
      _ -> NoParse Nothing
    _ -> NoParse Nothing
  _ -> NoParse Nothing

pDoStr :: Derivs -> Result String
pDoStr d = case dvChar d of
  Parsed 'd' d1 -> case dvChar d1 of
    Parsed 'o' d2 -> Parsed "do" d2
    _ -> NoParse Nothing
  _ -> NoParse Nothing

pExpression :: Derivs -> Result Expression
pExpression d = case dvWhitespace d of 
  Parsed _ d1 -> case dvUnary d1 of
    Parsed un d2 -> case dvBinaryOperations d2 of
      Parsed bo d3 -> case dvWhitespace d3 of
        Parsed _ d4 -> Parsed (Expression un bo) d4
      NoParse str -> NoParse str
    _ -> NoParse Nothing

pBinaryOperations :: Derivs -> Result [BinaryOperation]
pBinaryOperations d = case dvWhitespace d of 
  Parsed _ d1 -> case dvBinop d1 of
    Parsed bn d2 -> case dvUnary d2 of
      Parsed un d3 -> case dvBinaryOperations d3 of
        Parsed bops d4 -> Parsed ((BinaryOperation bn (UnaryExprUnary un)):bops) d4
        _ -> NoParse Nothing
      _ -> case dvExpression d2 of
        Parsed expr d3 -> case dvBinaryOperations d3 of
          Parsed bops d4 -> Parsed ((BinaryOperation bn (UnaryExprExpression expr)):bops) d4
          _ -> NoParse Nothing
        _ -> NoParse Nothing
    _ -> (Parsed [] d1)

pUnary :: Derivs -> Result UnaryPostfix
pUnary d = case dvWhitespace d of
  Parsed _ d1 -> case dvUnop d1 of
    Parsed unp d2 -> case dvUnary d2 of
      Parsed un d3 -> Parsed (Unary unp un) d3
      _ -> NoParse Nothing
    _ -> case dvPostfix d1 of
      Parsed ps d2 -> Parsed ps d2
      _ -> NoParse Nothing

pPostfix :: Derivs -> Result UnaryPostfix
pPostfix d = case dvWhitespace d of
  Parsed _ d1 -> case dvPrimary d1 of
    Parsed pr d2 -> case dvWhitespace d2 of
      Parsed _ d3 -> case dvCallExpr d3 of
        Parsed ce d4 -> Parsed (Postfix pr (Just ce)) d4
        _ -> Parsed (Postfix pr Nothing) d3
      _ -> NoParse Nothing
    _ -> NoParse Nothing
 
pCallExpr :: Derivs -> Result [Expression]
pCallExpr d = case dvChar d of
  Parsed '(' d1 -> case dvExpression d1 of
    Parsed ex d2 -> case dvExprList d2 of
      Parsed exs d3 -> case dvChar d3 of
        Parsed ')' d4 -> Parsed (ex:exs) d4
        _ -> NoParse Nothing
      _ -> NoParse Nothing
    _ -> NoParse Nothing
  _ -> NoParse Nothing

pExprList :: Derivs -> Result [Expression]
pExprList d = case dvChar d of
  Parsed ',' d1 -> case dvExpression d1 of
    Parsed ex d2 -> case dvExprList d2 of
      Parsed [] d3 -> Parsed [ex] d3
      Parsed exs d3 -> Parsed (ex:exs) d3
      _ -> NoParse Nothing
    _ -> NoParse Nothing
  Parsed _ d1 -> Parsed [] d
  _ -> NoParse Nothing

pPrimary :: Derivs -> Result Primary
pPrimary d = case dvIdentifier d of
  Parsed id d1 -> Parsed id d1
  _ -> case dvLiteral d of
    Parsed li d1 -> Parsed (PrimaryLiteral li) d1
    _ -> case dvChar d of
      Parsed '(' d1 -> case dvExpression d1 of
        Parsed expr d2 -> case dvChar d2 of
          Parsed ')' d3 -> Parsed (PrimaryExpressions expr) d3
          _ -> NoParse Nothing
        _ -> NoParse Nothing
      _ -> NoParse Nothing

pIdentifier :: Derivs -> Result Primary
pIdentifier d = case dvWhitespace d of
  Parsed _ d1 -> case dvChar d1 of
    Parsed c d2 -> case isLetter c of
      True -> case dvIdentifierContent d2 of
        Parsed s d3 -> case dvWhitespace d3 of
          Parsed _ d4 -> Parsed (Identifier (c:s)) d4
        _ -> NoParse Nothing
      False -> NoParse Nothing
    _ -> NoParse Nothing

pIdentifierContent :: Derivs -> Result String
pIdentifierContent d = case dvChar d of
  Parsed c d1 -> case isAlphaNum c of
    True -> case dvIdentifierContent d1 of
      Parsed [] d2 -> Parsed [c] d1
      Parsed s d2 -> Parsed (c:s) d2
      _ -> NoParse Nothing
    False -> Parsed [] d
  _ -> NoParse Nothing

pDot :: Derivs -> Result String
pDot d = case dvChar d of
  Parsed '.' d1 -> case dvChar d1 of
    Parsed '.' _ -> NoParse Nothing
    Parsed c d2 -> Parsed "." d1
    _ -> NoParse Nothing
  _ -> NoParse Nothing

pDecimalConst :: Derivs -> Result Int
pDecimalConst d = case pDecimalConstContent d of 
  Parsed s d1 -> case length s > 0 of
    True -> Parsed (read s :: Int) d1
    False -> NoParse Nothing
  _ -> NoParse Nothing

pDecimalConstContent :: Derivs -> Result String
pDecimalConstContent d = case dvChar d of
  Parsed c d1 -> case isDigit c of
    True -> case dvDecimalConstContent d1 of
      Parsed [] d2 -> Parsed [c] d1
      Parsed s d2 -> Parsed (c:s) d2
      _ -> NoParse Nothing
    False -> Parsed [] d
  _ -> NoParse Nothing

pDoubleConst :: Derivs -> Result Double
pDoubleConst d = case dvDecimalConst d of
  Parsed dc d1 -> case dvDot d1 of
    Parsed _ d2 -> case dvDecimalConstContent d2 of
      Parsed s d3 -> case length s > 0 of
        True -> Parsed ((fromIntegral dc :: Double) + ((read s :: Double) / (10^(length s)))) d3
        False -> Parsed (fromIntegral dc :: Double) d3
    _ -> NoParse Nothing
  _ -> case dvDot d of
    Parsed _ d1 -> case dvDecimalConst d1 of
      Parsed dc d2 -> Parsed ((fromIntegral dc :: Double) / (10^(length (show dc)))) d2
      _ -> NoParse Nothing
    _ -> NoParse Nothing 

pLiteral :: Derivs -> Result Literal
pLiteral d = case dvDoubleConst d of
  Parsed dc d1 -> Parsed (LiteralFloat dc) d1
  _ -> case dvDecimalConst d of 
    Parsed dc d1 -> Parsed (LiteralInt dc) d1
    _ -> NoParse Nothing

isWhiteSpace :: Char -> Bool
isWhiteSpace x = elem x [' ', '\t', '\r', '\n']

pWhitespace :: Derivs -> Result ()
pWhitespace d = case dvChar d of
  Parsed '#' d1 -> case dvComment d1 of
    Parsed _ d2 -> pWhitespace d2
  Parsed c d1 ->
    if isWhiteSpace c
    then pWhitespace d1
    else Parsed () d
  _ -> Parsed () d

pComment :: Derivs -> Result ()
pComment d = case dvChar d of
  Parsed '\n' d1 -> Parsed () d1
  Parsed _ d1 -> pComment d1
  _ -> Parsed () d

isSpecialChar :: Char -> Bool
isSpecialChar c = (not $ isAlphaNum c) && (not $ isWhiteSpace c) && (not $ elem c [';', ')', '(', ':', ',', '#'])

pOperator :: Derivs -> Result String
pOperator d = case dvWhitespace d of
  Parsed _ d1 -> case dvChar d1 of
    Parsed c d2 -> case isSpecialChar c of
      True -> case dvOperatorContent d2 of
        Parsed s d3 -> case dvWhitespace d3 of
          Parsed _ d4 -> Parsed (c:s) d4
        _ -> NoParse Nothing
      False -> NoParse Nothing
    _ -> NoParse Nothing

pOperatorContent :: Derivs -> Result String
pOperatorContent d = case dvChar d of
  Parsed c d1 -> case isSpecialChar c of
    True -> case dvOperatorContent d1 of
      Parsed [] d2 -> Parsed [c] d1
      Parsed s d2 -> Parsed (c:s) d2
      _ -> NoParse Nothing
    False -> Parsed [] d
  _ -> NoParse Nothing

pBinop :: Derivs -> Result UnOp
pBinop d = case dvOperator d of
  Parsed s d1 -> Parsed s d1
  _ -> NoParse Nothing

pUnop :: Derivs -> Result BinOp
pUnop d = case dvOperator d of
  Parsed s d1 -> Parsed s d1
  _ -> NoParse Nothing