module KoakPackrat where

import Data.Char

{--
stmt                := kdefs* #eof
kdefs               := ( 'def' defs | expressions ) ';'
defStr              := 'def'
defs                := prototype expressions
prototype           := prototype_start prototype_args
prototype_start     := ( unaryStr . decimal_const?
                       | binaryStr . decimal_const?
                       | identifier )
unaryStr            := 'unary'
binaryStr           := 'binary'
prototype_args      := '(' args_list ')' ':' type
args_list           := (identifier ':' type)*
type                := intStr | doubleStr | voidStr

intStr              := 'int'
doubleStr           := 'double'
voidStr             := 'void1

expressions         := for_expr
                     | if_expr
                     | while_expr
                     | expression exprConcat
exprConcat          := (':' expression)*
for_expr            := forStr identifier '=' expression ','
                       identifier '<' expression ','
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

expression          := unary binaryOperations
binaryOperations    := (#binop (unary | expression))*
unary               := #unop unary | postfix
postfix             := primary call_expr?
call_expr           := '(' (expression exprList)? ')'
exprList            := (',' expression)*
primary             := identifier
                     | literal
                     | '(' expression ')'
identifier          := [a-zA-Z] identifierContent 
identifierContent   := [a-zA-Z0-9]*
dot                 := '.' !'.'
decimal_const       := [0-9]+
double_const        := (decimal_const dot [0-9]* | dot decimal_const)
literal             := decimal_const | double_const
whitespace          := ' ' | '\n' | '\t'
#unop               := '!' | '-'
#binop              := '*' | '/' | '+' | '-' | '<' | '>' | '==' | '!=' | '='
char                := .
--}

test = "test;"

data Type = Int | Double | Void
data BinOp = Not | Minus
data UnOp = Multiplication | Division | Addition | Substraction | LessThan | GreaterThan | Equal | NotEqual | Assignment

data Stmt = Stmt {
  kdefs           :: [Kdefs]
}

data Kdefs = Defs {
  prototype       :: Prototype,
  defExpressions  :: Expressions
} | KExpressions Expressions

data Prototype = Prototype {
  name            :: String, -- TODO: Change?
  args            :: [(Primary, Type)],
  return          :: Type
}

data Expressions = ForExpr {
  init            :: (Primary, Expression),
  condition       :: (Primary, Expression),
  increment       :: Expression,
  expressions     :: Expressions
} | IfExpr {
  conditionIf     :: Expression,
  expressions     :: Expressions,
  elseExpressions :: Maybe Expressions
} | WhileExpr {
  conditionWhile  :: Expression,
  expressions     :: Expressions
} | Expr [Expression]

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

data Primary = Identifier String | PrimaryLiteral Literal | PrimaryExpressions Expressions

data Literal = LiteralInt Int | LiteralFloat Float

{--                   --}

instance Show Stmt where
  show (Stmt kdefs) = "{stmt: {kdefs: " ++ show kdefs ++ "}}"

instance Show Kdefs where
  show (Defs proto exprs) = "{defs: {prototype: " ++ show proto ++ ", defExpressions: " ++ show exprs ++ "}}"
  show (KExpressions exprs) = "{KExpressions: " ++ show exprs ++ "}"

instance Show Prototype where
  show (Prototype name args return) = "{prototype: {name: " ++ show name ++ ", args: " ++ show args ++ ", return: " ++ show return ++ "}}"

instance Show Expressions where
  show (ForExpr init condition increment expressions) = "{ForExpr: {init: " ++ show init ++ ", condition: " ++ show condition ++ ", increment: " ++ show increment ++ ", expressions: " ++ show expressions ++ "}}"
  show (IfExpr cond exprs elseExprs) = "{IfExpr: {condition: " ++ show cond ++ ", expressions: " ++ show exprs ++ ", elseExpressions: " ++ show elseExprs ++ "}}"
  show (WhileExpr cond exprs) = "{WhileExpr: {condition: " ++ show cond ++ ", expressions: " ++ show exprs ++ "}}"
  show (Expr exprs) = "{expressions: " ++ show exprs ++ "}"

instance Show Expression where
  show (Expression exprUnary binops) = "{expression: {exprUnary: " ++ show exprUnary ++ ", binaryOps: " ++ show binops ++ "}}"
instance Show BinaryOperation where
  show (BinaryOperation binop unaryExpr) = "{binaryOperation: {binop: " ++ show binop ++ ", unaryExpr: " ++ show unaryExpr ++ "}}"

instance Show UnaryExpr where
  show (UnaryExprUnary unary) = "{unaryExpr: " ++ show unary ++ "}"
  show (UnaryExprExpression expr) = "{unaryExpr: " ++ show expr ++ "}"

instance Show UnaryPostfix where
  show (Unary op unary) = "{unary: {op: " ++ show op ++ ", unary: " ++ show unary ++ "}}"
  show (Postfix primary callExpr) = "{postfix: {primary: " ++ show primary ++ ", callExpr: " ++ show callExpr ++ "}}"

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

instance Show BinOp where
  show Not = "not"
  show Minus = "minus"

instance Show UnOp where
  show Multiplication = "multiplication"
  show Division = "division"
  show Addition = "addition"
  show Substraction = "substraction"
  show LessThan = "lessThan"
  show GreaterThan = "greaterThan"
  show Equal = "equal"
  show NotEqual = "notEqual"
  show Assignment = "assignment"

{--                   --}

data Result v = Parsed v Derivs
              | NoParse

data Derivs = Derivs {
  dvStmt                :: Result [Kdefs],
  dvKdefs               :: Result Kdefs,
  dvDefStr              :: Result String,
  dvDefs                :: Result Kdefs,
  dvPrototype           :: Result Prototype,
  dvPrototypeStart      :: Result String,
  dvUnaryStr            :: Result String,
  dvBinaryStr           :: Result String,
  dvPrototypeArgs       :: Result ([(Primary, Type)], Type),
  dvArgsList            :: Result [(Primary, Type)],
  dvType                :: Result Type,
  dvIntStr              :: Result String,
  dvDoubleStr           :: Result String,
  dvVoidStr             :: Result String,
  dvExpressions         :: Result Expressions,
  dvExprConcat          :: Result [Expression],
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
  dvDoubleConst         :: Result Float,
  dvLiteral             :: Result Literal,
  dvWhitespace          :: Result (),
  dvUnop                :: Result UnOp,
  dvBinop               :: Result BinOp,
  dvChar                :: Result Char
}

{--                   --}

eval :: String -> Maybe Stmt
eval s = case dvStmt (parse s) of
  Parsed v rem -> (Just (Stmt v))
  _ -> Nothing

parse :: String -> Derivs
parse s = d where
    d             = Derivs stmt kdefs defStr defs prototype prototypeS unaryStr binaryStr prototypeArgs argsList types intStr doubleStr voidStr expressions exprConcat forExpr ifExpr elseExpr whileExpr forStr inStr ifStr thenStr elseStr whileStr doStr expression binaryOps unary postFix callExpr exprList primary identifier identifierC dot decimalConst decimalConstC doubleConst literal whitespace unop binop char
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
    intStr        = pIntStr d
    doubleStr     = pDoubleStr d
    voidStr       = pVoidStr d
    expressions   = pExpressions d
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
    unop          = pUnop d
    binop         = pBinop d
    char          = case s of
                    (c:s') -> Parsed c (parse s')
                    [] -> NoParse

{--                   --}

pStmt :: Derivs -> Result [Kdefs]
pStmt d = case dvKdefs d of
  Parsed kdef d1 -> case dvChar d1 of
    NoParse -> Parsed [kdef] d1
    _ -> case dvStmt d1 of
      Parsed stmt d2 -> Parsed (kdef:stmt) d2
      NoParse -> NoParse
  _ -> NoParse


pKdefs :: Derivs -> Result Kdefs
pKdefs d = case dvDefStr d of
  Parsed _ d1 -> case dvDefs d1 of
    Parsed defs d2 -> case dvChar d2 of
      Parsed ';' d3 -> Parsed defs d3
    _ -> NoParse
  _ -> case dvExpressions d of
    Parsed exprs d1 -> case dvChar d1 of
      Parsed ';' d2 -> Parsed (KExpressions exprs) d2
      _ -> NoParse
    _ -> NoParse

pDefStr :: Derivs -> Result String
pDefStr d = case dvChar d of
  Parsed 'd' d1 -> case dvChar d1 of
    Parsed 'e' d2 -> case dvChar d2 of
      Parsed 'f' d3 -> Parsed "def" d3
      _ -> NoParse
    _ -> NoParse
  _ -> NoParse

pDefs :: Derivs -> Result Kdefs
pDefs d = case dvPrototype d of
  Parsed proto d1 -> case dvExpressions d1 of
    Parsed exprs d2 -> Parsed (Defs proto exprs) d2
    _ -> NoParse
  _ -> NoParse

pPrototype :: Derivs -> Result Prototype
pPrototype d = case dvPrototypeStart d of
  Parsed proto d1 -> case dvPrototypeArgs d1 of
    Parsed (args, return) d2 -> Parsed (Prototype proto args return) d2
    _ -> NoParse
  _ -> NoParse

-- TODO: Weird ?
pPrototypeStart :: Derivs -> Result String
pPrototypeStart d = case dvUnaryStr d of
  Parsed un d1 -> case dvDecimalConst d1 of
    Parsed dc d2 -> Parsed ((show un) ++ (show dc)) d2
    _ -> NoParse
  _ -> case dvBinaryStr d of
    Parsed bn d1 -> case dvDecimalConst d1 of
      Parsed dc d2 -> Parsed ((show bn) ++ (show dc)) d2
      _ -> NoParse
    _ -> case dvIdentifier d of
      Parsed (Identifier id) d1 -> Parsed id d1
      _ -> NoParse

pUnaryStr :: Derivs -> Result String
pUnaryStr d = case dvChar d of
  Parsed 'u' d1 -> case dvChar d1 of
    Parsed 'n' d2 -> case dvChar d2 of
      Parsed 'a' d3 -> case dvChar d3 of
        Parsed 'r' d4 -> case dvChar d4 of
          Parsed 'y' d5 -> Parsed "unary" d5
          _ -> NoParse
        _ -> NoParse
      _ -> NoParse
    _ -> NoParse
  _ -> NoParse

pBinaryStr :: Derivs -> Result String
pBinaryStr d = case dvChar d of
  Parsed 'b' d1 -> case dvChar d1 of
    Parsed 'i' d2 -> case dvChar d2 of
      Parsed 'n' d3 -> case dvChar d3 of
        Parsed 'a' d4 -> case dvChar d4 of
          Parsed 'r' d5 -> case dvChar d5 of
            Parsed 'y' d6 -> Parsed "binary" d6
            _ -> NoParse
          _ -> NoParse
        _ -> NoParse
      _ -> NoParse
    _ -> NoParse
  _ -> NoParse 

pPrototypeArgs :: Derivs -> Result ([(Primary, Type)], Type)
pPrototypeArgs d = case dvChar d of
  Parsed '(' d1 -> case dvArgsList d1 of
    Parsed args d2 -> case dvChar d2 of
      Parsed ')' d3 -> case dvChar d3 of
        Parsed ':' d4 -> case dvType d4 of
          Parsed typ d5 -> Parsed (args, typ) d5
          _ -> NoParse
        _ -> NoParse
      _ -> NoParse
    _ -> NoParse
  _ -> NoParse

pArgsList :: Derivs -> Result [(Primary, Type)]
pArgsList d = case dvIdentifier d of
  Parsed id d1 -> case dvChar d1 of
    Parsed ':' d2 -> case dvType d2 of
      Parsed typ d3 -> case dvArgsList d3 of
        Parsed [] d4 -> Parsed [(id, typ)] d3
        Parsed lst d4 -> Parsed ((id, typ):lst) d4 
        _ -> NoParse
      _ -> NoParse
    _ -> NoParse
  _ -> Parsed [] d

pType :: Derivs -> Result Type
pType d = case dvIntStr d of
  Parsed _ d1 -> Parsed Int d1
  _ -> case dvDoubleStr d of
    Parsed _ d1 -> Parsed Double d1
    _ -> case dvVoidStr d of
      Parsed _ d1 -> Parsed Void d1
      _ -> NoParse

pIntStr :: Derivs -> Result String
pIntStr d = case dvChar d of
  Parsed 'i' d1 -> case dvChar d1 of
    Parsed 'n' d2 -> case dvChar d2 of
      Parsed 't' d3 -> Parsed "int" d3
      _ -> NoParse
    _ -> NoParse
  _ -> NoParse

pDoubleStr :: Derivs -> Result String
pDoubleStr d = case dvChar d of
  Parsed 'd' d1 -> case dvChar d1 of
    Parsed 'o' d2 -> case dvChar d2 of
      Parsed 'u' d3 -> case dvChar d3 of
        Parsed 'b' d4 -> case dvChar d4 of
          Parsed 'l' d5 -> case dvChar d5 of
            Parsed 'e' d6 -> Parsed "double" d6
            _ -> NoParse
          _ -> NoParse
        _ -> NoParse
      _ -> NoParse
    _ -> NoParse
  _ -> NoParse 

pVoidStr :: Derivs -> Result String
pVoidStr d = case dvChar d of
  Parsed 'v' d1 -> case dvChar d1 of
    Parsed 'o' d2 -> case dvChar d2 of
      Parsed 'i' d3 -> case dvChar d3 of
        Parsed 'd' d4 -> Parsed "void" d4
        _ -> NoParse
      _ -> NoParse
    _ -> NoParse
  _ -> NoParse 

pExpressions :: Derivs -> Result Expressions
pExpressions d = case dvForExpr d of
  Parsed expr d1 -> Parsed expr d1
  _ -> case dvIfExpr d of
    Parsed expr d1 -> Parsed expr d1
    _ -> case dvWhileExpr d of
      Parsed expr d1 -> Parsed expr d1
      _ -> case dvExpression d of
        Parsed expr d1 -> case dvExprConcat d1 of
          Parsed exprs d2 -> Parsed (Expr (expr:exprs)) d2
          _ -> NoParse
        _ -> NoParse
        
pExprConcat :: Derivs -> Result [Expression]
pExprConcat d = case dvExpression d of
  Parsed ex d1 -> case dvChar d1 of
    Parsed ':' d2 -> case dvExprConcat d2 of
      Parsed [] d3 -> NoParse
      Parsed exs d3 -> Parsed (ex:exs) d3
    NoParse -> NoParse
    _ -> Parsed [ex] d1
  _ -> Parsed [] d --TODO: To Change

pForExpr :: Derivs -> Result Expressions
pForExpr d = case dvForStr d of
  Parsed _ d1 -> case dvIdentifier d1 of
    Parsed id d2 -> case dvChar d2 of
      Parsed '=' d3 -> case dvExpression d3 of
        Parsed init d4 -> case dvChar d4 of
          Parsed ',' d5 -> case dvIdentifier d5 of
            Parsed id2 d6 -> case dvChar d6 of
              Parsed '<' d7 -> case dvExpression d7 of
                Parsed cond d8 -> case dvChar d8 of
                  Parsed ',' d9 -> case dvExpression d9 of
                    Parsed increment d10 -> case dvInStr d10 of
                      Parsed _ d11 -> case dvExpressions d11 of
                        Parsed exprs d12 -> Parsed (ForExpr (id, init) (id2, cond) increment exprs) d12
                        _ -> NoParse
                      _ -> NoParse
                    _ -> NoParse
                  _ -> NoParse
                _ -> NoParse
              _ -> NoParse
            _ -> NoParse
          _ -> NoParse
        _ -> NoParse
      _ -> NoParse
    _ -> NoParse
  _ -> NoParse

pIfExpr :: Derivs -> Result Expressions
pIfExpr d = case dvIfStr d of
  Parsed _ d1 -> case dvExpression d1 of
    Parsed cond d2 -> case dvThenStr d2 of
      Parsed _ d3 -> case dvExpressions d3 of
        Parsed exprs d4 -> case dvElseExpr d4 of
          Parsed elseExprs d5 -> Parsed (IfExpr cond exprs elseExprs) d5
          _ -> NoParse
        _ -> NoParse
      _ -> NoParse
    _ -> NoParse
  _ -> NoParse

-- TODO: To Change
pElseExpr :: Derivs -> Result (Maybe Expressions)
pElseExpr d = case dvElseStr d of
  Parsed _ d1 -> case dvExpressions d1 of
    Parsed exprs d2 -> Parsed (Just exprs) d2
    _ -> NoParse
  _ -> Parsed Nothing d

pWhileExpr :: Derivs -> Result Expressions
pWhileExpr d = case dvWhileStr d of
  Parsed _ d1 -> case dvExpression d1 of
    Parsed cond d2 -> case dvDoStr d2 of
      Parsed _ d3 -> case dvExpressions d3 of
        Parsed exprs d4 -> Parsed (WhileExpr cond exprs) d4
        _ -> NoParse
      _ -> NoParse
    _ -> NoParse
  _ -> NoParse

pForStr :: Derivs -> Result String
pForStr d = case dvChar d of
  Parsed 'f' d1 -> case dvChar d1 of
    Parsed 'o' d2 -> case dvChar d2 of
      Parsed 'r' d3 -> Parsed "for" d3
      _ -> NoParse
    _ -> NoParse
  _ -> NoParse

pInStr :: Derivs -> Result String
pInStr d = case dvChar d of
  Parsed 'i' d1 -> case dvChar d1 of
    Parsed 'n' d2 -> Parsed "in" d2
    _ -> NoParse
  _ -> NoParse

pIfStr :: Derivs -> Result String
pIfStr d = case dvChar d of
  Parsed 'i' d1 -> case dvChar d1 of
    Parsed 'f' d2 -> Parsed "if" d2
    _ -> NoParse
  _ -> NoParse

pThenStr :: Derivs -> Result String
pThenStr d = case dvChar d of
  Parsed 't' d1 -> case dvChar d1 of
    Parsed 'h' d2 -> case dvChar d2 of
      Parsed 'e' d3 -> case dvChar d3 of
        Parsed 'n' d4 -> Parsed "then" d4
        _ -> NoParse
      _ -> NoParse
    _ -> NoParse
  _ -> NoParse 

pElseStr :: Derivs -> Result String
pElseStr d = case dvChar d of
  Parsed 'e' d1 -> case dvChar d1 of
    Parsed 'l' d2 -> case dvChar d2 of
      Parsed 's' d3 -> case dvChar d3 of
        Parsed 'e' d4 -> Parsed "else" d4
        _ -> NoParse
      _ -> NoParse
    _ -> NoParse
  _ -> NoParse

pWhileStr :: Derivs -> Result String
pWhileStr d = case dvChar d of
  Parsed 'w' d1 -> case dvChar d1 of
    Parsed 'h' d2 -> case dvChar d2 of
      Parsed 'i' d3 -> case dvChar d3 of
        Parsed 'l' d4 -> case dvChar d4 of
          Parsed 'e' d5 -> Parsed "while" d5
          _ -> NoParse
        _ -> NoParse
      _ -> NoParse
    _ -> NoParse
  _ -> NoParse

pDoStr :: Derivs -> Result String
pDoStr d = case dvChar d of
  Parsed 'd' d1 -> case dvChar d1 of
    Parsed 'o' d2 -> Parsed "do" d2
    _ -> NoParse
  _ -> NoParse

pExpression :: Derivs -> Result Expression
pExpression d = case dvUnary d of
  Parsed un d' -> case dvBinaryOperations d' of
    Parsed bo d2 -> Parsed (Expression un bo) d2
    NoParse -> NoParse
  _ -> NoParse

pBinaryOperations :: Derivs -> Result [BinaryOperation]
pBinaryOperations d = case dvBinop d of
  Parsed bn d1 -> case dvUnary d1 of
    Parsed un d2 -> case dvBinaryOperations d2 of
      Parsed bops d3 -> Parsed ((BinaryOperation bn (UnaryExprUnary un)):bops) d3
      _ -> NoParse
    _ -> case dvExpression d1 of
      Parsed expr d2 -> case dvBinaryOperations d2 of
        Parsed bops d3 -> Parsed ((BinaryOperation bn (UnaryExprExpression expr)):bops) d3
        _ -> NoParse
      _ -> NoParse
  _ -> Parsed [] d

pUnary :: Derivs -> Result UnaryPostfix
pUnary d = case dvUnop d of
  Parsed unp d1 -> case dvUnary d1 of
    Parsed un d2 -> Parsed (Unary unp un) d2
  _ -> case dvPostfix d of
    Parsed ps d1 -> Parsed ps d1
    _ -> NoParse

pPostfix :: Derivs -> Result UnaryPostfix
pPostfix d = case dvPrimary d of
  Parsed pr d1 -> case dvCallExpr d1 of
    Parsed ce d2 -> Parsed (Postfix pr (Just ce)) d2
    _ -> Parsed (Postfix pr Nothing) d1
  _ -> NoParse

pCallExpr :: Derivs -> Result [Expression]
pCallExpr d = case dvChar d of
  Parsed '(' d1 -> case dvExprList d1 of
    Parsed ex d2 -> case dvChar d2 of
      Parsed '(' d3 -> Parsed ex d3
      _ -> NoParse
    _ -> NoParse
  _ -> NoParse

pExprList :: Derivs -> Result [Expression]
pExprList d = case dvExpression d of
  Parsed ex d1 -> case dvChar d1 of
    Parsed ',' d2 -> case dvExprList d2 of
      Parsed [] d3 -> NoParse
      Parsed exs d3 -> Parsed (ex:exs) d3
    NoParse -> NoParse
    _ -> Parsed [ex] d1
  _ -> Parsed [] d --TODO: To Change


pPrimary :: Derivs -> Result Primary
pPrimary d = case dvIdentifier d of
  Parsed id d1 -> Parsed id d1
  _ -> case dvLiteral d of
    Parsed li d1 -> Parsed (PrimaryLiteral li) d1
    _ -> case dvChar d of
      Parsed '(' d1 -> case dvExpressions d1 of
        Parsed expr d2 -> case dvChar d2 of
          Parsed ')' d3 -> Parsed (PrimaryExpressions expr) d3
          _ -> NoParse
        _ -> NoParse
      _ -> NoParse

pIdentifier :: Derivs -> Result Primary
pIdentifier d = case dvChar d of
  Parsed c d1 -> case isLetter c of
    True -> case dvIdentifierContent d1 of
      Parsed s d2 -> Parsed (Identifier (c:s)) d2
      _ -> NoParse
    False -> NoParse
  _ -> NoParse

pIdentifierContent :: Derivs -> Result String
pIdentifierContent d = case dvChar d of
  Parsed c d1 -> case isAlphaNum c of
    True -> case dvIdentifierContent d1 of
      Parsed [] d2 -> Parsed [c] d1
      Parsed s d2 -> Parsed (c:s) d2
      _ -> NoParse
    False -> Parsed [] d1
  _ -> NoParse

pDot :: Derivs -> Result String
pDot d = case dvChar d of
  Parsed '.' d1 -> case dvChar d1 of
    Parsed '.' _ -> NoParse
    Parsed c d2 -> Parsed "." d2
    _ -> NoParse
  _ -> NoParse

pDecimalConst :: Derivs -> Result Int
pDecimalConst d = case pDecimalConstContent d of 
  Parsed s d1 -> case length s > 0 of
    True -> Parsed (read s :: Int) d1
    False -> NoParse
  _ -> NoParse

pDecimalConstContent :: Derivs -> Result String
pDecimalConstContent d = case dvChar d of
  Parsed c d1 -> case isDigit c of
    True -> case dvDecimalConstContent d1 of
      Parsed [] d2 -> Parsed [c] d1
      Parsed s d2 -> Parsed (c:s) d2
      _ -> NoParse
    False -> Parsed [] d1
  _ -> NoParse

pDoubleConst :: Derivs -> Result Float
pDoubleConst d = case dvDecimalConst d of
  Parsed dc d1 -> case dvDot d1 of
    Parsed _ d2 -> case dvDecimalConstContent d2 of
      Parsed s d3 -> case length s > 0 of
        True -> Parsed ((fromIntegral dc :: Float) + ((read s :: Float) / (10^(length s)))) d3
        False -> Parsed (fromIntegral dc :: Float) d3
    _ -> NoParse
  _ -> case dvDot d of
    Parsed _ d1 -> case dvDecimalConst d1 of
      Parsed dc d2 -> Parsed ((fromIntegral dc :: Float) / (10^(length (show dc)))) d2
      _ -> NoParse
    _ -> NoParse

-- TODO: Change ?
pLiteral :: Derivs -> Result Literal
pLiteral d = case dvDoubleConst d of
  Parsed dc d1 -> Parsed (LiteralFloat dc) d1
  _ -> case dvDecimalConst d of 
    Parsed dc d1 -> Parsed (LiteralInt dc) d1
    _ -> NoParse

isWhiteSpace :: Char -> Bool
isWhiteSpace x = elem x [' ', '\t', '\r', '\n']

pWhitespace :: Derivs -> Result ()
pWhitespace d = case dvChar d of
      Parsed c d1 ->
        if isWhiteSpace c
        then pWhitespace d1
        else Parsed () d
      _ -> Parsed () d

pUnop :: Derivs -> Result UnOp
pUnop d = case dvChar d of
  Parsed '*' d1 -> Parsed Multiplication d1
  Parsed '/' d1 -> Parsed Division d1
  Parsed '+' d1 -> Parsed Addition d1
  Parsed '-' d1 -> Parsed Substraction d1
  Parsed '<' d1 -> Parsed LessThan d1
  Parsed '>' d1 -> Parsed GreaterThan d1
  Parsed '=' d1 -> case dvChar d1 of
    Parsed '=' d2 -> Parsed Equal d2
    NoParse -> NoParse
    _ -> Parsed Assignment d1
  Parsed '!' d1 -> case dvChar d1 of
    Parsed '=' d2 -> Parsed NotEqual d2
    _ -> NoParse
  _ -> NoParse

pBinop :: Derivs -> Result BinOp
pBinop d = case dvChar d of
  Parsed '!' d1 -> Parsed Not d1
  Parsed '-' d1 -> Parsed Minus d1
  _ -> NoParse