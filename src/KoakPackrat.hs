module KoakPackrat where

import Data.Char

{--
stmt            := kdefs* #eof
kdefs           := ( 'def' defs | expressions ) ';'
defs            := prototype expressions
prototype       := ( 'unary' . decimal_const?
                   | 'binary' . decimal_const?
                   | identifier )
                   protype_args
prototype_args  := '(' (identifier ':' type)* ')' ':' type
type            := 'int' | 'double' | 'void'
expressions     := for_expr
                 | if_expr
                 | while_expr
                 | expression (':' expression)*
for_expr        := 'for' identifier '=' expression ','
                   identifier '<' expression ','
                   expression 'in' expressions
if_expr         := 'if' expression 'then' expressions ('else' expressions)?
while_expr      := 'while' expression 'do' expressions
eexpression      := unary (#binop (unary | expression))*
unary           := #unop unary | postfix
postfix         := primary call_expr?
call_expr       := '(' (expression (',' expression)*)? ')'
primary         := identifier
                 | literal
                 | '(' expression ')'
identifier      := [a-zA-Z][a-zA-Z0-9]*
dot             := '.' !'.'
decimal_const   := [0-9]+
double_const    := (decimal_const dot [0-9]* | dot decimal_const)
literal         := decimal_const | double_const
#binop          := '*' | '/' | '+' | '-' | '<' | '>' | '==' | '!=' | '='
#unop           := '!' | '-'
--}

data Type = Int | Double | Void
data UnOp = Not | Minus
data BinOp = Multiplication | Division | Addition | Substraction | LessThan | MoreThan | Equal | NotEqual | Assignment

data Stmt = {
  kdefs           :: [kdefs]
}

data Kdefs = Defs {
  prototype       :: Prototype
  expressions     :: Expressions
} | KExpressions Expressions

data Prototype = {
  name            :: String -- TODO: Change?
  args            :: [(String, Type)]
  return          :: Type
}

data Expressions = ForExpr {
  init            :: (String, Expression)
  condition       :: (String, Expression)
  increment       :: Expression
  expressions     :: Expressions
} | IfExpr {
  condition       :: Expression
  expressions     :: Expressions
  elseExpressions :: Maybe Expressions
} | WhileExpr {
  condition       :: Expression
  expressions     :: Expressions
} | Expr [Expression]

data Expression = {
  unary           :: Unary
  binaryOps       :: [BinaryOperation]
}

data BinaryOperation = {
  binop           :: BinOp
  unaryExpr       :: UnaryExpr
}

data UnaryExpr = Unary | Expression

data UnaryPostfix = Unary {
  op              :: UnOp
  unary           :: Unary
} | Postfix {
  primary         :: Primary
  callExpr        :: Maybe [Expression]
}

data Primary = Identifier String | Literal | Expressions

data Literal = Int | Float

{--                   --}

data Derivs = Derivs {
  dvStmt                :: Result Stmt,
  dvKdefs               :: Result Kdefs,
  dvDefs                :: Result Defs,
  dvPrototype           :: Result Prototype,
  dvPrototypeArgs       :: Result ([(String, Type)], Type),
  dvType                :: Result Type,
  dvExpressions         :: Result Expressions,
  dvForExpr             :: Result ForExpr,
  dvIfExpr              :: Result IfExpr,
  dvWhileExpr           :: Result WhileExpr,
  dvExpression          :: Result Expression,
  dvBinaryOperations    :: Result [BinaryOperation],
  dvUnary               :: Result UnaryPostfix,
  dvPostfix             :: Result Postfix,
  dvCallExpr            :: Result [Expression],
  dvExprList            :: Result [Expression],
  dvPrimary             :: Result Primary,
  dvIdentifier          :: Result Identifier,
  dvIdentifierContent   :: Result String,
  dvDot                 :: Result String,
  dvDecimalConst        :: Result Int,
  dvDecimalConstContent :: Result String,
  dvDoubleConst         :: Result Float,
  dvLiteral             :: Result Literal,
  dvWhitespace          :: Result (),
  dvUnop                :: Result UnOp,
  dvBinop               :: Result BinOp,
  dvChar                :: Result Char,
}

{--                   --}

eval :: String -> Maybe Stmt
eval s = case dvStmt (parse s) of
  Parsed v rem -> (Just v)
  _ -> Nothing

parse :: String -> Derivs
parse s = d where
    d             = Derivs stmt kdefs defs prototype prototypeArgs types expressions forExpr ifExpr whileExpr expression binaryOps unary postFix callExpr exprList primary identifier identifierC dot decimalConst decimalConstC doubleConst literal whitespace unop binop char
    stmt          = pStmt d
    kdefs         = pKdefs d
    defs          = pDefs d
    prototype     = pPrototype d
    prototypeArgs = pPrototypeArgs d
    types         = pType d
    expressions   = pExpressions d
    forExpr       = pForExpr d
    ifExpr        = pIfExpr d
    whileExpr     = pWhileExpr d
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

pStmt :: Derivs -> Result Stmt

pKdefs :: Derivs -> Result Kdefs

pDefs :: Derivs -> Result Defs

pPrototype :: Derivs -> Result Prototype

pPrototypeArgs :: Derivs -> Result ([(String, Type)], Type)

pType :: Derivs -> Result Type

pExpressions :: Derivs -> Result Expressions

pForExpr :: Derivs -> Result ForExpr

pIfExpr :: Derivs -> Result IfExpr

pWhileExpr :: Derivs -> Result WhileExpr

pExpression :: Derivs -> Result Expression
pExpression d = case dvUnary d of
  Parsed un d' -> case dvBinaryOperations d' of
    Parsed bo d'' -> Parsed (Expression un bo) d''
    NoParse -> NoParse
  _ -> NoParse

pBinaryOperations :: Derivs -> Result [BinaryOperation]
pBinaryOperations d = case dvBinop d of
  Parsed bn d' -> case dvUnary d' of
    Parsed un d'' -> case dvBinaryOperations d'' of
      Parsed bops d''' -> Parsed ((BinaryOperation bn un)) d'''
      Parsed [] d''' -> Parsed [(BinaryOperation bn un)] d''
      _ -> NoParse
    _ -> case dvExpression d' of
      Parsed expr d'' -> case dvBinaryOperations d'' of
        Parsed bops d''' -> Parsed ((BinaryOperation bn expr)) d'''
        Parsed [] d''' -> Parsed [(BinaryOperation bn expr)] d''
        _ -> NoParse
      _ -> NoParse
  _ -> Parsed [] d

pUnary :: Derivs -> Result UnaryPostfix
pUnary d = case dvUnop d of
  Parsed unp d' -> case dvUnary d' of
    Parsed un d'' -> Parsed (Unary unp un) d''
  _ -> case dvPostfix d of
    Parsed ps d' -> Parsed (Postfix ps) d'
    _ -> NoParse

pPostfix :: Derivs -> Result Postfix
pPostfix d = case dvPrimary d of
  Parsed pr d' -> case dvCallExpr d' of
    Parsed ce d'' -> Parsed (Postfix pr (Just ce)) d''
    _ -> Parsed (Postfix pr Nothing) d'
  _ -> NoParse

pCallExpr :: Derivs -> Result [Expression]
pCallExpr d = case dvChar d of
  Parsed '(' d' -> case dvExprList d' of
    Parsed ex d'' -> case dvChar d'' of
      Parsed '(' d''' -> Parsed ex d'''
      _ -> NoParse
    _ -> NoParse
  _ -> NoParse

pExprList :: Derivs -> Result [Expression]
pExprList = case dvExpression d of
  Parsed ex d' -> case dvChar d' of
    Parsed ',' d'' -> case dvExprList d'' of
      Parsed [] d''' -> NoParse
      Parsed exs d''' -> Parsed (ex:exs) d'''
    NoParse -> NoParse
    _ -> Parsed [ex] d'
  _ -> Parsed [] d --TODO: To Change


pPrimary :: Derivs -> Result Primary
pPrimary d = case dvIdentifier d of
  Parsed id d' -> Parsed (Identifier id) d'
  _ -> case dvLiteral d of
    Parsed li d' -> Parsed (Literal li) d'
    _ -> case dvChar d of
      Parsed '(' d' -> case dvExpressions d' of
        Parsed expr d'' -> case dvChar d'' of
          Parsed ')' d''' -> Parsed (Expressions expr) d'''
          _ -> NoParse
        _ -> NoParse
      _ -> NoParse

pIdentifier :: Derivs -> Result Identifier
pIdentifier d = case dvChar d of
  Parsed c d' -> case isLetter c of
    True -> case dvIdentifierContent d' of
      Parsed s d'' -> Parsed (c:s) d''
      _ -> NoParse
    False -> NoParse
  _ -> NoParse

pIdentifierContent :: Derivs -> Result String
pIdentifierContent d = case dvChar d of
  Parsed c d' -> case isAlphaNum c of
    True -> case dvIdentifierContent d' of
      Parsed [] d'' -> Parsed [c] d'
      Parsed s d'' -> Parsed (c:s) d''
      _ -> NoParse
    False -> Parsed [] d'
  _ -> NoParse

pDot :: Derivs -> Result String
pDot d = case dvChar d of
  Parsed '.' d' -> case dvChar d' of
    Parsed '.' _ -> NoParse
    Parsed c d'' -> Parsed "." d''
    _ -> NoParse
  _ -> NoParse

pDecimalConst :: Derivs -> Result Int
pDecimalConst d = case pDecimalConstContent d of 
  Parsed s d' -> case length s > 0 of
    True -> Parsed (read c :: Int) d'
    False -> NoParse
  _ -> NoParse

pDecimalConstContent :: Derivs -> Result String
pDecimalConstContent d = case dvChar d of
  Parsed c d' -> case isDigit c of
    True -> case dvDecimalConstContent d' of
      Parsed [] d'' -> Parsed [c] d'
      Parsed s d'' -> Parsed (c:s) d''
      _ -> NoParse
    False -> Parsed [] d'
  _ -> NoParse

pDoubleConst :: Derivs -> Result Float
pDoubleConst d = case dvDecimalConst d of
  Parsed dc d' -> case dvDot d' of
    Parsed _ d'' -> case dvDecimalConstContent d'' of
      Parsed s d''' -> case length s > 0 of
        True -> Parsed (dc + ((read s :: Float) / (10^(length s)))) d'''
        False -> Parsed (fromInteger dc :: Float) d'''
    _ -> NoParse
  _ -> case dvDot d of
    Parsed _ d' -> case dvDecimalConst d' of
      Parsed dc d'' -> Parsed ((fromInteger dc :: Float) / (10^(length (show dc)))) d''
      _ -> NoParse
    _ -> NoParse

-- TODO: Change ?
pLiteral :: Derivs -> Result Literal
pLiteral d = case dvDoubleConst d of
  Parsed dc d' -> Parsed (Literal dc) d'
  _ -> case dvDecimalConst d of 
    Parsed dc d' -> Parsed (Literal dc) d'
    _ -> NoParse

isWhiteSpace :: Char -> Bool
isWhiteSpace x = elem x [' ', '\t', '\r', '\n']

pWhitespace :: Derivs -> Result ()
pWhitespace d = case dvChar d of
      Parsed c d' ->
        if isWhiteSpace c
        then pWhitespace d'
        else Parsed () d
      _ -> Parsed () d

pUnop :: Derivs -> Result UnOp
pUnop d = case dvChar d of
  Parsed '*' d' -> Parsed Multiplication d'
  Parsed '/' d' -> Parsed Division d'
  Parsed '+' d' -> Parsed Addition d'
  Parsed '-' d' -> Parsed Substraction d'
  Parsed '<' d' -> Parsed LessThan d'
  Parsed '>' d' -> Parsed GreaterThan d'
  Parsed '=' d' -> case dvChar d' of
    Parsed '=' d'' -> Parsed Equal d''
    NoParse -> NoParse
    _ -> Parsed Assignment d'
  Parsed '!' d' -> case dvChar d' of
    Parsed '=' d'' -> Parsed NotEqual d''
    _ -> NoParse
  _ -> NoParse

pBinop :: Derivs -> Result BinOp
pBinop d = case dvChar d of
  Parsed '!' d' = Parsed Not d'
  Parsed '-' d' = Parsed Minus d'
  _ -> NoParse