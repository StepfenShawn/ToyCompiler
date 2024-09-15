module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr as Exp
import qualified Text.Parsec.Token as Tok

import Lexer
import Syntax

-- 优先级binding
binary s f assoc = Exp.Infix (reservedOp s >> return (BinOp f)) assoc

table = [[binary "*" Times Exp.AssocLeft,
          binary "/" Divide Exp.AssocLeft],
          [binary "+" Plus Exp.AssocLeft,
           binary "-" Minus Exp.AssocLeft]]

expr :: Parser Expr
expr = Exp.buildExpressionParser table factor


int :: Parser Expr
int = do
    n <- integer
    return $ Float (fromInteger n)

floating :: Parser Expr
floating = do
    n <- float
    return $ Float n

variable :: Parser Expr
variable = do
    var <- identifier
    return $ Var var

function :: Parser Expr
function = do
    reserved "fn"
    name <- identifier
    args <- parens $ many variable
    body <- expr
    return $ Function name args body

call :: Parser Expr
call = do
    name <- identifier
    args <- parens $ commaSep expr
    return $ Call name args

factor :: Parser Expr
factor = try floating
        <|> try int
        <|> try function
        <|> try call
        <|> variable
        <|> parens expr

defn :: Parser Expr
defn = try function
    <|> expr

contents :: Parser a -> Parser a
contents p = do
    Tok.whiteSpace lexer
    r <- p
    eof
    return r

topleval :: Parser [Expr]
topleval = many $ do
    def <- defn
    reservedOp ";"
    return def

parseExpr :: String -> Either ParseError Expr
parseExpr s = parse (contents expr) "<stdin>" s

parseToplevel :: String -> Either ParseError [Expr]
parseToplevel s = parse (contents topleval) "<stdin>" s
