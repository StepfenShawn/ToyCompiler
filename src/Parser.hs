module Parser where
    import Text.Parsec
    import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr as Exp
import qualified Text.Parsec.Token as Tok

import Lexer
import Syntax

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