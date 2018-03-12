module Parser (
  parseFile
  ) where

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Expr
import qualified Text.Megaparsec.Char.Lexer as L
import System.Environment
import Control.Monad (void)
import Data.Void
import Data.Maybe (fromMaybe)

import Problem

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

list :: Parser a -> Parser [a]
list x = sepBy1 x (symbol ",")

identifier :: Parser String
identifier = (lexeme . try) ((:) <$> letterChar <*> many alphaNumChar)

variable :: Parser String
variable = (lexeme . try) ((:) <$> upperChar <*> many alphaNumChar)

propConstPos :: Parser (Prop String)
propConstPos = do
  name <- variable
  args <- optional (parens (list identifier))
  return $ Prop True name (fromMaybe [] args)

propTerm :: Parser (Prop Term)
propTerm = do
  not <- optional (symbol "not")
  let polarity = case not of
        Nothing -> True
        Just _ -> False
  name <- variable
  args <- optional (parens (list term))
  return $ Prop polarity name (fromMaybe [] args)

term :: Parser Term
term =    (variable   >>= (return.VarTerm)) 
      <|> (identifier >>= (return.ConstTerm))

domain :: Parser Domain
domain = do
  symbol "domain"
  name <- variable
  values <- list identifier
  return $ Domain name values

preds :: Parser [Predicate]
preds = do
  symbol "pred"
  list $ do
    name <- variable
    argdomains <- optional (parens (list variable))
    return $ Predicate name (fromMaybe [] argdomains)

p :: Parser Problem
p = do
  d <- many domain
  p <- preds
  symbol "initial"
  init <- list propConstPos
  symbol "goal"
  goal <- list propConstPos
  a <- many action
  return $ Problem d p init goal a

action :: Parser Action
action = do
  symbol "action"
  n <- identifier
  a <- optional (parens (list variable))
  let args = fromMaybe [] a
  symbol "from"
  from <- list propTerm
  symbol "to"
  to <- list propTerm
  return $ Action n args from to

parseFile :: String -> IO (Either String Problem)
parseFile fn = do
  contents <- readFile fn
  return (problemparse fn contents)

parseStdin :: IO (Either String Problem)
parseStdin = do
  contents <- getContents
  return (problemparse "<stdin>" contents)

problemparse :: String -> String -> Either String Problem
problemparse name contents = case parse (sc >> p <* eof) name contents of
  (Left err) -> Left $ parseErrorPretty err
  (Right p) -> Right $ p

