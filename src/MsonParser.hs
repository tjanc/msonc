module MsonParser
    ( MsonParser,
      msonParse,
      aMsonTypeDecl,
      aMsonSymbolName,
      aMsonPropertyDecl,
      aMsonValueDecl,
      aMsonPropertyName,
      aMsonValue,
    ) where

import Mson
import Text.Parsec hiding (State, many, (<|>))
import Text.Parsec.Indent
import Control.Applicative
import Control.Monad.State

type MsonParser a = IndentParser String () a

msonParse :: MsonParser a -> SourceName -> String -> Either ParseError a
msonParse aParser source_name input =
  runIndentParser (aParser <* eof) () source_name input

aMsonTypeDecl :: MsonParser TypeDecl
aMsonTypeDecl = do
  n <- (char '#' *> ws *> aMsonSymbolName <* ws)
  t <- optionMaybe (aMsonTypeDef <* ws)
  spaces
  string "## Properties"
  spaces
  ms <- (many1 aMsonPropertyDecl)
  return (TypeDecl n t ms)

aMsonPropertyDecl :: MsonParser PropertyDecl
aMsonPropertyDecl = do
  b <- withBlock PropertyDecl aMsonValueDecl aMsonPropertyDecl
  return b

aMsonValueDecl :: MsonParser ValueDecl
aMsonValueDecl = do
  listSymbol
  ws
  n <- aMsonPropertyName
  v <- optionMaybe (aColumnSeparator *> aMsonValue <* ws)
  t <- optionMaybe (aMsonTypeDef <* ws)
  d <- optionMaybe (aDescSeparator *> ws *> aMsonValueDesc <* ws)
  spaces
  return (ValueDecl n v t d)

aMsonPropertyName :: MsonParser PropertyName
aMsonPropertyName = PropertyName <$> (nameLiteral <* ws)

aMsonValue :: MsonParser Value
aMsonValue = Value <$> ((many1 alphaNum) <* ws)

aMsonTypeDef :: MsonParser TypeRef
aMsonTypeDef = TypeRef <$> ((char '(') *> (many1 alphaNum) <* (char ')'))

aMsonValueDesc :: MsonParser Desc
aMsonValueDesc = Desc <$> (many1 alphaNum)

aMsonSymbolName :: MsonParser SymbolName
aMsonSymbolName = SymbolName <$> (many1 alphaNum)

--------------------------------------------------------------------------------

ws :: MsonParser String
ws = many (oneOf " \t")

aColumnSeparator :: MsonParser ()
aColumnSeparator = do
  char ':'
  ws
  return ()

aDescSeparator :: MsonParser ()
aDescSeparator = do
  char '-'
  ws
  return ()

nameLiteral :: MsonParser String
nameLiteral = choice [enclosedName, openName]

enclosedName :: MsonParser String
enclosedName = char '`' *> (many1 (noneOf "`")) <* char '`'

openName :: MsonParser String
openName = many1 (alphaNum <|> char '_')

listSymbol :: MsonParser Char
listSymbol = oneOf "-+*"

