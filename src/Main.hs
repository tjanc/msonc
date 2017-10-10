module Main where

import Text.Parsec hiding (State, many, (<|>))
import Text.Parsec.Indent
import Control.Applicative
import Control.Monad.State
import System.Environment
import System.IO

type IParser a = IndentParser String () a

iParse :: IParser a -> SourceName -> String -> Either ParseError a
iParse aParser source_name input =
  runIndentParser aParser () source_name input

main :: IO ()
main = do
  args <- getArgs
  let msonFileName = head args
  contents <- readFile msonFileName
  case iParse (aMsonTypeDecl <* eof) msonFileName contents of
    Left  err    -> hPutStrLn stderr (show err)
    Right result -> putStrLn $ show result


data MSONTypeDecl = MSONTypeDecl MSONSymbolName (Maybe MSONTypeRef) [MSONPropertyDecl]
  deriving (Show)

data MSONSymbolName = MSONSymbolName String
  deriving (Show)

data MSONTypeRef = MSONTypeRef String
  deriving (Show)

data MSONPropertyDecl = MSONPropertyDecl MSONValueDecl [MSONPropertyDecl]
  deriving (Show)

data MSONValueDecl = MSONValueDecl MSONPropertyName (Maybe MSONValue) (Maybe MSONTypeRef) (Maybe MSONDesc)
  deriving (Show)

data MSONPropertyName = MSONPropertyName String
  deriving (Show)

data MSONValue = MSONValue String
  deriving (Show)

data MSONDesc = MSONDesc String
  deriving (Show)

--------------------------------------------------------------------------------

aMsonTypeDecl :: IParser MSONTypeDecl
aMsonTypeDecl = do
  n <- (char '#' *> ws *> aMsonSymbolName <* ws)
  t <- optionMaybe (aMsonTypeDef <* ws)
  spaces
  string "## Properties"
  spaces
  ms <- (many1 aMsonPropertyDecl)
  return (MSONTypeDecl n t ms)

aMsonPropertyDecl :: IParser MSONPropertyDecl
aMsonPropertyDecl = do
  b <- withBlock MSONPropertyDecl aMsonValueDecl aMsonPropertyDecl
  return b

aMsonValueDecl :: IParser MSONValueDecl
aMsonValueDecl = do
  listSymbol
  ws
  n <- aMsonPropertyName
  v <- optionMaybe (aColumnSeparator *> aMsonValue <* ws)
  t <- optionMaybe (aMsonTypeDef <* ws)
  d <- optionMaybe (aDescSeparator *> ws *> aMsonValueDesc <* ws)
  spaces
  return (MSONValueDecl n v t d)

aMsonPropertyName :: IParser MSONPropertyName
aMsonPropertyName = MSONPropertyName <$> (nameLiteral <* ws)

aMsonValue :: IParser MSONValue
aMsonValue = MSONValue <$> ((many1 alphaNum) <* ws)

aMsonTypeDef :: IParser MSONTypeRef
aMsonTypeDef = MSONTypeRef <$> ((char '(') *> (many1 alphaNum) <* (char ')'))

aMsonValueDesc :: IParser MSONDesc
aMsonValueDesc = MSONDesc <$> (many1 alphaNum)

aMsonSymbolName :: IParser MSONSymbolName
aMsonSymbolName = MSONSymbolName <$> (many1 alphaNum)

--------------------------------------------------------------------------------

ws :: IParser String
ws = many (oneOf " \t")

aColumnSeparator :: IParser ()
aColumnSeparator = do
  char ':'
  ws
  return ()

aDescSeparator :: IParser ()
aDescSeparator = do
  char '-'
  ws
  return ()

listSymbol :: IParser Char
listSymbol = oneOf "-+*"

enclosedName :: IParser String
enclosedName = char '`' *> (many1 (noneOf "`")) <* char '`'

openName :: IParser String
openName = many1 (alphaNum <|> char '_')

nameLiteral :: IParser String
nameLiteral = choice [enclosedName, openName]

--------------------------------------------------------------------------------


