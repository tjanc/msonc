module Mson
    ( TypeDecl(TypeDecl),
      SymbolName(SymbolName),
      TypeRef(TypeRef),
      PropertyDecl(PropertyDecl),
      ValueDecl(ValueDecl),
      PropertyName(PropertyName),
      Value(Value),
      Desc(Desc),
    ) where

data TypeDecl = TypeDecl SymbolName (Maybe TypeRef) [PropertyDecl]
  deriving (Show)

data SymbolName = SymbolName String
  deriving (Show)

data TypeRef = TypeRef String
  deriving (Show)

data PropertyDecl = PropertyDecl ValueDecl [PropertyDecl]
  deriving (Show)

data ValueDecl = ValueDecl PropertyName (Maybe Value) (Maybe TypeRef) (Maybe Desc)
  deriving (Show)

data PropertyName = PropertyName String
  deriving (Show)

data Value = Value String
  deriving (Show)

data Desc = Desc String
  deriving (Show)
