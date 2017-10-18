module MsonSpec where

data NamedType = NamedType NamedDeclaration [TypeSection]
  deriving (Show)

data NamedDeclaration = NamedDeclaration TypeName TypeDefinition
  deriving (Show)

data TypeName = TypeName String
  deriving (Show)

data TypeDefinition = TypeDefinition TypeSpecification [TypeAttribute]
  deriving (Show)

data TypeSpecification = TypeSpecification TypeName [TypeName]
  deriving (Show)

data TypeAttribute = AttrRequired
                   | AttrOptional
                   | AttrFixedType
                   | AttrNullable
                   | AttrSample
                   | AttrDefault
  deriving (Show)

data TypeSection = TypeDescription String
                 | TypeMemberGroup MemberTypeGroup
                 | TypeMembrs NestedMemberTypes
                 | TypeSample Sample
                 | TypeDefault Default
                 | TypeValidations Validations
  deriving (Show)

data Validations = Validations
  deriving (Show)

data Description = Description String
  deriving (Show)

data MemberTypeGroup = MemberTypeGroup MemberTypeSeparator [NestedMemberTypes]
  deriving (Show)

data MemberTypeSeparator = Items | Members | Properties
  deriving (Show)

data NestedMemberTypes = NestedMemberTypes [MemberType]
  deriving (Show)

data Sample = Sample (Maybe Value) SampleDefinition
  deriving (Show)

data SampleDefinition = StaticSample String | ValueMemberTypes [ValueMemberType]
  deriving (Show)

data ValueMemberType = ValueMemberType ValueMemberDeclaration [TypeSection]
  deriving (Show)

data ValueMemberDeclaration = ValueMemberDeclaration ValueDefinition (Maybe Description)
  deriving (Show)

data Default = Default (Maybe Value) SampleDefinition
  deriving (Show)

data MemberType = MemberType PropertyMemberDeclaration [TypeSection]
  deriving (Show)

data PropertyMemberDeclaration = PropertyMemberDeclaration PropertyName (Maybe ValueDefinition) (Maybe Description)
  deriving (Show)

data PropertyName = LiteralProperty LiteralValue
                  | VariableProperty VariablePropertyName
  deriving (Show)

data ValueDefinition = ValueDefinition [Value] (Maybe TypeDefinition)
  deriving (Show)

data LiteralValue = LiteralValue String
  deriving (Show)

data VariablePropertyName = VariablePropertyName LiteralValue
  deriving (Show)

data Value = Literal LiteralValue
           | Variable LiteralValue
  deriving (Show)
