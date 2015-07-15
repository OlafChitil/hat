module Wired where

import Language.Haskell.Exts.Annotated 
import Data.Maybe (fromMaybe)
import Data.List (stripPrefix)

type Arity = Int

-- Building name qualifiers

qNameHatPreludeIdent :: String -> l -> QName l
qNameHatPreludeIdent ident l = 
  Qual l (ModuleName l "Hat.Prelude") (Ident l ident)

qNameHatPreludeSymbol :: String -> l -> QName l
qNameHatPreludeSymbol ident l = 
  Qual l (ModuleName l "Hat.Prelude") (Symbol l ident)

qNamePreludeIdent :: String -> l -> QName l
qNamePreludeIdent ident l = 
  Qual l (ModuleName l "Prelude") (Ident l ident)

qNamePreludeSymbol :: String -> l -> QName l
qNamePreludeSymbol ident l = 
  Qual l (ModuleName l "Prelude") (Symbol l ident)

qNameHatIdent :: String -> l -> QName l
qNameHatIdent ident l = Qual l (ModuleName l "Hat") (Ident l ident)

qNameShortIdent :: String -> l -> QName l
qNameShortIdent ident l = Qual l (ModuleName l "T") (Ident l ident)

qNameShortArity :: String -> l -> Arity -> QName l
qNameShortArity ident l a = qNameShortIdent (ident ++ show a) l

qNameDeriveIdent :: String -> l -> QName l
qNameDeriveIdent ident l = 
  Qual l (ModuleName l "PreludeBasic") (Ident l ident)

qNameDeriveSymbol :: String -> l -> QName l
qNameDeriveSymbol ident l = 
  Qual l (ModuleName l "PreludeBasic") (Symbol l ident)

qNameBuiltinIdent :: String -> l -> QName l
qNameBuiltinIdent ident l = 
  Qual l (ModuleName l "PreludeBuiltinTypes") (Ident l ident)

qNameBuiltinSymbol :: String -> l -> QName l
qNameBuiltinSymbol ident l = 
  Qual l (ModuleName l "PreludeBuiltinTypes") (Symbol l ident)

qNameHatDeriveIdent :: String -> l -> QName l
qNameHatDeriveIdent ident l = 
  Qual l (ModuleName l "Hat.PreludeBasic") (Ident l ident)

qNameHatDeriveSymbol :: String -> l -> QName l
qNameHatDeriveSymbol ident l = 
  Qual l (ModuleName l "Hat.PreludeBasic") (Symbol l ident)

-- -----------------------------------------------------------------------------

nameTransModule :: ModuleName l -> ModuleName l
nameTransModule (ModuleName l name) = ModuleName l 
  (fromMaybe (if name == "Main" then name else "Hat." ++ name) 
    (stripPrefix "NotHat." name)) 

tracingModuleNameShort :: l -> ModuleName l
tracingModuleNameShort l = ModuleName l "T"

mkTypeToken :: l -> String -> QName l
mkTypeToken l id = 
  if id `elem` (map ("from"++) preIds ++ map ("to"++) preIds)
    then Qual l (tracingModuleNameShort l) (Ident l id)
    else UnQual l (Ident l id)
  where
  -- list should include all types allowed in foreign imports
  preIds = ["Id", "IO", "Tuple0", "Tuple2", "Char", "Int", "Integer", 
            "Float", "Double"]

-- names for trace constructors

qNameMkRoot :: l -> QName l
qNameMkRoot = qNameShortIdent "mkRoot"

qNameR :: l -> QName l
qNameR = qNameShortIdent "R"

qNameMkModule :: l -> QName l
qNameMkModule = qNameShortIdent "mkModule"

qNameMkAtomConstructor :: l -> Bool -> QName l
qNameMkAtomConstructor l withFields =
  qNameShortIdent  
    (if withFields then "mkConstructorWFields" else "mkConstructor") l

qNameMkAtomVariable :: l -> QName l
qNameMkAtomVariable = qNameShortIdent "mkVariable"

qNameMkSpan :: l -> QName l
qNameMkSpan = qNameShortIdent "mkSrcPos"

qNameMkNoSpan :: l -> QName l
qNameMkNoSpan = qNameShortIdent "mkNoSrcPos"

qNameMkExpValueApp :: l -> Arity -> QName l
qNameMkExpValueApp = qNameShortArity "mkValueApp"

qNameMkExpValueUse :: l -> QName l
qNameMkExpValueUse = qNameShortIdent "mkValueUse"

qNameMkAtomRational :: l -> QName l
qNameMkAtomRational = qNameShortIdent "mkAtomRational"

qNameMkAtomLambda :: l -> QName l
qNameMkAtomLambda = qNameShortIdent "mkLambda"

qNameMkAtomDoLambda :: l -> QName l
qNameMkAtomDoLambda = qNameShortIdent "mkDoLambda"

-- tokens for expression combinators

qNameAp :: l -> Arity -> QName l
qNameAp = qNameShortArity "ap"
qNameUAp :: l -> Arity -> QName l
qNameUAp = qNameShortArity "uap"

qNameApp :: l -> Arity -> QName l
qNameApp = qNameShortArity "app"
qNameUApp :: l -> Arity -> QName l
qNameUApp = qNameShortArity "uapp"

qNameFun :: l -> Arity -> QName l
qNameFun = qNameShortArity "fun"
qNameUFun :: l -> Arity -> QName l
qNameUFun = qNameShortArity "ufun"

qNameCon :: l -> Arity -> QName l
qNameCon = qNameShortArity "con"

qNamePa :: l -> Arity -> QName l
qNamePa = qNameShortArity "pa"

qNameCn :: l -> Arity -> QName l
qNameCn = qNameShortArity "cn"

qNameConstUse :: l -> QName l
qNameConstUse = qNameShortIdent "constUse"
qNameUConstUse :: l -> QName l
qNameUConstUse = qNameShortIdent "uconstUse"

qNameConstDef :: l -> QName l
qNameConstDef = qNameShortIdent "constDef" 
qNameUConstDef :: l -> QName l
qNameUConstDef = qNameShortIdent "uconstDef"

qNameGuard :: l -> QName l
qNameGuard = qNameShortIdent "cguard" 
qNameUGuard :: l -> QName l
qNameUGuard = qNameShortIdent "ucguard"

qNameIf :: l -> QName l
qNameIf = qNameShortIdent "cif"
qNameUIf :: l -> QName l
qNameUIf = qNameShortIdent "ucif"

qNameCase :: l -> QName l
qNameCase = qNameShortIdent "ccase"
qNameUCase :: l -> QName l
qNameUCase = qNameShortIdent "uccase"

qNameUpdate :: l -> Arity -> QName l
qNameUpdate = qNameShortArity "update"
qNameUUpdate :: l -> QName l
qNameUUpdate = qNameShortIdent "uupdate"

qNameProjection :: l -> QName l
qNameProjection = qNameShortIdent "projection"

qNameConChar :: l -> QName l
qNameConChar = qNameShortIdent "conChar"

qNameConInteger :: l -> QName l
qNameConInteger = qNameShortIdent "conInteger"

qNameFromLitString :: l -> QName l
qNameFromLitString = qNameShortIdent "fromLitString"

qNameFromExpList :: l -> QName l
qNameFromExpList = qNameShortIdent "fromExpList"

qNameWrapValClass :: l -> QName l
qNameWrapValClass = qNameShortIdent "WrapVal"

nameWrapValFun :: l -> Name l
nameWrapValFun l = Ident l "wrapVal"

qNameUWrapForward :: l -> QName l
qNameUWrapForward = qNameShortIdent "uwrapForward"


-- function for pattern-match failure error message
qNameFatal :: l -> QName l
qNameFatal = qNameShortIdent "fatal"

qNameRefSrcSpan :: l -> QName l
qNameRefSrcSpan = qNameShortIdent "RefSrcPos"

qNameRefExp :: l -> QName l
qNameRefExp = qNameShortIdent "RefExp"


-- For special symbols
-- lists:

qNameConNil :: l -> QName l
qNameConNil l = Qual l (tracingModuleNameShort l) (Ident l "Nil")

qNameTraceInfoConNil :: l -> QName l
qNameTraceInfoConNil l = Qual l (tracingModuleNameShort l) (Ident l "aNil")


-- Always refers to list constructor from the (NoHat) prelude:

qNameCons :: l -> QName l
qNameCons l = Special l (Cons l)

mkExpCons :: l -> Exp l
mkExpCons l = Con l (qNameCons l)

-- function main
nameMain :: l -> Name l
nameMain l = Ident l "main"


-- Names used for desugaring AST before actual transformation.
-- So they are transformed and we must ensure they are always in scope.
-- Hence qualify with PreludeBasic which is always imported qualified.

mkExpDeriveReturn :: l -> Exp l
mkExpDeriveReturn l = Var l (qNameDeriveIdent "return" l)

mkExpDeriveGuard :: l -> Exp l
mkExpDeriveGuard l = Var l (qNameDeriveIdent "guard" l)

mkExpDeriveFlip :: l -> Exp l
mkExpDeriveFlip l = Var l (qNameDeriveIdent "flip" l)

mkExpDeriveEnumFrom :: l -> Exp l
mkExpDeriveEnumFrom l = Var l (qNameDeriveIdent "enumFrom" l)

mkExpDeriveEnumFromTo :: l -> Exp l
mkExpDeriveEnumFromTo l = Var l (qNameDeriveIdent "enumFromTo" l)

mkExpDeriveEnumFromThen :: l -> Exp l
mkExpDeriveEnumFromThen l = Var l (qNameDeriveIdent "enumFromThen" l)

mkExpDeriveEnumFromThenTo :: l -> Exp l
mkExpDeriveEnumFromThenTo l = Var l (qNameDeriveIdent "enumFromThenTo" l)

mkExpDeriveFilter :: l -> Exp l
mkExpDeriveFilter l = Var l (qNameDeriveIdent "filter" l)

mkExpDeriveFoldr :: l -> Exp l
mkExpDeriveFoldr l = Var l (qNameDeriveIdent "foldr" l)

mkExpDeriveEqualEqual :: l -> Exp l
mkExpDeriveEqualEqual l = Var l (qNameDeriveSymbol "==" l)

mkExpDeriveGreaterEqual :: l -> Exp l
mkExpDeriveGreaterEqual l = Var l (qNameDeriveSymbol ">=" l)

mkExpDeriveMinus :: l -> Exp l
mkExpDeriveMinus l = Var l (qNameDeriveSymbol "-" l)

qNameDeriveGtGt :: l -> QName l
qNameDeriveGtGt = qNameDeriveSymbol ">>"

qNameDeriveGtGtEq :: l -> QName l
qNameDeriveGtGtEq = qNameDeriveSymbol ">>="

qNameDeriveFail :: l -> QName l
qNameDeriveFail = qNameDeriveIdent "fail"

mkExpDeriveAndAnd :: l -> Exp l
mkExpDeriveAndAnd l = Var l (qNameDeriveSymbol "&&" l)

mkExpDeriveTrue :: l -> Exp l
mkExpDeriveTrue l = Con l (qNameBuiltinIdent "True" l) 

mkExpDeriveFalse :: l -> Exp l
mkExpDeriveFalse l = Con l (qNameBuiltinIdent "False" l)

-- Not for desugaring, but for generated code.
-- Hence not transformed any more.
-- Has to refer to original, untransformed Prelude:

qNamePreludeTrue :: l -> QName l
qNamePreludeTrue = qNamePreludeIdent "True"

qNamePreludeFalse :: l -> QName l
qNamePreludeFalse = qNamePreludeIdent "False"

-- Already transformed, but basically used for desugaring:

expUndefined :: Exp SrcSpanInfo
expUndefined = Var noSpan (qNameHatDeriveIdent "gundefined" noSpan)

-- for integer literals
expFromInteger :: Exp SrcSpanInfo
expFromInteger = Var noSpan (qNameHatDeriveIdent "gfromInteger" noSpan)

-- for rational literals:
expConRational :: Exp SrcSpanInfo
expConRational = Var noSpan (qNameHatDeriveSymbol ":%" noSpan)

expFromRational :: Exp SrcSpanInfo
expFromRational = Var noSpan (qNameHatDeriveIdent "gfromRational" noSpan)

-- -------------------------------------------
-- bogus span, does not appear in the source
noSpan :: SrcSpanInfo
noSpan = noInfoSpan (SrcSpan "" 0 0 0 0)

