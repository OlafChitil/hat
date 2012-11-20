module Wired where

import Language.Haskell.Exts.Annotated 
import SynHelp
import Data.Maybe (fromMaybe)
import Data.List (stripPrefix)

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


-- Hardwired names.

qNamePreludeEq :: l -> QName l
qNamePreludeEq = qNamePreludeIdent "Eq"

qNamePreludeOrd :: l -> QName l
qNamePreludeOrd = qNamePreludeIdent "Ord"

qNamePreludeEQ :: l -> QName l
qNamePreludeEQ = qNamePreludeIdent "EQ"

qNamePreludeCompare :: l -> QName l
qNamePreludeCompare = qNamePreludeIdent "compare"

-- Syntax using identifiers in the standard Prelude.

mkExpPreludeAndAnd :: l -> Exp l
mkExpPreludeAndAnd l = Var l (qNamePreludeSymbol "&&" l)

mkExpTrue :: l -> Exp l
mkExpTrue l = Var l (qNamePreludeTrue l)

mkExpFalse :: l -> Exp l
mkExpFalse l = Var l (qNamePreludeFalse l)


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

qNamePreludeTrue :: l -> QName l
qNamePreludeTrue = qNamePreludeIdent "True"

qNamePreludeFalse :: l -> QName l
qNamePreludeFalse = qNamePreludeIdent "False"

-- Names from original (NotHat) prelude:

qNamePreludeGtGt :: l -> QName l
qNamePreludeGtGt = qNamePreludeSymbol ">>"

qNamePreludeGtGtEq :: l -> QName l
qNamePreludeGtGtEq = qNamePreludeSymbol ">>="

qNamePreludeFail :: l -> QName l
qNamePreludeFail = qNamePreludeIdent "fail"

mkExpPreludeEnumFrom :: l -> Exp l
mkExpPreludeEnumFrom l = Var l (qNamePreludeIdent "enumFrom" l)

mkExpPreludeEnumFromTo :: l -> Exp l
mkExpPreludeEnumFromTo l = Var l (qNamePreludeIdent "enumFromTo" l)

mkExpPreludeEnumFromThen :: l -> Exp l
mkExpPreludeEnumFromThen l = Var l (qNamePreludeIdent "enumFromThen" l)

mkExpPreludeEnumFromThenTo :: l -> Exp l
mkExpPreludeEnumFromThenTo l = Var l (qNamePreludeIdent "enumFromThenTo" l)

qNamePreludeCons :: l -> QName l
qNamePreludeCons l = Special l (Cons l)
mkExpPreludeCons :: l -> Exp l
mkExpPreludeCons l = Con l (qNamePreludeCons l)

mkExpPreludeFilter :: l -> Exp l
mkExpPreludeFilter l = Var l (qNamePreludeIdent "filter" l)

mkExpPreludeFoldr :: l -> Exp l
mkExpPreludeFoldr l = Var l (qNamePreludeIdent "foldr" l)

mkExpPreludeEqualEqual :: l -> Exp l
mkExpPreludeEqualEqual l = Var l (qNamePreludeSymbol "==" l)

mkExpPreludeGreaterEqual :: l -> Exp l
mkExpPreludeGreaterEqual l = Var l (qNamePreludeSymbol ">=" l)

mkExpPreludeMinus :: l -> Exp l
mkExpPreludeMinus l = Var l (qNamePreludeSymbol "-" l)

-- function main
nameMain :: l -> Name l
nameMain l = Ident l "main"
