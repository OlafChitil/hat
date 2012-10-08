{- ---------------------------------------------------------------------------
Defines data type IdKind which tells of which kind an identifier is, 
e.g. a module identifier or a class identifier
-}

module IdKind(IdKind(..)) where

data IdKind = Var
            | Arg
            | Method
            | TVar
            | Con
            | TCon
            | TSyn
            | TClass
            | TC
            | Modid
            | MethodInstance
            | MethodDefault
	    | Field

ordIdKind :: IdKind -> Int
ordIdKind  Var    =  1
ordIdKind  Arg    =  1
ordIdKind  Method =  1
ordIdKind  TVar   =  2
ordIdKind  Con    =  3
ordIdKind  TCon   =  4
ordIdKind  TSyn   =  4
ordIdKind  TClass =  4
ordIdKind  TC     =  4
ordIdKind  Modid  =  5
ordIdKind  MethodInstance =  6
ordIdKind  MethodDefault  = 7
ordIdKind  Field =   8

instance Eq IdKind where
         a      == b      = ordIdKind a == ordIdKind b

instance Ord IdKind where
         a  <= b = ordIdKind a <= ordIdKind b
         a  <  b = ordIdKind a <  ordIdKind b
         compare a  b = compare (ordIdKind a) (ordIdKind b)


instance Show IdKind where
  showsPrec d Var    = ("Identifier"++)
  showsPrec d Arg    = ("Argument"++)
  showsPrec d Method = ("Method"++)
  showsPrec d TVar   = ("Typevar"++)
  showsPrec d Con    = ("Constructor"++)
  showsPrec d TC     = ("Type constructor/class"++)
  showsPrec d TCon   = ("Type constructor"++)
  showsPrec d TSyn   = ("Type synonym"++)
  showsPrec d TClass = ("Type class"++)
  showsPrec d Modid  = ("Module identifier"++)
  showsPrec d MethodDefault  = ("Default method"++)
  showsPrec d MethodInstance = ("Instance method"++)
  showsPrec d Field = ("Field"++)

