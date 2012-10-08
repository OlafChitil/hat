-- Contains all parts of the Prelude and Char (both are mutually
-- recursive) that can be expressed in Haskell (assuming 
-- nhc98's ability to derive instances of [],(,,,) etc).
-- Hand-generated from Haskell 
module PreludeBasic 
  (module PreludeBasic, module PreludeBuiltin) 
  where

-- import PreludeBuiltin  -- Contains all `prim' values
-- import PreludeList
-- import PreludeText
-- import PreludeIO
-- import Ratio( Rational )

import PreludeBuiltinTypes
import PreludeBuiltin

infixr 9  .
infixr 8  ^, ^^, **
infixl 7  *, /, `quot`, `rem`, `div`, `mod`
infixl 6  +, -

-- The (:) operator is built-in syntax, and cannot legally be given
-- a fixity declaration; but its fixity is given by:
--   infixr 5  :

infix  4  ==, /=, <, <=, >=, >
infixr 3  &&
infixr 2  ||
infixl 1  >>, >>=
infixr 1  =<<
infixr 0  $, $!  --, `seq`


-- Internal functions used by the transformation in desugaring
-- and deriving of instances. 
-- Not exported from Prelude itself.

yield :: a -> ReadS a
yield x r = [(x,r)]

lift :: (a -> b -> ReadS c) -> ReadS a -> ReadS b -> ReadS c              
lift c f g r = concatMap (\(x,s) -> concatMap (\(y,t) -> c x y t) (g s)) (f r)

thenAp :: ReadS (a -> b) -> ReadS a -> ReadS b
thenAp = lift (\h x t -> [(h x,t)])

thenLex :: ReadS a -> String -> ReadS a
thenLex f xs = lift (\x y t -> if y==xs then [(x,t)] else []) f lex

alt :: ReadS a -> ReadS a -> ReadS a
alt f g r = f r ++ g r


-- Standard types, classes, instances and related functions

-- Equality and Ordered classes
class  Eq a  where
    (==), (/=)       :: a -> a -> Bool

        -- Minimal complete definition:
        --      (==) or (/=)
    x /= y           =  not (x == y)
    x == y           =  not (x /= y)


class  (Eq a) => Ord a  where
    compare              :: a -> a -> Ordering
    (<), (<=), (>=), (>) :: a -> a -> Bool
    max, min             :: a -> a -> a

        -- Minimal complete definition:
        --      (<=) or compare
        -- Using compare can be more efficient for complex types.
    compare x y
         | x == y    =  EQ
         | x <= y    =  LT
         | otherwise =  GT

    x <= y           =  compare x y /= GT
    x <  y           =  compare x y == LT
    x >= y           =  compare x y /= LT
    x >  y           =  compare x y == GT

-- note that (min x y, max x y) = (x,y) or (y,x)
    max x y 
         | x <= y    =  y
         | otherwise =  x
    min x y
         | x <= y    =  x
         | otherwise =  y

-- Enumeration and Bounded classes


class  Enum a  where
    succ, pred       :: a -> a
    toEnum           :: Int -> a
    fromEnum         :: a -> Int
    enumFrom         :: a -> [a]             -- [n..]
    enumFromThen     :: a -> a -> [a]        -- [n,n'..]
    enumFromTo       :: a -> a -> [a]        -- [n..m]
    enumFromThenTo   :: a -> a -> a -> [a]   -- [n,n'..m]

        -- Minimal complete definition:
        --      toEnum, fromEnum
--
-- NOTE: these default methods only make sense for types
--   that map injectively into Int using fromEnum
--  and toEnum.
    succ             =  toEnum . (+1) . fromEnum
    pred             =  toEnum . (subtract 1) . fromEnum
    enumFrom x       =  map toEnum [fromEnum x ..]
    enumFromTo x y   =  map toEnum [fromEnum x .. fromEnum y]
    enumFromThen x y =  map toEnum [fromEnum x, fromEnum y ..]
    enumFromThenTo x y z = 
                        map toEnum [fromEnum x, fromEnum y .. fromEnum z]


class  Bounded a  where
    minBound         :: a
    maxBound         :: a

-- Numeric classes


class  (Eq a, Show a) => Num a  where
    (+), (-), (*)    :: a -> a -> a
    negate           :: a -> a
    abs, signum      :: a -> a
    fromInteger      :: Integer -> a

        -- Minimal complete definition:
        --      All, except negate or (-)
    x - y            =  x + negate y
    negate x         =  0 - x


class  (Num a, Ord a) => Real a  where
    toRational       ::  a -> Rational


class  (Real a, Enum a) => Integral a  where
    quot, rem        :: a -> a -> a   
    div, mod         :: a -> a -> a
    quotRem, divMod  :: a -> a -> (a,a)
    toInteger        :: a -> Integer

        -- Minimal complete definition:
        --      quotRem, toInteger
    n `quot` d       =  q  where (q,r) = quotRem n d
    n `rem` d        =  r  where (q,r) = quotRem n d
    n `div` d        =  q  where (q,r) = divMod n d
    n `mod` d        =  r  where (q,r) = divMod n d
    divMod n d       =  if signum r == negate (signum d) then (q-1, r+d) else qr
                        -- replaced prefix - by negate
                        -- to make AuxFixity of nhc98 happy
                        where qr@(q,r) = quotRem n d


class  (Num a) => Fractional a  where
    (/)              :: a -> a -> a
    recip            :: a -> a
    fromRational     :: Rational -> a

        -- Minimal complete definition:
        --      fromRational and (recip or (/))
    recip x          =  1 / x
    x / y            =  x * recip y


class  (Fractional a) => Floating a  where
    pi                  :: a
    exp, log, sqrt      :: a -> a
    (**), logBase       :: a -> a -> a
    sin, cos, tan       :: a -> a
    asin, acos, atan    :: a -> a
    sinh, cosh, tanh    :: a -> a
    asinh, acosh, atanh :: a -> a

        -- Minimal complete definition:
        --      pi, exp, log, sin, cos, sinh, cosh
        --      asin, acos, atan
        --      asinh, acosh, atanh
    x ** y           =  exp (log x * y)
    logBase x y      =  log y / log x
    sqrt x           =  x ** 0.5
    tan  x           =  sin  x / cos  x
    tanh x           =  sinh x / cosh x



class  (Real a, Fractional a) => RealFrac a  where
    properFraction   :: (Integral b) => a -> (b,a)
    truncate, round  :: (Integral b) => a -> b
    ceiling, floor   :: (Integral b) => a -> b

        -- Minimal complete definition:
        --      properFraction
    truncate x       =  m  where (m,_) = properFraction x
    
    round x          =  let (n,r) = properFraction x
                            m     = if r < 0 then n - 1 else n + 1
                          in case signum (abs r - 0.5) `compare` 0 of
                                LT -> n
                                EQ  -> if even n then n else m
                                GT  -> m
-- original version
-- nhc doesn't like prefix - when prelude is not imported unqualified
--                          in case signum (abs r - 0.5) of
--                                -1 -> n
--                                0  -> if even n then n else m
--                                1  -> m
    
    ceiling x        =  if r > 0 then n + 1 else n
                        where (n,r) = properFraction x
    
    floor x          =  if r < 0 then n - 1 else n
                        where (n,r) = properFraction x


class  (RealFrac a, Floating a) => RealFloat a  where
    floatRadix       :: a -> Integer
    floatDigits      :: a -> Int
    floatRange       :: a -> (Int,Int)
    decodeFloat      :: a -> (Integer,Int)
    encodeFloat      :: Integer -> Int -> a
    exponent         :: a -> Int
    significand      :: a -> a
    scaleFloat       :: Int -> a -> a
    isNaN, isInfinite, isDenormalized, isNegativeZero, isIEEE
                     :: a -> Bool
    atan2            :: a -> a -> a

        -- Minimal complete definition:
        --      All except exponent, significand, 
        --                 scaleFloat, atan2
    exponent x       =  if m == 0 then 0 else n + floatDigits x
                        where (m,n) = decodeFloat x

    significand x    =  encodeFloat m (- floatDigits x)
                        where (m,_) = decodeFloat x

    scaleFloat k x   =  encodeFloat m (n+k)
                        where (m,n) = decodeFloat x

    atan2 y x
      | x>0           =  atan (y/x)
      | x==0 && y>0   =  pi/2
      | x<0  && y>0   =  pi + atan (y/x) 
      |(x<=0 && y<0)  ||
       (x<0 && isNegativeZero y) ||
       (isNegativeZero x && isNegativeZero y)
                      = -atan2 (-y) x
      | y==0 && (x<0 || isNegativeZero x)
                      =  pi    -- must be after the previous test on zero y
      | x==0 && y==0  =  y     -- must be after the other double zero tests
      | otherwise     =  x + y -- x or y is a NaN, return a NaN (via +)

-- Numeric functions


subtract         :: (Num a) => a -> a -> a
subtract         =  flip (-)


even, odd        :: (Integral a) => a -> Bool
even n           =  n `rem` 2 == 0
odd              =  not . even


gcd              :: (Integral a) => a -> a -> a
gcd 0 0          =  error "Prelude.gcd: gcd 0 0 is undefined"
gcd x y          =  gcd' (abs x) (abs y)
                    where gcd' x 0  =  x
                          gcd' x y  =  gcd' y (x `rem` y)


lcm              :: (Integral a) => a -> a -> a
lcm _ 0          =  0
lcm 0 _          =  0
lcm x y          =  abs ((x `quot` (gcd x y)) * y)


(^)              :: (Num a, Integral b) => a -> b -> a
x ^ 0            =  1
x ^ n | n > 0    =  f x (n-1) x
                    where f _ 0 y = y
                          f x n y = g x n  where
                                    g x n | even n  = g (x*x) (n `quot` 2)
                                          | otherwise = f x (n-1) (x*y)
_ ^ _            = error "Prelude.^: negative exponent"


(^^)             :: (Fractional a, Integral b) => a -> b -> a
x ^^ n           =  if n >= 0 then x^n else recip (x^(-n))


fromIntegral     :: (Integral a, Num b) => a -> b
fromIntegral     =  fromInteger . toInteger


realToFrac     :: (Real a, Fractional b) => a -> b
realToFrac      =  fromRational . toRational

-- Monadic classes


class  Functor f  where
    fmap              :: (a -> b) -> f a -> f b


class  Monad m  where
    (>>=)  :: m a -> (a -> m b) -> m b
    (>>)   :: m a -> m b -> m b
    return :: a -> m a
    fail   :: String -> m a

        -- Minimal complete definition:
        --      (>>=), return
    m >> k  =  m >>= \_ -> k
    fail s  = error s


sequence       :: Monad m => [m a] -> m [a] 
sequence       =  foldr mcons (return [])
                    where mcons p q = p >>= \x -> q >>= \y -> return (x:y)


sequence_      :: Monad m => [m a] -> m () 
sequence_      =  foldr (>>) (return ())

-- The xxxM functions take list arguments, but lift the function or
-- list element to a monad type

mapM             :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f as        =  sequence (map f as)


mapM_            :: Monad m => (a -> m b) -> [a] -> m ()
mapM_ f as       =  sequence_ (map f as)


(=<<)            :: Monad m => (a -> m b) -> m a -> m b
f =<< x          =  x >>= f


-- Trivial type


-- data  ()  =  ()  deriving (Eq, Ord, Enum, Bounded)

instance Eq () where
  () == () = True

instance Ord () where
  compare () () = EQ

instance Enum () where
  fromEnum () = 0
  toEnum   n | n==0  = ()
  toEnum   n         = error ("Prelude.Enum_Prelude.Unit_toEnum on " ++ show n)

instance Bounded () where
  minBound = ()
  maxBound = ()

-- Function type


-- data a -> b  -- No constructor for functions is exported.

-- identity function

id               :: a -> a
id x             =  x

-- constant function

const            :: a -> b -> a
const x _        =  x

-- function composition

(.)              :: (b -> c) -> (a -> b) -> a -> c
(.) f g x        =  f (g x)

-- flip f  takes its (first) two arguments in the reverse order of f.

flip             :: (a -> b -> c) -> b -> a -> c
flip f x y       =  f y x


-- seq :: a -> b -> b
-- seq = ...       -- Primitive

-- right-associating infix application operators 
-- (useful in continuation-passing style)

($), ($!) :: (a -> b) -> a -> b
f $  x    =  f x
f $! x    =  x `seq` f x


-- Boolean type


-- data  Bool  =  False | True  deriving (Eq, Ord, Enum, Read, Show, Bounded)

instance Eq Bool where
  True  == True  = True
  False == False = True
  _     == _     = False

instance Ord Bool where
  compare False False = EQ
  compare False True  = LT
  compare True  False = GT
  compare True  True  = EQ

instance Enum Bool where
  fromEnum   False = 0
  fromEnum   True  = 1

  toEnum   0 = False
  toEnum   1 = True
  toEnum   n = error ("(Prelude.toEnum "++show n++" :: Bool) is wrong")

  enumFrom b = enumFromTo b True
  enumFromThen b1 b2 = enumFromThenTo b1 b2 (b2 >= b1)

instance Read Bool where
  readsPrec p = readParen False 
		 ( \r -> [(False,s) | ("False",s) <- lex r] ++
			 [(True,s)  | ("True",s) <- lex r])

instance Show Bool where
  showsPrec p False = showString "False"
  showsPrec p True = showString "True"

instance Bounded Bool where
  minBound = False
  maxBound = True


-- Boolean functions


(&&), (||)       :: Bool -> Bool -> Bool
True  && x       =  x
False && _       =  False
True  || _       =  True
False || x       =  x
                                        

not              :: Bool -> Bool
not True         =  False
not False        =  True


otherwise        :: Bool
otherwise        =  True


-- Character type


-- data Char = ... 'a' | 'b' ... -- 2^16 unicode values


instance  Eq Char  where
    c == c'          =  fromEnum c == fromEnum c'


instance  Ord Char  where
    c <= c'          =  fromEnum c <= fromEnum c'


instance  Enum Char  where
    toEnum            = primIntToChar
    fromEnum          = primCharToInt
    enumFrom c        = map toEnum [fromEnum c .. fromEnum (maxBound::Char)]
    enumFromThen c c' = map toEnum [fromEnum c, fromEnum c' .. fromEnum lastChar]
                      where lastChar :: Char
                            lastChar | c' < c    = minBound
                                     | otherwise = maxBound


instance  Bounded Char  where
    minBound            =  '\0'
    maxBound            =  primUnicodeMaxBound


-- type  String = [Char]


-- Maybe type


data  Maybe a  =  Nothing | Just a      deriving (Eq, Ord, Read, Show)


maybe              :: b -> (a -> b) -> Maybe a -> b
maybe n f Nothing  =  n
maybe n f (Just x) =  f x


instance  Functor Maybe  where
    fmap f Nothing    =  Nothing
    fmap f (Just x)   =  Just (f x)
        

instance  Monad Maybe  where
    (Just x) >>= k   =  k x
    Nothing  >>= k   =  Nothing
    return           =  Just
    fail s           =  Nothing

-- Either type


data  Either a b  =  Left a | Right b   deriving (Eq, Ord, Read, Show)


either               :: (a -> c) -> (b -> c) -> Either a b -> c
either f g (Left x)  =  f x
either f g (Right y) =  g y

-- IO type


-- data  IO a  -- abstract


instance  Functor IO where
   fmap f x           =  x >>= (return . f)


instance Monad IO where
   (>>=)  = primIOBind
   return = primIOReturn
   fail s = ioError (userError s)

-- Ordering type


data  Ordering  =  LT | EQ | GT
          deriving (Eq, Ord, Enum, Read, Show, Bounded)


-- Standard numeric types.  The data declarations for these types cannot
-- be expressed directly in Haskell since the constructor lists would be
-- far too large.


-- data  Int  =  minBound ... -1 | 0 | 1 ... maxBound

instance Eq Int where
  a == b = primIntEq a b
  a /= b = primIntNe a b

instance Ord Int where
  a <  b = primIntLt a b
  a <= b = primIntLe a b
  a >= b = primIntGe a b
  a >  b = primIntGt a b

instance Num Int where
 a + b    = primIntPlus a b
 a - b    = primIntMinus a b
 a * b    = primIntTimes a b
 negate a = primIntNegate a
 abs    a = primIntAbs a
 signum a = primIntSignum a
 fromInteger i = primIntFromInteger i

instance Real Int where
    toRational i = (toInteger i) % 1

instance Integral Int  where
    n `quot`    d   	= primIntQuot n d
    n `rem`     d   	= primIntRem n d
    n `quotRem` d 	= (n `quot` d, n `rem` d)

    toInteger n 	= primIntegerFromInt n

instance Enum Int where
  toEnum = id
  fromEnum = id

  enumFrom x = enumFromTo x maxBound
  enumFromThen x y = enumFromThenTo x y (if y >= x then maxBound else minBound)
  enumFromTo n m = intEnumFromByTo n 1 m
  enumFromThenTo n n' m = intEnumFromByTo n (n'-n) m

-- need to avoid evaluating number beyond m, 
-- because m can be maxBound or minBound
intEnumFromByTo :: Int -> Int -> Int -> [Int]
intEnumFromByTo n d m =
  case d `compare` 0 of
    GT -> if n > m then [] else go (<= m-d) n 
    EQ -> repeat n
    LT -> if n < m then [] else go (>= m-d) n
  where
  go :: (Int -> Bool) -> Int -> [Int]
  go continue n = n : if (continue n) then go continue (n+d) else [] 
     

instance Bounded Int where
  minBound = primIntMinBound
  maxBound = primIntMaxBound


-- data  Integer  =  ... -1 | 0 | 1 ...

instance Eq Integer where
  a == b = primIntegerEq a b 
  a /= b = primIntegerNe a b 

instance Ord Integer where
  a <  b = primIntegerLt a b 
  a <= b = primIntegerLe a b
  a >= b = primIntegerGe a b 
  a >  b = primIntegerGt a b 

instance Num Integer where
 a + b    = primIntegerAdd a b 
 a - b    = primIntegerSub a b 
 a * b    = primIntegerMul a b 
 negate a = primIntegerNeg a
 abs i    = if i < 0 then negate i else i
 signum i = case compare i 0 of
		LT -> negate 1
		EQ ->  0
		GT ->  1
 fromInteger a = a -- id a

instance Real Integer where
    toRational i = i % 1

instance Integral Integer  where
    n `quot` d 	  = primIntegerQuot n d
    n `rem`  d    = primIntegerRem n d
    n `div`  d    = fst (divMod n d) 
    n `mod`  d 	  = snd (divMod n d) 

    n `quotRem` d = primIntegerQuotRem n d

    toInteger n   = n 

instance Enum Integer where
  succ x     = x+1
  pred x     = x-1
  toEnum x   = toInteger x
  fromEnum x = fromInteger x

  enumFrom = iterate (+1)
  enumFromThen n n' = iterate (+(n'-n)) n
  enumFromTo n m = takeWhile (<= m) (iterate (+1) n)
  enumFromThenTo n n' m = 
    let d = n'-n in 
      (if d >= 0 then takeWhile (<= m) (iterate (+d) n) 
                 else takeWhile (>= m) (iterate (+d) n) )


-- data  Float

instance Eq Float where
  a == b = primFloatEq a b
  a /= b = primFloatNe a b

instance Ord Float where
  a <  b = primFloatLt a b
  a <= b = primFloatLe a b
  a >= b = primFloatGe a b
  a >  b = primFloatGt a b

instance Num Float where
 a + b    = primFloatAdd a b
 a - b    = primFloatSub a b
 a * b    = primFloatMul a b
 negate a = 0 - a
 abs    a = primFloatAbs a
 signum a = primFloatSignum a
 fromInteger i = primFloatFromInteger i

instance Real Float where
    toRational x = case decodeFloat x of (m,n) -> (m%1)*(bf%1)^^n
			where bf     = floatRadix  x

instance Fractional Float where
  x / y = primFloatDiv x y
  fromRational x = x0
      where x0    = ff ef
            ff ef = if ef' == ef then yf else ff ef'
                   where yf :: Float
                         yf      = encodeFloat (round (x * (1 % bf) ^^ ef)) ef
                         (_,ef') = decodeFloat yf
                         bf      = floatRadix x0
            (_,ef) = decodeFloat (fromInteger (numerator x) `asTypeOf` x0
        	                                / fromInteger (denominator x))

instance  Floating Float where
    pi			=  primFloatPi
    exp	x		=  primFloatExp x
    log	x		=  primFloatLog x
    sqrt x		=  primFloatSqrt x
    sin	x		=  primFloatSin x
    cos	x		=  primFloatCos x
    tan	x		=  primFloatTan x
    asin x		=  primFloatAsin x
    acos x		=  primFloatAcos x
    atan x		=  primFloatAtan x
    sinh x              = 0.5  * (exp x - exp (-x))
    cosh x              = 0.5  * (exp x + exp (-x))
    tanh x              = (af-bf)/(af+bf) where af = exp x ; bf = exp (-x)
    asinh x = log (x + sqrt (1+x*x))
    acosh x = log (x + (x+1) * sqrt ((x-1)/(x+1)))
    atanh x = log ((x+1) / sqrt (1 - x*x))

instance  RealFrac Float where
    properFraction x =
        case decodeFloat x of
	  (m,n) -> if n >= 0 then
			(fromInteger m * fromInteger (floatRadix x) ^ n, 0)
		    else
			case quotRem m ((floatRadix x)^(negate n)) of
		          (w,r) -> (fromInteger w, encodeFloat r n)

instance  RealFloat Float  where
    floatRadix    = primFloatRadix
    floatDigits   = primFloatDigits
    floatRange    = primFloatRange
    decodeFloat x = primDecodeFloat x
    encodeFloat x y = primEncodeFloat x y

    isNaN 	   = primFloatIsNaN
    isInfinite     = primFloatIsInfinite
    isDenormalized = primFloatIsDenormalized
    isNegativeZero = primFloatIsNegativeZero
    isIEEE         = primFloatIsIEEE


-- data  Double

instance Eq Double where
  a == b = primDoubleEq a b
  a /= b = primDoubleNe a b

instance Ord Double where
  a <  b = primDoubleLt a b
  a <= b = primDoubleLe a b
  a >= b = primDoubleGe a b
  a >  b = primDoubleGt a b

instance Num Double where
 a + b    = primDoubleAdd a b
 a - b    = primDoubleSub a b
 a * b    = primDoubleMul a b
 negate a = (0 - a)
 abs    a = primDoubleAbs a
 signum a = primDoubleSignum a
 fromInteger i = primDoubleFromInteger i

instance  Real Double where
    toRational x = case decodeFloat x of (m,n) -> (m%1)*(b%1)^^n
			where b     = floatRadix  x

instance  Fractional Double  where
  x / y = primDoubleDiv x y
  fromRational x =
      let f ex = let y :: Double
                     y  = encodeFloat (round (x * (1 % bd) ^^ ex)) ex
                     e' = snd (decodeFloat y)
                     bd = floatRadix x'
                 in if e' == ex then y else f e'
          e    = snd (decodeFloat (fromInteger (numerator x) `asTypeOf` x'
        	                                / fromInteger (denominator x)))
          x'   = f e
      in x'


instance  Floating Double where
    pi                  =  primDoublePi
    exp x               =  primDoubleExp x
    log x               =  primDoubleLog x
    sqrt x              =  primDoubleSqrt x
    sin x               =  primDoubleSin x
    cos x               =  primDoubleCos x
    tan x               =  primDoubleTan x
    asin x              =  primDoubleAsin x
    acos x              =  primDoubleAcos x
    atan x              =  primDoubleAtan x
    sinh x              = fromRational (1%2) * (exp x - exp (-x))
    cosh x              = fromRational (1%2) * (exp x + exp (-x))
    tanh x              = (a-b)/(a+b) where a = exp x ; b = exp (-x)
    asinh x = log (x + sqrt (1+x*x))
    acosh x = log (x + (x+1) * sqrt ((x-1)/(x+1)))
    atanh x = log ((x+1) / sqrt (1 - x*x))

instance  RealFrac Double where
    properFraction x =
        case decodeFloat x of
	  (m,n) -> if n >= 0 then
			(fromInteger m * fromInteger (floatRadix x) ^ n, 0)
		    else
			case quotRem m ((floatRadix x)^(negate n)) of
		          (w,r) -> (fromInteger w, encodeFloat r n)


instance  RealFloat Double  where
    floatRadix    = primDoubleRadix 
    floatDigits   = primDoubleDigits
    floatRange    = primDoubleRange
    decodeFloat x = primDecodeDouble x
    encodeFloat x y = primEncodeDouble x y

    isNaN 	   = primDoubleIsNaN
    isInfinite     = primDoubleIsInfinite
    isDenormalized = primDoubleIsDenormalized
    isNegativeZero = primDoubleIsNegativeZero
    isIEEE         = primDoubleIsIEEE

-- The Enum instances for Floats and Doubles are slightly unusual.
-- The `toEnum' function truncates numbers to Int.  The definitions
-- of enumFrom and enumFromThen allow floats to be used in arithmetic
-- series: [0,0.1 .. 0.95].  However, roundoff errors make these somewhat
-- dubious.  This example may have either 10 or 11 elements, depending on
-- how 0.1 is represented.


instance  Enum Float  where
    succ x           =  x+1
    pred x           =  x-1
    toEnum           =  fromIntegral
    fromEnum         =  fromInteger . truncate   -- may overflow
    enumFrom         =  numericEnumFrom
    enumFromThen     =  numericEnumFromThen
    enumFromTo       =  numericEnumFromTo
    enumFromThenTo   =  numericEnumFromThenTo


instance  Enum Double  where
    succ x           =  x+1
    pred x           =  x-1
    toEnum           =  fromIntegral
    fromEnum         =  fromInteger . truncate   -- may overflow
    enumFrom         =  numericEnumFrom
    enumFromThen     =  numericEnumFromThen
    enumFromTo       =  numericEnumFromTo
    enumFromThenTo   =  numericEnumFromThenTo


numericEnumFrom         :: (Fractional a) => a -> [a]

numericEnumFromThen     :: (Fractional a) => a -> a -> [a]

numericEnumFromTo       :: (Fractional a, Ord a) => a -> a -> [a]

numericEnumFromThenTo   :: (Fractional a, Ord a) => a -> a -> a -> [a]
numericEnumFrom         =  iterate (+1)
numericEnumFromThen n m =  iterate (+(m-n)) n
numericEnumFromTo n m   =  takeWhile (<= m+1/2) (numericEnumFrom n)
numericEnumFromThenTo n n' m = takeWhile p (numericEnumFromThen n n')
                             where
                               p | n' > n    = (<= m + (n'-n)/2)
                                 | otherwise = (>= m + (n'-n)/2)

-- Lists

-- This data declaration is not legal Haskell
-- but it indicates the idea

-- data  [] a =  [] | a : [a]  deriving (Eq, Ord)

instance Eq a => Eq [a] where
  []     == []     = True
  (a:as) == (b:bs) = a == b && as == bs
  _      == _      = False

instance Ord a => Ord [a] where
  compare []     []     = EQ
  compare []     (b:bs) = LT
  compare (a:as) []     = GT
  compare (a:as) (b:bs) =
    case compare a b of
    	LT -> LT
	EQ -> compare as bs
	GT -> GT


instance Functor [] where
    fmap = map


instance  Monad []  where
    m >>= k          = concat (map k m)
    return x         = [x]
    fail s           = []

-- Tuples

-- data (,) a b = (,) a b
--   deriving (Eq, Ord, Bounded)

-- the types are defined in Hat to refer to them via T
-- hence instances cannot be derived

instance (Eq a,Eq b) => Eq (a,b) where
  (a,b) == (a',b') = a == a' && b == b'

instance (Ord a,Ord b) => Ord (a,b) where
  compare (a,b) (a',b') =
    case compare a a' of
    	LT -> LT
	EQ -> compare b b'
	GT -> GT

instance (Bounded a, Bounded b) => Bounded (a,b) where
  minBound = (minBound, minBound)
  maxBound = (maxBound, maxBound)


instance (Eq a,Eq b,Eq c) => Eq (a,b,c) where
  (a,b,c) == (a',b',c') = a == a' && b == b' && c == c'

instance (Ord a,Ord b,Ord c) => Ord (a,b,c) where
  compare (a,b,c) (a',b',c') =
    case compare a a' of
	EQ -> case compare b b' of
                EQ -> compare c c'
                x  -> x
	x  -> x

instance (Bounded a,Bounded b,Bounded c) => Bounded (a,b,c) where
  minBound = (minBound,minBound,minBound)
  maxBound = (maxBound,maxBound,maxBound)


instance (Eq a,Eq b,Eq c,Eq d) => Eq (a,b,c,d) where
  (a,b,c,d) == (a',b',c',d') = a == a' && b == b' && c == c' && d == d'

instance (Ord a,Ord b,Ord c,Ord d) => Ord (a,b,c,d) where
  compare (a,b,c,d) (a',b',c',d') =
    case compare a a' of
	EQ -> case compare b b' of
                EQ -> case compare c c' of
                        EQ -> compare d d'
                        x  -> x
                x  -> x
	x  -> x

instance (Bounded a,Bounded b,Bounded c,Bounded d) => Bounded (a,b,c,d) where
  minBound = (minBound,minBound,minBound,minBound)
  maxBound = (maxBound,maxBound,maxBound,maxBound)


instance (Eq a,Eq b,Eq c,Eq d,Eq e) => Eq (a,b,c,d,e) where
  (a,b,c,d,e) == (a',b',c',d',e') = 
    a == a' && b == b' && c == c' && d == d' && e == e'

instance (Ord a,Ord b,Ord c,Ord d,Ord e) => Ord (a,b,c,d,e) where
  compare (a,b,c,d,e) (a',b',c',d',e') =
    case compare a a' of
	EQ -> case compare b b' of
                EQ -> case compare c c' of
                        EQ -> case compare d d' of
                                EQ -> compare e e'
                                x  -> x
                        x  -> x
                x  -> x
	x  -> x

instance (Bounded a,Bounded b,Bounded c,Bounded d,Bounded e) 
  => Bounded (a,b,c,d,e) where
  minBound = (minBound,minBound,minBound,minBound,minBound)
  maxBound = (maxBound,maxBound,maxBound,maxBound,maxBound)


instance (Eq a,Eq b,Eq c,Eq d,Eq e,Eq f) => Eq (a,b,c,d,e,f) where
  (a,b,c,d,e,f) == (a',b',c',d',e',f') = 
    a == a' && b == b' && c == c' && d == d' && e == e' && f == f'

instance (Ord a,Ord b,Ord c,Ord d,Ord e,Ord f) => Ord (a,b,c,d,e,f) where
  compare (a,b,c,d,e,f) (a',b',c',d',e',f') =
    case compare a a' of
	EQ -> case compare b b' of
                EQ -> case compare c c' of
                        EQ -> case compare d d' of
                                EQ -> case compare e e' of
                                        EQ -> compare f f'
                                        x  -> x
                                x  -> x
                        x  -> x
                x  -> x
	x  -> x

instance (Bounded a,Bounded b,Bounded c,Bounded d,Bounded e,Bounded f) 
  => Bounded (a,b,c,d,e,f) where
  minBound = (minBound,minBound,minBound,minBound,minBound,minBound)
  maxBound = (maxBound,maxBound,maxBound,maxBound,maxBound,maxBound)


instance (Eq a,Eq b,Eq c,Eq d,Eq e,Eq f,Eq g) => Eq (a,b,c,d,e,f,g) where
  (a,b,c,d,e,f,g) == (a',b',c',d',e',f',g') = 
    a == a' && b == b' && c == c' && d == d' && e == e' && f == f' && g == g'

instance (Ord a,Ord b,Ord c,Ord d,Ord e,Ord f,Ord g) 
  => Ord (a,b,c,d,e,f,g) where
  compare (a,b,c,d,e,f,g) (a',b',c',d',e',f',g') =
    case compare a a' of
	EQ -> case compare b b' of
                EQ -> case compare c c' of
                        EQ -> case compare d d' of
                                EQ -> case compare e e' of
                                        EQ -> case compare f f' of
                                                EQ -> compare g g'
                                                x  -> x
                                        x  -> x
                                x  -> x
                        x  -> x
                x  -> x
	x  -> x

instance (Bounded a,Bounded b,Bounded c,Bounded d,Bounded e,Bounded f
         ,Bounded g) 
  => Bounded (a,b,c,d,e,f,g) where
  minBound = (minBound,minBound,minBound,minBound,minBound,minBound,minBound)
  maxBound = (maxBound,maxBound,maxBound,maxBound,maxBound,maxBound,maxBound)


instance (Eq a,Eq b,Eq c,Eq d,Eq e,Eq f,Eq g,Eq h) 
  => Eq (a,b,c,d,e,f,g,h) where
  (a,b,c,d,e,f,g,h) == (a',b',c',d',e',f',g',h') = 
    a == a' && b == b' && c == c' && d == d' && e == e' && f == f' && g == g'
    && h == h'

instance (Ord a,Ord b,Ord c,Ord d,Ord e,Ord f,Ord g,Ord h) 
  => Ord (a,b,c,d,e,f,g,h) where
  compare (a,b,c,d,e,f,g,h) (a',b',c',d',e',f',g',h') =
    case compare a a' of
	EQ -> case compare b b' of
                EQ -> case compare c c' of
                        EQ -> case compare d d' of
                                EQ -> case compare e e' of
                                        EQ -> case compare f f' of
                                                EQ -> case compare g g' of
                                                        EQ -> compare h h'
                                                x  -> x
                                        x  -> x
                                x  -> x
                        x  -> x
                x  -> x
	x  -> x

instance (Bounded a,Bounded b,Bounded c,Bounded d,Bounded e,Bounded f
         ,Bounded g,Bounded h) 
  => Bounded (a,b,c,d,e,f,g,h) where
  minBound = (minBound,minBound,minBound,minBound,minBound,minBound,minBound
             ,minBound)
  maxBound = (maxBound,maxBound,maxBound,maxBound,maxBound,maxBound,maxBound
             ,maxBound)

instance (Eq a,Eq b,Eq c,Eq d,Eq e,Eq f,Eq g,Eq h,Eq i) 
  => Eq (a,b,c,d,e,f,g,h,i) where
  (a,b,c,d,e,f,g,h,i) == (a',b',c',d',e',f',g',h',i') = 
    a == a' && b == b' && c == c' && d == d' && e == e' && f == f' && g == g'
    && h == h' && i == i'

instance (Ord a,Ord b,Ord c,Ord d,Ord e,Ord f,Ord g,Ord h,Ord i) 
  => Ord (a,b,c,d,e,f,g,h,i) where
  compare (a,b,c,d,e,f,g,h,i) (a',b',c',d',e',f',g',h',i') =
    case compare a a' of
     EQ -> case compare b b' of
      EQ -> case compare c c' of
       EQ -> case compare d d' of
        EQ -> case compare e e' of
         EQ -> case compare f f' of
          EQ -> case compare g g' of
           EQ -> case compare h h' of
            EQ -> compare i i'
            x  -> x
           x  -> x
          x  -> x
         x  -> x
        x  -> x
       x  -> x
      x  -> x

instance (Bounded a,Bounded b,Bounded c,Bounded d,Bounded e,Bounded f
         ,Bounded g,Bounded h,Bounded i) 
  => Bounded (a,b,c,d,e,f,g,h,i) where
  minBound = (minBound,minBound,minBound,minBound,minBound,minBound,minBound
             ,minBound,minBound)
  maxBound = (maxBound,maxBound,maxBound,maxBound,maxBound,maxBound,maxBound
             ,maxBound,maxBound)


instance (Eq a,Eq b,Eq c,Eq d,Eq e,Eq f,Eq g,Eq h,Eq i,Eq j) 
  => Eq (a,b,c,d,e,f,g,h,i,j) where
  (a,b,c,d,e,f,g,h,i,j) == (a',b',c',d',e',f',g',h',i',j') = 
    a == a' && b == b' && c == c' && d == d' && e == e' && f == f' && g == g'
    && h == h' && i == i' && j == j'

instance (Ord a,Ord b,Ord c,Ord d,Ord e,Ord f,Ord g,Ord h,Ord i,Ord j) 
  => Ord (a,b,c,d,e,f,g,h,i,j) where
  compare (a,b,c,d,e,f,g,h,i,j) (a',b',c',d',e',f',g',h',i',j') =
    case compare a a' of
     EQ -> case compare b b' of
      EQ -> case compare c c' of
       EQ -> case compare d d' of
        EQ -> case compare e e' of
         EQ -> case compare f f' of
          EQ -> case compare g g' of
           EQ -> case compare h h' of
            EQ -> case compare i i' of
             EQ -> compare j j'
             x  -> x
            x  -> x
           x  -> x
          x  -> x
         x  -> x
        x  -> x
       x  -> x
      x  -> x

instance (Bounded a,Bounded b,Bounded c,Bounded d,Bounded e,Bounded f
         ,Bounded g,Bounded h,Bounded i,Bounded j) 
  => Bounded (a,b,c,d,e,f,g,h,i,j) where
  minBound = (minBound,minBound,minBound,minBound,minBound,minBound,minBound
             ,minBound,minBound,minBound)
  maxBound = (maxBound,maxBound,maxBound,maxBound,maxBound,maxBound,maxBound
             ,maxBound,maxBound,maxBound)


instance (Eq a,Eq b,Eq c,Eq d,Eq e,Eq f,Eq g,Eq h,Eq i,Eq j,Eq k) 
  => Eq (a,b,c,d,e,f,g,h,i,j,k) where
  (a,b,c,d,e,f,g,h,i,j,k) == (a',b',c',d',e',f',g',h',i',j',k') = 
    a == a' && b == b' && c == c' && d == d' && e == e' && f == f' && g == g'
    && h == h' && i == i' && j == j' && k == k'

instance (Ord a,Ord b,Ord c,Ord d,Ord e,Ord f,Ord g,Ord h,Ord i,Ord j,Ord k) 
  => Ord (a,b,c,d,e,f,g,h,i,j,k) where
  compare (a,b,c,d,e,f,g,h,i,j,k) (a',b',c',d',e',f',g',h',i',j',k') =
    case compare a a' of
     EQ -> case compare b b' of
      EQ -> case compare c c' of
       EQ -> case compare d d' of
        EQ -> case compare e e' of
         EQ -> case compare f f' of
          EQ -> case compare g g' of
           EQ -> case compare h h' of
            EQ -> case compare i i' of
             EQ -> case compare j j' of
              EQ -> compare k k'
              x  -> x
             x  -> x
            x  -> x
           x  -> x
          x  -> x
         x  -> x
        x  -> x
       x  -> x
      x  -> x

instance (Bounded a,Bounded b,Bounded c,Bounded d,Bounded e,Bounded f
         ,Bounded g,Bounded h,Bounded i,Bounded j,Bounded k) 
  => Bounded (a,b,c,d,e,f,g,h,i,j,k) where
  minBound = (minBound,minBound,minBound,minBound,minBound,minBound,minBound
             ,minBound,minBound,minBound,minBound)
  maxBound = (maxBound,maxBound,maxBound,maxBound,maxBound,maxBound,maxBound
             ,maxBound,maxBound,maxBound,maxBound)


instance (Eq a,Eq b,Eq c,Eq d,Eq e,Eq f,Eq g,Eq h,Eq i,Eq j,Eq k,Eq l) 
  => Eq (a,b,c,d,e,f,g,h,i,j,k,l) where
  (a,b,c,d,e,f,g,h,i,j,k,l) == (a',b',c',d',e',f',g',h',i',j',k',l') = 
    a == a' && b == b' && c == c' && d == d' && e == e' && f == f' && g == g'
    && h == h' && i == i' && j == j' && k == k' && l == l'

instance (Ord a,Ord b,Ord c,Ord d,Ord e,Ord f,Ord g,Ord h,Ord i,Ord j,Ord k
         ,Ord l) 
  => Ord (a,b,c,d,e,f,g,h,i,j,k,l) where
  compare (a,b,c,d,e,f,g,h,i,j,k,l) (a',b',c',d',e',f',g',h',i',j',k',l') =
    case compare a a' of
     EQ -> case compare b b' of
      EQ -> case compare c c' of
       EQ -> case compare d d' of
        EQ -> case compare e e' of
         EQ -> case compare f f' of
          EQ -> case compare g g' of
           EQ -> case compare h h' of
            EQ -> case compare i i' of
             EQ -> case compare j j' of
              EQ -> case compare k k' of
                EQ -> compare l l'
                x  -> x
              x  -> x
             x  -> x
            x  -> x
           x  -> x
          x  -> x
         x  -> x
        x  -> x
       x  -> x
      x  -> x

instance (Bounded a,Bounded b,Bounded c,Bounded d,Bounded e,Bounded f
         ,Bounded g,Bounded h,Bounded i,Bounded j,Bounded k,Bounded l) 
  => Bounded (a,b,c,d,e,f,g,h,i,j,k,l) where
  minBound = (minBound,minBound,minBound,minBound,minBound,minBound,minBound
             ,minBound,minBound,minBound,minBound,minBound)
  maxBound = (maxBound,maxBound,maxBound,maxBound,maxBound,maxBound,maxBound
             ,maxBound,maxBound,maxBound,maxBound,maxBound)


instance (Eq a,Eq b,Eq c,Eq d,Eq e,Eq f,Eq g,Eq h,Eq i,Eq j,Eq k,Eq l,Eq m) 
  => Eq (a,b,c,d,e,f,g,h,i,j,k,l,m) where
  (a,b,c,d,e,f,g,h,i,j,k,l,m) == (a',b',c',d',e',f',g',h',i',j',k',l',m') = 
    a == a' && b == b' && c == c' && d == d' && e == e' && f == f' && g == g'
    && h == h' && i == i' && j == j' && k == k' && l == l' && m == m'

instance (Ord a,Ord b,Ord c,Ord d,Ord e,Ord f,Ord g,Ord h,Ord i,Ord j,Ord k
         ,Ord l,Ord m) 
  => Ord (a,b,c,d,e,f,g,h,i,j,k,l,m) where
  compare (a,b,c,d,e,f,g,h,i,j,k,l,m) 
    (a',b',c',d',e',f',g',h',i',j',k',l',m') =
    case compare a a' of
     EQ -> case compare b b' of
      EQ -> case compare c c' of
       EQ -> case compare d d' of
        EQ -> case compare e e' of
         EQ -> case compare f f' of
          EQ -> case compare g g' of
           EQ -> case compare h h' of
            EQ -> case compare i i' of
             EQ -> case compare j j' of
              EQ -> case compare k k' of
               EQ -> case compare l l' of
                EQ -> compare m m'
                x  -> x
               x  -> x
              x  -> x
             x  -> x
            x  -> x
           x  -> x
          x  -> x
         x  -> x
        x  -> x
       x  -> x
      x  -> x

instance (Bounded a,Bounded b,Bounded c,Bounded d,Bounded e,Bounded f
         ,Bounded g,Bounded h,Bounded i,Bounded j,Bounded k,Bounded l
         ,Bounded m) 
  => Bounded (a,b,c,d,e,f,g,h,i,j,k,l,m) where
  minBound = (minBound,minBound,minBound,minBound,minBound,minBound,minBound
             ,minBound,minBound,minBound,minBound,minBound,minBound)
  maxBound = (maxBound,maxBound,maxBound,maxBound,maxBound,maxBound,maxBound
             ,maxBound,maxBound,maxBound,maxBound,maxBound,maxBound)

instance (Eq a,Eq b,Eq c,Eq d,Eq e,Eq f,Eq g,Eq h,Eq i,Eq j,Eq k,Eq l,Eq m
         ,Eq n) 
  => Eq (a,b,c,d,e,f,g,h,i,j,k,l,m,n) where
  (a,b,c,d,e,f,g,h,i,j,k,l,m,n) == 
    (a',b',c',d',e',f',g',h',i',j',k',l',m',n') = 
    a == a' && b == b' && c == c' && d == d' && e == e' && f == f' && g == g'
    && h == h' && i == i' && j == j' && k == k' && l == l' && m == m' 
    && n == n'

instance (Ord a,Ord b,Ord c,Ord d,Ord e,Ord f,Ord g,Ord h,Ord i,Ord j,Ord k
         ,Ord l,Ord m,Ord n) 
  => Ord (a,b,c,d,e,f,g,h,i,j,k,l,m,n) where
  compare (a,b,c,d,e,f,g,h,i,j,k,l,m,n) 
    (a',b',c',d',e',f',g',h',i',j',k',l',m',n') =
    case compare a a' of
     EQ -> case compare b b' of
      EQ -> case compare c c' of
       EQ -> case compare d d' of
        EQ -> case compare e e' of
         EQ -> case compare f f' of
          EQ -> case compare g g' of
           EQ -> case compare h h' of
            EQ -> case compare i i' of
             EQ -> case compare j j' of
              EQ -> case compare k k' of
               EQ -> case compare l l' of
                EQ -> case compare m m' of
                 EQ -> compare n n'
                 x  -> x
                x  -> x
               x  -> x
              x  -> x
             x  -> x
            x  -> x
           x  -> x
          x  -> x
         x  -> x
        x  -> x
       x  -> x
      x  -> x

instance (Bounded a,Bounded b,Bounded c,Bounded d,Bounded e,Bounded f
         ,Bounded g,Bounded h,Bounded i,Bounded j,Bounded k,Bounded l
         ,Bounded m,Bounded n) 
  => Bounded (a,b,c,d,e,f,g,h,i,j,k,l,m,n) where
  minBound = (minBound,minBound,minBound,minBound,minBound,minBound,minBound
             ,minBound,minBound,minBound,minBound,minBound,minBound,minBound)
  maxBound = (maxBound,maxBound,maxBound,maxBound,maxBound,maxBound,maxBound
             ,maxBound,maxBound,maxBound,maxBound,maxBound,maxBound,maxBound)


instance (Eq a,Eq b,Eq c,Eq d,Eq e,Eq f,Eq g,Eq h,Eq i,Eq j,Eq k,Eq l,Eq m
         ,Eq n,Eq o) 
  => Eq (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) where
  (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) == 
    (a',b',c',d',e',f',g',h',i',j',k',l',m',n',o') = 
    a == a' && b == b' && c == c' && d == d' && e == e' && f == f' && g == g'
    && h == h' && i == i' && j == j' && k == k' && l == l' && m == m' 
    && n == n' && o == o'

instance (Ord a,Ord b,Ord c,Ord d,Ord e,Ord f,Ord g,Ord h,Ord i,Ord j,Ord k
         ,Ord l,Ord m,Ord n,Ord o) 
  => Ord (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) where
  compare (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) 
    (a',b',c',d',e',f',g',h',i',j',k',l',m',n',o') =
    case compare a a' of
     EQ -> case compare b b' of
      EQ -> case compare c c' of
       EQ -> case compare d d' of
        EQ -> case compare e e' of
         EQ -> case compare f f' of
          EQ -> case compare g g' of
           EQ -> case compare h h' of
            EQ -> case compare i i' of
             EQ -> case compare j j' of
              EQ -> case compare k k' of
               EQ -> case compare l l' of
                EQ -> case compare m m' of
                 EQ -> case compare n n' of
                  EQ -> compare o o'
                  x  -> x
                 x  -> x
                x  -> x
               x  -> x
              x  -> x
             x  -> x
            x  -> x
           x  -> x
          x  -> x
         x  -> x
        x  -> x
       x  -> x
      x  -> x

instance (Bounded a,Bounded b,Bounded c,Bounded d,Bounded e,Bounded f
         ,Bounded g,Bounded h,Bounded i,Bounded j,Bounded k,Bounded l
         ,Bounded m,Bounded n,Bounded o) 
  => Bounded (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) where
  minBound = (minBound,minBound,minBound,minBound,minBound,minBound,minBound
             ,minBound,minBound,minBound,minBound,minBound,minBound,minBound
             ,minBound)
  maxBound = (maxBound,maxBound,maxBound,maxBound,maxBound,maxBound,maxBound
             ,maxBound,maxBound,maxBound,maxBound,maxBound,maxBound,maxBound
             ,maxBound)


-- component projections for pairs:
-- (NB: not provided for triples, quadruples, etc.)

fst              :: (a,b) -> a
fst (x,y)        =  x


snd              :: (a,b) -> b
snd (x,y)        =  y

-- curry converts an uncurried function to a curried function;
-- uncurry converts a curried function to a function on pairs.

curry            :: ((a, b) -> c) -> a -> b -> c
curry f x y      =  f (x, y)


uncurry          :: (a -> b -> c) -> ((a, b) -> c)
uncurry f p      =  f (fst p) (snd p)

-- Misc functions

-- until p f  yields the result of applying f until p holds.

until            :: (a -> Bool) -> (a -> a) -> a -> a
until p f x 
     | p x       =  x
     | otherwise =  until p f (f x)

-- asTypeOf is a type-restricted version of const.  It is usually used
-- as an infix operator, and its typing forces its first argument
-- (which is usually overloaded) to have the same type as the second.

asTypeOf         :: a -> a -> a
asTypeOf         =  const

-- error stops execution and displays an error message


-- error            :: String -> a
-- error            =  primError

-- It is expected that compilers will recognize this and insert error
-- messages that are more appropriate to the context in which undefined 
-- appears. 


-- undefined        :: a
-- undefined        =  error "Prelude.undefined"



-- A.1  Prelude PreludeList
-- Standard list functions
-- module PreludeList (
--    map, (++), filter, concat,
--    head, last, tail, init, null, length, (!!), 
--    foldl, foldl1, scanl, scanl1, foldr, foldr1, scanr, scanr1,
--    iterate, repeat, replicate, cycle,
--    take, drop, splitAt, takeWhile, dropWhile, span, break,
--    lines, words, unlines, unwords, reverse, and, or,
--    any, all, elem, notElem, lookup,
--    sum, product, maximum, minimum, concatMap, 
--    zip, zip3, zipWith, zipWith3, unzip, unzip3)
--  where


-- HACK: for desugared list comprehensions

_foldr :: (a -> b -> b) -> [a] -> b -> b
_foldr f [] d = d
_foldr f ((:) x xs) d = f x (_foldr f xs d)

_filter :: Bool -> ([a]->[a]) -> [a] -> [a]
_filter b e r = if b then e r else r


infixl 9  !!
infixr 5  ++
infix  4  `elem`, `notElem`

-- Map and append

map :: (a -> b) -> [a] -> [b]
map f []     = []
map f (x:xs) = f x : map f xs


(++) :: [a] -> [a] -> [a]
[]     ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)


filter :: (a -> Bool) -> [a] -> [a]
filter p [] = []
filter p (x:xs) | p x       = x : filter p xs
                | otherwise = filter p xs


concat :: [[a]] -> [a]
concat xss = foldr (++) [] xss


-- head and tail extract the first element and remaining elements,
-- respectively, of a list, which must be non-empty.  last and init
-- are the dual functions working from the end of a finite list,
-- rather than the beginning.


head             :: [a] -> a
head (x:_)       =  x
head []          =  error "Prelude.head: empty list"


last             :: [a] -> a
last [x]         =  x
last (_:xs)      =  last xs
last []          =  error "Prelude.last: empty list"


tail             :: [a] -> [a]
tail (_:xs)      =  xs
tail []          =  error "Prelude.tail: empty list"


init             :: [a] -> [a]
init [x]         =  []
init (x:xs)      =  x : init xs
init []          =  error "Prelude.init: empty list"


null             :: [a] -> Bool
null []          =  True
null (_:_)       =  False

-- length returns the length of a finite list as an Int.

length           :: [a] -> Int
length []        =  0
length (_:l)     =  1 + length l

-- List index (subscript) operator, 0-origin

(!!)                :: [a] -> Int -> a
xs     !! n | n < 0 =  error "Prelude.!!: negative index"
[]     !! _         =  error "Prelude.!!: index too large"
(x:_)  !! 0         =  x
(_:xs) !! n         =  xs !! (n-1)

-- foldl, applied to a binary operator, a starting value (typically the
-- left-identity of the operator), and a list, reduces the list using
-- the binary operator, from left to right:
--  foldl f z [x1, x2, ..., xn] == (...((z `f` x1) `f` x2) `f`...) `f` xn
-- foldl1 is a variant that has no starting value argument, and  thus must
-- be applied to non-empty lists.  scanl is similar to foldl, but returns
-- a list of successive reduced values from the left:
--      scanl f z [x1, x2, ...] == [z, z `f` x1, (z `f` x1) `f` x2, ...]
-- Note that  last (scanl f z xs) == foldl f z xs.
-- scanl1 is similar, again without the starting element:
--      scanl1 f [x1, x2, ...] == [x1, x1 `f` x2, ...]


foldl            :: (a -> b -> a) -> a -> [b] -> a
foldl f z []     =  z
foldl f z (x:xs) =  foldl f (f z x) xs


foldl1           :: (a -> a -> a) -> [a] -> a
foldl1 f (x:xs)  =  foldl f x xs
foldl1 _ []      =  error "Prelude.foldl1: empty list"


scanl            :: (a -> b -> a) -> a -> [b] -> [a]
scanl f q xs     =  q : (case xs of
                            []   -> []
                            x:xs -> scanl f (f q x) xs)


scanl1           :: (a -> a -> a) -> [a] -> [a]
scanl1 f (x:xs)  =  scanl f x xs
scanl1 _ []      =  []

-- foldr, foldr1, scanr, and scanr1 are the right-to-left duals of the
-- above functions.


foldr            :: (a -> b -> b) -> b -> [a] -> b
foldr f z []     =  z
foldr f z (x:xs) =  f x (foldr f z xs)


foldr1           :: (a -> a -> a) -> [a] -> a
foldr1 f [x]     =  x
foldr1 f (x:xs)  =  f x (foldr1 f xs)
foldr1 _ []      =  error "Prelude.foldr1: empty list"


scanr             :: (a -> b -> b) -> b -> [a] -> [b]
scanr f q0 []     =  [q0]
scanr f q0 (x:xs) =  f x q : qs
                     where qs@(q:_) = scanr f q0 xs 


scanr1          :: (a -> a -> a) -> [a] -> [a]
scanr1 f []     =  []
scanr1 f [x]    =  [x]
scanr1 f (x:xs) =  f x q : qs
                   where qs@(q:_) = scanr1 f xs 

-- iterate f x returns an infinite list of repeated applications of f to x:
-- iterate f x == [x, f x, f (f x), ...]

iterate          :: (a -> a) -> a -> [a]
iterate f x      =  x : iterate f (f x)

-- repeat x is an infinite list, with x the value of every element.

repeat           :: a -> [a]
repeat x         =  xs where xs = x:xs

-- replicate n x is a list of length n with x the value of every element

replicate        :: Int -> a -> [a]
replicate n x    =  take n (repeat x)

-- cycle ties a finite list into a circular one, or equivalently,
-- the infinite repetition of the original list.  It is the identity
-- on infinite lists.


cycle            :: [a] -> [a]
cycle []         =  error "Prelude.cycle: empty list"
cycle xs         =  xs' where xs' = xs ++ xs'

-- take n, applied to a list xs, returns the prefix of xs of length n,
-- or xs itself if n > length xs.  drop n xs returns the suffix of xs
-- after the first n elements, or [] if n > length xs.  splitAt n xs
-- is equivalent to (take n xs, drop n xs).


take                   :: Int -> [a] -> [a]
take n _      | n <= 0 =  []
take _ []              =  []
take n (x:xs)          =  x : take (n-1) xs


drop                   :: Int -> [a] -> [a]
drop n xs     | n <= 0 =  xs
drop _ []              =  []
drop n (_:xs)          =  drop (n-1) xs


splitAt                  :: Int -> [a] -> ([a],[a])
splitAt n xs             =  (take n xs, drop n xs)

-- takeWhile, applied to a predicate p and a list xs, returns the longest
-- prefix (possibly empty) of xs of elements that satisfy p.  dropWhile p xs
-- returns the remaining suffix.  span p xs is equivalent to 
-- (takeWhile p xs, dropWhile p xs), while break p uses the negation of p.


takeWhile               :: (a -> Bool) -> [a] -> [a]
takeWhile p []          =  []
takeWhile p (x:xs) 
            | p x       =  x : takeWhile p xs
            | otherwise =  []


dropWhile               :: (a -> Bool) -> [a] -> [a]
dropWhile p []          =  []
dropWhile p xs@(x:xs')
            | p x       =  dropWhile p xs'
            | otherwise =  xs


span, break             :: (a -> Bool) -> [a] -> ([a],[a])
span p []            = ([],[])
span p xs@(x:xs') 
            | p x       =  (x:ys,zs) 
            | otherwise =  ([],xs)
                           where (ys,zs) = span p xs'

break p                 =  span (not . p)

-- lines breaks a string up into a list of strings at newline characters.
-- The resulting strings do not contain newlines.  Similary, words
-- breaks a string up into a list of words, which were delimited by
-- white space.  unlines and unwords are the inverse operations.
-- unlines joins lines with terminating newlines, and unwords joins
-- words with separating spaces.


lines            :: String -> [String]
lines ""         =  []
lines s          =  let (l, s') = break (== '\n') s
                      in  l : case s' of
                                []      -> []
                                (_:s'') -> lines s''


words            :: String -> [String]
words s          =  case dropWhile isSpace s of
                      "" -> []
                      s' -> w : words s''
                            where (w, s'') = break isSpace s'


unlines          :: [String] -> String
unlines          =  concatMap (++ "\n")


unwords          :: [String] -> String
unwords []       =  ""
unwords ws       =  foldr1 (\w s -> w ++ ' ':s) ws

-- reverse xs returns the elements of xs in reverse order.  xs must be finite.

reverse          :: [a] -> [a]
reverse          =  foldl (flip (:)) []

-- and returns the conjunction of a Boolean list.  For the result to be
-- True, the list must be finite; False, however, results from a False
-- value at a finite index of a finite or infinite list.  or is the
-- disjunctive dual of and.

and, or          :: [Bool] -> Bool
and              =  foldr (&&) True
or               =  foldr (||) False

-- Applied to a predicate and a list, any determines if any element
-- of the list satisfies the predicate.  Similarly, for all.

any, all         :: (a -> Bool) -> [a] -> Bool
any p            =  or . map p
all p            =  and . map p

-- elem is the list membership predicate, usually written in infix form,
-- e.g., x `elem` xs.  notElem is the negation.

elem, notElem    :: (Eq a) => a -> [a] -> Bool
elem x           =  any (== x)
notElem x        =  all (/= x)

-- lookup key assocs looks up a key in an association list.

lookup           :: (Eq a) => a -> [(a,b)] -> Maybe b
lookup key []    =  Nothing
lookup key ((x,y):xys)
    | key == x   =  Just y
    | otherwise  =  lookup key xys

-- sum and product compute the sum or product of a finite list of numbers.

sum, product     :: (Num a) => [a] -> a
sum              =  foldl (+) 0  
product          =  foldl (*) 1

-- maximum and minimum return the maximum or minimum value from a list,
-- which must be non-empty, finite, and of an ordered type.

maximum, minimum :: (Ord a) => [a] -> a
maximum []       =  error "Prelude.maximum: empty list"
maximum xs       =  foldl1 max xs

minimum []       =  error "Prelude.minimum: empty list"
minimum xs       =  foldl1 min xs


concatMap        :: (a -> [b]) -> [a] -> [b]
concatMap f      =  concat . map f

-- zip takes two lists and returns a list of corresponding pairs.  If one
-- input list is short, excess elements of the longer list are discarded.
-- zip3 takes three lists and returns a list of triples.  Zips for larger
-- tuples are in the List library


zip              :: [a] -> [b] -> [(a,b)]
zip              =  zipWith (,)

zip3             :: [a] -> [b] -> [c] -> [(a,b,c)]
zip3             =  zipWith3 (,,)

-- The zipWith family generalises the zip family by zipping with the
-- function given as the first argument, instead of a tupling function.
-- For example, zipWith (+) is applied to two lists to produce the list
-- of corresponding sums.


zipWith          :: (a->b->c) -> [a]->[b]->[c]
zipWith z (a:as) (b:bs)
                 =  z a b : zipWith z as bs
zipWith _ _ _    =  []


zipWith3         :: (a->b->c->d) -> [a]->[b]->[c]->[d]
zipWith3 z (a:as) (b:bs) (c:cs)
                 =  z a b c : zipWith3 z as bs cs
zipWith3 _ _ _ _ =  []


-- unzip transforms a list of pairs into a pair of lists.  


unzip            :: [(a,b)] -> ([a],[b])
unzip            =  foldr (\(a,b) ~(as,bs) -> (a:as,b:bs)) ([],[])


unzip3           :: [(a,b,c)] -> ([a],[b],[c])
unzip3           =  foldr (\(a,b,c) ~(as,bs,cs) -> (a:as,b:bs,c:cs))
                          ([],[],[])

-- A.2  Prelude PreludeText
-- module PreludeText (
--    ReadS, ShowS,
--    Read(readsPrec, readList),
--    Show(showsPrec, showList),
--    reads, shows, show, read, lex,
--    showChar, showString, readParen, showParen ) where

-- The instances of Read and Show for
-- Bool, Char, Maybe, Either, Ordering
-- are done via "deriving" clauses in Prelude.hs


type  ReadS a  = String -> [(a,String)]

type  ShowS    = String -> String


class  Read a  where
    readsPrec        :: Int -> ReadS a
    readList         :: ReadS [a]

-- Minimal complete definition:
-- readsPrec
    readList         = readParen False (\r -> [pr | ("[",s)  <- lex r,
                                                    pr       <- readl s])
                       where readl  s = [([],t)   | ("]",t)  <- lex s] ++
                                        [(x:xs,u) | (x,t)    <- reads s,
                                                    (xs,u)   <- readl' t]
                             readl' s = [([],t)   | ("]",t)  <- lex s] ++
                                        [(x:xs,v) | (",",t)  <- lex s,
                                                    (x,u)    <- reads t,
                                                    (xs,v)   <- readl' u]


class  Show a  where
    showsPrec        :: Int -> a -> ShowS
    show             :: a -> String 
    showList         :: [a] -> ShowS

-- Mimimal complete definition:
-- show or showsPrec
    showsPrec _ x s   = show x ++ s

    show x        = showsPrec 0 x ""

    showList []       = showString "[]"
    showList (x:xs)   = showChar '[' . shows x . showl xs
                        where showl []     = showChar ']'
                              showl (x:xs) = showChar ',' . shows x .
                                             showl xs

reads            :: (Read a) => ReadS a
reads            =  readsPrec 0


shows            :: (Show a) => a -> ShowS
shows            =  showsPrec 0


read             :: (Read a) => String -> a
read s           =  case [x | (x,t) <- reads s, ("","") <- lex t] of
                         [x] -> x
                         []  -> error "Prelude.read: no parse"
                         _   -> error "Prelude.read: ambiguous parse"


showChar         :: Char -> ShowS
showChar         =  (:)


showString       :: String -> ShowS
showString       =  (++)


showParen        :: Bool -> ShowS -> ShowS
showParen b p    =  if b then showChar '(' . p . showChar ')' else p


readParen        :: Bool -> ReadS a -> ReadS a
readParen b g    =  if b then mandatory else optional
                    where optional r  = g r ++ mandatory r
                          mandatory r = [(x,u) | ("(",s) <- lex r,
                                                 (x,t)   <- optional s,
                                                 (")",u) <- lex t    ]

-- This lexer is not completely faithful to the Haskell lexical syntax.
-- Current limitations:
--    Qualified names are not handled properly
--    Octal and hexidecimal numerics are not recognized as a single token
--    Comments are not treated properly


lex              :: ReadS String
lex ""           =  [("","")]
lex (c:s)
   | isSpace c   =  lex (dropWhile isSpace s)
lex ('\'':s)     =  [('\'':ch++"'", t) | (ch,'\'':t)  <- lexLitChar s,
                                         ch /= "'" ]
lex ('"':s)      =  [('"':str, t)      | (str,t) <- lexString s]
                    where
                    lexString ('"':s) = [("\"",s)]
                    lexString s = [(ch++str, u)
                                         | (ch,t)  <- lexStrItem s,
                                           (str,u) <- lexString t  ]

                    lexStrItem ('\\':'&':s) =  [("\\&",s)]
                    lexStrItem ('\\':c:s) | isSpace c
                                           =  [("\\&",t) | 
                                               '\\':t <-
                                                   [dropWhile isSpace s]]
                    lexStrItem s           =  lexLitChar s

lex (c:s) | isSingle c = [([c],s)]
          | isSym c    = [(c:sym,t)       | (sym,t) <- [span isSym s]]
          | isAlpha c  = [(c:nam,t)       | (nam,t) <- [span isIdChar s]]
          | isDigit c  = [(c:ds++fe,t)    | (ds,s)  <- [span isDigit s],
                                            (fe,t)  <- lexFracExp s     ]
          | otherwise  = []    -- bad character
             where
              isSingle c =  c `elem` ",;()[]{}_`"
              isSym c    =  c `elem` "!@#$%&*+./<=>?\\^|:-~"
              isIdChar c =  isAlphaNum c || c `elem` "_'"

              lexFracExp ('.':c:cs) | isDigit c
                            = [('.':ds++e,u) | (ds,t) <- lexDigits (c:cs),
                                               (e,u)  <- lexExp t]
              lexFracExp s  = lexExp s

              lexExp (e:s) | e `elem` "eE"
                       = [(e:c:ds,u) | (c:t)  <- [s], c `elem` "+-",
                                                 (ds,u) <- lexDigits t] ++
                         [(e:ds,t)   | (ds,t) <- lexDigits s]
              lexExp s = [("",s)]


instance  Show Int  where
    showsPrec n = showsPrec n . toInteger
-- Converting to Integer avoids
-- possible difficulty with minInt


instance  Read Int  where
  readsPrec p r = [(fromInteger i, t) | (i,t) <- readsPrec p r]
-- Reading at the Integer type avoids
-- possible difficulty with minInt


instance  Show Integer  where
    showsPrec           = showSigned showInt


instance  Read Integer  where
    readsPrec p         = readSigned readDec


instance  Show Float  where 
    showsPrec p         = showFloat
           

instance  Read Float  where
    readsPrec p         = readSigned readFloat


instance  Show Double  where
    showsPrec p         = showFloat


instance  Read Double  where
    readsPrec p         = readSigned readFloat


instance  Show ()  where
    showsPrec p () = showString "()"


instance Read () where
    readsPrec p    = readParen False
                            (\r -> [((),t) | ("(",s) <- lex r,
                                             (")",t) <- lex s ] )

instance  Show Char  where
    showsPrec p '\'' = showString "'\\''"
    showsPrec p c    = showChar '\'' . showLitChar c . showChar '\''

    showList cs = showChar '"' . showl cs
                 where showl ""       = showChar '"'
                       showl ('"':cs) = showString "\\\"" . showl cs
                       showl (c:cs)   = showLitChar c . showl cs


instance  Read Char  where
    readsPrec p      = readParen False
                            (\r -> [(c,t) | ('\'':s,t)<- lex r,
                                            (c,"\'")  <- readLitChar s])

    readList = readParen False (\r -> [(l,t) | ('"':s, t) <- lex r,
                                               (l,_)      <- readl s ])
        where readl ('"':s)      = [("",s)]
              readl ('\\':'&':s) = readl s
              readl s            = [(c:cs,u) | (c ,t) <- readLitChar s,
                                               (cs,u) <- readl t       ]


instance  (Show a) => Show [a]  where
    showsPrec p      = showList


instance  (Read a) => Read [a]  where
    readsPrec p      = readList

-- Tuples


instance  (Show a, Show b) => Show (a,b)  where
    showsPrec p (x,y) = showChar '(' . shows x . showChar ',' .
                                       shows y . showChar ')'


instance  (Read a, Read b) => Read (a,b)  where
    readsPrec p       = readParen False
                            (\r -> [((x,y), w) | ("(",s) <- lex r,
                                                 (x,t)   <- reads s,
                                                 (",",u) <- lex t,
                                                 (y,v)   <- reads u,
                                                 (")",w) <- lex v ] )


instance  (Read a, Read b, Read c) => Read (a,b,c)  where
    readsPrec p = readParen False
    	    	    	    (\r0 -> [((x1,x2,x3), w) | 
					("(",s1) <- lex r0,
					(x1, r1) <- reads s1,
					(",",s2) <- lex r1,
					(x2, r2) <- reads s2,
					(",",s3) <- lex r2,
					(x3, r3) <- reads s3,
					(")",w)  <- lex r3 ] )

instance  (Read a, Read b, Read c, Read d) => Read (a,b,c,d)  where
    readsPrec p = readParen False
    	    	    	    (\r0 -> [((x1,x2,x3,x4), w) | 
					("(",s1) <- lex r0,
					(x1, r1) <- reads s1,
					(",",s2) <- lex r1,
					(x2, r2) <- reads s2,
					(",",s3) <- lex r2,
					(x3, r3) <- reads s3,
					(",",s4) <- lex r3,
					(x4, r4) <- reads s4,
					(")",w)  <- lex r4 ] )

instance  (Read a, Read b, Read c, Read d, Read e) => Read (a,b,c,d,e)  where
    readsPrec p = readParen False
    	    	    	    (\r0 -> [((x1,x2,x3,x4,x5), w) | 
					("(",s1) <- lex r0,
					(x1, r1) <- reads s1,
					(",",s2) <- lex r1,
					(x2, r2) <- reads s2,
					(",",s3) <- lex r2,
					(x3, r3) <- reads s3,
					(",",s4) <- lex r3,
					(x4, r4) <- reads s4,
					(",",s5) <- lex r4,
					(x5, r5) <- reads s5,
					(")",w)  <- lex r5 ] )

instance  (Read a, Read b, Read c, Read d, Read e, Read f) =>
	 Read (a,b,c,d,e,f)  where
    readsPrec p = readParen False
    	    	    	    (\r0 -> [((x1,x2,x3,x4,x5,x6), w) | 
					("(",s1) <- lex r0,
					(x1, r1) <- reads s1,
					(",",s2) <- lex r1,
					(x2, r2) <- reads s2,
					(",",s3) <- lex r2,
					(x3, r3) <- reads s3,
					(",",s4) <- lex r3,
					(x4, r4) <- reads s4,
					(",",s5) <- lex r4,
					(x5, r5) <- reads s5,
					(",",s6) <- lex r5,
					(x6, r6) <- reads s6,
					(")",w)  <- lex r6 ] )

instance  (Read a, Read b, Read c, Read d, Read e, Read f, Read g) =>
	 Read (a,b,c,d,e,f,g)  where
    readsPrec p = readParen False
    	    	    	    (\r0 -> [((x1,x2,x3,x4,x5,x6,x7), w) | 
					("(",s1) <- lex r0,
					(x1, r1) <- reads s1,
					(",",s2) <- lex r1,
					(x2, r2) <- reads s2,
					(",",s3) <- lex r2,
					(x3, r3) <- reads s3,
					(",",s4) <- lex r3,
					(x4, r4) <- reads s4,
					(",",s5) <- lex r4,
					(x5, r5) <- reads s5,
					(",",s6) <- lex r5,
					(x6, r6) <- reads s6,
					(",",s7) <- lex r6,
					(x7, r7) <- reads s7,
					(")",w)  <- lex r7 ] )

instance (Read a, Read b, Read c, Read d, Read e, Read f, Read g,
	  Read h) =>
	 Read (a,b,c,d,e,f,g,h)  where
    readsPrec p = readParen False
    	    	    	    (\r0 -> [((x1,x2,x3,x4,x5,x6,x7,x8), w) | 
					("(",s1) <- lex r0,
					(x1, r1) <- reads s1,
					(",",s2) <- lex r1,
					(x2, r2) <- reads s2,
					(",",s3) <- lex r2,
					(x3, r3) <- reads s3,
					(",",s4) <- lex r3,
					(x4, r4) <- reads s4,
					(",",s5) <- lex r4,
					(x5, r5) <- reads s5,
					(",",s6) <- lex r5,
					(x6, r6) <- reads s6,
					(",",s7) <- lex r6,
					(x7, r7) <- reads s7,
					(",",s8) <- lex r7,
					(x8, r8) <- reads s8,
					(")",w)  <- lex r8 ] )

instance (Read a, Read b, Read c, Read d, Read e, Read f, Read g,
	  Read h, Read i) =>
	 Read (a,b,c,d,e,f,g,h,i)  where
    readsPrec p = readParen False
    	    	    	    (\r0 -> [((x1,x2,x3,x4,x5,x6,x7,x8,x9), w) | 
					("(",s1) <- lex r0,
					(x1, r1) <- reads s1,
					(",",s2) <- lex r1,
					(x2, r2) <- reads s2,
					(",",s3) <- lex r2,
					(x3, r3) <- reads s3,
					(",",s4) <- lex r3,
					(x4, r4) <- reads s4,
					(",",s5) <- lex r4,
					(x5, r5) <- reads s5,
					(",",s6) <- lex r5,
					(x6, r6) <- reads s6,
					(",",s7) <- lex r6,
					(x7, r7) <- reads s7,
					(",",s8) <- lex r7,
					(x8, r8) <- reads s8,
					(",",s9) <- lex r8,
					(x9, r9) <- reads s9,
					(")",w)  <- lex r9 ] )

instance (Read a, Read b, Read c, Read d, Read e, Read f, Read g,
	  Read h, Read i, Read j) =>
	 Read (a,b,c,d,e,f,g,h,i,j)  where
    readsPrec p = readParen False
    	    	    	    (\r0 -> [((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10), w) | 
					("(",s1) <- lex r0,
					(x1, r1) <- reads s1,
					(",",s2) <- lex r1,
					(x2, r2) <- reads s2,
					(",",s3) <- lex r2,
					(x3, r3) <- reads s3,
					(",",s4) <- lex r3,
					(x4, r4) <- reads s4,
					(",",s5) <- lex r4,
					(x5, r5) <- reads s5,
					(",",s6) <- lex r5,
					(x6, r6) <- reads s6,
					(",",s7) <- lex r6,
					(x7, r7) <- reads s7,
					(",",s8) <- lex r7,
					(x8, r8) <- reads s8,
					(",",s9) <- lex r8,
					(x9, r9) <- reads s9,
					(",",s10) <- lex r9,
					(x10, r10) <- reads s10,
					(")",w)  <- lex r10 ] )

instance (Read a, Read b, Read c, Read d, Read e, Read f, Read g,
	  Read h, Read i, Read j, Read k) =>
	 Read (a,b,c,d,e,f,g,h,i,j,k)  where
    readsPrec p = readParen False
    	    	    (\r0 -> [((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11), w) | 
					("(",s1) <- lex r0,
					(x1, r1) <- reads s1,
					(",",s2) <- lex r1,
					(x2, r2) <- reads s2,
					(",",s3) <- lex r2,
					(x3, r3) <- reads s3,
					(",",s4) <- lex r3,
					(x4, r4) <- reads s4,
					(",",s5) <- lex r4,
					(x5, r5) <- reads s5,
					(",",s6) <- lex r5,
					(x6, r6) <- reads s6,
					(",",s7) <- lex r6,
					(x7, r7) <- reads s7,
					(",",s8) <- lex r7,
					(x8, r8) <- reads s8,
					(",",s9) <- lex r8,
					(x9, r9) <- reads s9,
					(",",s10) <- lex r9,
					(x10, r10) <- reads s10,
					(",",s11) <- lex r10,
					(x11, r11) <- reads s11,
					(")",w)  <- lex r11 ] )

instance (Read a, Read b, Read c, Read d, Read e, Read f, Read g,
	  Read h, Read i, Read j, Read k, Read l) =>
	 Read (a,b,c,d,e,f,g,h,i,j,k,l)  where
    readsPrec p = readParen False
    	    	    (\r0 -> [((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12), w) | 
					("(",s1) <- lex r0,
					(x1, r1) <- reads s1,
					(",",s2) <- lex r1,
					(x2, r2) <- reads s2,
					(",",s3) <- lex r2,
					(x3, r3) <- reads s3,
					(",",s4) <- lex r3,
					(x4, r4) <- reads s4,
					(",",s5) <- lex r4,
					(x5, r5) <- reads s5,
					(",",s6) <- lex r5,
					(x6, r6) <- reads s6,
					(",",s7) <- lex r6,
					(x7, r7) <- reads s7,
					(",",s8) <- lex r7,
					(x8, r8) <- reads s8,
					(",",s9) <- lex r8,
					(x9, r9) <- reads s9,
					(",",s10) <- lex r9,
					(x10, r10) <- reads s10,
					(",",s11) <- lex r10,
					(x11, r11) <- reads s11,
					(",",s12) <- lex r11,
					(x12, r12) <- reads s12,
					(")",w)  <- lex r12 ] )

instance (Read a, Read b, Read c, Read d, Read e, Read f, Read g,
	  Read h, Read i, Read j, Read k, Read l, Read m) =>
	 Read (a,b,c,d,e,f,g,h,i,j,k,l,m)  where
    readsPrec p = readParen False
    	    	    (\r0 -> [((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13), w) | 
					("(",s1) <- lex r0,
					(x1, r1) <- reads s1,
					(",",s2) <- lex r1,
					(x2, r2) <- reads s2,
					(",",s3) <- lex r2,
					(x3, r3) <- reads s3,
					(",",s4) <- lex r3,
					(x4, r4) <- reads s4,
					(",",s5) <- lex r4,
					(x5, r5) <- reads s5,
					(",",s6) <- lex r5,
					(x6, r6) <- reads s6,
					(",",s7) <- lex r6,
					(x7, r7) <- reads s7,
					(",",s8) <- lex r7,
					(x8, r8) <- reads s8,
					(",",s9) <- lex r8,
					(x9, r9) <- reads s9,
					(",",s10) <- lex r9,
					(x10, r10) <- reads s10,
					(",",s11) <- lex r10,
					(x11, r11) <- reads s11,
					(",",s12) <- lex r11,
					(x12, r12) <- reads s12,
					(",",s13) <- lex r12,
					(x13, r13) <- reads s13,
					(")",w)  <- lex r13 ] )

instance (Read a, Read b, Read c, Read d, Read e, Read f, Read g,
	  Read h, Read i, Read j, Read k, Read l, Read m, Read n) =>
	 Read (a,b,c,d,e,f,g,h,i,j,k,l,m,n)  where
    readsPrec p = readParen False
    	    	    (\r0 -> [((x1,x2,x3,x4,x5,x6,x7
                              ,x8,x9,x10,x11,x12,x13,x14), w) | 
					("(",s1) <- lex r0,
					(x1, r1) <- reads s1,
					(",",s2) <- lex r1,
					(x2, r2) <- reads s2,
					(",",s3) <- lex r2,
					(x3, r3) <- reads s3,
					(",",s4) <- lex r3,
					(x4, r4) <- reads s4,
					(",",s5) <- lex r4,
					(x5, r5) <- reads s5,
					(",",s6) <- lex r5,
					(x6, r6) <- reads s6,
					(",",s7) <- lex r6,
					(x7, r7) <- reads s7,
					(",",s8) <- lex r7,
					(x8, r8) <- reads s8,
					(",",s9) <- lex r8,
					(x9, r9) <- reads s9,
					(",",s10) <- lex r9,
					(x10, r10) <- reads s10,
					(",",s11) <- lex r10,
					(x11, r11) <- reads s11,
					(",",s12) <- lex r11,
					(x12, r12) <- reads s12,
					(",",s13) <- lex r12,
					(x13, r13) <- reads s13,
					(",",s14) <- lex r13,
					(x14, r14) <- reads s14,
					(")",w)  <- lex r14 ] )

instance (Read a, Read b, Read c, Read d, Read e, Read f, Read g,
	  Read h, Read i, Read j, Read k, Read l, Read m, Read n, Read o) =>
	 Read (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o)  where
    readsPrec p = readParen False
    	    	    (\r0 -> [((x1,x2,x3,x4,x5,x6,x7
                              ,x8,x9,x10,x11,x12,x13,x14,x15), w) | 
					("(",s1) <- lex r0,
					(x1, r1) <- reads s1,
					(",",s2) <- lex r1,
					(x2, r2) <- reads s2,
					(",",s3) <- lex r2,
					(x3, r3) <- reads s3,
					(",",s4) <- lex r3,
					(x4, r4) <- reads s4,
					(",",s5) <- lex r4,
					(x5, r5) <- reads s5,
					(",",s6) <- lex r5,
					(x6, r6) <- reads s6,
					(",",s7) <- lex r6,
					(x7, r7) <- reads s7,
					(",",s8) <- lex r7,
					(x8, r8) <- reads s8,
					(",",s9) <- lex r8,
					(x9, r9) <- reads s9,
					(",",s10) <- lex r9,
					(x10, r10) <- reads s10,
					(",",s11) <- lex r10,
					(x11, r11) <- reads s11,
					(",",s12) <- lex r11,
					(x12, r12) <- reads s12,
					(",",s13) <- lex r12,
					(x13, r13) <- reads s13,
					(",",s14) <- lex r13,
					(x14, r14) <- reads s14,
					(",",s15) <- lex r14,
					(x15, r15) <- reads s15,
					(")",w)  <- lex r15 ] )


instance  (Show a, Show b, Show c) => Show (a,b,c)  where
    showsPrec p (x,y,z) = showChar '(' . shows x . showString "," .
    	    	    	    	         shows y . showString "," .
					 shows z . showChar ')'

--     showsType  ~(x,y,z) = showChar '(' . showsType x . showChar ',' .
--     	    	    	    	         showsType y . showChar ',' .
-- 					 showsType z . showChar ')'

instance  (Show a, Show b, Show c, Show d) => Show (a,b,c,d)  where
    showsPrec p (x,y,z,u) = showChar '(' . shows x . showString "," .
    	    	    	    	           shows y . showString "," .
                                           shows z . showString "," .
					   shows u . showChar ')'

--     showsType  ~(x,y,z,u) = showChar '(' . showsType x . showChar ',' .
--     	    	    	    	           showsType y . showChar ',' .
-- 					   showsType z . showChar ',' .
-- 					   showsType u . showChar ')'

instance  (Show a, Show b, Show c, Show d, Show e) => Show (a,b,c,d,e)  where
    showsPrec p (x,y,z,u,v) = showChar '(' . shows x . showString "," .
    	    	    	    	           shows y . showString "," .
                                           shows z . showString "," .
					   shows u . showString "," .
					   shows v . showChar ')'

--     showsType  ~(x,y,z,u,v) = showChar '(' . showsType x . showChar ',' .
--     	    	    	    	           showsType y . showChar ',' .
-- 					   showsType z . showChar ',' .
-- 					   showsType u . showChar ',' .
-- 					   showsType v . showChar ')'

instance  (Show a, Show b, Show c, Show d, Show e, Show f) =>
	 Show (a,b,c,d,e,f)  where
    showsPrec p (x,y,z,u,v,w) = showChar '(' . shows x . showString "," .
    	    	    	    	           shows y . showString "," .
                                           shows z . showString "," .
					   shows u . showString "," .
					   shows v . showString "," .
					   shows w . showChar ')'

--     showsType  ~(x,y,z,u,v,w) = showChar '(' . showsType x . showChar ',' .
--     	    	    	    	           showsType y . showChar ',' .
-- 					   showsType z . showChar ',' .
-- 					   showsType u . showChar ',' .
-- 					   showsType v . showChar ',' .
-- 					   showsType w . showChar ')'

instance  (Show a, Show b, Show c, Show d, Show e, Show f, Show g) =>
	 Show (a,b,c,d,e,f,g)  where
    showsPrec p (x,y,z,u,v,w,t) = showChar '(' . shows x . showString "," .
    	    	    	    	           shows y . showString "," .
                                           shows z . showString "," .
					   shows u . showString "," .
					   shows v . showString "," .
					   shows w . showString "," .
					   shows t . showChar ')'

--     showsType  ~(x,y,z,u,v,w,t) = showChar '(' . showsType x . showChar ',' .
--     	    	    	    	           showsType y . showChar ',' .
-- 					   showsType z . showChar ',' .
-- 					   showsType u . showChar ',' .
-- 					   showsType v . showChar ',' .
-- 					   showsType w . showChar ',' .
-- 					   showsType t . showChar ')'

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g,
	  Show h) =>
	 Show (a,b,c,d,e,f,g,h)  where
    showsPrec p (x,y,z,u,v,w,t,a) = showChar '(' . shows x . showString "," .
    	    	    	    	           shows y . showString "," .
                                           shows z . showString "," .
					   shows u . showString "," .
					   shows v . showString "," .
					   shows w . showString "," .
					   shows t . showString "," .
					   shows a . showChar ')'

--     showsType  ~(x,y,z,u,v,w,t,a) = showChar '(' . showsType x . showChar ',' .
--     	    	    	    	           showsType y . showChar ',' .
-- 					   showsType z . showChar ',' .
-- 					   showsType u . showChar ',' .
-- 					   showsType v . showChar ',' .
-- 					   showsType w . showChar ',' .
-- 					   showsType t . showChar ',' .
-- 					   showsType a . showChar ')'

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g,
	  Show h, Show i) =>
	 Show (a,b,c,d,e,f,g,h,i)  where
    showsPrec p (x,y,z,u,v,w,t,a,b) =
			 showChar '(' . shows x . showString "," .
    	    	    	    	           shows y . showString "," .
                                           shows z . showString "," .
					   shows u . showString "," .
					   shows v . showString "," .
					   shows w . showString "," .
					   shows t . showString "," .
					   shows a . showString "," .
					   shows b . showChar ')'

--     showsType  ~(x,y,z,u,v,w,t,a,b) =
-- 			 showChar '(' . showsType x . showChar ',' .
--     	    	    	    	           showsType y . showChar ',' .
-- 					   showsType z . showChar ',' .
-- 					   showsType u . showChar ',' .
-- 					   showsType v . showChar ',' .
-- 					   showsType w . showChar ',' .
-- 					   showsType t . showChar ',' .
-- 					   showsType a . showChar ',' .
-- 					   showsType b . showChar ')'

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g,
	  Show h, Show i, Show j) =>
	 Show (a,b,c,d,e,f,g,h,i,j)  where
    showsPrec p (x,y,z,u,v,w,t,a,b,c) =
			 showChar '(' . shows x . showString "," .
    	    	    	    	           shows y . showString "," .
                                           shows z . showString "," .
					   shows u . showString "," .
					   shows v . showString "," .
					   shows w . showString "," .
					   shows t . showString "," .
					   shows a . showString "," .
					   shows b . showString "," .
					   shows c . showChar ')'

--     showsType  ~(x,y,z,u,v,w,t,a,b,c) =
-- 			 showChar '(' . showsType x . showChar ',' .
--     	    	    	    	           showsType y . showChar ',' .
-- 					   showsType z . showChar ',' .
-- 					   showsType u . showChar ',' .
-- 					   showsType v . showChar ',' .
-- 					   showsType w . showChar ',' .
-- 					   showsType t . showChar ',' .
-- 					   showsType a . showChar ',' .
-- 					   showsType b . showChar ',' .
-- 					   showsType c . showChar ')'

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g,
	  Show h, Show i, Show j, Show k) =>
	 Show (a,b,c,d,e,f,g,h,i,j,k)  where
    showsPrec p (x,y,z,u,v,w,t,a,b,c,d) =
			 showChar '(' . shows x . showString "," .
    	    	    	    	           shows y . showString "," .
                                           shows z . showString "," .
					   shows u . showString "," .
					   shows v . showString "," .
					   shows w . showString "," .
					   shows t . showString "," .
					   shows a . showString "," .
					   shows b . showString "," .
					   shows c . showString "," .
					   shows d . showChar ')'

--     showsType  ~(x,y,z,u,v,w,t,a,b,c,d) =
-- 			 showChar '(' . showsType x . showChar ',' .
--     	    	    	    	           showsType y . showChar ',' .
-- 					   showsType z . showChar ',' .
-- 					   showsType u . showChar ',' .
-- 					   showsType v . showChar ',' .
-- 					   showsType w . showChar ',' .
-- 					   showsType t . showChar ',' .
-- 					   showsType a . showChar ',' .
-- 					   showsType b . showChar ',' .
-- 					   showsType c . showChar ',' .
-- 					   showsType d . showChar ')'

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g,
	  Show h, Show i, Show j, Show k, Show l) =>
	 Show (a,b,c,d,e,f,g,h,i,j,k,l)  where
    showsPrec p (x,y,z,u,v,w,t,a,b,c,d,e) =
			 showChar '(' . shows x . showString "," .
    	    	    	    	           shows y . showString "," .
                                           shows z . showString "," .
					   shows u . showString "," .
					   shows v . showString "," .
					   shows w . showString "," .
					   shows t . showString "," .
					   shows a . showString "," .
					   shows b . showString "," .
					   shows c . showString "," .
					   shows d . showString "," .
					   shows e . showChar ')'

--     showsType  ~(x,y,z,u,v,w,t,a,b,c,d,e) =
-- 			 showChar '(' . showsType x . showChar ',' .
--     	    	    	    	           showsType y . showChar ',' .
-- 					   showsType z . showChar ',' .
-- 					   showsType u . showChar ',' .
-- 					   showsType v . showChar ',' .
-- 					   showsType w . showChar ',' .
-- 					   showsType t . showChar ',' .
-- 					   showsType a . showChar ',' .
-- 					   showsType b . showChar ',' .
-- 					   showsType c . showChar ',' .
-- 					   showsType d . showChar ',' .
-- 					   showsType e . showChar ')'

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g,
	  Show h, Show i, Show j, Show k, Show l, Show m) =>
	 Show (a,b,c,d,e,f,g,h,i,j,k,l,m)  where
    showsPrec p (x,y,z,u,v,w,t,a,b,c,d,e,f) =
			 showChar '(' . shows x . showString "," .
    	    	    	    	           shows y . showString "," .
                                           shows z . showString "," .
					   shows u . showString "," .
					   shows v . showString "," .
					   shows w . showString "," .
					   shows t . showString "," .
					   shows a . showString "," .
					   shows b . showString "," .
					   shows c . showString "," .
					   shows d . showString "," .
					   shows e . showString "," .
					   shows f . showChar ')'

--     showsType  ~(x,y,z,u,v,w,t,a,b,c,d,e,f) =
-- 			 showChar '(' . showsType x . showChar ',' .
--     	    	    	    	           showsType y . showChar ',' .
-- 					   showsType z . showChar ',' .
-- 					   showsType u . showChar ',' .
-- 					   showsType v . showChar ',' .
-- 					   showsType w . showChar ',' .
-- 					   showsType t . showChar ',' .
-- 					   showsType a . showChar ',' .
-- 					   showsType b . showChar ',' .
-- 					   showsType c . showChar ',' .
-- 					   showsType d . showChar ',' .
-- 					   showsType e . showChar ',' .
-- 					   showsType f . showChar ')'

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g,
	  Show h, Show i, Show j, Show k, Show l, Show m, Show n) =>
	 Show (a,b,c,d,e,f,g,h,i,j,k,l,m,n)  where
    showsPrec p (x,y,z,u,v,w,t,a,b,c,d,e,f,g) =
			 showChar '(' . shows x . showString "," .
    	    	    	    	           shows y . showString "," .
                                           shows z . showString "," .
					   shows u . showString "," .
					   shows v . showString "," .
					   shows w . showString "," .
					   shows t . showString "," .
					   shows a . showString "," .
					   shows b . showString "," .
					   shows c . showString "," .
					   shows d . showString "," .
					   shows e . showString "," .
					   shows f . showString "," .
					   shows g . showChar ')'

--     showsType  ~(x,y,z,u,v,w,t,a,b,c,d,e,f,g) =
-- 			 showChar '(' . showsType x . showChar ',' .
--     	    	    	    	           showsType y . showChar ',' .
-- 					   showsType z . showChar ',' .
-- 					   showsType u . showChar ',' .
-- 					   showsType v . showChar ',' .
-- 					   showsType w . showChar ',' .
-- 					   showsType t . showChar ',' .
-- 					   showsType a . showChar ',' .
-- 					   showsType b . showChar ',' .
-- 					   showsType c . showChar ',' .
-- 					   showsType d . showChar ',' .
-- 					   showsType e . showChar ',' .
-- 					   showsType f . showChar ',' .
-- 					   showsType g . showChar ')'

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g,
	  Show h, Show i, Show j, Show k, Show l, Show m, Show n, Show o) =>
	 Show (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o)  where
    showsPrec p (x,y,z,u,v,w,t,a,b,c,d,e,f,g,h) =
			 showChar '(' . shows x . showString "," .
    	    	    	    	           shows y . showString "," .
                                           shows z . showString "," .
					   shows u . showString "," .
					   shows v . showString "," .
					   shows w . showString "," .
					   shows t . showString "," .
					   shows a . showString "," .
					   shows b . showString "," .
					   shows c . showString "," .
					   shows d . showString "," .
					   shows e . showString "," .
					   shows f . showString "," .
					   shows g . showString "," .
					   shows h . showChar ')'

--     showsType  ~(x,y,z,u,v,w,t,a,b,c,d,e,f,g,h) =
-- 			 showChar '(' . showsType x . showChar ',' .
--     	    	    	    	           showsType y . showChar ',' .
-- 					   showsType z . showChar ',' .
-- 					   showsType u . showChar ',' .
-- 					   showsType v . showChar ',' .
-- 					   showsType w . showChar ',' .
-- 					   showsType t . showChar ',' .
-- 					   showsType a . showChar ',' .
-- 					   showsType b . showChar ',' .
-- 					   showsType c . showChar ',' .
-- 					   showsType d . showChar ',' .
-- 					   showsType e . showChar ',' .
-- 					   showsType f . showChar ',' .
-- 					   showsType g . showChar ',' .
-- 					   showsType h . showChar ')'


-- A.3  Prelude PreludeIO
-- module PreludeIO (
--    FilePath, IOError, ioError, userError, catch,
--    putChar, putStr, putStrLn, print,
--    getChar, getLine, getContents, interact,
--    readFile, writeFile, appendFile, readIO, readLn
--  ) where


type  FilePath = String


-- data IOError    -- The internals of this type are system dependent

{- not needed, because IOError = Prelude.IOError and instance available from Prelude
instance  Show IOError where
  show = primIOErrorShow
-}

{- currently excluded because instance Eq IOError missing in ghc 5.02
instance  Eq IOError  where 
  (==) = primIOErrorEq
-}

-- ioError          ::  IOError -> IO a 
-- ioError          =   primIOError

-- userError        ::  String -> IOError
-- userError        =   primUserError

-- catch            ::  IO a -> (IOError -> IO a) -> IO a 
-- catch            =   primCatch

-- putChar          :: Char -> IO ()
-- putChar          =  primPutChar

putStr           :: String -> IO ()
putStr s         =  mapM_ putChar s

putStrLn         :: String -> IO ()
putStrLn s       =  do putStr s
                       putStr "\n"

print            :: Show a => a -> IO ()
print x          =  putStrLn (show x)

-- getChar          :: IO Char
-- getChar          =  primGetChar

getLine          :: IO String
getLine          =  do c <- getChar
                       if c == '\n' then return "" else 
                          do s <- getLine
                             return (c:s)
            
-- getContents      :: IO String
-- getContents      =  primGetContents

interact         ::  (String -> String) -> IO ()
interact f       =  do s <- getContents
                       putStr (f s)

-- readFile         :: FilePath -> IO String
-- readFile         =  primReadFile

-- writeFile        :: FilePath -> String -> IO ()
-- writeFile        =  primWriteFile

-- appendFile       :: FilePath -> String -> IO ()
-- appendFile       =  primAppendFile

  -- raises an exception instead of an error

readIO   :: Read a => String -> IO a
readIO s =  case [x | (x,t) <- reads s, ("","") <- lex t] of
              [x] -> return x
              []  -> ioError (userError "Prelude.readIO: no parse")
              _   -> ioError (userError "Prelude.readIO: ambiguous parse")


readLn           :: Read a => IO a
readLn           =  do l <- getLine
                       r <- readIO l
                       return r




-- import Char(isSpace, isAlpha, isDigit, isAlphaNum,
--            showLitChar, readLitChar, lexLitChar)

-- module Char ( 
--    isAscii, isLatin1, isControl, isPrint, isSpace, isUpper, isLower,
--    isAlpha, isDigit, isOctDigit, isHexDigit, isAlphaNum,
--    digitToInt, intToDigit,
--    toUpper, toLower,
--    ord, chr,
--    readLitChar, showLitChar, lexLitChar,
--
-- ...and what the Prelude exports
--    Char, String
--    ) where

-- import Array  -- used for character name table.


-- Digit conversion operations
digitToInt :: Char -> Int
digitToInt c
  | isDigit c            =  fromEnum c - fromEnum '0'
  | c >= 'a' && c <= 'f' =  fromEnum c - fromEnum 'a' + 10
  | c >= 'A' && c <= 'F' =  fromEnum c - fromEnum 'A' + 10
  | otherwise            =  error "Char.digitToInt: not a digit"

intToDigit :: Int -> Char
intToDigit i
  | i >= 0  && i <=  9   =  toEnum (fromEnum '0' + i)
  | i >= 10 && i <= 15   =  toEnum (fromEnum 'a' + i - 10)
  | otherwise            =  error "Char.intToDigit: not a digit"



-- Character code functions
ord                     :: Char -> Int
ord                     =  fromEnum

chr                     :: Int  -> Char
chr                     =  toEnum

-- Text functions
readLitChar             :: ReadS Char
readLitChar ('\\':s)    =  readEsc s
        where
        readEsc ('a':s)  = [('\a',s)]
        readEsc ('b':s)  = [('\b',s)]
        readEsc ('f':s)  = [('\f',s)]
        readEsc ('n':s)  = [('\n',s)]
        readEsc ('r':s)  = [('\r',s)]
        readEsc ('t':s)  = [('\t',s)]
        readEsc ('v':s)  = [('\v',s)]
        readEsc ('\\':s) = [('\\',s)]
        readEsc ('"':s)  = [('"',s)]
        readEsc ('\'':s) = [('\'',s)]
        readEsc ('^':c:s) | c >= '@' && c <= '_'
                         = [(chr (ord c - ord '@'), s)]
        readEsc s@(d:_) | isDigit d
                         = [(chr n, t) | (n,t) <- readDec s]
        readEsc ('o':s)  = [(chr n, t) | (n,t) <- readOct s]
        readEsc ('x':s)  = [(chr n, t) | (n,t) <- readHex s]
        readEsc s@(c:_) | isUpper c
                         = let table = ('\DEL', "DEL") : {- assocs -} asciiTab
                           in case [(c,s') | (c, mne) <- table,
                                             ([],s') <- [match mne s]]
                              of (pr:_) -> [pr]
                                 []     -> []
        readEsc _        = []

        match                         :: (Eq a) => [a] -> [a] -> ([a],[a])
        match (x:xs) (y:ys) | x == y  =  match xs ys
        match xs     ys               =  (xs,ys)
readLitChar (c:s)       =  [(c,s)]

showLitChar               :: Char -> ShowS
showLitChar c | c > '\DEL' =  showChar '\\' . 
                              protectEsc isDigit (shows (ord c))
showLitChar '\DEL'         =  showString "\\DEL"
showLitChar '\\'           =  showString "\\\\"
showLitChar c | c >= ' '   =  showChar c
showLitChar '\a'           =  showString "\\a"
showLitChar '\b'           =  showString "\\b"
showLitChar '\f'           =  showString "\\f"
showLitChar '\n'           =  showString "\\n"
showLitChar '\r'           =  showString "\\r"
showLitChar '\t'           =  showString "\\t"
showLitChar '\v'           =  showString "\\v"
showLitChar '\SO'          =  protectEsc (== 'H') (showString "\\SO")
showLitChar c              =  showString ('\\' : snd (asciiTab !! ord c){-!c-})

protectEsc p f             = f . cont
                             where cont s@(c:_) | p c = "\\&" ++ s
                                   cont s             = s
asciiTab = {- listArray ('\NUL', ' ') -}
           zip ['\NUL'..' ']
           ["NUL", "SOH", "STX", "ETX", "EOT", "ENQ", "ACK", "BEL",
            "BS",  "HT",  "LF",  "VT",  "FF",  "CR",  "SO",  "SI", 
            "DLE", "DC1", "DC2", "DC3", "DC4", "NAK", "SYN", "ETB",
            "CAN", "EM",  "SUB", "ESC", "FS",  "GS",  "RS",  "US", 
            "SP"] 

lexLitChar          :: ReadS String
lexLitChar ('\\':s) =  [('\\':esc, t) | (esc,t) <- lexEsc s]
        where
          lexEsc (c:s)     | c `elem` "abfnrtv\\\"'" = [([c],s)]
          lexEsc s@(d:_)   | isDigit d               = lexDigits s
          lexEsc ('^':c:s) | c >= '@' && c <= '_'    = [(['^',c],s)]
          -- Very crude approximation to \XYZ.  Let readers work this out.
          lexEsc s@(c:_)   | isUpper c               = [span isCharName s]
          lexEsc _                                   = []
          isCharName c = isUpper c || isDigit c

lexLitChar (c:s)    =  [([c],s)]
lexLitChar ""       =  []


-- import Numeric(showSigned, showInt, readSigned, readDec, showFloat,
--               readFloat, lexDigits)
-- import Numeric (readDec, readOct, lexDigits, readHex)

-- module Numeric(fromRat,
--               showSigned, showInt,
--               readSigned, readInt,
--               readDec, readOct, readHex, 
--               floatToDigits,
--               showEFloat, showFFloat, showGFloat, showFloat, 
--               readFloat, lexDigits) where

-- import Ratio  ( (%), numerator, denominator )
-- import Array  ( (!), Array, array )

-- This converts a rational to a floating.  This should be used in the
-- Fractional instances of Float and Double.

fromRat :: (RealFloat a) => Rational -> a
fromRat x = 
    if x == 0 then encodeFloat 0 0              -- Handle exceptional cases
    else if x < 0 then - fromRat' (-x)          -- first.
    else fromRat' x

-- Conversion process:
-- Scale the rational number by the RealFloat base until
-- it lies in the range of the mantissa (as used by decodeFloat/encodeFloat).
-- Then round the rational to an Integer and encode it with the exponent
-- that we got from the scaling.
-- To speed up the scaling process we compute the log2 of the number to get
-- a first guess of the exponent.
fromRat' :: (RealFloat a) => Rational -> a
fromRat' x = r
  where b = floatRadix r
        p = floatDigits r
        (minExp0, _) = floatRange r
        minExp = minExp0 - p            -- the real minimum exponent
        xMin = toRational (expt b (p-1))
        xMax = toRational (expt b p)
        p0 = (integerLogBase b (numerator x) -
              integerLogBase b (denominator x) - p) `max` minExp
        f = if p0 < 0 then 1 % expt b (-p0) else expt b p0 % 1
        (x', p') = scaleRat (toRational b) minExp xMin xMax p0 (x / f)
        r = encodeFloat (round x') p'

-- Scale x until xMin <= x < xMax, or p (the exponent) <= minExp.
scaleRat :: Rational -> Int -> Rational -> Rational -> 
             Int -> Rational -> (Rational, Int)
scaleRat b minExp xMin xMax p x =
    if p <= minExp then
        (x, p)
    else if x >= xMax then
        scaleRat b minExp xMin xMax (p+1) (x/b)
    else if x < xMin  then
        scaleRat b minExp xMin xMax (p-1) (x*b)
    else
        (x, p)

-- Exponentiation with a cache for the most common numbers.
minExpt = 0::Int
maxExpt = 1100::Int
expt :: Integer -> Int -> Integer
expt base n =
    if base == 2 && n >= minExpt && n <= maxExpt then
        expts {-!n-} !! (n-minExpt)
    else
        base^n

expts :: {- Array Int Integer -} [Integer]
expts = [2^n | n <- [minExpt .. maxExpt]]
        {- array (minExpt,maxExpt) [(n,2^n) | n <- [minExpt .. maxExpt]] -}

-- Compute the (floor of the) log of i in base b.
-- Simplest way would be just divide i by b until it's smaller then b,
-- but that would be very slow!  We are just slightly more clever.
integerLogBase :: Integer -> Integer -> Int
integerLogBase b i =
     if i < b then
        0
     else
        -- Try squaring the base first to cut down the number of divisions.
        let l = 2 * integerLogBase (b*b) i
            doDiv :: Integer -> Int -> Int
            doDiv i l = if i < b then l else doDiv (i `div` b) (l+1)
        in  doDiv (i `div` (b^l)) l


-- Misc utilities to show integers and floats 

showSigned    :: Real a => (a -> ShowS) -> Int -> a -> ShowS
showSigned showPos p x | x < 0 = showParen (p > 6)
                                           (showChar '-' . showPos (-x))
                       | otherwise = showPos x

-- showInt, showOct, showHex are used for positive numbers only
showInt, showOct, showHex :: Integral a => a -> ShowS
showOct = showIntAtBase  8 intToDigit
showInt = showIntAtBase 10 intToDigit
showHex = showIntAtBase 16 intToDigit

showIntAtBase :: Integral a 
      => a              -- base
      -> (Int -> Char)  -- digit to char
      -> a              -- number to show
      -> ShowS
showIntAtBase base intToDig n rest
  | n < 0     = error "Numeric.showIntAtBase: can't show negative numbers"
  | n' == 0   = rest'
  | otherwise = showIntAtBase base intToDig n' rest'
  where
    (n',d) = quotRem n base
    rest'  = intToDig (fromIntegral d) : rest

readSigned :: (Real a) => ReadS a -> ReadS a
readSigned readPos = readParen False read'
                     where read' r  = read'' r ++
                                      [(-x,t) | ("-",s) <- lex r,
                                                (x,t)   <- read'' s]
                           read'' r = [(n,s)  | (str,s) <- lex r,
                                                (n,"")  <- readPos str]


-- readInt reads a string of digits using an arbitrary base.  
-- Leading minus signs must be handled elsewhere.

readInt :: (Integral a) => a -> (Char -> Bool) -> (Char -> Int) -> ReadS a
readInt radix isDig digToInt s =
   [(foldl1 (\n d -> n * radix + d) (map (fromIntegral . digToInt) ds), r)
   | (ds,r) <- nonnull isDig s ]

-- Unsigned readers for various bases
readDec, readOct, readHex :: (Integral a) => ReadS a
readDec = readInt 10 isDigit    digitToInt
readOct = readInt  8 isOctDigit digitToInt
readHex = readInt 16 isHexDigit digitToInt


showEFloat     :: (RealFloat a) => Maybe Int -> a -> ShowS
showFFloat     :: (RealFloat a) => Maybe Int -> a -> ShowS
showGFloat     :: (RealFloat a) => Maybe Int -> a -> ShowS
showFloat      :: (RealFloat a) => a -> ShowS

showEFloat d x =  showString (formatRealFloat FFExponent d x)
showFFloat d x =  showString (formatRealFloat FFFixed d x)
showGFloat d x =  showString (formatRealFloat FFGeneric d x)
showFloat      =  showGFloat Nothing 

-- These are the format types.  This type is not exported.

data FFFormat = FFExponent | FFFixed | FFGeneric

formatRealFloat :: (RealFloat a) => FFFormat -> Maybe Int -> a -> String
formatRealFloat fmt decs x 
  = s
  where 
    base = 10
    s = if isNaN x then 
            "NaN"
        else if isInfinite x then 
            if x < 0 then "-Infinity" else "Infinity"
        else if x < 0 || isNegativeZero x then 
            '-' : doFmt fmt (floatToDigits (toInteger base) (-x))
        else 
            doFmt fmt (floatToDigits (toInteger base) x)
    
    doFmt fmt (is, e)
      = let 
           ds = map intToDigit is
        in  
        case fmt of
          FFGeneric -> 
              doFmt (if e < 0 || e > 7 then FFExponent else FFFixed)
                    (is, e)
          FFExponent ->
            case decs of
              Nothing ->
                case ds of
                   []    -> "0.0e0"
                   [d]   -> d : ".0e" ++ show (e-1)
                   d:ds  -> d : '.' : ds ++ 'e':show (e-1)
          
              Just dec ->
                let dec' = max dec 1 in
                case is of
                  [] -> '0':'.':take dec' (repeat '0') ++ "e0"
                  _ ->
                    let (ei, is') = roundTo base (dec'+1) is
                        d:ds = map intToDigit
                                   (if ei > 0 then init is' else is')
                    in d:'.':ds  ++ "e" ++ show (e-1+ei)
          
          FFFixed ->
            case decs of
               Nothing  -- Always prints a decimal point
                 | e > 0     -> take e (ds ++ repeat '0')
                                ++ '.' : mk0 (drop e ds)
                 | otherwise -> "0." ++ mk0 (replicate (-e) '0' ++ ds)
              
               Just dec ->  -- Print decimal point iff dec > 0
                 let dec' = max dec 0 in
                 if e >= 0 then
                   let (ei, is') = roundTo base (dec' + e) is
                       (ls, rs)  = splitAt (e+ei) 
                                              (map intToDigit is')
                   in  mk0 ls ++ mkdot0 rs
                 else
                   let (ei, is') = roundTo base dec' 
                                           (replicate (-e) 0 ++ is)
                       d : ds = map intToDigit 
                                    (if ei > 0 then is' else 0:is')
                   in  d : mkdot0 ds
            where   
              mk0 "" = "0"        -- Print 0.34, not .34
              mk0 s  = s  
    
              mkdot0 "" = ""       -- Print 34, not 34.
              mkdot0 s  = '.' : s  -- when the format specifies no
           -- digits after the decimal point
    

roundTo :: Int -> Int -> [Int] -> (Int, [Int])
roundTo base d is = case f d is of
                (0, is) -> (0, is)
                (1, is) -> (1, 1 : is)
  where b2 = base `div` 2
        f n [] = (0, replicate n 0)
        f 0 (i:_) = (if i >= b2 then 1 else 0, [])
        f d (i:is) = 
            let (c, ds) = f (d-1) is
                i' = c + i
            in  if i' == base then (1, 0:ds) else (0, i':ds)

--
-- Based on "Printing Floating-Point Numbers Quickly and Accurately"
-- by R.G. Burger and R. K. Dybvig, in PLDI 96.
-- The version here uses a much slower logarithm estimator.  
-- It should be improved.

-- This function returns a non-empty list of digits (Ints in [0..base-1])
-- and an exponent.  In general, if
--      floatToDigits r = ([a, b, ... z], e)
-- then
--      r = 0.ab..z * base^e
-- 

floatToDigits :: (RealFloat a) => Integer -> a -> ([Int], Int)

floatToDigits _ 0 = ([], 0)
floatToDigits base x =
    let (f0, e0) = decodeFloat x
        (minExp0, _) = floatRange x
        p = floatDigits x
        b = floatRadix x
        minExp = minExp0 - p            -- the real minimum exponent

        -- Haskell requires that f be adjusted so denormalized numbers
        -- will have an impossibly low exponent.  Adjust for this.
        f :: Integer
        e :: Int
        (f, e) = let n = minExp - e0
                 in  if n > 0 then (f0 `div` (b^n), e0+n) else (f0, e0)

        (r, s, mUp, mDn) =
           if e >= 0 then
               let be = b^e in
               if f == b^(p-1) then
                   (f*be*b*2, 2*b, be*b, b)
               else
                   (f*be*2, 2, be, be)
           else
               if e > minExp && f == b^(p-1) then
                   (f*b*2, b^(-e+1)*2, b, 1)
               else
                   (f*2, b^(-e)*2, 1, 1)
        k = 
            let k0 =
                    if b==2 && base==10 then
                        -- logBase 10 2 is slightly bigger than 3/10 so
                        -- the following will err on the low side.  Ignoring
                        -- the fraction will make it err even more.
                        -- Haskell promises that p-1 <= logBase b f < p.
                        (p - 1 + e0) * 3 `div` 10
                    else
                        ceiling (((log (fromInteger (f+1)) + 
                                 fromIntegral e * log (fromInteger b)) / 
                                  log (fromInteger base)) 
                                 :: Double {-DEFAULT-})
                fixup n =
                    if n >= 0 then
                        if r + mUp <= expt base n * s then n else fixup (n+1)
                    else
                        if expt base (-n) * (r + mUp) <= s then n
                                                           else fixup (n+1)
            in  fixup k0

        gen ds rn sN mUpN mDnN =
            let (dn, rn') = (rn * base) `divMod` sN
                mUpN' = mUpN * base
                mDnN' = mDnN * base
            in  case (rn' < mDnN', rn' + mUpN' > sN) of
                (True,  False) -> dn : ds
                (False, True)  -> dn+1 : ds
                (True,  True)  -> if rn' * 2 < sN then dn : ds else dn+1 : ds
                (False, False) -> gen (dn:ds) rn' sN mUpN' mDnN'
        rds =
            if k >= 0 then
                gen [] r (s * expt base k) mUp mDn
            else
                let bk = expt base (-k)
                in  gen [] (r * bk) s (mUp * bk) (mDn * bk)
    in  (map fromIntegral (reverse rds), k)



-- This floating point reader uses a less restrictive syntax for floating
-- point than the Haskell lexer.  The `.' is optional.

readFloat     :: (RealFloat a) => ReadS a
readFloat r    = [(fromRational ((n%1)*10^^(k-d)),t) | (n,d,s) <- readFix r,
                                                       (k,t)   <- readExp s] ++
                 [ (0/0, t) | ("NaN",t)      <- lex r] ++
                 [ (1/0, t) | ("Infinity",t) <- lex r]
               where 
                 readFix r = [(read (ds++ds'), length ds', t)
                             | (ds,d) <- lexDigits r,
                               (ds',t) <- lexFrac d ]
               
                 lexFrac ('.':ds) = lexDigits ds
                 lexFrac s        = [("",s)]        
                 
                 readExp (e:s) | e `elem` "eE" = readExp' s
                 readExp s                     = [(0,s)]
                 
                 readExp' ('-':s) = [(-k,t) | (k,t) <- readDec s]
                 readExp' ('+':s) = readDec s
                 readExp' s       = readDec s

lexDigits        :: ReadS String 
lexDigits        =  nonnull isDigit

nonnull          :: (Char -> Bool) -> ReadS String
nonnull p s      =  [(cs,t) | (cs@(_:_),t) <- [span p s]]


-- module  Ratio (
--    Ratio, Rational, (%), numerator, denominator, approxRational ) where

infixl 7  %

prec = 7 :: Int

data  Ratio a = !a :% !a  deriving (Eq)  -- no class Integral a class context in 2010
type  Rational          =  Ratio Integer


(%)                     :: (Integral a) => a -> a -> Ratio a
numerator, denominator  :: (Integral a) => Ratio a -> a
approxRational          :: (RealFrac a) => a -> a -> Rational


-- "reduce" is a subsidiary function used only in this module.
-- It normalises a ratio by dividing both numerator
-- and denominator by their greatest common divisor.
--
-- E.g., 12 `reduce` 8    ==  3 :%   2
--       12 `reduce` (-8) ==  3 :% (-2)

reduce _ 0              =  error "Ratio.% : zero denominator"
reduce x y              =  (x `quot` d) :% (y `quot` d)
                           where d = gcd x y

x % y                   =  reduce (x * signum y) (abs y)

numerator (x :% _)      =  x

denominator (_ :% y)    =  y


instance  (Integral a)  => Ord (Ratio a)  where
    (x:%y) <= (x':%y')  =  x * y' <= x' * y
    (x:%y) <  (x':%y')  =  x * y' <  x' * y

instance  (Integral a)  => Num (Ratio a)  where
    (x:%y) + (x':%y')   =  reduce (x*y' + x'*y) (y*y')
    (x:%y) * (x':%y')   =  reduce (x * x') (y * y')
    negate (x:%y)       =  (-x) :% y
    abs (x:%y)          =  abs x :% y
    signum (x:%y)       =  signum x :% 1
    fromInteger x       =  fromInteger x :% 1

instance  (Integral a)  => Real (Ratio a)  where
    toRational (x:%y)   =  toInteger x :% toInteger y

instance  (Integral a)  => Fractional (Ratio a)  where
    (x:%y) / (x':%y')   =  (x*y') % (y*x')
    recip (x:%y)        =  y % x
    fromRational (x:%y) =  fromInteger x :% fromInteger y

instance  (Integral a)  => RealFrac (Ratio a)  where
    properFraction (x:%y) = (fromIntegral q, r:%y)
                            where (q,r) = quotRem x y

instance  (Integral a)  => Enum (Ratio a)  where
    toEnum           =  fromIntegral
    fromEnum         =  fromInteger . truncate -- May overflow
    enumFrom         =  numericEnumFrom     -- These numericEnumXXX functions
    enumFromThen     =  numericEnumFromThen -- are as defined in Prelude.hs
    enumFromTo       =  numericEnumFromTo   -- but not exported from it!
    enumFromThenTo   =  numericEnumFromThenTo

instance  (Read a, Integral a)  => Read (Ratio a)  where
    readsPrec p  =  readParen (p > prec)
                              (\r -> [(x%y,u) | (x,s)   <- reads r,
                                                ("%",t) <- lex s,
                                                (y,u)   <- reads t ])

instance  (Integral a)  => Show (Ratio a)  where
    showsPrec p (x:%y)  =  showParen (p > prec)
                               (shows x . showString " % " . shows y)



approxRational x eps    =  simplest (x-eps) (x+eps)
        where simplest x y | y < x      =  simplest y x
                           | x == y     =  xr
                           | x > 0      =  simplest' n d n' d'
                           | y < 0      =  - simplest' (-n') d' (-n) d
                           | otherwise  =  0 :% 1
                                        where xr@(n:%d) = toRational x
                                              (n':%d')  = toRational y

              simplest' n d n' d'       -- assumes 0 < n%d < n'%d'
                        | r == 0     =  q :% 1
                        | q /= q'    =  (q+1) :% 1
                        | otherwise  =  (q*n''+d'') :% n''
                                     where (q,r)      =  quotRem n d
                                           (q',r')    =  quotRem n' d'
                                           (n'':%d'') =  simplest' d' r' d r

