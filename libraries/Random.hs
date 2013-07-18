-- some parts are taken from the nhc98/Hugs implementation of Random
module Random (
  RandomGen(next, split, genRange),
  StdGen, mkStdGen,
  Random( random,   randomR, 
  randoms,  randomRs,
  randomIO, randomRIO ),
  getStdRandom, getStdGen, setStdGen, newStdGen
  ) where

import PreludeBuiltinTypes as NotHat.T
import RandomBuiltin as NotHat.T
import qualified NotHat.System.Random as NotHat.Random
import Char

---------------- The RandomGen class ------------------------

class RandomGen g where
  genRange :: g -> (Int, Int)
  next     :: g -> (Int, g)
  split    :: g -> (g, g)
   
  -- Default method
  genRange g = (minBound,maxBound)

---------------- A standard instance of RandomGen -----------

-- data StdGen = ... -- Abstract

instance RandomGen StdGen where
  genRange = primStdGenGenRange
  next = primStdGenNext
  split = primStdGenSplit

instance Read StdGen where
  readsPrec = primStdGenReadsPrec

instance Show StdGen where
  showsPrec = primStdGenShowsPrec

foreign import ccall "NotHat.Random.genRange"
 primStdGenGenRange :: StdGen -> (Int,Int)
foreign import ccall "NotHat.Random.next"
 primStdGenNext :: StdGen -> (Int,StdGen)
foreign import ccall "NotHat.Random.split"
 primStdGenSplit :: StdGen -> (StdGen,StdGen)
foreign import ccall "NotHat.Prelude.readsPrec"
 primStdGenReadsPrec :: Int -> String -> [(StdGen,String)]
foreign import ccall "NotHat.Prelude.showsPrec"
 primStdGenShowsPrec :: Int -> StdGen -> String -> String


foreign import ccall "NotHat.Random.mkStdGen"
 mkStdGen :: Int -> StdGen

 ---------------- The Random class ---------------------------
class Random a where
  randomR :: RandomGen g => (a, a) -> g -> (a, g)
  random  :: RandomGen g => g -> (a, g)

  randomRs :: RandomGen g => (a, a) -> g -> [a]
  randoms  :: RandomGen g => g -> [a]

  randomRIO :: (a,a) -> IO a
  randomIO  :: IO a
     
  -- Default methods
  randoms g = x : randoms g' 
    where 
    (x,g') = random g
  randomRs range g = x : randomRs range g'
    where
    (x,g') = randomR range g
  randomIO        = getStdRandom random
  randomRIO range = getStdRandom (randomR range)

instance Random Int     where 
  randomR (a,b) g = randomIvalInteger (toInteger a, toInteger b) g
  random g        = randomR (minBound,maxBound) g

instance Random Integer where 
  randomR ival g = randomIvalInteger ival g
  random g	 = randomR (toInteger (minBound::Int), toInteger (maxBound::Int)) g

instance Random Float where
  random g        = randomIvalDouble (0::Double,1) realToFrac g
  randomR (a,b) g = randomIvalDouble (realToFrac a, realToFrac b) realToFrac g

instance Random Double  where 
  randomR ival g = randomIvalDouble ival id g
  random g       = randomR (0::Double,1) g

instance Random Bool where 
  randomR (a,b) g = 
    case 
      (randomIvalInteger (bool2Integer a, bool2Integer b) g) of
        (x, g) -> (int2Bool x, g)
    where
    bool2Integer :: Bool -> Integer
    bool2Integer False = 0
    bool2Integer True  = 1

    int2Bool :: Int -> Bool
    int2Bool 0	= False
    int2Bool _	= True

  random g = randomR (minBound,maxBound) g

instance Random Char where 
  randomR (a,b) g = 
      case (randomIvalInteger (toInteger (ord a), toInteger (ord b)) g) of
        (x,g) -> (chr x, g)
  random g = randomR (minBound,maxBound) g


-- internal:
randomIvalInteger :: (RandomGen g, Num a) => (Integer, Integer) -> g -> (a, g)
randomIvalInteger (l,h) rng
 | l > h     = randomIvalInteger (h,l) rng
 | otherwise = case (f n 1 rng) of (v, rng') -> (fromInteger (l + v `mod` k), rng')
     where
       k = h - l + 1
       b = 2147483561
       n = iLogBase b k

       f 0 acc g = (acc, g)
       f n acc g = 
          let
	   (x,g')   = next g
	  in
	  f (n-1) (fromInt x + acc * b) g'

randomIvalDouble :: (RandomGen g, Fractional a) => (Double, Double) -> (Double -> a) -> g -> (a, g)
randomIvalDouble (l,h) fromDouble rng 
  | l > h     = randomIvalDouble (h,l) fromDouble rng
  | otherwise = 
       case (randomIvalInteger (toInteger (minBound::Int), toInteger (maxBound::Int)) rng) of
         (x, rng') -> 
	    let
	     scaled_x = 
		fromDouble ((l+h)/2) + 
                fromDouble ((h-l) / realToFrac intRange) *
		fromIntegral (x::Int)
	    in
	    (scaled_x, rng')

fromInt :: Num a => Int -> a
fromInt = fromInteger . toInteger

intRange :: Integer
intRange  = toInteger (maxBound::Int) - toInteger (minBound::Int)

iLogBase :: Integer -> Integer -> Integer
iLogBase b i = if i < b then 1 else 1 + iLogBase b (i `div` b)

---------------- The global random generator ----------------

foreign import ccall "NotHat.Random.newStdGen"
 newStdGen    :: IO StdGen
foreign import ccall "NotHat.Random.setStdGen"
 setStdGen    :: StdGen -> IO ()
foreign import ccall "NotHat.Random.getStdGen"
 getStdGen    :: IO StdGen 
foreign import ccall "NotHat.Random.getStdRandom"
 getStdRandom :: (StdGen -> (a, StdGen)) -> IO a