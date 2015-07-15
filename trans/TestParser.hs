module Main where

-- Test parser

-- import Language.Haskell.Exts.Annotated hiding ()
x :: Int
x = 42
  where
  -- y = 3

{-
type My a = You Int a 
type You a b = a -> b
-}

data Help = H1 {why :: Int} | H2 {why :: Int, there :: Char}

class Test a where
  test :: a -> Int
  test _ = 0

instance Test Int where
  test _ = 42

instance Test Char where
  test _ = 3
{-
(x,y) = (3,odd)

data L = L Int (Bool -> Char) deriving Show

infixl 5  +++, `L`

x +++ y = 2

foreign import stdcall "NotHat.Random.genRange"
  primStdGenGenRange :: Int -> Int
-}

main = do
--  parseResult <- parseFile "TestParser2.hs"
--  let result = fmap (fmap (\_ -> L)) parseResult
--  print result
    print "ok"
