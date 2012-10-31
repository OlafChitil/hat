-- Test parser

import Language.Haskell.Exts.Annotated hiding ()

data L = L deriving Show

f :: Bool -> Bool
f True = False
f False = True

main = do
  parseResult <- parseFile "TestParser.hs"
  let result = fmap (fmap (\_ -> L)) parseResult
  print result