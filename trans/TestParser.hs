-- Test parser

import Language.Haskell.Exts.Annotated hiding ()

data L = L deriving Show

foreign import stdcall "NotHat.Random.genRange"
  primStdGenGenRange :: Int -> Int

main = do
  parseResult <- parseFile "TestParser2.hs"
  let result = fmap (fmap (\_ -> L)) parseResult
  print result