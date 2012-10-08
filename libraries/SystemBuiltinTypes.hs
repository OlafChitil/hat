module SystemBuiltinTypes where

data ExitCode = ExitSuccess | ExitFailure Int 
                deriving (Eq, Ord, Read, Show)
