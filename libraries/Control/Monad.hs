-- Haskell 2010 module reexports Haskell 98 module plus added functions

module Control.Monad (  
    Functor(fmap),  Monad((>>=), (>>), return, fail),  MonadPlus(mzero, mplus),  
    mapM,  mapM_,  forM,  forM_,  sequence,  sequence_,  (=<<),  (>=>),  (<=<),  
    forever,  void,  join,  msum,  filterM,  mapAndUnzipM,  zipWithM,  
    zipWithM_,  foldM,  foldM_,  replicateM,  replicateM_,  guard,  when,  
    unless,  liftM,  liftM2,  liftM3,  liftM4,  liftM5,  ap  
  ) where

import Monad

-- | 'forM' is 'mapM' with its arguments flipped
forM            :: Monad m => [a] -> (a -> m b) -> m [b]
forM            = flip mapM

-- | 'forM_' is 'mapM_' with its arguments flipped
forM_           :: Monad m => [a] -> (a -> m b) -> m ()
forM_           = flip mapM_

-- | Left-to-right Kleisli composition of monads.
(>=>)       :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
f >=> g     = \x -> f x >>= g

-- | Right-to-left Kleisli composition of monads. @('>=>')@, with the arguments flipped
(<=<)       :: Monad m => (b -> m c) -> (a -> m b) -> (a -> m c)
(<=<)       = flip (>=>)

-- | @'forever' act@ repeats the action infinitely.
forever     :: (Monad m) => m a -> m b
forever a   = let a' = a >> a' in a'

-- | @'void' value@ discards or ignores the result of evaluation, such as the return value of an 'IO' action.
void :: Functor f => f a -> f ()
void = fmap (const ())

-- | Like 'foldM', but discards the result.
foldM_            :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m ()
foldM_ f a xs     = foldM f a xs >> return ()

-- | @'replicateM' n act@ performs the action @n@ times,
-- gathering the results.
replicateM        :: (Monad m) => Int -> m a -> m [a]
replicateM n x    = sequence (replicate n x)

-- | Like 'replicateM', but discards the result.
replicateM_       :: (Monad m) => Int -> m a -> m ()
replicateM_ n x   = sequence_ (replicate n x)
