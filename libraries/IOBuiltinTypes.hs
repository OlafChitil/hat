module IOBuiltinTypes where

import Ix(Ix(..))

data IOMode      =  ReadMode | WriteMode | AppendMode | ReadWriteMode
                     deriving (Eq, Ord, Ix, Bounded, Enum, Read, Show)
data BufferMode  =  NoBuffering | LineBuffering 
                  |  BlockBuffering (Maybe Int)
                     deriving (Eq, Ord, Read, Show)
data SeekMode    =  AbsoluteSeek | RelativeSeek | SeekFromEnd
                     deriving (Eq, Ord, Ix, Bounded, Enum, Read, Show)
