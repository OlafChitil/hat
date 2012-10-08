--  Wrapper that exports just the stuff normally defined in the 
--  standard module Numeric. 
module Numeric(fromRat,
               showSigned, showIntAtBase,
               showInt, showOct, showHex,
               readSigned, readInt,
               readDec, readOct, readHex, 
               floatToDigits,
               showEFloat, showFFloat, showGFloat, showFloat, 
               readFloat, lexDigits) where

import PreludeBasic

