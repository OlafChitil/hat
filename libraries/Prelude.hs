{-# hat-trans -cpp #-}
-- Wrapper that exports just the stuff normally defined in the 
-- Prelude. 
-- The export list was copied from the Haskell 98 report.
module Prelude (
    -- for internal purposes of desugared list comprehensions
    -- hopefully to disappear soon
    _filter,_foldr,
    -- module PreludeList:
    map, (++), filter, concat,
    head, last, tail, init, null, length, (!!), 
    foldl, foldl1, scanl, scanl1, foldr, foldr1, scanr, scanr1,
    iterate, repeat, replicate, cycle,
    take, drop, splitAt, takeWhile, dropWhile, span, break,
    lines, words, unlines, unwords, reverse, and, or,
    any, all, elem, notElem, lookup,
    sum, product, maximum, minimum, concatMap, 
    zip, zip3, zipWith, zipWith3, unzip, unzip3,
    -- module PreludeText:
    ReadS, ShowS,
    Read(readsPrec, readList),
    Show(showsPrec, show, showList),
    reads, shows, read, lex,
    showChar, showString, readParen, showParen, 
    -- module PreludeIO:
    FilePath, IOError, ioError, userError, catch,
    putChar, putStr, putStrLn, print,
    getChar, getLine, getContents, interact,
    readFile, writeFile, appendFile, readIO, readLn,
    -- defined here:
    Bool(False, True),
    Maybe(Nothing, Just),
    Either(Left, Right),
    Ordering(LT, EQ, GT),
    Char, String, Int, Integer, Float, Double, Rational, IO,

--      These built-in types are defined in the Prelude, but
--      are denoted by built-in syntax, and cannot legally
--      appear in an export list.
--  But they do here, because the .hx interface needs the exports.
-- Commented out for Cabal-Hat
-- #ifdef HX
--  List type: 
--    []((:), []),
--  Tuple types: up to 15
--    (,)((,)), (,,)((,,)),(,,,)((,,,)),(,,,,)((,,,,)),(,,,,,)((,,,,,)),
--    (,,,,,,)((,,,,,,)),(,,,,,,,)((,,,,,,,)),(,,,,,,,,)((,,,,,,,,)),
--    (,,,,,,,,,)((,,,,,,,,,)),(,,,,,,,,,,)((,,,,,,,,,,)),
--    (,,,,,,,,,,,)((,,,,,,,,,,,)),(,,,,,,,,,,,,)((,,,,,,,,,,,,)),
--    (,,,,,,,,,,,,,)((,,,,,,,,,,,,,)),(,,,,,,,,,,,,,,)((,,,,,,,,,,,,,,)),
--  Trivial type: 
--    ()(()),
--  Functions: 
--    (->),
-- #endif

    Eq((==), (/=)),
    Ord(compare, (<), (<=), (>=), (>), max, min),
    Enum(succ, pred, toEnum, fromEnum, enumFrom, enumFromThen,
         enumFromTo, enumFromThenTo),
    Bounded(minBound, maxBound),
    Num((+), (-), (*), negate, abs, signum, fromInteger),
    Real(toRational),
    Integral(quot, rem, div, mod, quotRem, divMod, toInteger),
    Fractional((/), recip, fromRational),
    Floating(pi, exp, log, sqrt, (**), logBase, sin, cos, tan,
             asin, acos, atan, sinh, cosh, tanh, asinh, acosh, atanh),
    RealFrac(properFraction, truncate, round, ceiling, floor),
    RealFloat(floatRadix, floatDigits, floatRange, decodeFloat,
              encodeFloat, exponent, significand, scaleFloat, isNaN,
              isInfinite, isDenormalized, isIEEE, isNegativeZero, atan2),
    Monad((>>=), (>>), return, fail),
    Functor(fmap),
    mapM, mapM_, sequence, sequence_, (=<<), 
    maybe, either,
    (&&), (||), not, otherwise,
    subtract, even, odd, gcd, lcm, (^), (^^), 
    fromIntegral, realToFrac, 
    fst, snd, curry, uncurry, id, const, (.), flip, ($), until,
    asTypeOf, error, undefined,
    seq, ($!)
  ) where

import PreludeBasic
