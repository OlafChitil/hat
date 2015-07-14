-- Simple 'make' for tracing
-- only works with ghc
-- authors: Neil Mitchel, Olaf Chitil
module Main where

import System.Environment (getArgs)
import System.Process (system)
import System.Exit (ExitCode(..), exitFailure)
import Data.List (nub, partition, isPrefixOf, intersperse, concat)
import Data.Char (isSpace)
import System.Directory (doesFileExist, findExecutable)
import Control.Monad (when)


main = do x <- getArgs
          Just hatPath <- findExecutable "hat-make"
          let hatFolder = dropFilename hatPath
          when (null x) (error "Please give the name of the file to hat-make on the command line")
          let opts = init x
          let optStr = concat (intersperse " " opts)
          let y = last x
          systemTry $ "ghc -M -dep-makefile .depend " ++ y ++ " " ++ optStr
          mak <- readFile ".depend"
          let (files,depends) = parseMakefile mak
          translate hatFolder files depends
          compileAll hatFolder optStr y

dropFilename x = reverse $ dropWhile (\x -> not $ x `elem` "\\/") $ reverse x


parseMakefile :: String -> ([String], [(String, String)])
parseMakefile src =
        (
            nub $ concat [[a,b] | (a,b) <- res]
            ,
            nub $ [(a,b) | (a,b) <- res, a /= b]
        )
    where
        res = concatMap f $ lines src
    
        f :: String -> [(String, String)]
        f ('#':_) = []
        f x = [(filename a, filename b)]
            where (a,_:b) = break (== ':') x

filename x = reverse $ tail $ dropWhile (/= '.') $ reverse $ filter (not . isSpace) x


translate :: String -> [String] -> [(String, String)] -> IO ()
translate hatFolder files deps =
        if null files then return ()
        else if null yes then error "Circular dependancies"
        else mapM_ (translateFile hatFolder) yes >> translate hatFolder no newdeps
    where
        newdeps = [(a,b) | (a,b) <- deps, not (b `elem` yes)]
        (yes,no) = partition isDoable files
        isDoable x = not $ any ((==) x . fst) deps


translateFile :: String -> FilePath -> IO ()
translateFile hatFolder file = do
    fil <- pickFile file
    putStrLn $ "Converting with Hat, " ++ fil
    systemTry $ "hat-trans " ++ fil


pickFile :: FilePath -> IO FilePath
pickFile x = do hs  <- doesFileExist (x++".hs")
                lhs <- doesFileExist (x++".lhs")
                return $ if lhs then x ++ ".lhs" else x ++ ".hs"

compileAll :: String -> String -> String -> IO ()
compileAll hatFolder optStr file = do
    putStrLn "Compiling with GHC..."
    systemTry $ "ghc --make Hat/" ++ file ++ " " ++ optStr
                    -- " -ffi -fglasgow-exts -cpp -i.;" ++
                    -- hatFolder ++ "/hs " ++
                    -- hatFolder ++ "/c/hat-c.o " ++
                    -- hatFolder ++ "/c/ntohl.o"

systemTry :: String -> IO ()
systemTry x = do putStrLn x
                 y <- system x
                 when (y /= ExitSuccess) exitFailure


prettyFile :: FilePath -> FilePath
prettyFile x = x  -- for Unix
-- prettyFile x = map g (if "./" `isPrefixOf` x then drop 2 x else x)
--     where
--        g '/' = '\\'
--        g x = x
