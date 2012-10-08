-- HatCover: Colin Runciman, University of York, October 2004
-- Print Haskell sources highlighting the maximal expressions with
-- instances recorded in a given Hat trace.

import HighlightStyle (highlightOn, highlightOff, Highlight(..), Colour(..))
import Data.List (isPrefixOf, isSuffixOf)
import System.IO (stderr, hPutStrLn)
import System.Environment (getArgs, getProgName)
import System.Exit (exitWith, ExitCode(..))
import HatCover (hatCover, Interval, LC)


printCover :: (String, String) -> [(String, [Interval LC])] -> IO ()
printCover hiOnOff = mapM_ (printModuleCover hiOnOff)

printModuleCover :: (String, String) -> (String, [Interval LC]) -> IO ()
printModuleCover hiOnOff (f, c) =
  do
    src <- readFile f
    printLo hiOnOff (1,1) c (map expand (lines src))

printLo :: (String, String) -> LC -> [Interval LC] -> [String] -> IO ()
printLo _ _      [] srcLines =
  mapM_ putStrLn srcLines
printLo hiOnOff (lineNo,colNo) (((lstart,cstart),(lstop,cstop)):ivals) srcLines =
  do
    mapM_ putStrLn (take lnLo srcLines)
    putStr (take chLo (head srcLines'))
    printHi hiOnOff
      (lstart,cstart) (lstop,cstop) ivals
      (drop chLo (head srcLines') : tail srcLines')
  where
  lnLo = lstart-lineNo
  chLo = cstart-(if lnLo==0 then colNo else 1)
  srcLines' = drop lnLo srcLines 
  
printHi :: (String, String) -> LC -> LC -> [Interval LC] -> [String] -> IO ()
printHi hiOnOff (lineNo,colNo) (lstop,cstop) ivals srcLines =
  do
    mapM_ (putStrLn . high hiOnOff) (take lnHi srcLines)
    putStr (high hiOnOff (take chHi (head srcLines')))
    printLo hiOnOff
      (lstop,cstop+1) ivals
      (drop chHi (head srcLines') : tail srcLines')    
  where
  lnHi = lstop-lineNo
  chHi = 1+cstop-(if lnHi==0 then colNo else 1)
  srcLines' = drop lnHi srcLines
  
high :: (String, String) -> String -> String
high (hiOn, hiOff) s =
  takeWhile (==' ') s ++ hiOn ++ dropWhile (== ' ') s ++ hiOff

main =
  do
    args    <- getArgs
    prog    <- getProgName
    let (options, nonOptions) = span ("-" `isPrefixOf`) args
    let hiOnOpt  = [drop (length "-hion=") opt | opt <- options,
                                                 "-hion=" `isPrefixOf` opt]
    let hiOn     = if null hiOnOpt then highlightOn [Bold] else head hiOnOpt
    let hiOffOpt = [drop (length "-hioff=") opt | opt <- options,
                                                  "-hioff=" `isPrefixOf` opt]
    let hiOff    = if null hiOnOpt then highlightOff else head hiOffOpt
    hatfile <- case nonOptions of
               (t:_) -> return (rectify ".hat" t)
               _     -> do hPutStrLn stderr (prog++": no trace file")
                           exitWith (ExitFailure 1)
    let moduleNames = map (rectify ".hs") (tail nonOptions)
    res <- hatCover hatfile moduleNames
    printCover (hiOn, hiOff) res

rectify :: String -> FilePath -> FilePath
rectify ext f | ext `isSuffixOf` f = f
              | otherwise          = f ++ ext

expand :: String -> String
expand = expandFrom 1

expandFrom :: Int -> String -> String
expandFrom _ "" = ""
expandFrom n (x:xs) = f (expandFrom (n+d) xs)
  where
  (d, f) = if x=='\t' then (8 - (n-1)`mod`8, (take d spaces ++))
                      else (1, (x:))

spaces :: String
spaces = repeat ' '


