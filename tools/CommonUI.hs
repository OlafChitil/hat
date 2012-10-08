module CommonUI
  ( hatObserve, hatTrail, hatDelta, hatDetect, hatAnim, hatExplore, hatView
  , shortHelpText
  , Keep(..)
  , Options(..), initialOptions
  , OptionCmd(..), optionCmd
  , onOff, number, safeReadInt
  , optionsUpdate, showOption, showOnOff
  ) where

import Data.List	(isPrefixOf,isSuffixOf,intersperse)
import Data.Char	(isDigit,digitToInt)
import Numeric	        (readDec)
import LowLevel	        (FileNode(..))
import HighlightStyle	(highlight,Highlight(..))
import SrcRef           (SrcRef(..))

-- Links to other hat-tools
hatObserve :: FilePath -> String -> String
hatObserve f pat = "xterm -e hat-observe "++f++" \""++esc pat++"\" &"
  where
      esc []        = []
      esc ('"':xs) = '\\': '"': esc xs
      esc ('$':xs) = '\\': '$': esc xs
      esc ('`':xs) = '\\': '`': esc xs
      esc (x:xs)    = x: esc xs

hatDelta :: FilePath -> FileNode -> String
hatDelta f n = "xterm -e hat-delta "++f++" "++show (int n)++" &"

hatDetect :: FilePath -> FileNode -> String
hatDetect f n = "xterm -e hat-delta --detect "++f++" "++show (int n)++" &"

hatTrail :: FilePath -> FileNode -> String
hatTrail f n  = "xterm -e hat-trail "++f++" -remote "++show (int n)++" &"

hatAnim :: FilePath -> FileNode -> String
hatAnim f n   = "xterm -e hat-anim "++f++" "++show (int n)++" &"

hatExplore :: FilePath -> FileNode -> String
hatExplore f n = "xterm -e hat-explore "++f++" "++show (int n)++" &"

hatView :: SrcRef -> String
hatView sr = "xterm -e hat-view "++SrcRef.filename sr++" "
                                 ++show (SrcRef.line sr)++" "
                                 ++show (SrcRef.column sr)++" "
                                 ++show (SrcRef.lineend sr)++" "
                                 ++show (SrcRef.columnend sr)++" &"

-- Interactive help
shortHelpText  = ":? for help, :q to quit"

-- Filtering modes
data Keep = All | Unique | MostGeneralSoFar | MostGeneral deriving Eq
instance Show Keep where
  showsPrec p All               = showString "all"
  showsPrec p Unique            = showString "unique"
  showsPrec p MostGeneralSoFar  = showString "generalise"
  showsPrec p MostGeneral       = showString "mostgeneral"

-- User-changeable display options
data Options =
    Options
	{ cutoffDepth   :: Int		-- expression auto-cutoff depth
	, unevalMode    :: Bool		-- show unevaluated stuff in full?
	, stringSugar   :: Bool		-- show strings with sugar?
	, listSugar     :: Bool		-- show lists with sugar?
	, recursiveMode	:: Bool		--
	, colourBracks  :: Bool
	, equations	    :: Bool		-- always show equations?
	, showQual	    :: Bool		-- show identifiers qualified?
	, filterMode	:: Keep		-- show duplicate equations?
	}
initialOptions = Options
	{ cutoffDepth	= 10
	, unevalMode	= False
	, stringSugar	= True
	, listSugar	= True
	, recursiveMode	= True
	, colourBracks = False
	, equations	= True
	, showQual	= False
	, filterMode	= Unique
	}

-- Command interpreter for setting display options
data OptionCmd
	= Uneval Bool | Strings Bool | Lists Bool | Recursive Bool
	| CutOff Int  | Deeper Int   | Shallower Int | Qualify Bool
	| Filter Keep | Equations Bool

optionCmd :: [String] -> Maybe OptionCmd
optionCmd (s:ss)
    | s `isPrefixOf` "uneval"      = onOff Uneval ss
    | s `isPrefixOf` "strSugar"    = onOff Strings ss
    | s `isPrefixOf` "listSugar"   = onOff Lists ss
    | s `isPrefixOf` "recursive"   = onOff Recursive ss
    | s `isPrefixOf` "all"         = Just (Filter All)
    | s `isPrefixOf` "unique"      = Just (Filter Unique)
    | s `isPrefixOf` "generalise"  = Just (Filter MostGeneralSoFar)
    | s `isPrefixOf` "mostgeneral" = Just (Filter MostGeneral)
    | s `isPrefixOf` "cutoff"      = Just (number CutOff ss 10)
    | s `isPrefixOf` "qualified"   = onOff Qualify ss
    | s `isPrefixOf` "equations"   = onOff Equations ss
    | s `isPrefixOf` "eqns"        = onOff Equations ss
    | head s == '+'                = Just (number Deeper (tail s:ss) 1)
    | head s == '-'                = Just (number Shallower (tail s:ss) 1)
    | otherwise                    = Nothing

onOff :: (Bool->a) -> [String] -> Maybe a
onOff mode s | null s    = Just (mode True)
             | otherwise = case head s of "on"     -> Just (mode True)
                                          "active" -> Just (mode True)
                                          "off"    -> Just (mode False)
                                          "no"     -> Just (mode False)
                                          _        -> Nothing

number :: (Int->a) -> [String] -> Int -> a
number cons s def = (cons . safeReadInt def . unwords) s

--safeRead takes a default value to return if the parse is not successful
safeReadInt :: Int -> String -> Int
safeReadInt d s =
  case readDec s of
    [] -> d
    ((n,_):_) -> n

-- change just one field of the Options value
optionsUpdate :: OptionCmd -> Options -> Options
optionsUpdate (Uneval b)    opts  =  opts { unevalMode = b }
optionsUpdate (Strings b)   opts  =  opts { stringSugar = b }
optionsUpdate (Lists b)     opts  =  opts { listSugar = b }
optionsUpdate (Recursive b) opts  =  opts { recursiveMode = b }
optionsUpdate (CutOff n)    opts  =  opts { cutoffDepth = n }
optionsUpdate (Deeper n)    opts  =  opts { cutoffDepth = cutoffDepth opts + n }
optionsUpdate (Shallower n) opts  =  opts { cutoffDepth =
						 max (cutoffDepth opts - n) 1 }
optionsUpdate (Qualify b)   opts  =  opts { showQual = b }
optionsUpdate (Filter k)    opts  =  opts { filterMode = k }
optionsUpdate (Equations b) opts  =  opts { equations = b }

-- describe one field of the Options value in English
showOption :: OptionCmd -> Options -> String
showOption (Uneval b)    opts  = "  "++highlight [Underscore] "uneval"
                                 ++ showOnOff (unevalMode opts)
                                 ++ "show unevaluated exprs in full"
showOption (Strings b)   opts  = "  "++highlight [Underscore] "strSugar"
                                 ++ showOnOff (stringSugar opts)
                                 ++ "sugar syntax for strings"
showOption (Lists b)     opts  = "  "++highlight [Underscore] "listSugar"
                                 ++ showOnOff (listSugar opts)
                                 ++ "sugar syntax for lists"
showOption (Recursive b) opts  = "  "++highlight [Underscore] "recursive"
                                 ++ showOnOff (recursiveMode opts)
                                 ++ "show recursive calls"
showOption (CutOff n)    opts  = "  "++highlight [Underscore] "cutoff"
                                 ++ "\t"++show (cutoffDepth opts)++"\t"
                                 ++ "expression cutoff depth"
showOption (Deeper n)    opts  = showOption (CutOff 0) opts
showOption (Shallower n) opts  = showOption (CutOff 0) opts
showOption (Qualify b)   opts  = "  "++highlight [Underscore] "qualified"
                                 ++ showOnOff (showQual opts)
                                 ++ "show identifier names fully qualified"
showOption (Filter k)    opts  = "  show ("
                                 ++ concat
                                     (intersperse ","
                                       (map keep [All,Unique,MostGeneralSoFar
                                                 ,MostGeneral]))
                                 ++ ") equations"
  where
    fm = filterMode opts
    keep k | k==fm      = highlight [Underscore] (show k)
           | otherwise  = show k
showOption (Equations b) opts  = "  "++highlight [Underscore] "equations"
                                ++ showOnOff (equations opts)
                                ++ "show right-hand-side of equations"

showOnOff :: Bool -> String
showOnOff True  = "\t"++ highlight [Bold] "on" ++"\t"
showOnOff False = "\toff\t"

