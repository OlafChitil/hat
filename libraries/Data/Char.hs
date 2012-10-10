-- For Haskell 2010 re-export from Haskell 98 Prelude basic plus 
-- additional type and functions

module Data.Char (  
    Char,  String,  isControl,  isSpace,  isLower,  isUpper,  isAlpha,  
    isAlphaNum,  isPrint,  isDigit,  isOctDigit,  isHexDigit,  isLetter,  
    isMark,  isNumber,  isPunctuation,  isSymbol,  isSeparator,  isAscii,  
    isLatin1,  isAsciiUpper,  isAsciiLower,  
    GeneralCategory(UppercaseLetter,  
                    LowercaseLetter,  
                    TitlecaseLetter,  
                    ModifierLetter,  
                    OtherLetter,  
                    NonSpacingMark,  
                    SpacingCombiningMark,  
                    EnclosingMark,  
                    DecimalNumber,  
                    LetterNumber,  
                    OtherNumber,  
                    ConnectorPunctuation,  
                    DashPunctuation,  
                    OpenPunctuation,  
                    ClosePunctuation,  
                    InitialQuote,  
                    FinalQuote,  
                    OtherPunctuation,  
                    MathSymbol,  
                    CurrencySymbol,  
                    ModifierSymbol,  
                    OtherSymbol,  
                    Space,  
                    LineSeparator,  
                    ParagraphSeparator,  
                    Control,  
                    Format,  
                    Surrogate,  
                    PrivateUse,  
                    NotAssigned),  
    generalCategory,  toUpper,  toLower,  toTitle,  digitToInt,  intToDigit,  
    ord,  chr,  showLitChar,  lexLitChar,  readLitChar  
  ) where

import PreludeBasic
import Ix
import PreludeBuiltinTypes  -- (fromBool)
import qualified NotHat.Data.Char  -- not to be transformed

foreign import haskell "Data.Char.isLetter"
  isLetter :: Char -> Bool
foreign import haskell "Data.Char.isMark"
  isMark :: Char -> Bool
foreign import haskell "Data.Char.isNumber"
  isNumber :: Char -> Bool
foreign import haskell "Data.Char.isPunctuation"
  isPunctuation :: Char -> Bool
foreign import haskell "Data.Char.isSymbol"
  isSymbol :: Char -> Bool
foreign import haskell "Data.Char.isSeparator"
  isSeparator :: Char -> Bool
foreign import haskell "Data.Char.isAsciiUpper"
  isAsciiUpper :: Char -> Bool
foreign import haskell "Data.Char.isAsciiLower"
  isAsciiLower :: Char -> Bool

foreign import haskell "Data.Char.toTitle"
  toTitle :: Char -> Char

data GeneralCategory
        = UppercaseLetter       -- ^ Lu: Letter, Uppercase
        | LowercaseLetter       -- ^ Ll: Letter, Lowercase
        | TitlecaseLetter       -- ^ Lt: Letter, Titlecase
        | ModifierLetter        -- ^ Lm: Letter, Modifier
        | OtherLetter           -- ^ Lo: Letter, Other
        | NonSpacingMark        -- ^ Mn: Mark, Non-Spacing
        | SpacingCombiningMark  -- ^ Mc: Mark, Spacing Combining
        | EnclosingMark         -- ^ Me: Mark, Enclosing
        | DecimalNumber         -- ^ Nd: Number, Decimal
        | LetterNumber          -- ^ Nl: Number, Letter
        | OtherNumber           -- ^ No: Number, Other
        | ConnectorPunctuation  -- ^ Pc: Punctuation, Connector
        | DashPunctuation       -- ^ Pd: Punctuation, Dash
        | OpenPunctuation       -- ^ Ps: Punctuation, Open
        | ClosePunctuation      -- ^ Pe: Punctuation, Close
        | InitialQuote          -- ^ Pi: Punctuation, Initial quote
        | FinalQuote            -- ^ Pf: Punctuation, Final quote
        | OtherPunctuation      -- ^ Po: Punctuation, Other
        | MathSymbol            -- ^ Sm: Symbol, Math
        | CurrencySymbol        -- ^ Sc: Symbol, Currency
        | ModifierSymbol        -- ^ Sk: Symbol, Modifier
        | OtherSymbol           -- ^ So: Symbol, Other
        | Space                 -- ^ Zs: Separator, Space
        | LineSeparator         -- ^ Zl: Separator, Line
        | ParagraphSeparator    -- ^ Zp: Separator, Paragraph
        | Control               -- ^ Cc: Other, Control
        | Format                -- ^ Cf: Other, Format
        | Surrogate             -- ^ Cs: Other, Surrogate
        | PrivateUse            -- ^ Co: Other, Private Use
        | NotAssigned           -- ^ Cn: Other, Not Assigned
        deriving (Eq, Ord, Enum, Read, Show, Bounded, Ix)

foreign import haskell 
  "(\\_ i -> Prelude.fromEnum (Data.Char.generalCategory (Prelude.toEnum i))) Prelude.True"
  genCat :: Int -> Int

generalCategory :: Char -> GeneralCategory
generalCategory c = toEnum $ genCat $ ord c
