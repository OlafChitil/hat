module Hat.Data.Char
       (Char, String, gisControl, aisControl, hisControl,
        gisSpace, aisSpace, hisSpace, gisLower, aisLower,
        hisLower, gisUpper, aisUpper, hisUpper, gisAlpha,
        aisAlpha, hisAlpha, gisAlphaNum, aisAlphaNum,
        hisAlphaNum, gisPrint, aisPrint, hisPrint, gisDigit,
        aisDigit, hisDigit, gisOctDigit, aisOctDigit,
        hisOctDigit, gisHexDigit, aisHexDigit, hisHexDigit,
        gisLetter, aisLetter, hisLetter, gisMark, aisMark,
        hisMark, gisNumber, aisNumber, hisNumber,
        gisPunctuation, aisPunctuation, hisPunctuation,
        gisSymbol, aisSymbol, hisSymbol, gisSeparator,
        aisSeparator, hisSeparator, gisAscii, aisAscii,
        hisAscii, gisLatin1, aisLatin1, hisLatin1,
        gisAsciiUpper, aisAsciiUpper, hisAsciiUpper,
        gisAsciiLower, aisAsciiLower, hisAsciiLower,
        GeneralCategory(UppercaseLetter, LowercaseLetter,
                        TitlecaseLetter, ModifierLetter, OtherLetter,
                        NonSpacingMark, SpacingCombiningMark, EnclosingMark,
                        DecimalNumber, LetterNumber, OtherNumber,
                        ConnectorPunctuation, DashPunctuation,
                        OpenPunctuation, ClosePunctuation, InitialQuote,
                        FinalQuote, OtherPunctuation, MathSymbol,
                        CurrencySymbol, ModifierSymbol, OtherSymbol, Space,
                        LineSeparator, ParagraphSeparator, Control, Format,
                        Surrogate, PrivateUse, NotAssigned),
        aUppercaseLetter, aLowercaseLetter, aTitlecaseLetter,
        aModifierLetter, aOtherLetter, aNonSpacingMark,
        aSpacingCombiningMark, aEnclosingMark,
        aDecimalNumber, aLetterNumber, aOtherNumber,
        aConnectorPunctuation, aDashPunctuation,
        aOpenPunctuation, aClosePunctuation, aInitialQuote,
        aFinalQuote, aOtherPunctuation, aMathSymbol,
        aCurrencySymbol, aModifierSymbol, aOtherSymbol,
        aSpace, aLineSeparator, aParagraphSeparator,
        aControl, aFormat, aSurrogate, aPrivateUse,
        aNotAssigned, ggeneralCategory, ageneralCategory,
        hgeneralCategory, gtoUpper, atoUpper, htoUpper,
        gtoLower, atoLower, htoLower, gtoTitle, atoTitle,
        htoTitle, gdigitToInt, adigitToInt, hdigitToInt,
        gintToDigit, aintToDigit, hintToDigit, gord, gchr,
        gshowLitChar, ashowLitChar, hshowLitChar,
        glexLitChar, alexLitChar, hlexLitChar, greadLitChar,
        areadLitChar, hreadLitChar)
       where
import qualified Prelude
import qualified Hat.Hat as T
import qualified Hat.PreludeBasic
import qualified Hat.PreludeBuiltinTypes
import Hat.Prelude
import Hat.PreludeBasic
import Hat.Ix
import Hat.PreludeBuiltinTypes as T
import qualified Data.Char
 
gisLetter ::
          T.RefSrcPos -> T.RefExp -> T.R (T.Fun Char Bool)
gisLetter pisLetter p
  = T.ufun1 aisLetter pisLetter p hisLetter
hisLetter z1isLetter kisLetter
  = T.fromBool kisLetter
      (Data.Char.isLetter (T.toChar kisLetter z1isLetter))
 
gisMark ::
        T.RefSrcPos -> T.RefExp -> T.R (T.Fun Char Bool)
gisMark pisMark p = T.ufun1 aisMark pisMark p hisMark
hisMark z1isMark kisMark
  = T.fromBool kisMark
      (Data.Char.isMark (T.toChar kisMark z1isMark))
 
gisNumber ::
          T.RefSrcPos -> T.RefExp -> T.R (T.Fun Char Bool)
gisNumber pisNumber p
  = T.ufun1 aisNumber pisNumber p hisNumber
hisNumber z1isNumber kisNumber
  = T.fromBool kisNumber
      (Data.Char.isNumber (T.toChar kisNumber z1isNumber))
 
gisPunctuation ::
               T.RefSrcPos -> T.RefExp -> T.R (T.Fun Char Bool)
gisPunctuation pisPunctuation p
  = T.ufun1 aisPunctuation pisPunctuation p
      hisPunctuation
hisPunctuation z1isPunctuation kisPunctuation
  = T.fromBool kisPunctuation
      (Data.Char.isPunctuation
         (T.toChar kisPunctuation z1isPunctuation))
 
gisSymbol ::
          T.RefSrcPos -> T.RefExp -> T.R (T.Fun Char Bool)
gisSymbol pisSymbol p
  = T.ufun1 aisSymbol pisSymbol p hisSymbol
hisSymbol z1isSymbol kisSymbol
  = T.fromBool kisSymbol
      (Data.Char.isSymbol (T.toChar kisSymbol z1isSymbol))
 
gisSeparator ::
             T.RefSrcPos -> T.RefExp -> T.R (T.Fun Char Bool)
gisSeparator pisSeparator p
  = T.ufun1 aisSeparator pisSeparator p hisSeparator
hisSeparator z1isSeparator kisSeparator
  = T.fromBool kisSeparator
      (Data.Char.isSeparator
         (T.toChar kisSeparator z1isSeparator))
 
gisAsciiUpper ::
              T.RefSrcPos -> T.RefExp -> T.R (T.Fun Char Bool)
gisAsciiUpper pisAsciiUpper p
  = T.ufun1 aisAsciiUpper pisAsciiUpper p hisAsciiUpper
hisAsciiUpper z1isAsciiUpper kisAsciiUpper
  = T.fromBool kisAsciiUpper
      (Data.Char.isAsciiUpper
         (T.toChar kisAsciiUpper z1isAsciiUpper))
 
gisAsciiLower ::
              T.RefSrcPos -> T.RefExp -> T.R (T.Fun Char Bool)
gisAsciiLower pisAsciiLower p
  = T.ufun1 aisAsciiLower pisAsciiLower p hisAsciiLower
hisAsciiLower z1isAsciiLower kisAsciiLower
  = T.fromBool kisAsciiLower
      (Data.Char.isAsciiLower
         (T.toChar kisAsciiLower z1isAsciiLower))
 
gtoTitle ::
         T.RefSrcPos -> T.RefExp -> T.R (T.Fun Char Char)
gtoTitle ptoTitle p
  = T.ufun1 atoTitle ptoTitle p htoTitle
htoTitle z1toTitle ktoTitle
  = T.fromChar ktoTitle
      (Data.Char.toTitle (T.toChar ktoTitle z1toTitle))
 
data GeneralCategory = UppercaseLetter
                     | LowercaseLetter
                     | TitlecaseLetter
                     | ModifierLetter
                     | OtherLetter
                     | NonSpacingMark
                     | SpacingCombiningMark
                     | EnclosingMark
                     | DecimalNumber
                     | LetterNumber
                     | OtherNumber
                     | ConnectorPunctuation
                     | DashPunctuation
                     | OpenPunctuation
                     | ClosePunctuation
                     | InitialQuote
                     | FinalQuote
                     | OtherPunctuation
                     | MathSymbol
                     | CurrencySymbol
                     | ModifierSymbol
                     | OtherSymbol
                     | Space
                     | LineSeparator
                     | ParagraphSeparator
                     | Control
                     | Format
                     | Surrogate
                     | PrivateUse
                     | NotAssigned
 
instance T.WrapVal GeneralCategory where
        wrapVal pwrapVal kwrapVal@UppercaseLetter p
          = T.R kwrapVal
              (T.mkValueUse p pwrapVal aUppercaseLetter)
        wrapVal pwrapVal kwrapVal@LowercaseLetter p
          = T.R kwrapVal
              (T.mkValueUse p pwrapVal aLowercaseLetter)
        wrapVal pwrapVal kwrapVal@TitlecaseLetter p
          = T.R kwrapVal
              (T.mkValueUse p pwrapVal aTitlecaseLetter)
        wrapVal pwrapVal kwrapVal@ModifierLetter p
          = T.R kwrapVal
              (T.mkValueUse p pwrapVal aModifierLetter)
        wrapVal pwrapVal kwrapVal@OtherLetter p
          = T.R kwrapVal (T.mkValueUse p pwrapVal aOtherLetter)
        wrapVal pwrapVal kwrapVal@NonSpacingMark p
          = T.R kwrapVal
              (T.mkValueUse p pwrapVal aNonSpacingMark)
        wrapVal pwrapVal kwrapVal@SpacingCombiningMark p
          = T.R kwrapVal
              (T.mkValueUse p pwrapVal aSpacingCombiningMark)
        wrapVal pwrapVal kwrapVal@EnclosingMark p
          = T.R kwrapVal
              (T.mkValueUse p pwrapVal aEnclosingMark)
        wrapVal pwrapVal kwrapVal@DecimalNumber p
          = T.R kwrapVal
              (T.mkValueUse p pwrapVal aDecimalNumber)
        wrapVal pwrapVal kwrapVal@LetterNumber p
          = T.R kwrapVal
              (T.mkValueUse p pwrapVal aLetterNumber)
        wrapVal pwrapVal kwrapVal@OtherNumber p
          = T.R kwrapVal (T.mkValueUse p pwrapVal aOtherNumber)
        wrapVal pwrapVal kwrapVal@ConnectorPunctuation p
          = T.R kwrapVal
              (T.mkValueUse p pwrapVal aConnectorPunctuation)
        wrapVal pwrapVal kwrapVal@DashPunctuation p
          = T.R kwrapVal
              (T.mkValueUse p pwrapVal aDashPunctuation)
        wrapVal pwrapVal kwrapVal@OpenPunctuation p
          = T.R kwrapVal
              (T.mkValueUse p pwrapVal aOpenPunctuation)
        wrapVal pwrapVal kwrapVal@ClosePunctuation p
          = T.R kwrapVal
              (T.mkValueUse p pwrapVal aClosePunctuation)
        wrapVal pwrapVal kwrapVal@InitialQuote p
          = T.R kwrapVal
              (T.mkValueUse p pwrapVal aInitialQuote)
        wrapVal pwrapVal kwrapVal@FinalQuote p
          = T.R kwrapVal (T.mkValueUse p pwrapVal aFinalQuote)
        wrapVal pwrapVal kwrapVal@OtherPunctuation p
          = T.R kwrapVal
              (T.mkValueUse p pwrapVal aOtherPunctuation)
        wrapVal pwrapVal kwrapVal@MathSymbol p
          = T.R kwrapVal (T.mkValueUse p pwrapVal aMathSymbol)
        wrapVal pwrapVal kwrapVal@CurrencySymbol p
          = T.R kwrapVal
              (T.mkValueUse p pwrapVal aCurrencySymbol)
        wrapVal pwrapVal kwrapVal@ModifierSymbol p
          = T.R kwrapVal
              (T.mkValueUse p pwrapVal aModifierSymbol)
        wrapVal pwrapVal kwrapVal@OtherSymbol p
          = T.R kwrapVal (T.mkValueUse p pwrapVal aOtherSymbol)
        wrapVal pwrapVal kwrapVal@Space p
          = T.R kwrapVal (T.mkValueUse p pwrapVal aSpace)
        wrapVal pwrapVal kwrapVal@LineSeparator p
          = T.R kwrapVal
              (T.mkValueUse p pwrapVal aLineSeparator)
        wrapVal pwrapVal kwrapVal@ParagraphSeparator p
          = T.R kwrapVal
              (T.mkValueUse p pwrapVal aParagraphSeparator)
        wrapVal pwrapVal kwrapVal@Control p
          = T.R kwrapVal (T.mkValueUse p pwrapVal aControl)
        wrapVal pwrapVal kwrapVal@Format p
          = T.R kwrapVal (T.mkValueUse p pwrapVal aFormat)
        wrapVal pwrapVal kwrapVal@Surrogate p
          = T.R kwrapVal (T.mkValueUse p pwrapVal aSurrogate)
        wrapVal pwrapVal kwrapVal@PrivateUse p
          = T.R kwrapVal (T.mkValueUse p pwrapVal aPrivateUse)
        wrapVal pwrapVal kwrapVal@NotAssigned p
          = T.R kwrapVal (T.mkValueUse p pwrapVal aNotAssigned)
 
instance Eq GeneralCategory where
        (%==) !== p = T.ufun2 (+>>=#>=>>=$!==) (%==) p (*==)
          where (T.R UppercaseLetter _ *==
                   T.R UppercaseLetter _)
                  p
                  = T.con0 T.mkNoSrcPos p Hat.PreludeBuiltinTypes.True
                      Hat.PreludeBuiltinTypes.aTrue
                (T.R LowercaseLetter _ *== T.R LowercaseLetter _) p
                  = T.con0 T.mkNoSrcPos p Hat.PreludeBuiltinTypes.True
                      Hat.PreludeBuiltinTypes.aTrue
                (T.R TitlecaseLetter _ *== T.R TitlecaseLetter _) p
                  = T.con0 T.mkNoSrcPos p Hat.PreludeBuiltinTypes.True
                      Hat.PreludeBuiltinTypes.aTrue
                (T.R ModifierLetter _ *== T.R ModifierLetter _) p
                  = T.con0 T.mkNoSrcPos p Hat.PreludeBuiltinTypes.True
                      Hat.PreludeBuiltinTypes.aTrue
                (T.R OtherLetter _ *== T.R OtherLetter _) p
                  = T.con0 T.mkNoSrcPos p Hat.PreludeBuiltinTypes.True
                      Hat.PreludeBuiltinTypes.aTrue
                (T.R NonSpacingMark _ *== T.R NonSpacingMark _) p
                  = T.con0 T.mkNoSrcPos p Hat.PreludeBuiltinTypes.True
                      Hat.PreludeBuiltinTypes.aTrue
                (T.R SpacingCombiningMark _ *==
                   T.R SpacingCombiningMark _)
                  p
                  = T.con0 T.mkNoSrcPos p Hat.PreludeBuiltinTypes.True
                      Hat.PreludeBuiltinTypes.aTrue
                (T.R EnclosingMark _ *== T.R EnclosingMark _) p
                  = T.con0 T.mkNoSrcPos p Hat.PreludeBuiltinTypes.True
                      Hat.PreludeBuiltinTypes.aTrue
                (T.R DecimalNumber _ *== T.R DecimalNumber _) p
                  = T.con0 T.mkNoSrcPos p Hat.PreludeBuiltinTypes.True
                      Hat.PreludeBuiltinTypes.aTrue
                (T.R LetterNumber _ *== T.R LetterNumber _) p
                  = T.con0 T.mkNoSrcPos p Hat.PreludeBuiltinTypes.True
                      Hat.PreludeBuiltinTypes.aTrue
                (T.R OtherNumber _ *== T.R OtherNumber _) p
                  = T.con0 T.mkNoSrcPos p Hat.PreludeBuiltinTypes.True
                      Hat.PreludeBuiltinTypes.aTrue
                (T.R ConnectorPunctuation _ *==
                   T.R ConnectorPunctuation _)
                  p
                  = T.con0 T.mkNoSrcPos p Hat.PreludeBuiltinTypes.True
                      Hat.PreludeBuiltinTypes.aTrue
                (T.R DashPunctuation _ *== T.R DashPunctuation _) p
                  = T.con0 T.mkNoSrcPos p Hat.PreludeBuiltinTypes.True
                      Hat.PreludeBuiltinTypes.aTrue
                (T.R OpenPunctuation _ *== T.R OpenPunctuation _) p
                  = T.con0 T.mkNoSrcPos p Hat.PreludeBuiltinTypes.True
                      Hat.PreludeBuiltinTypes.aTrue
                (T.R ClosePunctuation _ *== T.R ClosePunctuation _) p
                  = T.con0 T.mkNoSrcPos p Hat.PreludeBuiltinTypes.True
                      Hat.PreludeBuiltinTypes.aTrue
                (T.R InitialQuote _ *== T.R InitialQuote _) p
                  = T.con0 T.mkNoSrcPos p Hat.PreludeBuiltinTypes.True
                      Hat.PreludeBuiltinTypes.aTrue
                (T.R FinalQuote _ *== T.R FinalQuote _) p
                  = T.con0 T.mkNoSrcPos p Hat.PreludeBuiltinTypes.True
                      Hat.PreludeBuiltinTypes.aTrue
                (T.R OtherPunctuation _ *== T.R OtherPunctuation _) p
                  = T.con0 T.mkNoSrcPos p Hat.PreludeBuiltinTypes.True
                      Hat.PreludeBuiltinTypes.aTrue
                (T.R MathSymbol _ *== T.R MathSymbol _) p
                  = T.con0 T.mkNoSrcPos p Hat.PreludeBuiltinTypes.True
                      Hat.PreludeBuiltinTypes.aTrue
                (T.R CurrencySymbol _ *== T.R CurrencySymbol _) p
                  = T.con0 T.mkNoSrcPos p Hat.PreludeBuiltinTypes.True
                      Hat.PreludeBuiltinTypes.aTrue
                (T.R ModifierSymbol _ *== T.R ModifierSymbol _) p
                  = T.con0 T.mkNoSrcPos p Hat.PreludeBuiltinTypes.True
                      Hat.PreludeBuiltinTypes.aTrue
                (T.R OtherSymbol _ *== T.R OtherSymbol _) p
                  = T.con0 T.mkNoSrcPos p Hat.PreludeBuiltinTypes.True
                      Hat.PreludeBuiltinTypes.aTrue
                (T.R Space _ *== T.R Space _) p
                  = T.con0 T.mkNoSrcPos p Hat.PreludeBuiltinTypes.True
                      Hat.PreludeBuiltinTypes.aTrue
                (T.R LineSeparator _ *== T.R LineSeparator _) p
                  = T.con0 T.mkNoSrcPos p Hat.PreludeBuiltinTypes.True
                      Hat.PreludeBuiltinTypes.aTrue
                (T.R ParagraphSeparator _ *==
                   T.R ParagraphSeparator _)
                  p
                  = T.con0 T.mkNoSrcPos p Hat.PreludeBuiltinTypes.True
                      Hat.PreludeBuiltinTypes.aTrue
                (T.R Control _ *== T.R Control _) p
                  = T.con0 T.mkNoSrcPos p Hat.PreludeBuiltinTypes.True
                      Hat.PreludeBuiltinTypes.aTrue
                (T.R Format _ *== T.R Format _) p
                  = T.con0 T.mkNoSrcPos p Hat.PreludeBuiltinTypes.True
                      Hat.PreludeBuiltinTypes.aTrue
                (T.R Surrogate _ *== T.R Surrogate _) p
                  = T.con0 T.mkNoSrcPos p Hat.PreludeBuiltinTypes.True
                      Hat.PreludeBuiltinTypes.aTrue
                (T.R PrivateUse _ *== T.R PrivateUse _) p
                  = T.con0 T.mkNoSrcPos p Hat.PreludeBuiltinTypes.True
                      Hat.PreludeBuiltinTypes.aTrue
                (T.R NotAssigned _ *== T.R NotAssigned _) p
                  = T.con0 T.mkNoSrcPos p Hat.PreludeBuiltinTypes.True
                      Hat.PreludeBuiltinTypes.aTrue
                (_ *== _) p
                  = T.con0 T.mkNoSrcPos p Hat.PreludeBuiltinTypes.False
                      Hat.PreludeBuiltinTypes.aFalse
 
instance Ord GeneralCategory where
        gcompare pcompare p
          = T.ufun2 c99v23v99v25compare pcompare p hcompare
          where hcompare fy1 fy2 p
                  = T.uap2 T.mkNoSrcPos p (gcompare T.mkNoSrcPos p)
                      (T.uwrapForward p (hlocalFromEnum fy1 p))
                      (T.uwrapForward p (hlocalFromEnum fy2 p))
                  where glocalFromEnum plocalFromEnum p
                          = T.ufun1 c99v23v99v25localFromEnum plocalFromEnum p
                              hlocalFromEnum
                        alocalFromEnum = c99v23v99v25localFromEnum
                        hlocalFromEnum (T.R UppercaseLetter _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (0))
                              :: T.R Hat.PreludeBuiltinTypes.Int
                        hlocalFromEnum (T.R LowercaseLetter _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (1))
                              :: T.R Hat.PreludeBuiltinTypes.Int
                        hlocalFromEnum (T.R TitlecaseLetter _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (2))
                              :: T.R Hat.PreludeBuiltinTypes.Int
                        hlocalFromEnum (T.R ModifierLetter _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (3))
                              :: T.R Hat.PreludeBuiltinTypes.Int
                        hlocalFromEnum (T.R OtherLetter _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (4))
                              :: T.R Hat.PreludeBuiltinTypes.Int
                        hlocalFromEnum (T.R NonSpacingMark _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (5))
                              :: T.R Hat.PreludeBuiltinTypes.Int
                        hlocalFromEnum (T.R SpacingCombiningMark _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (6))
                              :: T.R Hat.PreludeBuiltinTypes.Int
                        hlocalFromEnum (T.R EnclosingMark _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (7))
                              :: T.R Hat.PreludeBuiltinTypes.Int
                        hlocalFromEnum (T.R DecimalNumber _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (8))
                              :: T.R Hat.PreludeBuiltinTypes.Int
                        hlocalFromEnum (T.R LetterNumber _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (9))
                              :: T.R Hat.PreludeBuiltinTypes.Int
                        hlocalFromEnum (T.R OtherNumber _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (10))
                              :: T.R Hat.PreludeBuiltinTypes.Int
                        hlocalFromEnum (T.R ConnectorPunctuation _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (11))
                              :: T.R Hat.PreludeBuiltinTypes.Int
                        hlocalFromEnum (T.R DashPunctuation _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (12))
                              :: T.R Hat.PreludeBuiltinTypes.Int
                        hlocalFromEnum (T.R OpenPunctuation _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (13))
                              :: T.R Hat.PreludeBuiltinTypes.Int
                        hlocalFromEnum (T.R ClosePunctuation _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (14))
                              :: T.R Hat.PreludeBuiltinTypes.Int
                        hlocalFromEnum (T.R InitialQuote _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (15))
                              :: T.R Hat.PreludeBuiltinTypes.Int
                        hlocalFromEnum (T.R FinalQuote _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (16))
                              :: T.R Hat.PreludeBuiltinTypes.Int
                        hlocalFromEnum (T.R OtherPunctuation _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (17))
                              :: T.R Hat.PreludeBuiltinTypes.Int
                        hlocalFromEnum (T.R MathSymbol _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (18))
                              :: T.R Hat.PreludeBuiltinTypes.Int
                        hlocalFromEnum (T.R CurrencySymbol _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (19))
                              :: T.R Hat.PreludeBuiltinTypes.Int
                        hlocalFromEnum (T.R ModifierSymbol _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (20))
                              :: T.R Hat.PreludeBuiltinTypes.Int
                        hlocalFromEnum (T.R OtherSymbol _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (21))
                              :: T.R Hat.PreludeBuiltinTypes.Int
                        hlocalFromEnum (T.R Space _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (22))
                              :: T.R Hat.PreludeBuiltinTypes.Int
                        hlocalFromEnum (T.R LineSeparator _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (23))
                              :: T.R Hat.PreludeBuiltinTypes.Int
                        hlocalFromEnum (T.R ParagraphSeparator _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (24))
                              :: T.R Hat.PreludeBuiltinTypes.Int
                        hlocalFromEnum (T.R Control _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (25))
                              :: T.R Hat.PreludeBuiltinTypes.Int
                        hlocalFromEnum (T.R Format _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (26))
                              :: T.R Hat.PreludeBuiltinTypes.Int
                        hlocalFromEnum (T.R Surrogate _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (27))
                              :: T.R Hat.PreludeBuiltinTypes.Int
                        hlocalFromEnum (T.R PrivateUse _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (28))
                              :: T.R Hat.PreludeBuiltinTypes.Int
                        hlocalFromEnum (T.R NotAssigned _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (29))
                              :: T.R Hat.PreludeBuiltinTypes.Int
                        hlocalFromEnum _ p = T.fatal p
 
instance Enum GeneralCategory where
        gfromEnum pfromEnum p
          = T.ufun1 c99v28v99v31fromEnum pfromEnum p hfromEnum
          where hfromEnum (T.R UppercaseLetter _) p
                  = T.uap1 T.mkNoSrcPos p
                      (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                      (T.conInteger T.mkNoSrcPos p (0))
                hfromEnum (T.R LowercaseLetter _) p
                  = T.uap1 T.mkNoSrcPos p
                      (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                      (T.conInteger T.mkNoSrcPos p (1))
                hfromEnum (T.R TitlecaseLetter _) p
                  = T.uap1 T.mkNoSrcPos p
                      (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                      (T.conInteger T.mkNoSrcPos p (2))
                hfromEnum (T.R ModifierLetter _) p
                  = T.uap1 T.mkNoSrcPos p
                      (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                      (T.conInteger T.mkNoSrcPos p (3))
                hfromEnum (T.R OtherLetter _) p
                  = T.uap1 T.mkNoSrcPos p
                      (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                      (T.conInteger T.mkNoSrcPos p (4))
                hfromEnum (T.R NonSpacingMark _) p
                  = T.uap1 T.mkNoSrcPos p
                      (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                      (T.conInteger T.mkNoSrcPos p (5))
                hfromEnum (T.R SpacingCombiningMark _) p
                  = T.uap1 T.mkNoSrcPos p
                      (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                      (T.conInteger T.mkNoSrcPos p (6))
                hfromEnum (T.R EnclosingMark _) p
                  = T.uap1 T.mkNoSrcPos p
                      (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                      (T.conInteger T.mkNoSrcPos p (7))
                hfromEnum (T.R DecimalNumber _) p
                  = T.uap1 T.mkNoSrcPos p
                      (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                      (T.conInteger T.mkNoSrcPos p (8))
                hfromEnum (T.R LetterNumber _) p
                  = T.uap1 T.mkNoSrcPos p
                      (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                      (T.conInteger T.mkNoSrcPos p (9))
                hfromEnum (T.R OtherNumber _) p
                  = T.uap1 T.mkNoSrcPos p
                      (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                      (T.conInteger T.mkNoSrcPos p (10))
                hfromEnum (T.R ConnectorPunctuation _) p
                  = T.uap1 T.mkNoSrcPos p
                      (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                      (T.conInteger T.mkNoSrcPos p (11))
                hfromEnum (T.R DashPunctuation _) p
                  = T.uap1 T.mkNoSrcPos p
                      (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                      (T.conInteger T.mkNoSrcPos p (12))
                hfromEnum (T.R OpenPunctuation _) p
                  = T.uap1 T.mkNoSrcPos p
                      (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                      (T.conInteger T.mkNoSrcPos p (13))
                hfromEnum (T.R ClosePunctuation _) p
                  = T.uap1 T.mkNoSrcPos p
                      (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                      (T.conInteger T.mkNoSrcPos p (14))
                hfromEnum (T.R InitialQuote _) p
                  = T.uap1 T.mkNoSrcPos p
                      (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                      (T.conInteger T.mkNoSrcPos p (15))
                hfromEnum (T.R FinalQuote _) p
                  = T.uap1 T.mkNoSrcPos p
                      (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                      (T.conInteger T.mkNoSrcPos p (16))
                hfromEnum (T.R OtherPunctuation _) p
                  = T.uap1 T.mkNoSrcPos p
                      (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                      (T.conInteger T.mkNoSrcPos p (17))
                hfromEnum (T.R MathSymbol _) p
                  = T.uap1 T.mkNoSrcPos p
                      (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                      (T.conInteger T.mkNoSrcPos p (18))
                hfromEnum (T.R CurrencySymbol _) p
                  = T.uap1 T.mkNoSrcPos p
                      (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                      (T.conInteger T.mkNoSrcPos p (19))
                hfromEnum (T.R ModifierSymbol _) p
                  = T.uap1 T.mkNoSrcPos p
                      (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                      (T.conInteger T.mkNoSrcPos p (20))
                hfromEnum (T.R OtherSymbol _) p
                  = T.uap1 T.mkNoSrcPos p
                      (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                      (T.conInteger T.mkNoSrcPos p (21))
                hfromEnum (T.R Space _) p
                  = T.uap1 T.mkNoSrcPos p
                      (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                      (T.conInteger T.mkNoSrcPos p (22))
                hfromEnum (T.R LineSeparator _) p
                  = T.uap1 T.mkNoSrcPos p
                      (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                      (T.conInteger T.mkNoSrcPos p (23))
                hfromEnum (T.R ParagraphSeparator _) p
                  = T.uap1 T.mkNoSrcPos p
                      (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                      (T.conInteger T.mkNoSrcPos p (24))
                hfromEnum (T.R Control _) p
                  = T.uap1 T.mkNoSrcPos p
                      (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                      (T.conInteger T.mkNoSrcPos p (25))
                hfromEnum (T.R Format _) p
                  = T.uap1 T.mkNoSrcPos p
                      (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                      (T.conInteger T.mkNoSrcPos p (26))
                hfromEnum (T.R Surrogate _) p
                  = T.uap1 T.mkNoSrcPos p
                      (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                      (T.conInteger T.mkNoSrcPos p (27))
                hfromEnum (T.R PrivateUse _) p
                  = T.uap1 T.mkNoSrcPos p
                      (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                      (T.conInteger T.mkNoSrcPos p (28))
                hfromEnum (T.R NotAssigned _) p
                  = T.uap1 T.mkNoSrcPos p
                      (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                      (T.conInteger T.mkNoSrcPos p (29))
                hfromEnum _ p = T.fatal p
        gtoEnum ptoEnum p
          = T.ufun1 c99v28v99v31toEnum ptoEnum p htoEnum
          where htoEnum fv99v28v99v31n p
                  = T.ucguard
                      (T.uap2 T.mkNoSrcPos p
                         ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                         fv99v28v99v31n
                         (T.uap1 T.mkNoSrcPos p
                            (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                            (T.conInteger T.mkNoSrcPos p (0))))
                      (h99v28v99v31n p)
                      (y1toEnum fv99v28v99v31n p)
                  where h99v28v99v31n p
                          = T.con0 T.mkNoSrcPos p UppercaseLetter
                              aUppercaseLetter
                        h99v28v99v31n p = y1toEnum fv99v28v99v31n p
                htoEnum fv99v28v99v31n p = y1toEnum fv99v28v99v31n p
                y1toEnum fv99v28v99v31n p
                  = T.ucguard
                      (T.uap2 T.mkNoSrcPos p
                         ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                         fv99v28v99v31n
                         (T.uap1 T.mkNoSrcPos p
                            (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                            (T.conInteger T.mkNoSrcPos p (1))))
                      (h99v28v99v31n p)
                      (y2toEnum fv99v28v99v31n p)
                  where h99v28v99v31n p
                          = T.con0 T.mkNoSrcPos p LowercaseLetter
                              aLowercaseLetter
                        h99v28v99v31n p = y2toEnum fv99v28v99v31n p
                y1toEnum fv99v28v99v31n p = y2toEnum fv99v28v99v31n p
                y2toEnum fv99v28v99v31n p
                  = T.ucguard
                      (T.uap2 T.mkNoSrcPos p
                         ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                         fv99v28v99v31n
                         (T.uap1 T.mkNoSrcPos p
                            (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                            (T.conInteger T.mkNoSrcPos p (2))))
                      (h99v28v99v31n p)
                      (y3toEnum fv99v28v99v31n p)
                  where h99v28v99v31n p
                          = T.con0 T.mkNoSrcPos p TitlecaseLetter
                              aTitlecaseLetter
                        h99v28v99v31n p = y3toEnum fv99v28v99v31n p
                y2toEnum fv99v28v99v31n p = y3toEnum fv99v28v99v31n p
                y3toEnum fv99v28v99v31n p
                  = T.ucguard
                      (T.uap2 T.mkNoSrcPos p
                         ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                         fv99v28v99v31n
                         (T.uap1 T.mkNoSrcPos p
                            (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                            (T.conInteger T.mkNoSrcPos p (3))))
                      (h99v28v99v31n p)
                      (y4toEnum fv99v28v99v31n p)
                  where h99v28v99v31n p
                          = T.con0 T.mkNoSrcPos p ModifierLetter
                              aModifierLetter
                        h99v28v99v31n p = y4toEnum fv99v28v99v31n p
                y3toEnum fv99v28v99v31n p = y4toEnum fv99v28v99v31n p
                y4toEnum fv99v28v99v31n p
                  = T.ucguard
                      (T.uap2 T.mkNoSrcPos p
                         ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                         fv99v28v99v31n
                         (T.uap1 T.mkNoSrcPos p
                            (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                            (T.conInteger T.mkNoSrcPos p (4))))
                      (h99v28v99v31n p)
                      (y5toEnum fv99v28v99v31n p)
                  where h99v28v99v31n p
                          = T.con0 T.mkNoSrcPos p OtherLetter aOtherLetter
                        h99v28v99v31n p = y5toEnum fv99v28v99v31n p
                y4toEnum fv99v28v99v31n p = y5toEnum fv99v28v99v31n p
                y5toEnum fv99v28v99v31n p
                  = T.ucguard
                      (T.uap2 T.mkNoSrcPos p
                         ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                         fv99v28v99v31n
                         (T.uap1 T.mkNoSrcPos p
                            (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                            (T.conInteger T.mkNoSrcPos p (5))))
                      (h99v28v99v31n p)
                      (y6toEnum fv99v28v99v31n p)
                  where h99v28v99v31n p
                          = T.con0 T.mkNoSrcPos p NonSpacingMark
                              aNonSpacingMark
                        h99v28v99v31n p = y6toEnum fv99v28v99v31n p
                y5toEnum fv99v28v99v31n p = y6toEnum fv99v28v99v31n p
                y6toEnum fv99v28v99v31n p
                  = T.ucguard
                      (T.uap2 T.mkNoSrcPos p
                         ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                         fv99v28v99v31n
                         (T.uap1 T.mkNoSrcPos p
                            (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                            (T.conInteger T.mkNoSrcPos p (6))))
                      (h99v28v99v31n p)
                      (y7toEnum fv99v28v99v31n p)
                  where h99v28v99v31n p
                          = T.con0 T.mkNoSrcPos p SpacingCombiningMark
                              aSpacingCombiningMark
                        h99v28v99v31n p = y7toEnum fv99v28v99v31n p
                y6toEnum fv99v28v99v31n p = y7toEnum fv99v28v99v31n p
                y7toEnum fv99v28v99v31n p
                  = T.ucguard
                      (T.uap2 T.mkNoSrcPos p
                         ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                         fv99v28v99v31n
                         (T.uap1 T.mkNoSrcPos p
                            (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                            (T.conInteger T.mkNoSrcPos p (7))))
                      (h99v28v99v31n p)
                      (y8toEnum fv99v28v99v31n p)
                  where h99v28v99v31n p
                          = T.con0 T.mkNoSrcPos p EnclosingMark aEnclosingMark
                        h99v28v99v31n p = y8toEnum fv99v28v99v31n p
                y7toEnum fv99v28v99v31n p = y8toEnum fv99v28v99v31n p
                y8toEnum fv99v28v99v31n p
                  = T.ucguard
                      (T.uap2 T.mkNoSrcPos p
                         ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                         fv99v28v99v31n
                         (T.uap1 T.mkNoSrcPos p
                            (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                            (T.conInteger T.mkNoSrcPos p (8))))
                      (h99v28v99v31n p)
                      (y9toEnum fv99v28v99v31n p)
                  where h99v28v99v31n p
                          = T.con0 T.mkNoSrcPos p DecimalNumber aDecimalNumber
                        h99v28v99v31n p = y9toEnum fv99v28v99v31n p
                y8toEnum fv99v28v99v31n p = y9toEnum fv99v28v99v31n p
                y9toEnum fv99v28v99v31n p
                  = T.ucguard
                      (T.uap2 T.mkNoSrcPos p
                         ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                         fv99v28v99v31n
                         (T.uap1 T.mkNoSrcPos p
                            (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                            (T.conInteger T.mkNoSrcPos p (9))))
                      (h99v28v99v31n p)
                      (y10toEnum fv99v28v99v31n p)
                  where h99v28v99v31n p
                          = T.con0 T.mkNoSrcPos p LetterNumber aLetterNumber
                        h99v28v99v31n p = y10toEnum fv99v28v99v31n p
                y9toEnum fv99v28v99v31n p
                  = y10toEnum fv99v28v99v31n p
                y10toEnum fv99v28v99v31n p
                  = T.ucguard
                      (T.uap2 T.mkNoSrcPos p
                         ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                         fv99v28v99v31n
                         (T.uap1 T.mkNoSrcPos p
                            (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                            (T.conInteger T.mkNoSrcPos p (10))))
                      (h99v28v99v31n p)
                      (y11toEnum fv99v28v99v31n p)
                  where h99v28v99v31n p
                          = T.con0 T.mkNoSrcPos p OtherNumber aOtherNumber
                        h99v28v99v31n p = y11toEnum fv99v28v99v31n p
                y10toEnum fv99v28v99v31n p
                  = y11toEnum fv99v28v99v31n p
                y11toEnum fv99v28v99v31n p
                  = T.ucguard
                      (T.uap2 T.mkNoSrcPos p
                         ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                         fv99v28v99v31n
                         (T.uap1 T.mkNoSrcPos p
                            (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                            (T.conInteger T.mkNoSrcPos p (11))))
                      (h99v28v99v31n p)
                      (y12toEnum fv99v28v99v31n p)
                  where h99v28v99v31n p
                          = T.con0 T.mkNoSrcPos p ConnectorPunctuation
                              aConnectorPunctuation
                        h99v28v99v31n p = y12toEnum fv99v28v99v31n p
                y11toEnum fv99v28v99v31n p
                  = y12toEnum fv99v28v99v31n p
                y12toEnum fv99v28v99v31n p
                  = T.ucguard
                      (T.uap2 T.mkNoSrcPos p
                         ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                         fv99v28v99v31n
                         (T.uap1 T.mkNoSrcPos p
                            (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                            (T.conInteger T.mkNoSrcPos p (12))))
                      (h99v28v99v31n p)
                      (y13toEnum fv99v28v99v31n p)
                  where h99v28v99v31n p
                          = T.con0 T.mkNoSrcPos p DashPunctuation
                              aDashPunctuation
                        h99v28v99v31n p = y13toEnum fv99v28v99v31n p
                y12toEnum fv99v28v99v31n p
                  = y13toEnum fv99v28v99v31n p
                y13toEnum fv99v28v99v31n p
                  = T.ucguard
                      (T.uap2 T.mkNoSrcPos p
                         ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                         fv99v28v99v31n
                         (T.uap1 T.mkNoSrcPos p
                            (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                            (T.conInteger T.mkNoSrcPos p (13))))
                      (h99v28v99v31n p)
                      (y14toEnum fv99v28v99v31n p)
                  where h99v28v99v31n p
                          = T.con0 T.mkNoSrcPos p OpenPunctuation
                              aOpenPunctuation
                        h99v28v99v31n p = y14toEnum fv99v28v99v31n p
                y13toEnum fv99v28v99v31n p
                  = y14toEnum fv99v28v99v31n p
                y14toEnum fv99v28v99v31n p
                  = T.ucguard
                      (T.uap2 T.mkNoSrcPos p
                         ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                         fv99v28v99v31n
                         (T.uap1 T.mkNoSrcPos p
                            (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                            (T.conInteger T.mkNoSrcPos p (14))))
                      (h99v28v99v31n p)
                      (y15toEnum fv99v28v99v31n p)
                  where h99v28v99v31n p
                          = T.con0 T.mkNoSrcPos p ClosePunctuation
                              aClosePunctuation
                        h99v28v99v31n p = y15toEnum fv99v28v99v31n p
                y14toEnum fv99v28v99v31n p
                  = y15toEnum fv99v28v99v31n p
                y15toEnum fv99v28v99v31n p
                  = T.ucguard
                      (T.uap2 T.mkNoSrcPos p
                         ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                         fv99v28v99v31n
                         (T.uap1 T.mkNoSrcPos p
                            (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                            (T.conInteger T.mkNoSrcPos p (15))))
                      (h99v28v99v31n p)
                      (y16toEnum fv99v28v99v31n p)
                  where h99v28v99v31n p
                          = T.con0 T.mkNoSrcPos p InitialQuote aInitialQuote
                        h99v28v99v31n p = y16toEnum fv99v28v99v31n p
                y15toEnum fv99v28v99v31n p
                  = y16toEnum fv99v28v99v31n p
                y16toEnum fv99v28v99v31n p
                  = T.ucguard
                      (T.uap2 T.mkNoSrcPos p
                         ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                         fv99v28v99v31n
                         (T.uap1 T.mkNoSrcPos p
                            (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                            (T.conInteger T.mkNoSrcPos p (16))))
                      (h99v28v99v31n p)
                      (y17toEnum fv99v28v99v31n p)
                  where h99v28v99v31n p
                          = T.con0 T.mkNoSrcPos p FinalQuote aFinalQuote
                        h99v28v99v31n p = y17toEnum fv99v28v99v31n p
                y16toEnum fv99v28v99v31n p
                  = y17toEnum fv99v28v99v31n p
                y17toEnum fv99v28v99v31n p
                  = T.ucguard
                      (T.uap2 T.mkNoSrcPos p
                         ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                         fv99v28v99v31n
                         (T.uap1 T.mkNoSrcPos p
                            (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                            (T.conInteger T.mkNoSrcPos p (17))))
                      (h99v28v99v31n p)
                      (y18toEnum fv99v28v99v31n p)
                  where h99v28v99v31n p
                          = T.con0 T.mkNoSrcPos p OtherPunctuation
                              aOtherPunctuation
                        h99v28v99v31n p = y18toEnum fv99v28v99v31n p
                y17toEnum fv99v28v99v31n p
                  = y18toEnum fv99v28v99v31n p
                y18toEnum fv99v28v99v31n p
                  = T.ucguard
                      (T.uap2 T.mkNoSrcPos p
                         ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                         fv99v28v99v31n
                         (T.uap1 T.mkNoSrcPos p
                            (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                            (T.conInteger T.mkNoSrcPos p (18))))
                      (h99v28v99v31n p)
                      (y19toEnum fv99v28v99v31n p)
                  where h99v28v99v31n p
                          = T.con0 T.mkNoSrcPos p MathSymbol aMathSymbol
                        h99v28v99v31n p = y19toEnum fv99v28v99v31n p
                y18toEnum fv99v28v99v31n p
                  = y19toEnum fv99v28v99v31n p
                y19toEnum fv99v28v99v31n p
                  = T.ucguard
                      (T.uap2 T.mkNoSrcPos p
                         ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                         fv99v28v99v31n
                         (T.uap1 T.mkNoSrcPos p
                            (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                            (T.conInteger T.mkNoSrcPos p (19))))
                      (h99v28v99v31n p)
                      (y20toEnum fv99v28v99v31n p)
                  where h99v28v99v31n p
                          = T.con0 T.mkNoSrcPos p CurrencySymbol
                              aCurrencySymbol
                        h99v28v99v31n p = y20toEnum fv99v28v99v31n p
                y19toEnum fv99v28v99v31n p
                  = y20toEnum fv99v28v99v31n p
                y20toEnum fv99v28v99v31n p
                  = T.ucguard
                      (T.uap2 T.mkNoSrcPos p
                         ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                         fv99v28v99v31n
                         (T.uap1 T.mkNoSrcPos p
                            (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                            (T.conInteger T.mkNoSrcPos p (20))))
                      (h99v28v99v31n p)
                      (y21toEnum fv99v28v99v31n p)
                  where h99v28v99v31n p
                          = T.con0 T.mkNoSrcPos p ModifierSymbol
                              aModifierSymbol
                        h99v28v99v31n p = y21toEnum fv99v28v99v31n p
                y20toEnum fv99v28v99v31n p
                  = y21toEnum fv99v28v99v31n p
                y21toEnum fv99v28v99v31n p
                  = T.ucguard
                      (T.uap2 T.mkNoSrcPos p
                         ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                         fv99v28v99v31n
                         (T.uap1 T.mkNoSrcPos p
                            (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                            (T.conInteger T.mkNoSrcPos p (21))))
                      (h99v28v99v31n p)
                      (y22toEnum fv99v28v99v31n p)
                  where h99v28v99v31n p
                          = T.con0 T.mkNoSrcPos p OtherSymbol aOtherSymbol
                        h99v28v99v31n p = y22toEnum fv99v28v99v31n p
                y21toEnum fv99v28v99v31n p
                  = y22toEnum fv99v28v99v31n p
                y22toEnum fv99v28v99v31n p
                  = T.ucguard
                      (T.uap2 T.mkNoSrcPos p
                         ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                         fv99v28v99v31n
                         (T.uap1 T.mkNoSrcPos p
                            (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                            (T.conInteger T.mkNoSrcPos p (22))))
                      (h99v28v99v31n p)
                      (y23toEnum fv99v28v99v31n p)
                  where h99v28v99v31n p
                          = T.con0 T.mkNoSrcPos p Space aSpace
                        h99v28v99v31n p = y23toEnum fv99v28v99v31n p
                y22toEnum fv99v28v99v31n p
                  = y23toEnum fv99v28v99v31n p
                y23toEnum fv99v28v99v31n p
                  = T.ucguard
                      (T.uap2 T.mkNoSrcPos p
                         ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                         fv99v28v99v31n
                         (T.uap1 T.mkNoSrcPos p
                            (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                            (T.conInteger T.mkNoSrcPos p (23))))
                      (h99v28v99v31n p)
                      (y24toEnum fv99v28v99v31n p)
                  where h99v28v99v31n p
                          = T.con0 T.mkNoSrcPos p LineSeparator aLineSeparator
                        h99v28v99v31n p = y24toEnum fv99v28v99v31n p
                y23toEnum fv99v28v99v31n p
                  = y24toEnum fv99v28v99v31n p
                y24toEnum fv99v28v99v31n p
                  = T.ucguard
                      (T.uap2 T.mkNoSrcPos p
                         ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                         fv99v28v99v31n
                         (T.uap1 T.mkNoSrcPos p
                            (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                            (T.conInteger T.mkNoSrcPos p (24))))
                      (h99v28v99v31n p)
                      (y25toEnum fv99v28v99v31n p)
                  where h99v28v99v31n p
                          = T.con0 T.mkNoSrcPos p ParagraphSeparator
                              aParagraphSeparator
                        h99v28v99v31n p = y25toEnum fv99v28v99v31n p
                y24toEnum fv99v28v99v31n p
                  = y25toEnum fv99v28v99v31n p
                y25toEnum fv99v28v99v31n p
                  = T.ucguard
                      (T.uap2 T.mkNoSrcPos p
                         ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                         fv99v28v99v31n
                         (T.uap1 T.mkNoSrcPos p
                            (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                            (T.conInteger T.mkNoSrcPos p (25))))
                      (h99v28v99v31n p)
                      (y26toEnum fv99v28v99v31n p)
                  where h99v28v99v31n p
                          = T.con0 T.mkNoSrcPos p Control aControl
                        h99v28v99v31n p = y26toEnum fv99v28v99v31n p
                y25toEnum fv99v28v99v31n p
                  = y26toEnum fv99v28v99v31n p
                y26toEnum fv99v28v99v31n p
                  = T.ucguard
                      (T.uap2 T.mkNoSrcPos p
                         ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                         fv99v28v99v31n
                         (T.uap1 T.mkNoSrcPos p
                            (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                            (T.conInteger T.mkNoSrcPos p (26))))
                      (h99v28v99v31n p)
                      (y27toEnum fv99v28v99v31n p)
                  where h99v28v99v31n p
                          = T.con0 T.mkNoSrcPos p Format aFormat
                        h99v28v99v31n p = y27toEnum fv99v28v99v31n p
                y26toEnum fv99v28v99v31n p
                  = y27toEnum fv99v28v99v31n p
                y27toEnum fv99v28v99v31n p
                  = T.ucguard
                      (T.uap2 T.mkNoSrcPos p
                         ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                         fv99v28v99v31n
                         (T.uap1 T.mkNoSrcPos p
                            (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                            (T.conInteger T.mkNoSrcPos p (27))))
                      (h99v28v99v31n p)
                      (y28toEnum fv99v28v99v31n p)
                  where h99v28v99v31n p
                          = T.con0 T.mkNoSrcPos p Surrogate aSurrogate
                        h99v28v99v31n p = y28toEnum fv99v28v99v31n p
                y27toEnum fv99v28v99v31n p
                  = y28toEnum fv99v28v99v31n p
                y28toEnum fv99v28v99v31n p
                  = T.ucguard
                      (T.uap2 T.mkNoSrcPos p
                         ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                         fv99v28v99v31n
                         (T.uap1 T.mkNoSrcPos p
                            (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                            (T.conInteger T.mkNoSrcPos p (28))))
                      (h99v28v99v31n p)
                      (y29toEnum fv99v28v99v31n p)
                  where h99v28v99v31n p
                          = T.con0 T.mkNoSrcPos p PrivateUse aPrivateUse
                        h99v28v99v31n p = y29toEnum fv99v28v99v31n p
                y28toEnum fv99v28v99v31n p
                  = y29toEnum fv99v28v99v31n p
                y29toEnum fv99v28v99v31n p
                  = T.ucguard
                      (T.uap2 T.mkNoSrcPos p
                         ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                         fv99v28v99v31n
                         (T.uap1 T.mkNoSrcPos p
                            (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                            (T.conInteger T.mkNoSrcPos p (29))))
                      (h99v28v99v31n p)
                      (y30toEnum fv99v28v99v31n p)
                  where h99v28v99v31n p
                          = T.con0 T.mkNoSrcPos p NotAssigned aNotAssigned
                        h99v28v99v31n p = y30toEnum fv99v28v99v31n p
                y29toEnum fv99v28v99v31n p
                  = y30toEnum fv99v28v99v31n p
                y30toEnum _ p
                  = T.uwrapForward p
                      (herror
                         (T.fromLitString T.mkNoSrcPos p
                            "toEnum: argument out of bounds")
                         p)
        genumFrom penumFrom p
          = T.ufun1 c99v28v99v31enumFrom penumFrom p henumFrom
          where henumFrom fy1 p
                  = T.uap2 T.mkNoSrcPos p (genumFromTo T.mkNoSrcPos p)
                      fy1
                      (T.con0 T.mkNoSrcPos p NotAssigned aNotAssigned)
        genumFromThen penumFromThen p
          = T.ufun2 c99v28v99v31enumFromThen penumFromThen p
              henumFromThen
          where henumFromThen fy1 fy2 p
                  = T.uap3 T.mkNoSrcPos p
                      (genumFromThenTo T.mkNoSrcPos p)
                      fy1
                      fy2
                      (T.ucif p
                         (T.uap2 T.mkNoSrcPos p ((!>=) T.mkNoSrcPos p)
                            (T.uap1 T.mkNoSrcPos p (gfromEnum T.mkNoSrcPos p)
                               fy2)
                            (T.uap1 T.mkNoSrcPos p (gfromEnum T.mkNoSrcPos p)
                               fy1))
                         (T.con0 T.mkNoSrcPos p NotAssigned aNotAssigned)
                         (T.con0 T.mkNoSrcPos p UppercaseLetter
                            aUppercaseLetter))
 
instance Read GeneralCategory where
        greadsPrec preadsPrec p
          = T.ufun1 c99v34v99v37readsPrec preadsPrec p
              hreadsPrec
          where hreadsPrec fy1 p
                  = T.uap2 T.mkNoSrcPos p
                      (Hat.PreludeBasic.galt T.mkNoSrcPos p)
                      (T.uwrapForward p
                         (hreadParen
                            (T.con0 T.mkNoSrcPos p Hat.PreludeBuiltinTypes.False
                               Hat.PreludeBuiltinTypes.aFalse)
                            (T.uwrapForward p
                               (Hat.PreludeBasic.hthenLex
                                  (T.uap1 T.mkNoSrcPos p
                                     (Hat.PreludeBasic.gyield T.mkNoSrcPos p)
                                     (T.con0 T.mkNoSrcPos p UppercaseLetter
                                        aUppercaseLetter))
                                  (T.fromLitString T.mkNoSrcPos p
                                     "UppercaseLetter")
                                  p))
                            p))
                      (T.uap2 T.mkNoSrcPos p
                         (Hat.PreludeBasic.galt T.mkNoSrcPos p)
                         (T.uwrapForward p
                            (hreadParen
                               (T.con0 T.mkNoSrcPos p
                                  Hat.PreludeBuiltinTypes.False
                                  Hat.PreludeBuiltinTypes.aFalse)
                               (T.uwrapForward p
                                  (Hat.PreludeBasic.hthenLex
                                     (T.uap1 T.mkNoSrcPos p
                                        (Hat.PreludeBasic.gyield T.mkNoSrcPos p)
                                        (T.con0 T.mkNoSrcPos p LowercaseLetter
                                           aLowercaseLetter))
                                     (T.fromLitString T.mkNoSrcPos p
                                        "LowercaseLetter")
                                     p))
                               p))
                         (T.uap2 T.mkNoSrcPos p
                            (Hat.PreludeBasic.galt T.mkNoSrcPos p)
                            (T.uwrapForward p
                               (hreadParen
                                  (T.con0 T.mkNoSrcPos p
                                     Hat.PreludeBuiltinTypes.False
                                     Hat.PreludeBuiltinTypes.aFalse)
                                  (T.uwrapForward p
                                     (Hat.PreludeBasic.hthenLex
                                        (T.uap1 T.mkNoSrcPos p
                                           (Hat.PreludeBasic.gyield T.mkNoSrcPos
                                              p)
                                           (T.con0 T.mkNoSrcPos p
                                              TitlecaseLetter
                                              aTitlecaseLetter))
                                        (T.fromLitString T.mkNoSrcPos p
                                           "TitlecaseLetter")
                                        p))
                                  p))
                            (T.uap2 T.mkNoSrcPos p
                               (Hat.PreludeBasic.galt T.mkNoSrcPos p)
                               (T.uwrapForward p
                                  (hreadParen
                                     (T.con0 T.mkNoSrcPos p
                                        Hat.PreludeBuiltinTypes.False
                                        Hat.PreludeBuiltinTypes.aFalse)
                                     (T.uwrapForward p
                                        (Hat.PreludeBasic.hthenLex
                                           (T.uap1 T.mkNoSrcPos p
                                              (Hat.PreludeBasic.gyield
                                                 T.mkNoSrcPos
                                                 p)
                                              (T.con0 T.mkNoSrcPos p
                                                 ModifierLetter
                                                 aModifierLetter))
                                           (T.fromLitString T.mkNoSrcPos p
                                              "ModifierLetter")
                                           p))
                                     p))
                               (T.uap2 T.mkNoSrcPos p
                                  (Hat.PreludeBasic.galt T.mkNoSrcPos p)
                                  (T.uwrapForward p
                                     (hreadParen
                                        (T.con0 T.mkNoSrcPos p
                                           Hat.PreludeBuiltinTypes.False
                                           Hat.PreludeBuiltinTypes.aFalse)
                                        (T.uwrapForward p
                                           (Hat.PreludeBasic.hthenLex
                                              (T.uap1 T.mkNoSrcPos p
                                                 (Hat.PreludeBasic.gyield
                                                    T.mkNoSrcPos
                                                    p)
                                                 (T.con0 T.mkNoSrcPos p
                                                    OtherLetter
                                                    aOtherLetter))
                                              (T.fromLitString T.mkNoSrcPos p
                                                 "OtherLetter")
                                              p))
                                        p))
                                  (T.uap2 T.mkNoSrcPos p
                                     (Hat.PreludeBasic.galt T.mkNoSrcPos p)
                                     (T.uwrapForward p
                                        (hreadParen
                                           (T.con0 T.mkNoSrcPos p
                                              Hat.PreludeBuiltinTypes.False
                                              Hat.PreludeBuiltinTypes.aFalse)
                                           (T.uwrapForward p
                                              (Hat.PreludeBasic.hthenLex
                                                 (T.uap1 T.mkNoSrcPos p
                                                    (Hat.PreludeBasic.gyield
                                                       T.mkNoSrcPos
                                                       p)
                                                    (T.con0 T.mkNoSrcPos p
                                                       NonSpacingMark
                                                       aNonSpacingMark))
                                                 (T.fromLitString T.mkNoSrcPos p
                                                    "NonSpacingMark")
                                                 p))
                                           p))
                                     (T.uap2 T.mkNoSrcPos p
                                        (Hat.PreludeBasic.galt T.mkNoSrcPos p)
                                        (T.uwrapForward p
                                           (hreadParen
                                              (T.con0 T.mkNoSrcPos p
                                                 Hat.PreludeBuiltinTypes.False
                                                 Hat.PreludeBuiltinTypes.aFalse)
                                              (T.uwrapForward p
                                                 (Hat.PreludeBasic.hthenLex
                                                    (T.uap1 T.mkNoSrcPos p
                                                       (Hat.PreludeBasic.gyield
                                                          T.mkNoSrcPos
                                                          p)
                                                       (T.con0 T.mkNoSrcPos p
                                                          SpacingCombiningMark
                                                          aSpacingCombiningMark))
                                                    (T.fromLitString
                                                       T.mkNoSrcPos
                                                       p
                                                       "SpacingCombiningMark")
                                                    p))
                                              p))
                                        (T.uap2 T.mkNoSrcPos p
                                           (Hat.PreludeBasic.galt T.mkNoSrcPos
                                              p)
                                           (T.uwrapForward p
                                              (hreadParen
                                                 (T.con0 T.mkNoSrcPos p
                                                    Hat.PreludeBuiltinTypes.False
                                                    Hat.PreludeBuiltinTypes.aFalse)
                                                 (T.uwrapForward p
                                                    (Hat.PreludeBasic.hthenLex
                                                       (T.uap1 T.mkNoSrcPos p
                                                          (Hat.PreludeBasic.gyield
                                                             T.mkNoSrcPos
                                                             p)
                                                          (T.con0 T.mkNoSrcPos p
                                                             EnclosingMark
                                                             aEnclosingMark))
                                                       (T.fromLitString
                                                          T.mkNoSrcPos
                                                          p
                                                          "EnclosingMark")
                                                       p))
                                                 p))
                                           (T.uap2 T.mkNoSrcPos p
                                              (Hat.PreludeBasic.galt
                                                 T.mkNoSrcPos
                                                 p)
                                              (T.uwrapForward p
                                                 (hreadParen
                                                    (T.con0 T.mkNoSrcPos p
                                                       Hat.PreludeBuiltinTypes.False
                                                       Hat.PreludeBuiltinTypes.aFalse)
                                                    (T.uwrapForward p
                                                       (Hat.PreludeBasic.hthenLex
                                                          (T.uap1 T.mkNoSrcPos p
                                                             (Hat.PreludeBasic.gyield
                                                                T.mkNoSrcPos
                                                                p)
                                                             (T.con0
                                                                T.mkNoSrcPos
                                                                p
                                                                DecimalNumber
                                                                aDecimalNumber))
                                                          (T.fromLitString
                                                             T.mkNoSrcPos
                                                             p
                                                             "DecimalNumber")
                                                          p))
                                                    p))
                                              (T.uap2 T.mkNoSrcPos p
                                                 (Hat.PreludeBasic.galt
                                                    T.mkNoSrcPos
                                                    p)
                                                 (T.uwrapForward p
                                                    (hreadParen
                                                       (T.con0 T.mkNoSrcPos p
                                                          Hat.PreludeBuiltinTypes.False
                                                          Hat.PreludeBuiltinTypes.aFalse)
                                                       (T.uwrapForward p
                                                          (Hat.PreludeBasic.hthenLex
                                                             (T.uap1
                                                                T.mkNoSrcPos
                                                                p
                                                                (Hat.PreludeBasic.gyield
                                                                   T.mkNoSrcPos
                                                                   p)
                                                                (T.con0
                                                                   T.mkNoSrcPos
                                                                   p
                                                                   LetterNumber
                                                                   aLetterNumber))
                                                             (T.fromLitString
                                                                T.mkNoSrcPos
                                                                p
                                                                "LetterNumber")
                                                             p))
                                                       p))
                                                 (T.uap2 T.mkNoSrcPos p
                                                    (Hat.PreludeBasic.galt
                                                       T.mkNoSrcPos
                                                       p)
                                                    (T.uwrapForward p
                                                       (hreadParen
                                                          (T.con0 T.mkNoSrcPos p
                                                             Hat.PreludeBuiltinTypes.False
                                                             Hat.PreludeBuiltinTypes.aFalse)
                                                          (T.uwrapForward p
                                                             (Hat.PreludeBasic.hthenLex
                                                                (T.uap1
                                                                   T.mkNoSrcPos
                                                                   p
                                                                   (Hat.PreludeBasic.gyield
                                                                      T.mkNoSrcPos
                                                                      p)
                                                                   (T.con0
                                                                      T.mkNoSrcPos
                                                                      p
                                                                      OtherNumber
                                                                      aOtherNumber))
                                                                (T.fromLitString
                                                                   T.mkNoSrcPos
                                                                   p
                                                                   "OtherNumber")
                                                                p))
                                                          p))
                                                    (T.uap2 T.mkNoSrcPos p
                                                       (Hat.PreludeBasic.galt
                                                          T.mkNoSrcPos
                                                          p)
                                                       (T.uwrapForward p
                                                          (hreadParen
                                                             (T.con0
                                                                T.mkNoSrcPos
                                                                p
                                                                Hat.PreludeBuiltinTypes.False
                                                                Hat.PreludeBuiltinTypes.aFalse)
                                                             (T.uwrapForward p
                                                                (Hat.PreludeBasic.hthenLex
                                                                   (T.uap1
                                                                      T.mkNoSrcPos
                                                                      p
                                                                      (Hat.PreludeBasic.gyield
                                                                         T.mkNoSrcPos
                                                                         p)
                                                                      (T.con0
                                                                         T.mkNoSrcPos
                                                                         p
                                                                         ConnectorPunctuation
                                                                         aConnectorPunctuation))
                                                                   (T.fromLitString
                                                                      T.mkNoSrcPos
                                                                      p
                                                                      "ConnectorPunctuation")
                                                                   p))
                                                             p))
                                                       (T.uap2 T.mkNoSrcPos p
                                                          (Hat.PreludeBasic.galt
                                                             T.mkNoSrcPos
                                                             p)
                                                          (T.uwrapForward p
                                                             (hreadParen
                                                                (T.con0
                                                                   T.mkNoSrcPos
                                                                   p
                                                                   Hat.PreludeBuiltinTypes.False
                                                                   Hat.PreludeBuiltinTypes.aFalse)
                                                                (T.uwrapForward
                                                                   p
                                                                   (Hat.PreludeBasic.hthenLex
                                                                      (T.uap1
                                                                         T.mkNoSrcPos
                                                                         p
                                                                         (Hat.PreludeBasic.gyield
                                                                            T.mkNoSrcPos
                                                                            p)
                                                                         (T.con0
                                                                            T.mkNoSrcPos
                                                                            p
                                                                            DashPunctuation
                                                                            aDashPunctuation))
                                                                      (T.fromLitString
                                                                         T.mkNoSrcPos
                                                                         p
                                                                         "DashPunctuation")
                                                                      p))
                                                                p))
                                                          (T.uap2 T.mkNoSrcPos p
                                                             (Hat.PreludeBasic.galt
                                                                T.mkNoSrcPos
                                                                p)
                                                             (T.uwrapForward p
                                                                (hreadParen
                                                                   (T.con0
                                                                      T.mkNoSrcPos
                                                                      p
                                                                      Hat.PreludeBuiltinTypes.False
                                                                      Hat.PreludeBuiltinTypes.aFalse)
                                                                   (T.uwrapForward
                                                                      p
                                                                      (Hat.PreludeBasic.hthenLex
                                                                         (T.uap1
                                                                            T.mkNoSrcPos
                                                                            p
                                                                            (Hat.PreludeBasic.gyield
                                                                               T.mkNoSrcPos
                                                                               p)
                                                                            (T.con0
                                                                               T.mkNoSrcPos
                                                                               p
                                                                               OpenPunctuation
                                                                               aOpenPunctuation))
                                                                         (T.fromLitString
                                                                            T.mkNoSrcPos
                                                                            p
                                                                            "OpenPunctuation")
                                                                         p))
                                                                   p))
                                                             (T.uap2
                                                                T.mkNoSrcPos
                                                                p
                                                                (Hat.PreludeBasic.galt
                                                                   T.mkNoSrcPos
                                                                   p)
                                                                (T.uwrapForward
                                                                   p
                                                                   (hreadParen
                                                                      (T.con0
                                                                         T.mkNoSrcPos
                                                                         p
                                                                         Hat.PreludeBuiltinTypes.False
                                                                         Hat.PreludeBuiltinTypes.aFalse)
                                                                      (T.uwrapForward
                                                                         p
                                                                         (Hat.PreludeBasic.hthenLex
                                                                            (T.uap1
                                                                               T.mkNoSrcPos
                                                                               p
                                                                               (Hat.PreludeBasic.gyield
                                                                                  T.mkNoSrcPos
                                                                                  p)
                                                                               (T.con0
                                                                                  T.mkNoSrcPos
                                                                                  p
                                                                                  ClosePunctuation
                                                                                  aClosePunctuation))
                                                                            (T.fromLitString
                                                                               T.mkNoSrcPos
                                                                               p
                                                                               "ClosePunctuation")
                                                                            p))
                                                                      p))
                                                                (T.uap2
                                                                   T.mkNoSrcPos
                                                                   p
                                                                   (Hat.PreludeBasic.galt
                                                                      T.mkNoSrcPos
                                                                      p)
                                                                   (T.uwrapForward
                                                                      p
                                                                      (hreadParen
                                                                         (T.con0
                                                                            T.mkNoSrcPos
                                                                            p
                                                                            Hat.PreludeBuiltinTypes.False
                                                                            Hat.PreludeBuiltinTypes.aFalse)
                                                                         (T.uwrapForward
                                                                            p
                                                                            (Hat.PreludeBasic.hthenLex
                                                                               (T.uap1
                                                                                  T.mkNoSrcPos
                                                                                  p
                                                                                  (Hat.PreludeBasic.gyield
                                                                                     T.mkNoSrcPos
                                                                                     p)
                                                                                  (T.con0
                                                                                     T.mkNoSrcPos
                                                                                     p
                                                                                     InitialQuote
                                                                                     aInitialQuote))
                                                                               (T.fromLitString
                                                                                  T.mkNoSrcPos
                                                                                  p
                                                                                  "InitialQuote")
                                                                               p))
                                                                         p))
                                                                   (T.uap2
                                                                      T.mkNoSrcPos
                                                                      p
                                                                      (Hat.PreludeBasic.galt
                                                                         T.mkNoSrcPos
                                                                         p)
                                                                      (T.uwrapForward
                                                                         p
                                                                         (hreadParen
                                                                            (T.con0
                                                                               T.mkNoSrcPos
                                                                               p
                                                                               Hat.PreludeBuiltinTypes.False
                                                                               Hat.PreludeBuiltinTypes.aFalse)
                                                                            (T.uwrapForward
                                                                               p
                                                                               (Hat.PreludeBasic.hthenLex
                                                                                  (T.uap1
                                                                                     T.mkNoSrcPos
                                                                                     p
                                                                                     (Hat.PreludeBasic.gyield
                                                                                        T.mkNoSrcPos
                                                                                        p)
                                                                                     (T.con0
                                                                                        T.mkNoSrcPos
                                                                                        p
                                                                                        FinalQuote
                                                                                        aFinalQuote))
                                                                                  (T.fromLitString
                                                                                     T.mkNoSrcPos
                                                                                     p
                                                                                     "FinalQuote")
                                                                                  p))
                                                                            p))
                                                                      (T.uap2
                                                                         T.mkNoSrcPos
                                                                         p
                                                                         (Hat.PreludeBasic.galt
                                                                            T.mkNoSrcPos
                                                                            p)
                                                                         (T.uwrapForward
                                                                            p
                                                                            (hreadParen
                                                                               (T.con0
                                                                                  T.mkNoSrcPos
                                                                                  p
                                                                                  Hat.PreludeBuiltinTypes.False
                                                                                  Hat.PreludeBuiltinTypes.aFalse)
                                                                               (T.uwrapForward
                                                                                  p
                                                                                  (Hat.PreludeBasic.hthenLex
                                                                                     (T.uap1
                                                                                        T.mkNoSrcPos
                                                                                        p
                                                                                        (Hat.PreludeBasic.gyield
                                                                                           T.mkNoSrcPos
                                                                                           p)
                                                                                        (T.con0
                                                                                           T.mkNoSrcPos
                                                                                           p
                                                                                           OtherPunctuation
                                                                                           aOtherPunctuation))
                                                                                     (T.fromLitString
                                                                                        T.mkNoSrcPos
                                                                                        p
                                                                                        "OtherPunctuation")
                                                                                     p))
                                                                               p))
                                                                         (T.uap2
                                                                            T.mkNoSrcPos
                                                                            p
                                                                            (Hat.PreludeBasic.galt
                                                                               T.mkNoSrcPos
                                                                               p)
                                                                            (T.uwrapForward
                                                                               p
                                                                               (hreadParen
                                                                                  (T.con0
                                                                                     T.mkNoSrcPos
                                                                                     p
                                                                                     Hat.PreludeBuiltinTypes.False
                                                                                     Hat.PreludeBuiltinTypes.aFalse)
                                                                                  (T.uwrapForward
                                                                                     p
                                                                                     (Hat.PreludeBasic.hthenLex
                                                                                        (T.uap1
                                                                                           T.mkNoSrcPos
                                                                                           p
                                                                                           (Hat.PreludeBasic.gyield
                                                                                              T.mkNoSrcPos
                                                                                              p)
                                                                                           (T.con0
                                                                                              T.mkNoSrcPos
                                                                                              p
                                                                                              MathSymbol
                                                                                              aMathSymbol))
                                                                                        (T.fromLitString
                                                                                           T.mkNoSrcPos
                                                                                           p
                                                                                           "MathSymbol")
                                                                                        p))
                                                                                  p))
                                                                            (T.uap2
                                                                               T.mkNoSrcPos
                                                                               p
                                                                               (Hat.PreludeBasic.galt
                                                                                  T.mkNoSrcPos
                                                                                  p)
                                                                               (T.uwrapForward
                                                                                  p
                                                                                  (hreadParen
                                                                                     (T.con0
                                                                                        T.mkNoSrcPos
                                                                                        p
                                                                                        Hat.PreludeBuiltinTypes.False
                                                                                        Hat.PreludeBuiltinTypes.aFalse)
                                                                                     (T.uwrapForward
                                                                                        p
                                                                                        (Hat.PreludeBasic.hthenLex
                                                                                           (T.uap1
                                                                                              T.mkNoSrcPos
                                                                                              p
                                                                                              (Hat.PreludeBasic.gyield
                                                                                                 T.mkNoSrcPos
                                                                                                 p)
                                                                                              (T.con0
                                                                                                 T.mkNoSrcPos
                                                                                                 p
                                                                                                 CurrencySymbol
                                                                                                 aCurrencySymbol))
                                                                                           (T.fromLitString
                                                                                              T.mkNoSrcPos
                                                                                              p
                                                                                              "CurrencySymbol")
                                                                                           p))
                                                                                     p))
                                                                               (T.uap2
                                                                                  T.mkNoSrcPos
                                                                                  p
                                                                                  (Hat.PreludeBasic.galt
                                                                                     T.mkNoSrcPos
                                                                                     p)
                                                                                  (T.uwrapForward
                                                                                     p
                                                                                     (hreadParen
                                                                                        (T.con0
                                                                                           T.mkNoSrcPos
                                                                                           p
                                                                                           Hat.PreludeBuiltinTypes.False
                                                                                           Hat.PreludeBuiltinTypes.aFalse)
                                                                                        (T.uwrapForward
                                                                                           p
                                                                                           (Hat.PreludeBasic.hthenLex
                                                                                              (T.uap1
                                                                                                 T.mkNoSrcPos
                                                                                                 p
                                                                                                 (Hat.PreludeBasic.gyield
                                                                                                    T.mkNoSrcPos
                                                                                                    p)
                                                                                                 (T.con0
                                                                                                    T.mkNoSrcPos
                                                                                                    p
                                                                                                    ModifierSymbol
                                                                                                    aModifierSymbol))
                                                                                              (T.fromLitString
                                                                                                 T.mkNoSrcPos
                                                                                                 p
                                                                                                 "ModifierSymbol")
                                                                                              p))
                                                                                        p))
                                                                                  (T.uap2
                                                                                     T.mkNoSrcPos
                                                                                     p
                                                                                     (Hat.PreludeBasic.galt
                                                                                        T.mkNoSrcPos
                                                                                        p)
                                                                                     (T.uwrapForward
                                                                                        p
                                                                                        (hreadParen
                                                                                           (T.con0
                                                                                              T.mkNoSrcPos
                                                                                              p
                                                                                              Hat.PreludeBuiltinTypes.False
                                                                                              Hat.PreludeBuiltinTypes.aFalse)
                                                                                           (T.uwrapForward
                                                                                              p
                                                                                              (Hat.PreludeBasic.hthenLex
                                                                                                 (T.uap1
                                                                                                    T.mkNoSrcPos
                                                                                                    p
                                                                                                    (Hat.PreludeBasic.gyield
                                                                                                       T.mkNoSrcPos
                                                                                                       p)
                                                                                                    (T.con0
                                                                                                       T.mkNoSrcPos
                                                                                                       p
                                                                                                       OtherSymbol
                                                                                                       aOtherSymbol))
                                                                                                 (T.fromLitString
                                                                                                    T.mkNoSrcPos
                                                                                                    p
                                                                                                    "OtherSymbol")
                                                                                                 p))
                                                                                           p))
                                                                                     (T.uap2
                                                                                        T.mkNoSrcPos
                                                                                        p
                                                                                        (Hat.PreludeBasic.galt
                                                                                           T.mkNoSrcPos
                                                                                           p)
                                                                                        (T.uwrapForward
                                                                                           p
                                                                                           (hreadParen
                                                                                              (T.con0
                                                                                                 T.mkNoSrcPos
                                                                                                 p
                                                                                                 Hat.PreludeBuiltinTypes.False
                                                                                                 Hat.PreludeBuiltinTypes.aFalse)
                                                                                              (T.uwrapForward
                                                                                                 p
                                                                                                 (Hat.PreludeBasic.hthenLex
                                                                                                    (T.uap1
                                                                                                       T.mkNoSrcPos
                                                                                                       p
                                                                                                       (Hat.PreludeBasic.gyield
                                                                                                          T.mkNoSrcPos
                                                                                                          p)
                                                                                                       (T.con0
                                                                                                          T.mkNoSrcPos
                                                                                                          p
                                                                                                          Space
                                                                                                          aSpace))
                                                                                                    (T.fromLitString
                                                                                                       T.mkNoSrcPos
                                                                                                       p
                                                                                                       "Space")
                                                                                                    p))
                                                                                              p))
                                                                                        (T.uap2
                                                                                           T.mkNoSrcPos
                                                                                           p
                                                                                           (Hat.PreludeBasic.galt
                                                                                              T.mkNoSrcPos
                                                                                              p)
                                                                                           (T.uwrapForward
                                                                                              p
                                                                                              (hreadParen
                                                                                                 (T.con0
                                                                                                    T.mkNoSrcPos
                                                                                                    p
                                                                                                    Hat.PreludeBuiltinTypes.False
                                                                                                    Hat.PreludeBuiltinTypes.aFalse)
                                                                                                 (T.uwrapForward
                                                                                                    p
                                                                                                    (Hat.PreludeBasic.hthenLex
                                                                                                       (T.uap1
                                                                                                          T.mkNoSrcPos
                                                                                                          p
                                                                                                          (Hat.PreludeBasic.gyield
                                                                                                             T.mkNoSrcPos
                                                                                                             p)
                                                                                                          (T.con0
                                                                                                             T.mkNoSrcPos
                                                                                                             p
                                                                                                             LineSeparator
                                                                                                             aLineSeparator))
                                                                                                       (T.fromLitString
                                                                                                          T.mkNoSrcPos
                                                                                                          p
                                                                                                          "LineSeparator")
                                                                                                       p))
                                                                                                 p))
                                                                                           (T.uap2
                                                                                              T.mkNoSrcPos
                                                                                              p
                                                                                              (Hat.PreludeBasic.galt
                                                                                                 T.mkNoSrcPos
                                                                                                 p)
                                                                                              (T.uwrapForward
                                                                                                 p
                                                                                                 (hreadParen
                                                                                                    (T.con0
                                                                                                       T.mkNoSrcPos
                                                                                                       p
                                                                                                       Hat.PreludeBuiltinTypes.False
                                                                                                       Hat.PreludeBuiltinTypes.aFalse)
                                                                                                    (T.uwrapForward
                                                                                                       p
                                                                                                       (Hat.PreludeBasic.hthenLex
                                                                                                          (T.uap1
                                                                                                             T.mkNoSrcPos
                                                                                                             p
                                                                                                             (Hat.PreludeBasic.gyield
                                                                                                                T.mkNoSrcPos
                                                                                                                p)
                                                                                                             (T.con0
                                                                                                                T.mkNoSrcPos
                                                                                                                p
                                                                                                                ParagraphSeparator
                                                                                                                aParagraphSeparator))
                                                                                                          (T.fromLitString
                                                                                                             T.mkNoSrcPos
                                                                                                             p
                                                                                                             "ParagraphSeparator")
                                                                                                          p))
                                                                                                    p))
                                                                                              (T.uap2
                                                                                                 T.mkNoSrcPos
                                                                                                 p
                                                                                                 (Hat.PreludeBasic.galt
                                                                                                    T.mkNoSrcPos
                                                                                                    p)
                                                                                                 (T.uwrapForward
                                                                                                    p
                                                                                                    (hreadParen
                                                                                                       (T.con0
                                                                                                          T.mkNoSrcPos
                                                                                                          p
                                                                                                          Hat.PreludeBuiltinTypes.False
                                                                                                          Hat.PreludeBuiltinTypes.aFalse)
                                                                                                       (T.uwrapForward
                                                                                                          p
                                                                                                          (Hat.PreludeBasic.hthenLex
                                                                                                             (T.uap1
                                                                                                                T.mkNoSrcPos
                                                                                                                p
                                                                                                                (Hat.PreludeBasic.gyield
                                                                                                                   T.mkNoSrcPos
                                                                                                                   p)
                                                                                                                (T.con0
                                                                                                                   T.mkNoSrcPos
                                                                                                                   p
                                                                                                                   Control
                                                                                                                   aControl))
                                                                                                             (T.fromLitString
                                                                                                                T.mkNoSrcPos
                                                                                                                p
                                                                                                                "Control")
                                                                                                             p))
                                                                                                       p))
                                                                                                 (T.uap2
                                                                                                    T.mkNoSrcPos
                                                                                                    p
                                                                                                    (Hat.PreludeBasic.galt
                                                                                                       T.mkNoSrcPos
                                                                                                       p)
                                                                                                    (T.uwrapForward
                                                                                                       p
                                                                                                       (hreadParen
                                                                                                          (T.con0
                                                                                                             T.mkNoSrcPos
                                                                                                             p
                                                                                                             Hat.PreludeBuiltinTypes.False
                                                                                                             Hat.PreludeBuiltinTypes.aFalse)
                                                                                                          (T.uwrapForward
                                                                                                             p
                                                                                                             (Hat.PreludeBasic.hthenLex
                                                                                                                (T.uap1
                                                                                                                   T.mkNoSrcPos
                                                                                                                   p
                                                                                                                   (Hat.PreludeBasic.gyield
                                                                                                                      T.mkNoSrcPos
                                                                                                                      p)
                                                                                                                   (T.con0
                                                                                                                      T.mkNoSrcPos
                                                                                                                      p
                                                                                                                      Format
                                                                                                                      aFormat))
                                                                                                                (T.fromLitString
                                                                                                                   T.mkNoSrcPos
                                                                                                                   p
                                                                                                                   "Format")
                                                                                                                p))
                                                                                                          p))
                                                                                                    (T.uap2
                                                                                                       T.mkNoSrcPos
                                                                                                       p
                                                                                                       (Hat.PreludeBasic.galt
                                                                                                          T.mkNoSrcPos
                                                                                                          p)
                                                                                                       (T.uwrapForward
                                                                                                          p
                                                                                                          (hreadParen
                                                                                                             (T.con0
                                                                                                                T.mkNoSrcPos
                                                                                                                p
                                                                                                                Hat.PreludeBuiltinTypes.False
                                                                                                                Hat.PreludeBuiltinTypes.aFalse)
                                                                                                             (T.uwrapForward
                                                                                                                p
                                                                                                                (Hat.PreludeBasic.hthenLex
                                                                                                                   (T.uap1
                                                                                                                      T.mkNoSrcPos
                                                                                                                      p
                                                                                                                      (Hat.PreludeBasic.gyield
                                                                                                                         T.mkNoSrcPos
                                                                                                                         p)
                                                                                                                      (T.con0
                                                                                                                         T.mkNoSrcPos
                                                                                                                         p
                                                                                                                         Surrogate
                                                                                                                         aSurrogate))
                                                                                                                   (T.fromLitString
                                                                                                                      T.mkNoSrcPos
                                                                                                                      p
                                                                                                                      "Surrogate")
                                                                                                                   p))
                                                                                                             p))
                                                                                                       (T.uap2
                                                                                                          T.mkNoSrcPos
                                                                                                          p
                                                                                                          (Hat.PreludeBasic.galt
                                                                                                             T.mkNoSrcPos
                                                                                                             p)
                                                                                                          (T.uwrapForward
                                                                                                             p
                                                                                                             (hreadParen
                                                                                                                (T.con0
                                                                                                                   T.mkNoSrcPos
                                                                                                                   p
                                                                                                                   Hat.PreludeBuiltinTypes.False
                                                                                                                   Hat.PreludeBuiltinTypes.aFalse)
                                                                                                                (T.uwrapForward
                                                                                                                   p
                                                                                                                   (Hat.PreludeBasic.hthenLex
                                                                                                                      (T.uap1
                                                                                                                         T.mkNoSrcPos
                                                                                                                         p
                                                                                                                         (Hat.PreludeBasic.gyield
                                                                                                                            T.mkNoSrcPos
                                                                                                                            p)
                                                                                                                         (T.con0
                                                                                                                            T.mkNoSrcPos
                                                                                                                            p
                                                                                                                            PrivateUse
                                                                                                                            aPrivateUse))
                                                                                                                      (T.fromLitString
                                                                                                                         T.mkNoSrcPos
                                                                                                                         p
                                                                                                                         "PrivateUse")
                                                                                                                      p))
                                                                                                                p))
                                                                                                          (T.uwrapForward
                                                                                                             p
                                                                                                             (hreadParen
                                                                                                                (T.con0
                                                                                                                   T.mkNoSrcPos
                                                                                                                   p
                                                                                                                   Hat.PreludeBuiltinTypes.False
                                                                                                                   Hat.PreludeBuiltinTypes.aFalse)
                                                                                                                (T.uwrapForward
                                                                                                                   p
                                                                                                                   (Hat.PreludeBasic.hthenLex
                                                                                                                      (T.uap1
                                                                                                                         T.mkNoSrcPos
                                                                                                                         p
                                                                                                                         (Hat.PreludeBasic.gyield
                                                                                                                            T.mkNoSrcPos
                                                                                                                            p)
                                                                                                                         (T.con0
                                                                                                                            T.mkNoSrcPos
                                                                                                                            p
                                                                                                                            NotAssigned
                                                                                                                            aNotAssigned))
                                                                                                                      (T.fromLitString
                                                                                                                         T.mkNoSrcPos
                                                                                                                         p
                                                                                                                         "NotAssigned")
                                                                                                                      p))
                                                                                                                p))))))))))))))))))))))))))))))
 
instance Show GeneralCategory where
        gshowsPrec pshowsPrec p
          = T.ufun2 c99v40v99v43showsPrec pshowsPrec p
              hshowsPrec
          where hshowsPrec fy1 (T.R UppercaseLetter _) p
                  = T.uap1 T.mkNoSrcPos p (gshowString T.mkNoSrcPos p)
                      (T.fromLitString T.mkNoSrcPos p "UppercaseLetter")
                hshowsPrec fy1 (T.R LowercaseLetter _) p
                  = T.uap1 T.mkNoSrcPos p (gshowString T.mkNoSrcPos p)
                      (T.fromLitString T.mkNoSrcPos p "LowercaseLetter")
                hshowsPrec fy1 (T.R TitlecaseLetter _) p
                  = T.uap1 T.mkNoSrcPos p (gshowString T.mkNoSrcPos p)
                      (T.fromLitString T.mkNoSrcPos p "TitlecaseLetter")
                hshowsPrec fy1 (T.R ModifierLetter _) p
                  = T.uap1 T.mkNoSrcPos p (gshowString T.mkNoSrcPos p)
                      (T.fromLitString T.mkNoSrcPos p "ModifierLetter")
                hshowsPrec fy1 (T.R OtherLetter _) p
                  = T.uap1 T.mkNoSrcPos p (gshowString T.mkNoSrcPos p)
                      (T.fromLitString T.mkNoSrcPos p "OtherLetter")
                hshowsPrec fy1 (T.R NonSpacingMark _) p
                  = T.uap1 T.mkNoSrcPos p (gshowString T.mkNoSrcPos p)
                      (T.fromLitString T.mkNoSrcPos p "NonSpacingMark")
                hshowsPrec fy1 (T.R SpacingCombiningMark _) p
                  = T.uap1 T.mkNoSrcPos p (gshowString T.mkNoSrcPos p)
                      (T.fromLitString T.mkNoSrcPos p
                         "SpacingCombiningMark")
                hshowsPrec fy1 (T.R EnclosingMark _) p
                  = T.uap1 T.mkNoSrcPos p (gshowString T.mkNoSrcPos p)
                      (T.fromLitString T.mkNoSrcPos p "EnclosingMark")
                hshowsPrec fy1 (T.R DecimalNumber _) p
                  = T.uap1 T.mkNoSrcPos p (gshowString T.mkNoSrcPos p)
                      (T.fromLitString T.mkNoSrcPos p "DecimalNumber")
                hshowsPrec fy1 (T.R LetterNumber _) p
                  = T.uap1 T.mkNoSrcPos p (gshowString T.mkNoSrcPos p)
                      (T.fromLitString T.mkNoSrcPos p "LetterNumber")
                hshowsPrec fy1 (T.R OtherNumber _) p
                  = T.uap1 T.mkNoSrcPos p (gshowString T.mkNoSrcPos p)
                      (T.fromLitString T.mkNoSrcPos p "OtherNumber")
                hshowsPrec fy1 (T.R ConnectorPunctuation _) p
                  = T.uap1 T.mkNoSrcPos p (gshowString T.mkNoSrcPos p)
                      (T.fromLitString T.mkNoSrcPos p
                         "ConnectorPunctuation")
                hshowsPrec fy1 (T.R DashPunctuation _) p
                  = T.uap1 T.mkNoSrcPos p (gshowString T.mkNoSrcPos p)
                      (T.fromLitString T.mkNoSrcPos p "DashPunctuation")
                hshowsPrec fy1 (T.R OpenPunctuation _) p
                  = T.uap1 T.mkNoSrcPos p (gshowString T.mkNoSrcPos p)
                      (T.fromLitString T.mkNoSrcPos p "OpenPunctuation")
                hshowsPrec fy1 (T.R ClosePunctuation _) p
                  = T.uap1 T.mkNoSrcPos p (gshowString T.mkNoSrcPos p)
                      (T.fromLitString T.mkNoSrcPos p "ClosePunctuation")
                hshowsPrec fy1 (T.R InitialQuote _) p
                  = T.uap1 T.mkNoSrcPos p (gshowString T.mkNoSrcPos p)
                      (T.fromLitString T.mkNoSrcPos p "InitialQuote")
                hshowsPrec fy1 (T.R FinalQuote _) p
                  = T.uap1 T.mkNoSrcPos p (gshowString T.mkNoSrcPos p)
                      (T.fromLitString T.mkNoSrcPos p "FinalQuote")
                hshowsPrec fy1 (T.R OtherPunctuation _) p
                  = T.uap1 T.mkNoSrcPos p (gshowString T.mkNoSrcPos p)
                      (T.fromLitString T.mkNoSrcPos p "OtherPunctuation")
                hshowsPrec fy1 (T.R MathSymbol _) p
                  = T.uap1 T.mkNoSrcPos p (gshowString T.mkNoSrcPos p)
                      (T.fromLitString T.mkNoSrcPos p "MathSymbol")
                hshowsPrec fy1 (T.R CurrencySymbol _) p
                  = T.uap1 T.mkNoSrcPos p (gshowString T.mkNoSrcPos p)
                      (T.fromLitString T.mkNoSrcPos p "CurrencySymbol")
                hshowsPrec fy1 (T.R ModifierSymbol _) p
                  = T.uap1 T.mkNoSrcPos p (gshowString T.mkNoSrcPos p)
                      (T.fromLitString T.mkNoSrcPos p "ModifierSymbol")
                hshowsPrec fy1 (T.R OtherSymbol _) p
                  = T.uap1 T.mkNoSrcPos p (gshowString T.mkNoSrcPos p)
                      (T.fromLitString T.mkNoSrcPos p "OtherSymbol")
                hshowsPrec fy1 (T.R Space _) p
                  = T.uap1 T.mkNoSrcPos p (gshowString T.mkNoSrcPos p)
                      (T.fromLitString T.mkNoSrcPos p "Space")
                hshowsPrec fy1 (T.R LineSeparator _) p
                  = T.uap1 T.mkNoSrcPos p (gshowString T.mkNoSrcPos p)
                      (T.fromLitString T.mkNoSrcPos p "LineSeparator")
                hshowsPrec fy1 (T.R ParagraphSeparator _) p
                  = T.uap1 T.mkNoSrcPos p (gshowString T.mkNoSrcPos p)
                      (T.fromLitString T.mkNoSrcPos p "ParagraphSeparator")
                hshowsPrec fy1 (T.R Control _) p
                  = T.uap1 T.mkNoSrcPos p (gshowString T.mkNoSrcPos p)
                      (T.fromLitString T.mkNoSrcPos p "Control")
                hshowsPrec fy1 (T.R Format _) p
                  = T.uap1 T.mkNoSrcPos p (gshowString T.mkNoSrcPos p)
                      (T.fromLitString T.mkNoSrcPos p "Format")
                hshowsPrec fy1 (T.R Surrogate _) p
                  = T.uap1 T.mkNoSrcPos p (gshowString T.mkNoSrcPos p)
                      (T.fromLitString T.mkNoSrcPos p "Surrogate")
                hshowsPrec fy1 (T.R PrivateUse _) p
                  = T.uap1 T.mkNoSrcPos p (gshowString T.mkNoSrcPos p)
                      (T.fromLitString T.mkNoSrcPos p "PrivateUse")
                hshowsPrec fy1 (T.R NotAssigned _) p
                  = T.uap1 T.mkNoSrcPos p (gshowString T.mkNoSrcPos p)
                      (T.fromLitString T.mkNoSrcPos p "NotAssigned")
                hshowsPrec _ _ p = T.fatal p
 
instance Bounded GeneralCategory where
        gminBound pminBound p
          = T.uconstUse pminBound p sminBound
        sminBound
          = T.uconstDef p c99v46v99v52minBound
              (\ p ->
                 T.con0 T.mkNoSrcPos p UppercaseLetter
                   aUppercaseLetter)
        gmaxBound pmaxBound p
          = T.uconstUse pmaxBound p smaxBound
        smaxBound
          = T.uconstDef p c99v46v99v52maxBound
              (\ p ->
                 T.con0 T.mkNoSrcPos p NotAssigned aNotAssigned)
 
instance Ix GeneralCategory where
        grange prange p
          = T.ufun1 c99v55v99v56range prange p hrange
          where hrange (T.R (T.Tuple2 fy1 fy2) _) p
                  = T.uwrapForward p
                      (hmap (grtoEnum T.mkNoSrcPos p)
                         (T.uap2 T.mkNoSrcPos p (genumFromTo T.mkNoSrcPos p)
                            (T.uwrapForward p (hrfromEnum fy1 p))
                            (T.uwrapForward p (hrfromEnum fy2 p)))
                         p)
                  where  
                        grtoEnum ::
                                 T.RefSrcPos ->
                                   T.RefExp -> T.R (T.Fun Int GeneralCategory)
                         
                        hrtoEnum ::
                                 T.R Int -> T.RefExp -> T.R GeneralCategory
                        grtoEnum prtoEnum p
                          = T.ufun1 c99v55v99v56rtoEnum prtoEnum p hrtoEnum
                        artoEnum = c99v55v99v56rtoEnum
                        hrtoEnum fv99v55v99v56n p
                          = T.ucguard
                              (T.uap2 T.mkNoSrcPos p
                                 ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                                 fv99v55v99v56n
                                 (T.uap1 T.mkNoSrcPos p
                                    (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos
                                       p)
                                    (T.conInteger T.mkNoSrcPos p (0))))
                              (h99v55v99v56n p)
                              (y1rtoEnum fv99v55v99v56n p)
                          where h99v55v99v56n p
                                  = T.con0 T.mkNoSrcPos p UppercaseLetter
                                      aUppercaseLetter
                                h99v55v99v56n p = y1rtoEnum fv99v55v99v56n p
                        hrtoEnum fv99v55v99v56n p
                          = y1rtoEnum fv99v55v99v56n p
                        y1rtoEnum fv99v55v99v56n p
                          = T.ucguard
                              (T.uap2 T.mkNoSrcPos p
                                 ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                                 fv99v55v99v56n
                                 (T.uap1 T.mkNoSrcPos p
                                    (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos
                                       p)
                                    (T.conInteger T.mkNoSrcPos p (1))))
                              (h99v55v99v56n p)
                              (y2rtoEnum fv99v55v99v56n p)
                          where h99v55v99v56n p
                                  = T.con0 T.mkNoSrcPos p LowercaseLetter
                                      aLowercaseLetter
                                h99v55v99v56n p = y2rtoEnum fv99v55v99v56n p
                        y1rtoEnum fv99v55v99v56n p
                          = y2rtoEnum fv99v55v99v56n p
                        y2rtoEnum fv99v55v99v56n p
                          = T.ucguard
                              (T.uap2 T.mkNoSrcPos p
                                 ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                                 fv99v55v99v56n
                                 (T.uap1 T.mkNoSrcPos p
                                    (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos
                                       p)
                                    (T.conInteger T.mkNoSrcPos p (2))))
                              (h99v55v99v56n p)
                              (y3rtoEnum fv99v55v99v56n p)
                          where h99v55v99v56n p
                                  = T.con0 T.mkNoSrcPos p TitlecaseLetter
                                      aTitlecaseLetter
                                h99v55v99v56n p = y3rtoEnum fv99v55v99v56n p
                        y2rtoEnum fv99v55v99v56n p
                          = y3rtoEnum fv99v55v99v56n p
                        y3rtoEnum fv99v55v99v56n p
                          = T.ucguard
                              (T.uap2 T.mkNoSrcPos p
                                 ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                                 fv99v55v99v56n
                                 (T.uap1 T.mkNoSrcPos p
                                    (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos
                                       p)
                                    (T.conInteger T.mkNoSrcPos p (3))))
                              (h99v55v99v56n p)
                              (y4rtoEnum fv99v55v99v56n p)
                          where h99v55v99v56n p
                                  = T.con0 T.mkNoSrcPos p ModifierLetter
                                      aModifierLetter
                                h99v55v99v56n p = y4rtoEnum fv99v55v99v56n p
                        y3rtoEnum fv99v55v99v56n p
                          = y4rtoEnum fv99v55v99v56n p
                        y4rtoEnum fv99v55v99v56n p
                          = T.ucguard
                              (T.uap2 T.mkNoSrcPos p
                                 ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                                 fv99v55v99v56n
                                 (T.uap1 T.mkNoSrcPos p
                                    (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos
                                       p)
                                    (T.conInteger T.mkNoSrcPos p (4))))
                              (h99v55v99v56n p)
                              (y5rtoEnum fv99v55v99v56n p)
                          where h99v55v99v56n p
                                  = T.con0 T.mkNoSrcPos p OtherLetter
                                      aOtherLetter
                                h99v55v99v56n p = y5rtoEnum fv99v55v99v56n p
                        y4rtoEnum fv99v55v99v56n p
                          = y5rtoEnum fv99v55v99v56n p
                        y5rtoEnum fv99v55v99v56n p
                          = T.ucguard
                              (T.uap2 T.mkNoSrcPos p
                                 ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                                 fv99v55v99v56n
                                 (T.uap1 T.mkNoSrcPos p
                                    (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos
                                       p)
                                    (T.conInteger T.mkNoSrcPos p (5))))
                              (h99v55v99v56n p)
                              (y6rtoEnum fv99v55v99v56n p)
                          where h99v55v99v56n p
                                  = T.con0 T.mkNoSrcPos p NonSpacingMark
                                      aNonSpacingMark
                                h99v55v99v56n p = y6rtoEnum fv99v55v99v56n p
                        y5rtoEnum fv99v55v99v56n p
                          = y6rtoEnum fv99v55v99v56n p
                        y6rtoEnum fv99v55v99v56n p
                          = T.ucguard
                              (T.uap2 T.mkNoSrcPos p
                                 ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                                 fv99v55v99v56n
                                 (T.uap1 T.mkNoSrcPos p
                                    (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos
                                       p)
                                    (T.conInteger T.mkNoSrcPos p (6))))
                              (h99v55v99v56n p)
                              (y7rtoEnum fv99v55v99v56n p)
                          where h99v55v99v56n p
                                  = T.con0 T.mkNoSrcPos p SpacingCombiningMark
                                      aSpacingCombiningMark
                                h99v55v99v56n p = y7rtoEnum fv99v55v99v56n p
                        y6rtoEnum fv99v55v99v56n p
                          = y7rtoEnum fv99v55v99v56n p
                        y7rtoEnum fv99v55v99v56n p
                          = T.ucguard
                              (T.uap2 T.mkNoSrcPos p
                                 ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                                 fv99v55v99v56n
                                 (T.uap1 T.mkNoSrcPos p
                                    (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos
                                       p)
                                    (T.conInteger T.mkNoSrcPos p (7))))
                              (h99v55v99v56n p)
                              (y8rtoEnum fv99v55v99v56n p)
                          where h99v55v99v56n p
                                  = T.con0 T.mkNoSrcPos p EnclosingMark
                                      aEnclosingMark
                                h99v55v99v56n p = y8rtoEnum fv99v55v99v56n p
                        y7rtoEnum fv99v55v99v56n p
                          = y8rtoEnum fv99v55v99v56n p
                        y8rtoEnum fv99v55v99v56n p
                          = T.ucguard
                              (T.uap2 T.mkNoSrcPos p
                                 ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                                 fv99v55v99v56n
                                 (T.uap1 T.mkNoSrcPos p
                                    (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos
                                       p)
                                    (T.conInteger T.mkNoSrcPos p (8))))
                              (h99v55v99v56n p)
                              (y9rtoEnum fv99v55v99v56n p)
                          where h99v55v99v56n p
                                  = T.con0 T.mkNoSrcPos p DecimalNumber
                                      aDecimalNumber
                                h99v55v99v56n p = y9rtoEnum fv99v55v99v56n p
                        y8rtoEnum fv99v55v99v56n p
                          = y9rtoEnum fv99v55v99v56n p
                        y9rtoEnum fv99v55v99v56n p
                          = T.ucguard
                              (T.uap2 T.mkNoSrcPos p
                                 ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                                 fv99v55v99v56n
                                 (T.uap1 T.mkNoSrcPos p
                                    (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos
                                       p)
                                    (T.conInteger T.mkNoSrcPos p (9))))
                              (h99v55v99v56n p)
                              (y10rtoEnum fv99v55v99v56n p)
                          where h99v55v99v56n p
                                  = T.con0 T.mkNoSrcPos p LetterNumber
                                      aLetterNumber
                                h99v55v99v56n p = y10rtoEnum fv99v55v99v56n p
                        y9rtoEnum fv99v55v99v56n p
                          = y10rtoEnum fv99v55v99v56n p
                        y10rtoEnum fv99v55v99v56n p
                          = T.ucguard
                              (T.uap2 T.mkNoSrcPos p
                                 ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                                 fv99v55v99v56n
                                 (T.uap1 T.mkNoSrcPos p
                                    (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos
                                       p)
                                    (T.conInteger T.mkNoSrcPos p (10))))
                              (h99v55v99v56n p)
                              (y11rtoEnum fv99v55v99v56n p)
                          where h99v55v99v56n p
                                  = T.con0 T.mkNoSrcPos p OtherNumber
                                      aOtherNumber
                                h99v55v99v56n p = y11rtoEnum fv99v55v99v56n p
                        y10rtoEnum fv99v55v99v56n p
                          = y11rtoEnum fv99v55v99v56n p
                        y11rtoEnum fv99v55v99v56n p
                          = T.ucguard
                              (T.uap2 T.mkNoSrcPos p
                                 ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                                 fv99v55v99v56n
                                 (T.uap1 T.mkNoSrcPos p
                                    (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos
                                       p)
                                    (T.conInteger T.mkNoSrcPos p (11))))
                              (h99v55v99v56n p)
                              (y12rtoEnum fv99v55v99v56n p)
                          where h99v55v99v56n p
                                  = T.con0 T.mkNoSrcPos p ConnectorPunctuation
                                      aConnectorPunctuation
                                h99v55v99v56n p = y12rtoEnum fv99v55v99v56n p
                        y11rtoEnum fv99v55v99v56n p
                          = y12rtoEnum fv99v55v99v56n p
                        y12rtoEnum fv99v55v99v56n p
                          = T.ucguard
                              (T.uap2 T.mkNoSrcPos p
                                 ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                                 fv99v55v99v56n
                                 (T.uap1 T.mkNoSrcPos p
                                    (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos
                                       p)
                                    (T.conInteger T.mkNoSrcPos p (12))))
                              (h99v55v99v56n p)
                              (y13rtoEnum fv99v55v99v56n p)
                          where h99v55v99v56n p
                                  = T.con0 T.mkNoSrcPos p DashPunctuation
                                      aDashPunctuation
                                h99v55v99v56n p = y13rtoEnum fv99v55v99v56n p
                        y12rtoEnum fv99v55v99v56n p
                          = y13rtoEnum fv99v55v99v56n p
                        y13rtoEnum fv99v55v99v56n p
                          = T.ucguard
                              (T.uap2 T.mkNoSrcPos p
                                 ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                                 fv99v55v99v56n
                                 (T.uap1 T.mkNoSrcPos p
                                    (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos
                                       p)
                                    (T.conInteger T.mkNoSrcPos p (13))))
                              (h99v55v99v56n p)
                              (y14rtoEnum fv99v55v99v56n p)
                          where h99v55v99v56n p
                                  = T.con0 T.mkNoSrcPos p OpenPunctuation
                                      aOpenPunctuation
                                h99v55v99v56n p = y14rtoEnum fv99v55v99v56n p
                        y13rtoEnum fv99v55v99v56n p
                          = y14rtoEnum fv99v55v99v56n p
                        y14rtoEnum fv99v55v99v56n p
                          = T.ucguard
                              (T.uap2 T.mkNoSrcPos p
                                 ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                                 fv99v55v99v56n
                                 (T.uap1 T.mkNoSrcPos p
                                    (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos
                                       p)
                                    (T.conInteger T.mkNoSrcPos p (14))))
                              (h99v55v99v56n p)
                              (y15rtoEnum fv99v55v99v56n p)
                          where h99v55v99v56n p
                                  = T.con0 T.mkNoSrcPos p ClosePunctuation
                                      aClosePunctuation
                                h99v55v99v56n p = y15rtoEnum fv99v55v99v56n p
                        y14rtoEnum fv99v55v99v56n p
                          = y15rtoEnum fv99v55v99v56n p
                        y15rtoEnum fv99v55v99v56n p
                          = T.ucguard
                              (T.uap2 T.mkNoSrcPos p
                                 ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                                 fv99v55v99v56n
                                 (T.uap1 T.mkNoSrcPos p
                                    (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos
                                       p)
                                    (T.conInteger T.mkNoSrcPos p (15))))
                              (h99v55v99v56n p)
                              (y16rtoEnum fv99v55v99v56n p)
                          where h99v55v99v56n p
                                  = T.con0 T.mkNoSrcPos p InitialQuote
                                      aInitialQuote
                                h99v55v99v56n p = y16rtoEnum fv99v55v99v56n p
                        y15rtoEnum fv99v55v99v56n p
                          = y16rtoEnum fv99v55v99v56n p
                        y16rtoEnum fv99v55v99v56n p
                          = T.ucguard
                              (T.uap2 T.mkNoSrcPos p
                                 ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                                 fv99v55v99v56n
                                 (T.uap1 T.mkNoSrcPos p
                                    (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos
                                       p)
                                    (T.conInteger T.mkNoSrcPos p (16))))
                              (h99v55v99v56n p)
                              (y17rtoEnum fv99v55v99v56n p)
                          where h99v55v99v56n p
                                  = T.con0 T.mkNoSrcPos p FinalQuote aFinalQuote
                                h99v55v99v56n p = y17rtoEnum fv99v55v99v56n p
                        y16rtoEnum fv99v55v99v56n p
                          = y17rtoEnum fv99v55v99v56n p
                        y17rtoEnum fv99v55v99v56n p
                          = T.ucguard
                              (T.uap2 T.mkNoSrcPos p
                                 ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                                 fv99v55v99v56n
                                 (T.uap1 T.mkNoSrcPos p
                                    (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos
                                       p)
                                    (T.conInteger T.mkNoSrcPos p (17))))
                              (h99v55v99v56n p)
                              (y18rtoEnum fv99v55v99v56n p)
                          where h99v55v99v56n p
                                  = T.con0 T.mkNoSrcPos p OtherPunctuation
                                      aOtherPunctuation
                                h99v55v99v56n p = y18rtoEnum fv99v55v99v56n p
                        y17rtoEnum fv99v55v99v56n p
                          = y18rtoEnum fv99v55v99v56n p
                        y18rtoEnum fv99v55v99v56n p
                          = T.ucguard
                              (T.uap2 T.mkNoSrcPos p
                                 ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                                 fv99v55v99v56n
                                 (T.uap1 T.mkNoSrcPos p
                                    (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos
                                       p)
                                    (T.conInteger T.mkNoSrcPos p (18))))
                              (h99v55v99v56n p)
                              (y19rtoEnum fv99v55v99v56n p)
                          where h99v55v99v56n p
                                  = T.con0 T.mkNoSrcPos p MathSymbol aMathSymbol
                                h99v55v99v56n p = y19rtoEnum fv99v55v99v56n p
                        y18rtoEnum fv99v55v99v56n p
                          = y19rtoEnum fv99v55v99v56n p
                        y19rtoEnum fv99v55v99v56n p
                          = T.ucguard
                              (T.uap2 T.mkNoSrcPos p
                                 ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                                 fv99v55v99v56n
                                 (T.uap1 T.mkNoSrcPos p
                                    (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos
                                       p)
                                    (T.conInteger T.mkNoSrcPos p (19))))
                              (h99v55v99v56n p)
                              (y20rtoEnum fv99v55v99v56n p)
                          where h99v55v99v56n p
                                  = T.con0 T.mkNoSrcPos p CurrencySymbol
                                      aCurrencySymbol
                                h99v55v99v56n p = y20rtoEnum fv99v55v99v56n p
                        y19rtoEnum fv99v55v99v56n p
                          = y20rtoEnum fv99v55v99v56n p
                        y20rtoEnum fv99v55v99v56n p
                          = T.ucguard
                              (T.uap2 T.mkNoSrcPos p
                                 ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                                 fv99v55v99v56n
                                 (T.uap1 T.mkNoSrcPos p
                                    (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos
                                       p)
                                    (T.conInteger T.mkNoSrcPos p (20))))
                              (h99v55v99v56n p)
                              (y21rtoEnum fv99v55v99v56n p)
                          where h99v55v99v56n p
                                  = T.con0 T.mkNoSrcPos p ModifierSymbol
                                      aModifierSymbol
                                h99v55v99v56n p = y21rtoEnum fv99v55v99v56n p
                        y20rtoEnum fv99v55v99v56n p
                          = y21rtoEnum fv99v55v99v56n p
                        y21rtoEnum fv99v55v99v56n p
                          = T.ucguard
                              (T.uap2 T.mkNoSrcPos p
                                 ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                                 fv99v55v99v56n
                                 (T.uap1 T.mkNoSrcPos p
                                    (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos
                                       p)
                                    (T.conInteger T.mkNoSrcPos p (21))))
                              (h99v55v99v56n p)
                              (y22rtoEnum fv99v55v99v56n p)
                          where h99v55v99v56n p
                                  = T.con0 T.mkNoSrcPos p OtherSymbol
                                      aOtherSymbol
                                h99v55v99v56n p = y22rtoEnum fv99v55v99v56n p
                        y21rtoEnum fv99v55v99v56n p
                          = y22rtoEnum fv99v55v99v56n p
                        y22rtoEnum fv99v55v99v56n p
                          = T.ucguard
                              (T.uap2 T.mkNoSrcPos p
                                 ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                                 fv99v55v99v56n
                                 (T.uap1 T.mkNoSrcPos p
                                    (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos
                                       p)
                                    (T.conInteger T.mkNoSrcPos p (22))))
                              (h99v55v99v56n p)
                              (y23rtoEnum fv99v55v99v56n p)
                          where h99v55v99v56n p
                                  = T.con0 T.mkNoSrcPos p Space aSpace
                                h99v55v99v56n p = y23rtoEnum fv99v55v99v56n p
                        y22rtoEnum fv99v55v99v56n p
                          = y23rtoEnum fv99v55v99v56n p
                        y23rtoEnum fv99v55v99v56n p
                          = T.ucguard
                              (T.uap2 T.mkNoSrcPos p
                                 ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                                 fv99v55v99v56n
                                 (T.uap1 T.mkNoSrcPos p
                                    (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos
                                       p)
                                    (T.conInteger T.mkNoSrcPos p (23))))
                              (h99v55v99v56n p)
                              (y24rtoEnum fv99v55v99v56n p)
                          where h99v55v99v56n p
                                  = T.con0 T.mkNoSrcPos p LineSeparator
                                      aLineSeparator
                                h99v55v99v56n p = y24rtoEnum fv99v55v99v56n p
                        y23rtoEnum fv99v55v99v56n p
                          = y24rtoEnum fv99v55v99v56n p
                        y24rtoEnum fv99v55v99v56n p
                          = T.ucguard
                              (T.uap2 T.mkNoSrcPos p
                                 ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                                 fv99v55v99v56n
                                 (T.uap1 T.mkNoSrcPos p
                                    (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos
                                       p)
                                    (T.conInteger T.mkNoSrcPos p (24))))
                              (h99v55v99v56n p)
                              (y25rtoEnum fv99v55v99v56n p)
                          where h99v55v99v56n p
                                  = T.con0 T.mkNoSrcPos p ParagraphSeparator
                                      aParagraphSeparator
                                h99v55v99v56n p = y25rtoEnum fv99v55v99v56n p
                        y24rtoEnum fv99v55v99v56n p
                          = y25rtoEnum fv99v55v99v56n p
                        y25rtoEnum fv99v55v99v56n p
                          = T.ucguard
                              (T.uap2 T.mkNoSrcPos p
                                 ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                                 fv99v55v99v56n
                                 (T.uap1 T.mkNoSrcPos p
                                    (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos
                                       p)
                                    (T.conInteger T.mkNoSrcPos p (25))))
                              (h99v55v99v56n p)
                              (y26rtoEnum fv99v55v99v56n p)
                          where h99v55v99v56n p
                                  = T.con0 T.mkNoSrcPos p Control aControl
                                h99v55v99v56n p = y26rtoEnum fv99v55v99v56n p
                        y25rtoEnum fv99v55v99v56n p
                          = y26rtoEnum fv99v55v99v56n p
                        y26rtoEnum fv99v55v99v56n p
                          = T.ucguard
                              (T.uap2 T.mkNoSrcPos p
                                 ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                                 fv99v55v99v56n
                                 (T.uap1 T.mkNoSrcPos p
                                    (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos
                                       p)
                                    (T.conInteger T.mkNoSrcPos p (26))))
                              (h99v55v99v56n p)
                              (y27rtoEnum fv99v55v99v56n p)
                          where h99v55v99v56n p
                                  = T.con0 T.mkNoSrcPos p Format aFormat
                                h99v55v99v56n p = y27rtoEnum fv99v55v99v56n p
                        y26rtoEnum fv99v55v99v56n p
                          = y27rtoEnum fv99v55v99v56n p
                        y27rtoEnum fv99v55v99v56n p
                          = T.ucguard
                              (T.uap2 T.mkNoSrcPos p
                                 ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                                 fv99v55v99v56n
                                 (T.uap1 T.mkNoSrcPos p
                                    (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos
                                       p)
                                    (T.conInteger T.mkNoSrcPos p (27))))
                              (h99v55v99v56n p)
                              (y28rtoEnum fv99v55v99v56n p)
                          where h99v55v99v56n p
                                  = T.con0 T.mkNoSrcPos p Surrogate aSurrogate
                                h99v55v99v56n p = y28rtoEnum fv99v55v99v56n p
                        y27rtoEnum fv99v55v99v56n p
                          = y28rtoEnum fv99v55v99v56n p
                        y28rtoEnum fv99v55v99v56n p
                          = T.ucguard
                              (T.uap2 T.mkNoSrcPos p
                                 ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                                 fv99v55v99v56n
                                 (T.uap1 T.mkNoSrcPos p
                                    (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos
                                       p)
                                    (T.conInteger T.mkNoSrcPos p (28))))
                              (h99v55v99v56n p)
                              (y29rtoEnum fv99v55v99v56n p)
                          where h99v55v99v56n p
                                  = T.con0 T.mkNoSrcPos p PrivateUse aPrivateUse
                                h99v55v99v56n p = y29rtoEnum fv99v55v99v56n p
                        y28rtoEnum fv99v55v99v56n p
                          = y29rtoEnum fv99v55v99v56n p
                        y29rtoEnum fv99v55v99v56n p
                          = T.ucguard
                              (T.uap2 T.mkNoSrcPos p
                                 ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                                 fv99v55v99v56n
                                 (T.uap1 T.mkNoSrcPos p
                                    (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos
                                       p)
                                    (T.conInteger T.mkNoSrcPos p (29))))
                              (h99v55v99v56n p)
                              (T.fatal p)
                          where h99v55v99v56n p
                                  = T.con0 T.mkNoSrcPos p NotAssigned
                                      aNotAssigned
                                h99v55v99v56n p = T.fatal p
                        y29rtoEnum _ p = T.fatal p
                         
                        grfromEnum ::
                                   T.RefSrcPos ->
                                     T.RefExp -> T.R (T.Fun GeneralCategory Int)
                         
                        hrfromEnum ::
                                   T.R GeneralCategory -> T.RefExp -> T.R Int
                        grfromEnum prfromEnum p
                          = T.ufun1 c99v55v99v56rfromEnum prfromEnum p
                              hrfromEnum
                        arfromEnum = c99v55v99v56rfromEnum
                        hrfromEnum (T.R UppercaseLetter _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (0))
                        hrfromEnum (T.R LowercaseLetter _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (1))
                        hrfromEnum (T.R TitlecaseLetter _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (2))
                        hrfromEnum (T.R ModifierLetter _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (3))
                        hrfromEnum (T.R OtherLetter _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (4))
                        hrfromEnum (T.R NonSpacingMark _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (5))
                        hrfromEnum (T.R SpacingCombiningMark _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (6))
                        hrfromEnum (T.R EnclosingMark _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (7))
                        hrfromEnum (T.R DecimalNumber _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (8))
                        hrfromEnum (T.R LetterNumber _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (9))
                        hrfromEnum (T.R OtherNumber _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (10))
                        hrfromEnum (T.R ConnectorPunctuation _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (11))
                        hrfromEnum (T.R DashPunctuation _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (12))
                        hrfromEnum (T.R OpenPunctuation _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (13))
                        hrfromEnum (T.R ClosePunctuation _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (14))
                        hrfromEnum (T.R InitialQuote _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (15))
                        hrfromEnum (T.R FinalQuote _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (16))
                        hrfromEnum (T.R OtherPunctuation _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (17))
                        hrfromEnum (T.R MathSymbol _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (18))
                        hrfromEnum (T.R CurrencySymbol _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (19))
                        hrfromEnum (T.R ModifierSymbol _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (20))
                        hrfromEnum (T.R OtherSymbol _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (21))
                        hrfromEnum (T.R Space _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (22))
                        hrfromEnum (T.R LineSeparator _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (23))
                        hrfromEnum (T.R ParagraphSeparator _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (24))
                        hrfromEnum (T.R Control _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (25))
                        hrfromEnum (T.R Format _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (26))
                        hrfromEnum (T.R Surrogate _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (27))
                        hrfromEnum (T.R PrivateUse _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (28))
                        hrfromEnum (T.R NotAssigned _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (29))
                        hrfromEnum _ p = T.fatal p
        gindex pindex p
          = T.ufun2 c99v55v99v56index pindex p hindex
          where hindex (T.R (T.Tuple2 fy1 fy2) _) fy3 p
                  = T.uap2 T.mkNoSrcPos p ((!-) T.mkNoSrcPos p)
                      (T.uwrapForward p (hifromEnum fy3 p))
                      (T.uwrapForward p (hifromEnum fy2 p))
                  where  
                        gifromEnum ::
                                   T.RefSrcPos ->
                                     T.RefExp -> T.R (T.Fun GeneralCategory Int)
                         
                        hifromEnum ::
                                   T.R GeneralCategory -> T.RefExp -> T.R Int
                        gifromEnum pifromEnum p
                          = T.ufun1 c99v55v99v56ifromEnum pifromEnum p
                              hifromEnum
                        aifromEnum = c99v55v99v56ifromEnum
                        hifromEnum (T.R UppercaseLetter _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (0))
                        hifromEnum (T.R LowercaseLetter _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (1))
                        hifromEnum (T.R TitlecaseLetter _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (2))
                        hifromEnum (T.R ModifierLetter _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (3))
                        hifromEnum (T.R OtherLetter _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (4))
                        hifromEnum (T.R NonSpacingMark _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (5))
                        hifromEnum (T.R SpacingCombiningMark _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (6))
                        hifromEnum (T.R EnclosingMark _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (7))
                        hifromEnum (T.R DecimalNumber _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (8))
                        hifromEnum (T.R LetterNumber _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (9))
                        hifromEnum (T.R OtherNumber _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (10))
                        hifromEnum (T.R ConnectorPunctuation _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (11))
                        hifromEnum (T.R DashPunctuation _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (12))
                        hifromEnum (T.R OpenPunctuation _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (13))
                        hifromEnum (T.R ClosePunctuation _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (14))
                        hifromEnum (T.R InitialQuote _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (15))
                        hifromEnum (T.R FinalQuote _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (16))
                        hifromEnum (T.R OtherPunctuation _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (17))
                        hifromEnum (T.R MathSymbol _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (18))
                        hifromEnum (T.R CurrencySymbol _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (19))
                        hifromEnum (T.R ModifierSymbol _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (20))
                        hifromEnum (T.R OtherSymbol _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (21))
                        hifromEnum (T.R Space _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (22))
                        hifromEnum (T.R LineSeparator _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (23))
                        hifromEnum (T.R ParagraphSeparator _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (24))
                        hifromEnum (T.R Control _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (25))
                        hifromEnum (T.R Format _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (26))
                        hifromEnum (T.R Surrogate _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (27))
                        hifromEnum (T.R PrivateUse _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (28))
                        hifromEnum (T.R NotAssigned _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (29))
                        hifromEnum _ p = T.fatal p
        ginRange pinRange p
          = T.ufun2 c99v55v99v56inRange pinRange p hinRange
          where hinRange (T.R (T.Tuple2 fy1 fy2) _) fy3 p
                  = T.uap2 T.mkNoSrcPos p (ginRange T.mkNoSrcPos p)
                      (T.con2 T.mkNoSrcPos p T.Tuple2 T.aTuple2
                         (T.uwrapForward p (hnfromEnum fy1 p))
                         (T.uwrapForward p (hnfromEnum fy2 p)))
                      (T.uwrapForward p (hnfromEnum fy3 p))
                  where  
                        gnfromEnum ::
                                   T.RefSrcPos ->
                                     T.RefExp -> T.R (T.Fun GeneralCategory Int)
                         
                        hnfromEnum ::
                                   T.R GeneralCategory -> T.RefExp -> T.R Int
                        gnfromEnum pnfromEnum p
                          = T.ufun1 c99v55v99v56nfromEnum pnfromEnum p
                              hnfromEnum
                        anfromEnum = c99v55v99v56nfromEnum
                        hnfromEnum (T.R UppercaseLetter _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (0))
                        hnfromEnum (T.R LowercaseLetter _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (1))
                        hnfromEnum (T.R TitlecaseLetter _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (2))
                        hnfromEnum (T.R ModifierLetter _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (3))
                        hnfromEnum (T.R OtherLetter _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (4))
                        hnfromEnum (T.R NonSpacingMark _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (5))
                        hnfromEnum (T.R SpacingCombiningMark _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (6))
                        hnfromEnum (T.R EnclosingMark _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (7))
                        hnfromEnum (T.R DecimalNumber _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (8))
                        hnfromEnum (T.R LetterNumber _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (9))
                        hnfromEnum (T.R OtherNumber _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (10))
                        hnfromEnum (T.R ConnectorPunctuation _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (11))
                        hnfromEnum (T.R DashPunctuation _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (12))
                        hnfromEnum (T.R OpenPunctuation _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (13))
                        hnfromEnum (T.R ClosePunctuation _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (14))
                        hnfromEnum (T.R InitialQuote _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (15))
                        hnfromEnum (T.R FinalQuote _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (16))
                        hnfromEnum (T.R OtherPunctuation _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (17))
                        hnfromEnum (T.R MathSymbol _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (18))
                        hnfromEnum (T.R CurrencySymbol _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (19))
                        hnfromEnum (T.R ModifierSymbol _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (20))
                        hnfromEnum (T.R OtherSymbol _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (21))
                        hnfromEnum (T.R Space _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (22))
                        hnfromEnum (T.R LineSeparator _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (23))
                        hnfromEnum (T.R ParagraphSeparator _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (24))
                        hnfromEnum (T.R Control _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (25))
                        hnfromEnum (T.R Format _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (26))
                        hnfromEnum (T.R Surrogate _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (27))
                        hnfromEnum (T.R PrivateUse _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (28))
                        hnfromEnum (T.R NotAssigned _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (29))
                        hnfromEnum _ p = T.fatal p
 
ggenCat ::
        T.RefSrcPos -> T.RefExp -> T.R (T.Fun Int Int)
ggenCat pgenCat p = T.ufun1 agenCat pgenCat p hgenCat
hgenCat z1genCat kgenCat
  = T.fromInt kgenCat
      ((\_ i -> Prelude.fromEnum (Data.Char.generalCategory (Prelude.toEnum i))) Prelude.True
         (T.toInt kgenCat z1genCat))
 
ggeneralCategory ::
                 T.RefSrcPos ->
                   T.RefExp -> T.R (T.Fun Char GeneralCategory)
 
hgeneralCategory ::
                 T.R Char -> T.RefExp -> T.R GeneralCategory
ggeneralCategory pgeneralCategory p
  = T.ufun1 ageneralCategory pgeneralCategory p
      hgeneralCategory
hgeneralCategory fc p
  = T.uwrapForward p
      ((*$) (gtoEnum T.mkNoSrcPos p)
         (T.uwrapForward p
            ((*$) (ggenCat T.mkNoSrcPos p)
               (T.uap1 T.mkNoSrcPos p (gord T.mkNoSrcPos p) fc)
               p))
         p)
aClosePunctuation
  = T.mkConstructor tChar 830011 830026 3 (0)
      "ClosePunctuation"
aConnectorPunctuation
  = T.mkConstructor tChar 800011 800030 3 (0)
      "ConnectorPunctuation"
aControl
  = T.mkConstructor tChar 940011 940017 3 (0) "Control"
aCurrencySymbol
  = T.mkConstructor tChar 880011 880024 3 (0)
      "CurrencySymbol"
aDashPunctuation
  = T.mkConstructor tChar 810011 810025 3 (0)
      "DashPunctuation"
aDecimalNumber
  = T.mkConstructor tChar 770011 770023 3 (0)
      "DecimalNumber"
aEnclosingMark
  = T.mkConstructor tChar 760011 760023 3 (0)
      "EnclosingMark"
aFinalQuote
  = T.mkConstructor tChar 850011 850020 3 (0)
      "FinalQuote"
aFormat
  = T.mkConstructor tChar 950011 950016 3 (0) "Format"
aInitialQuote
  = T.mkConstructor tChar 840011 840022 3 (0)
      "InitialQuote"
aLetterNumber
  = T.mkConstructor tChar 780011 780022 3 (0)
      "LetterNumber"
aLineSeparator
  = T.mkConstructor tChar 920011 920023 3 (0)
      "LineSeparator"
aLowercaseLetter
  = T.mkConstructor tChar 700011 700025 3 (0)
      "LowercaseLetter"
aMathSymbol
  = T.mkConstructor tChar 870011 870020 3 (0)
      "MathSymbol"
aModifierLetter
  = T.mkConstructor tChar 720011 720024 3 (0)
      "ModifierLetter"
aModifierSymbol
  = T.mkConstructor tChar 890011 890024 3 (0)
      "ModifierSymbol"
aNonSpacingMark
  = T.mkConstructor tChar 740011 740024 3 (0)
      "NonSpacingMark"
aNotAssigned
  = T.mkConstructor tChar 980011 980021 3 (0)
      "NotAssigned"
aOpenPunctuation
  = T.mkConstructor tChar 820011 820025 3 (0)
      "OpenPunctuation"
aOtherLetter
  = T.mkConstructor tChar 730011 730021 3 (0)
      "OtherLetter"
aOtherNumber
  = T.mkConstructor tChar 790011 790021 3 (0)
      "OtherNumber"
aOtherPunctuation
  = T.mkConstructor tChar 860011 860026 3 (0)
      "OtherPunctuation"
aOtherSymbol
  = T.mkConstructor tChar 900011 900021 3 (0)
      "OtherSymbol"
aParagraphSeparator
  = T.mkConstructor tChar 930011 930028 3 (0)
      "ParagraphSeparator"
aPrivateUse
  = T.mkConstructor tChar 970011 970020 3 (0)
      "PrivateUse"
aSpace
  = T.mkConstructor tChar 910011 910015 3 (0) "Space"
aSpacingCombiningMark
  = T.mkConstructor tChar 750011 750030 3 (0)
      "SpacingCombiningMark"
aSurrogate
  = T.mkConstructor tChar 960011 960019 3 (0)
      "Surrogate"
aTitlecaseLetter
  = T.mkConstructor tChar 710011 710025 3 (0)
      "TitlecaseLetter"
aUppercaseLetter
  = T.mkConstructor tChar 690011 690025 3 (0)
      "UppercaseLetter"
agenCat
  = T.mkVariable tChar 1010001 1030011 3 (1) "genCat"
      Prelude.False
ageneralCategory
  = T.mkVariable tChar 1060001 1060043 3 (1)
      "generalCategory"
      Prelude.False
aisAsciiLower
  = T.mkVariable tChar 620001 630017 3 (1)
      "isAsciiLower"
      Prelude.False
aisAsciiUpper
  = T.mkVariable tChar 600001 610017 3 (1)
      "isAsciiUpper"
      Prelude.False
aisLetter
  = T.mkVariable tChar 480001 490013 3 (1) "isLetter"
      Prelude.False
aisMark
  = T.mkVariable tChar 500001 510011 3 (1) "isMark"
      Prelude.False
aisNumber
  = T.mkVariable tChar 520001 530013 3 (1) "isNumber"
      Prelude.False
aisPunctuation
  = T.mkVariable tChar 540001 550018 3 (1)
      "isPunctuation"
      Prelude.False
aisSeparator
  = T.mkVariable tChar 580001 590016 3 (1)
      "isSeparator"
      Prelude.False
aisSymbol
  = T.mkVariable tChar 560001 570013 3 (1) "isSymbol"
      Prelude.False
atoTitle
  = T.mkVariable tChar 650001 660012 3 (1) "toTitle"
      Prelude.False
(+>>=#>=>>=$!==)
  = T.mkVariable tChar 990019 990020 3 (-1) "=="
      Prelude.False
c99v23v99v25localFromEnum
  = T.mkVariable tChar 990023 990025 3 (1)
      "localFromEnum"
      Prelude.True
c99v23v99v25compare
  = T.mkVariable tChar 990023 990025 3 (-1) "compare"
      Prelude.False
c99v28v99v31enumFrom
  = T.mkVariable tChar 990028 990031 3 (-1) "enumFrom"
      Prelude.False
c99v28v99v31enumFromThen
  = T.mkVariable tChar 990028 990031 3 (-1)
      "enumFromThen"
      Prelude.False
c99v28v99v31fromEnum
  = T.mkVariable tChar 990028 990031 3 (-1) "fromEnum"
      Prelude.False
c99v28v99v31toEnum
  = T.mkVariable tChar 990028 990031 3 (-1) "toEnum"
      Prelude.False
c99v34v99v37readsPrec
  = T.mkVariable tChar 990034 990037 3 (-1) "readsPrec"
      Prelude.False
c99v40v99v43showsPrec
  = T.mkVariable tChar 990040 990043 3 (-1) "showsPrec"
      Prelude.False
c99v46v99v52maxBound
  = T.mkVariable tChar 990046 990052 3 (-1) "maxBound"
      Prelude.False
c99v46v99v52minBound
  = T.mkVariable tChar 990046 990052 3 (-1) "minBound"
      Prelude.False
c99v55v99v56rfromEnum
  = T.mkVariable tChar 990055 990056 3 (1) "rfromEnum"
      Prelude.True
c99v55v99v56rtoEnum
  = T.mkVariable tChar 990055 990056 3 (1) "rtoEnum"
      Prelude.True
c99v55v99v56ifromEnum
  = T.mkVariable tChar 990055 990056 3 (1) "ifromEnum"
      Prelude.True
c99v55v99v56nfromEnum
  = T.mkVariable tChar 990055 990056 3 (1) "nfromEnum"
      Prelude.True
c99v55v99v56inRange
  = T.mkVariable tChar 990055 990056 3 (-1) "inRange"
      Prelude.False
c99v55v99v56index
  = T.mkVariable tChar 990055 990056 3 (-1) "index"
      Prelude.False
c99v55v99v56range
  = T.mkVariable tChar 990055 990056 3 (-1) "range"
      Prelude.False
p = T.mkRoot
tChar
  = T.mkModule "Data.Char" "Data/Char.hs" Prelude.False