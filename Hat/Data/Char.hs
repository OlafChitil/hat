module Hat.Data.Char
  (Char(),String(),gisControl,aisControl,hisControl,gisSpace,aisSpace,hisSpace
    ,gisLower,aisLower,hisLower,gisUpper,aisUpper,hisUpper,gisAlpha,aisAlpha
    ,hisAlpha,gisAlphaNum,aisAlphaNum,hisAlphaNum,gisPrint,aisPrint,hisPrint
    ,gisDigit,aisDigit,hisDigit,gisOctDigit,aisOctDigit,hisOctDigit,gisHexDigit
    ,aisHexDigit,hisHexDigit,gisLetter,aisLetter,hisLetter,gisMark,aisMark
    ,hisMark,gisNumber,aisNumber,hisNumber,gisPunctuation,aisPunctuation
    ,hisPunctuation,gisSymbol,aisSymbol,hisSymbol,gisSeparator,aisSeparator
    ,hisSeparator,gisAscii,aisAscii,hisAscii,gisLatin1,aisLatin1,hisLatin1
    ,gisAsciiUpper,aisAsciiUpper,hisAsciiUpper,gisAsciiLower,aisAsciiLower
    ,hisAsciiLower,GeneralCategory(UppercaseLetter,LowercaseLetter
      ,TitlecaseLetter,ModifierLetter,OtherLetter,NonSpacingMark
      ,SpacingCombiningMark,EnclosingMark,DecimalNumber,LetterNumber,OtherNumber
      ,ConnectorPunctuation,DashPunctuation,OpenPunctuation,ClosePunctuation
      ,InitialQuote,FinalQuote,OtherPunctuation,MathSymbol,CurrencySymbol
      ,ModifierSymbol,OtherSymbol,Space,LineSeparator,ParagraphSeparator,Control
      ,Format,Surrogate,PrivateUse,NotAssigned),aUppercaseLetter
    ,aLowercaseLetter,aTitlecaseLetter,aModifierLetter,aOtherLetter
    ,aNonSpacingMark,aSpacingCombiningMark,aEnclosingMark,aDecimalNumber
    ,aLetterNumber,aOtherNumber,aConnectorPunctuation,aDashPunctuation
    ,aOpenPunctuation,aClosePunctuation,aInitialQuote,aFinalQuote
    ,aOtherPunctuation,aMathSymbol,aCurrencySymbol,aModifierSymbol,aOtherSymbol
    ,aSpace,aLineSeparator,aParagraphSeparator,aControl,aFormat,aSurrogate
    ,aPrivateUse,aNotAssigned,ggeneralCategory,ageneralCategory,hgeneralCategory
    ,gtoUpper,atoUpper,htoUpper,gtoLower,atoLower,htoLower,gtoTitle,atoTitle
    ,htoTitle,gdigitToInt,adigitToInt,hdigitToInt,gintToDigit,aintToDigit
    ,hintToDigit,gord,gchr,gshowLitChar,ashowLitChar,hshowLitChar,glexLitChar
    ,alexLitChar,hlexLitChar,greadLitChar,areadLitChar,hreadLitChar) where

import qualified Hat.PreludeBasic 
import qualified Prelude 
import Hat.Hack 
import qualified Hat.Hat as T 
import Hat.Hat  (WrapVal(wrapVal))
import Hat.Prelude 
import Hat.PreludeBasic 
import Hat.Ix 
import Hat.PreludeBuiltinTypes 
import qualified Data.Char 

gisLetter :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun Char Bool)

gisLetter pisLetter p = T.ufun1 aisLetter pisLetter p hisLetter

hisLetter z1isLetter kisLetter =
  fromBool kisLetter (Data.Char.isLetter (T.toChar kisLetter z1isLetter))

gisMark :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun Char Bool)

gisMark pisMark p = T.ufun1 aisMark pisMark p hisMark

hisMark z1isMark kisMark =
  fromBool kisMark (Data.Char.isMark (T.toChar kisMark z1isMark))

gisNumber :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun Char Bool)

gisNumber pisNumber p = T.ufun1 aisNumber pisNumber p hisNumber

hisNumber z1isNumber kisNumber =
  fromBool kisNumber (Data.Char.isNumber (T.toChar kisNumber z1isNumber))

gisPunctuation :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun Char Bool)

gisPunctuation pisPunctuation p =
  T.ufun1 aisPunctuation pisPunctuation p hisPunctuation

hisPunctuation z1isPunctuation kisPunctuation =
  fromBool kisPunctuation
    (Data.Char.isPunctuation (T.toChar kisPunctuation z1isPunctuation))

gisSymbol :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun Char Bool)

gisSymbol pisSymbol p = T.ufun1 aisSymbol pisSymbol p hisSymbol

hisSymbol z1isSymbol kisSymbol =
  fromBool kisSymbol (Data.Char.isSymbol (T.toChar kisSymbol z1isSymbol))

gisSeparator :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun Char Bool)

gisSeparator pisSeparator p = T.ufun1 aisSeparator pisSeparator p hisSeparator

hisSeparator z1isSeparator kisSeparator =
  fromBool kisSeparator
    (Data.Char.isSeparator (T.toChar kisSeparator z1isSeparator))

gisAsciiUpper :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun Char Bool)

gisAsciiUpper pisAsciiUpper p =
  T.ufun1 aisAsciiUpper pisAsciiUpper p hisAsciiUpper

hisAsciiUpper z1isAsciiUpper kisAsciiUpper =
  fromBool kisAsciiUpper
    (Data.Char.isAsciiUpper (T.toChar kisAsciiUpper z1isAsciiUpper))

gisAsciiLower :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun Char Bool)

gisAsciiLower pisAsciiLower p =
  T.ufun1 aisAsciiLower pisAsciiLower p hisAsciiLower

hisAsciiLower z1isAsciiLower kisAsciiLower =
  fromBool kisAsciiLower
    (Data.Char.isAsciiLower (T.toChar kisAsciiLower z1isAsciiLower))

gtoTitle :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun Char Char)

gtoTitle ptoTitle p = T.ufun1 atoTitle ptoTitle p htoTitle

htoTitle z1toTitle ktoTitle =
  T.fromChar ktoTitle (Data.Char.toTitle (T.toChar ktoTitle z1toTitle))

data GeneralCategory =
  UppercaseLetter  | LowercaseLetter  | TitlecaseLetter  | ModifierLetter 
  | OtherLetter  | NonSpacingMark  | SpacingCombiningMark  | EnclosingMark 
  | DecimalNumber  | LetterNumber  | OtherNumber  | ConnectorPunctuation 
  | DashPunctuation  | OpenPunctuation  | ClosePunctuation  | InitialQuote 
  | FinalQuote  | OtherPunctuation  | MathSymbol  | CurrencySymbol 
  | ModifierSymbol  | OtherSymbol  | Space  | LineSeparator 
  | ParagraphSeparator  | Control  | Format  | Surrogate  | PrivateUse 
  | NotAssigned 

instance T.WrapVal (GeneralCategory)
  where
  
  wrapVal pwrapVal (kwrapVal@UppercaseLetter) p =
    T.R kwrapVal (T.mkValueUse p pwrapVal aUppercaseLetter)
  wrapVal pwrapVal (kwrapVal@LowercaseLetter) p =
    T.R kwrapVal (T.mkValueUse p pwrapVal aLowercaseLetter)
  wrapVal pwrapVal (kwrapVal@TitlecaseLetter) p =
    T.R kwrapVal (T.mkValueUse p pwrapVal aTitlecaseLetter)
  wrapVal pwrapVal (kwrapVal@ModifierLetter) p =
    T.R kwrapVal (T.mkValueUse p pwrapVal aModifierLetter)
  wrapVal pwrapVal (kwrapVal@OtherLetter) p =
    T.R kwrapVal (T.mkValueUse p pwrapVal aOtherLetter)
  wrapVal pwrapVal (kwrapVal@NonSpacingMark) p =
    T.R kwrapVal (T.mkValueUse p pwrapVal aNonSpacingMark)
  wrapVal pwrapVal (kwrapVal@SpacingCombiningMark) p =
    T.R kwrapVal (T.mkValueUse p pwrapVal aSpacingCombiningMark)
  wrapVal pwrapVal (kwrapVal@EnclosingMark) p =
    T.R kwrapVal (T.mkValueUse p pwrapVal aEnclosingMark)
  wrapVal pwrapVal (kwrapVal@DecimalNumber) p =
    T.R kwrapVal (T.mkValueUse p pwrapVal aDecimalNumber)
  wrapVal pwrapVal (kwrapVal@LetterNumber) p =
    T.R kwrapVal (T.mkValueUse p pwrapVal aLetterNumber)
  wrapVal pwrapVal (kwrapVal@OtherNumber) p =
    T.R kwrapVal (T.mkValueUse p pwrapVal aOtherNumber)
  wrapVal pwrapVal (kwrapVal@ConnectorPunctuation) p =
    T.R kwrapVal (T.mkValueUse p pwrapVal aConnectorPunctuation)
  wrapVal pwrapVal (kwrapVal@DashPunctuation) p =
    T.R kwrapVal (T.mkValueUse p pwrapVal aDashPunctuation)
  wrapVal pwrapVal (kwrapVal@OpenPunctuation) p =
    T.R kwrapVal (T.mkValueUse p pwrapVal aOpenPunctuation)
  wrapVal pwrapVal (kwrapVal@ClosePunctuation) p =
    T.R kwrapVal (T.mkValueUse p pwrapVal aClosePunctuation)
  wrapVal pwrapVal (kwrapVal@InitialQuote) p =
    T.R kwrapVal (T.mkValueUse p pwrapVal aInitialQuote)
  wrapVal pwrapVal (kwrapVal@FinalQuote) p =
    T.R kwrapVal (T.mkValueUse p pwrapVal aFinalQuote)
  wrapVal pwrapVal (kwrapVal@OtherPunctuation) p =
    T.R kwrapVal (T.mkValueUse p pwrapVal aOtherPunctuation)
  wrapVal pwrapVal (kwrapVal@MathSymbol) p =
    T.R kwrapVal (T.mkValueUse p pwrapVal aMathSymbol)
  wrapVal pwrapVal (kwrapVal@CurrencySymbol) p =
    T.R kwrapVal (T.mkValueUse p pwrapVal aCurrencySymbol)
  wrapVal pwrapVal (kwrapVal@ModifierSymbol) p =
    T.R kwrapVal (T.mkValueUse p pwrapVal aModifierSymbol)
  wrapVal pwrapVal (kwrapVal@OtherSymbol) p =
    T.R kwrapVal (T.mkValueUse p pwrapVal aOtherSymbol)
  wrapVal pwrapVal (kwrapVal@Space) p =
    T.R kwrapVal (T.mkValueUse p pwrapVal aSpace)
  wrapVal pwrapVal (kwrapVal@LineSeparator) p =
    T.R kwrapVal (T.mkValueUse p pwrapVal aLineSeparator)
  wrapVal pwrapVal (kwrapVal@ParagraphSeparator) p =
    T.R kwrapVal (T.mkValueUse p pwrapVal aParagraphSeparator)
  wrapVal pwrapVal (kwrapVal@Control) p =
    T.R kwrapVal (T.mkValueUse p pwrapVal aControl)
  wrapVal pwrapVal (kwrapVal@Format) p =
    T.R kwrapVal (T.mkValueUse p pwrapVal aFormat)
  wrapVal pwrapVal (kwrapVal@Surrogate) p =
    T.R kwrapVal (T.mkValueUse p pwrapVal aSurrogate)
  wrapVal pwrapVal (kwrapVal@PrivateUse) p =
    T.R kwrapVal (T.mkValueUse p pwrapVal aPrivateUse)
  wrapVal pwrapVal (kwrapVal@NotAssigned) p =
    T.R kwrapVal (T.mkValueUse p pwrapVal aNotAssigned)
  

instance Eq (GeneralCategory)
  where
  
  (!==) (%==) p =
    T.ufun2 (+>>=#>=>>=$!==) (%==) p (*==)
    where
    
    (*==) (T.R UppercaseLetter _) (T.R UppercaseLetter _) p =
      T.con0 T.mkNoSrcPos p Hat.Prelude.True Hat.Prelude.aTrue
    (*==) (T.R LowercaseLetter _) (T.R LowercaseLetter _) p =
      T.con0 T.mkNoSrcPos p Hat.Prelude.True Hat.Prelude.aTrue
    (*==) (T.R TitlecaseLetter _) (T.R TitlecaseLetter _) p =
      T.con0 T.mkNoSrcPos p Hat.Prelude.True Hat.Prelude.aTrue
    (*==) (T.R ModifierLetter _) (T.R ModifierLetter _) p =
      T.con0 T.mkNoSrcPos p Hat.Prelude.True Hat.Prelude.aTrue
    (*==) (T.R OtherLetter _) (T.R OtherLetter _) p =
      T.con0 T.mkNoSrcPos p Hat.Prelude.True Hat.Prelude.aTrue
    (*==) (T.R NonSpacingMark _) (T.R NonSpacingMark _) p =
      T.con0 T.mkNoSrcPos p Hat.Prelude.True Hat.Prelude.aTrue
    (*==) (T.R SpacingCombiningMark _) (T.R SpacingCombiningMark _) p =
      T.con0 T.mkNoSrcPos p Hat.Prelude.True Hat.Prelude.aTrue
    (*==) (T.R EnclosingMark _) (T.R EnclosingMark _) p =
      T.con0 T.mkNoSrcPos p Hat.Prelude.True Hat.Prelude.aTrue
    (*==) (T.R DecimalNumber _) (T.R DecimalNumber _) p =
      T.con0 T.mkNoSrcPos p Hat.Prelude.True Hat.Prelude.aTrue
    (*==) (T.R LetterNumber _) (T.R LetterNumber _) p =
      T.con0 T.mkNoSrcPos p Hat.Prelude.True Hat.Prelude.aTrue
    (*==) (T.R OtherNumber _) (T.R OtherNumber _) p =
      T.con0 T.mkNoSrcPos p Hat.Prelude.True Hat.Prelude.aTrue
    (*==) (T.R ConnectorPunctuation _) (T.R ConnectorPunctuation _) p =
      T.con0 T.mkNoSrcPos p Hat.Prelude.True Hat.Prelude.aTrue
    (*==) (T.R DashPunctuation _) (T.R DashPunctuation _) p =
      T.con0 T.mkNoSrcPos p Hat.Prelude.True Hat.Prelude.aTrue
    (*==) (T.R OpenPunctuation _) (T.R OpenPunctuation _) p =
      T.con0 T.mkNoSrcPos p Hat.Prelude.True Hat.Prelude.aTrue
    (*==) (T.R ClosePunctuation _) (T.R ClosePunctuation _) p =
      T.con0 T.mkNoSrcPos p Hat.Prelude.True Hat.Prelude.aTrue
    (*==) (T.R InitialQuote _) (T.R InitialQuote _) p =
      T.con0 T.mkNoSrcPos p Hat.Prelude.True Hat.Prelude.aTrue
    (*==) (T.R FinalQuote _) (T.R FinalQuote _) p =
      T.con0 T.mkNoSrcPos p Hat.Prelude.True Hat.Prelude.aTrue
    (*==) (T.R OtherPunctuation _) (T.R OtherPunctuation _) p =
      T.con0 T.mkNoSrcPos p Hat.Prelude.True Hat.Prelude.aTrue
    (*==) (T.R MathSymbol _) (T.R MathSymbol _) p =
      T.con0 T.mkNoSrcPos p Hat.Prelude.True Hat.Prelude.aTrue
    (*==) (T.R CurrencySymbol _) (T.R CurrencySymbol _) p =
      T.con0 T.mkNoSrcPos p Hat.Prelude.True Hat.Prelude.aTrue
    (*==) (T.R ModifierSymbol _) (T.R ModifierSymbol _) p =
      T.con0 T.mkNoSrcPos p Hat.Prelude.True Hat.Prelude.aTrue
    (*==) (T.R OtherSymbol _) (T.R OtherSymbol _) p =
      T.con0 T.mkNoSrcPos p Hat.Prelude.True Hat.Prelude.aTrue
    (*==) (T.R Space _) (T.R Space _) p =
      T.con0 T.mkNoSrcPos p Hat.Prelude.True Hat.Prelude.aTrue
    (*==) (T.R LineSeparator _) (T.R LineSeparator _) p =
      T.con0 T.mkNoSrcPos p Hat.Prelude.True Hat.Prelude.aTrue
    (*==) (T.R ParagraphSeparator _) (T.R ParagraphSeparator _) p =
      T.con0 T.mkNoSrcPos p Hat.Prelude.True Hat.Prelude.aTrue
    (*==) (T.R Control _) (T.R Control _) p =
      T.con0 T.mkNoSrcPos p Hat.Prelude.True Hat.Prelude.aTrue
    (*==) (T.R Format _) (T.R Format _) p =
      T.con0 T.mkNoSrcPos p Hat.Prelude.True Hat.Prelude.aTrue
    (*==) (T.R Surrogate _) (T.R Surrogate _) p =
      T.con0 T.mkNoSrcPos p Hat.Prelude.True Hat.Prelude.aTrue
    (*==) (T.R PrivateUse _) (T.R PrivateUse _) p =
      T.con0 T.mkNoSrcPos p Hat.Prelude.True Hat.Prelude.aTrue
    (*==) (T.R NotAssigned _) (T.R NotAssigned _) p =
      T.con0 T.mkNoSrcPos p Hat.Prelude.True Hat.Prelude.aTrue
    (*==) _ _ p = T.con0 T.mkNoSrcPos p Hat.Prelude.False Hat.Prelude.aFalse
    
  

instance Ord (GeneralCategory)
  where
  
  gcompare pcompare p =
    T.ufun2 a99v23v99v25compare pcompare p hcompare
    where
    
    hcompare fy1 fy2 p =
      T.uap2 T.mkNoSrcPos p (Hat.Prelude.gcompare T.mkNoSrcPos p)
        (T.uwrapForward p (hlocalFromEnum fy1 p) :: T.R Hat.Prelude.Int)
        (T.uwrapForward p (hlocalFromEnum fy2 p))
      where
      
      glocalFromEnum plocalFromEnum p =
        T.ufun1 a99v23v99v25localFromEnum plocalFromEnum p hlocalFromEnum
      
      alocalFromEnum = a99v23v99v25localFromEnum
      
      hlocalFromEnum (T.R (UppercaseLetter) _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 0)
      hlocalFromEnum (T.R (LowercaseLetter) _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 1)
      hlocalFromEnum (T.R (TitlecaseLetter) _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 2)
      hlocalFromEnum (T.R (ModifierLetter) _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 3)
      hlocalFromEnum (T.R (OtherLetter) _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 4)
      hlocalFromEnum (T.R (NonSpacingMark) _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 5)
      hlocalFromEnum (T.R (SpacingCombiningMark) _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 6)
      hlocalFromEnum (T.R (EnclosingMark) _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 7)
      hlocalFromEnum (T.R (DecimalNumber) _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 8)
      hlocalFromEnum (T.R (LetterNumber) _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 9)
      hlocalFromEnum (T.R (OtherNumber) _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 10)
      hlocalFromEnum (T.R (ConnectorPunctuation) _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 11)
      hlocalFromEnum (T.R (DashPunctuation) _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 12)
      hlocalFromEnum (T.R (OpenPunctuation) _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 13)
      hlocalFromEnum (T.R (ClosePunctuation) _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 14)
      hlocalFromEnum (T.R (InitialQuote) _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 15)
      hlocalFromEnum (T.R (FinalQuote) _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 16)
      hlocalFromEnum (T.R (OtherPunctuation) _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 17)
      hlocalFromEnum (T.R (MathSymbol) _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 18)
      hlocalFromEnum (T.R (CurrencySymbol) _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 19)
      hlocalFromEnum (T.R (ModifierSymbol) _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 20)
      hlocalFromEnum (T.R (OtherSymbol) _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 21)
      hlocalFromEnum (T.R (Space) _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 22)
      hlocalFromEnum (T.R (LineSeparator) _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 23)
      hlocalFromEnum (T.R (ParagraphSeparator) _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 24)
      hlocalFromEnum (T.R (Control) _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 25)
      hlocalFromEnum (T.R (Format) _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 26)
      hlocalFromEnum (T.R (Surrogate) _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 27)
      hlocalFromEnum (T.R (PrivateUse) _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 28)
      hlocalFromEnum (T.R (NotAssigned) _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 29)
      hlocalFromEnum _ p = T.fatal p
      
    
  

instance Enum (GeneralCategory)
  where
  
  gfromEnum pfromEnum p =
    T.ufun1 a99v28v99v31fromEnum pfromEnum p hfromEnum
    where
    
    hfromEnum (T.R UppercaseLetter _) p =
      T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
        (T.conInteger T.mkNoSrcPos p 0)
    hfromEnum (T.R LowercaseLetter _) p =
      T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
        (T.conInteger T.mkNoSrcPos p 1)
    hfromEnum (T.R TitlecaseLetter _) p =
      T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
        (T.conInteger T.mkNoSrcPos p 2)
    hfromEnum (T.R ModifierLetter _) p =
      T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
        (T.conInteger T.mkNoSrcPos p 3)
    hfromEnum (T.R OtherLetter _) p =
      T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
        (T.conInteger T.mkNoSrcPos p 4)
    hfromEnum (T.R NonSpacingMark _) p =
      T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
        (T.conInteger T.mkNoSrcPos p 5)
    hfromEnum (T.R SpacingCombiningMark _) p =
      T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
        (T.conInteger T.mkNoSrcPos p 6)
    hfromEnum (T.R EnclosingMark _) p =
      T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
        (T.conInteger T.mkNoSrcPos p 7)
    hfromEnum (T.R DecimalNumber _) p =
      T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
        (T.conInteger T.mkNoSrcPos p 8)
    hfromEnum (T.R LetterNumber _) p =
      T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
        (T.conInteger T.mkNoSrcPos p 9)
    hfromEnum (T.R OtherNumber _) p =
      T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
        (T.conInteger T.mkNoSrcPos p 10)
    hfromEnum (T.R ConnectorPunctuation _) p =
      T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
        (T.conInteger T.mkNoSrcPos p 11)
    hfromEnum (T.R DashPunctuation _) p =
      T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
        (T.conInteger T.mkNoSrcPos p 12)
    hfromEnum (T.R OpenPunctuation _) p =
      T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
        (T.conInteger T.mkNoSrcPos p 13)
    hfromEnum (T.R ClosePunctuation _) p =
      T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
        (T.conInteger T.mkNoSrcPos p 14)
    hfromEnum (T.R InitialQuote _) p =
      T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
        (T.conInteger T.mkNoSrcPos p 15)
    hfromEnum (T.R FinalQuote _) p =
      T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
        (T.conInteger T.mkNoSrcPos p 16)
    hfromEnum (T.R OtherPunctuation _) p =
      T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
        (T.conInteger T.mkNoSrcPos p 17)
    hfromEnum (T.R MathSymbol _) p =
      T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
        (T.conInteger T.mkNoSrcPos p 18)
    hfromEnum (T.R CurrencySymbol _) p =
      T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
        (T.conInteger T.mkNoSrcPos p 19)
    hfromEnum (T.R ModifierSymbol _) p =
      T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
        (T.conInteger T.mkNoSrcPos p 20)
    hfromEnum (T.R OtherSymbol _) p =
      T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
        (T.conInteger T.mkNoSrcPos p 21)
    hfromEnum (T.R Space _) p =
      T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
        (T.conInteger T.mkNoSrcPos p 22)
    hfromEnum (T.R LineSeparator _) p =
      T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
        (T.conInteger T.mkNoSrcPos p 23)
    hfromEnum (T.R ParagraphSeparator _) p =
      T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
        (T.conInteger T.mkNoSrcPos p 24)
    hfromEnum (T.R Control _) p =
      T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
        (T.conInteger T.mkNoSrcPos p 25)
    hfromEnum (T.R Format _) p =
      T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
        (T.conInteger T.mkNoSrcPos p 26)
    hfromEnum (T.R Surrogate _) p =
      T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
        (T.conInteger T.mkNoSrcPos p 27)
    hfromEnum (T.R PrivateUse _) p =
      T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
        (T.conInteger T.mkNoSrcPos p 28)
    hfromEnum (T.R NotAssigned _) p =
      T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
        (T.conInteger T.mkNoSrcPos p 29)
    hfromEnum _ p = T.fatal p
    
  
  gtoEnum ptoEnum p =
    T.ufun1 a99v28v99v31toEnum ptoEnum p htoEnum
    where
    
    htoEnum fv99v28v99v31n p =
      T.ucguard
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fv99v28v99v31n
          (T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p 0))) (h p) (y1toEnum fv99v28v99v31n p)
      where
      
      h p = T.con0 T.mkNoSrcPos p UppercaseLetter aUppercaseLetter
      h p = y1toEnum fv99v28v99v31n p
      
    htoEnum fv99v28v99v31n p = y1toEnum fv99v28v99v31n p
    
    y1toEnum fv99v28v99v31n p =
      T.ucguard
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fv99v28v99v31n
          (T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p 1))) (h p) (y2toEnum fv99v28v99v31n p)
      where
      
      h p = T.con0 T.mkNoSrcPos p LowercaseLetter aLowercaseLetter
      h p = y2toEnum fv99v28v99v31n p
      
    y1toEnum fv99v28v99v31n p = y2toEnum fv99v28v99v31n p
    
    y2toEnum fv99v28v99v31n p =
      T.ucguard
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fv99v28v99v31n
          (T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p 2))) (h p) (y3toEnum fv99v28v99v31n p)
      where
      
      h p = T.con0 T.mkNoSrcPos p TitlecaseLetter aTitlecaseLetter
      h p = y3toEnum fv99v28v99v31n p
      
    y2toEnum fv99v28v99v31n p = y3toEnum fv99v28v99v31n p
    
    y3toEnum fv99v28v99v31n p =
      T.ucguard
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fv99v28v99v31n
          (T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p 3))) (h p) (y4toEnum fv99v28v99v31n p)
      where
      
      h p = T.con0 T.mkNoSrcPos p ModifierLetter aModifierLetter
      h p = y4toEnum fv99v28v99v31n p
      
    y3toEnum fv99v28v99v31n p = y4toEnum fv99v28v99v31n p
    
    y4toEnum fv99v28v99v31n p =
      T.ucguard
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fv99v28v99v31n
          (T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p 4))) (h p) (y5toEnum fv99v28v99v31n p)
      where
      
      h p = T.con0 T.mkNoSrcPos p OtherLetter aOtherLetter
      h p = y5toEnum fv99v28v99v31n p
      
    y4toEnum fv99v28v99v31n p = y5toEnum fv99v28v99v31n p
    
    y5toEnum fv99v28v99v31n p =
      T.ucguard
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fv99v28v99v31n
          (T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p 5))) (h p) (y6toEnum fv99v28v99v31n p)
      where
      
      h p = T.con0 T.mkNoSrcPos p NonSpacingMark aNonSpacingMark
      h p = y6toEnum fv99v28v99v31n p
      
    y5toEnum fv99v28v99v31n p = y6toEnum fv99v28v99v31n p
    
    y6toEnum fv99v28v99v31n p =
      T.ucguard
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fv99v28v99v31n
          (T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p 6))) (h p) (y7toEnum fv99v28v99v31n p)
      where
      
      h p = T.con0 T.mkNoSrcPos p SpacingCombiningMark aSpacingCombiningMark
      h p = y7toEnum fv99v28v99v31n p
      
    y6toEnum fv99v28v99v31n p = y7toEnum fv99v28v99v31n p
    
    y7toEnum fv99v28v99v31n p =
      T.ucguard
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fv99v28v99v31n
          (T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p 7))) (h p) (y8toEnum fv99v28v99v31n p)
      where
      
      h p = T.con0 T.mkNoSrcPos p EnclosingMark aEnclosingMark
      h p = y8toEnum fv99v28v99v31n p
      
    y7toEnum fv99v28v99v31n p = y8toEnum fv99v28v99v31n p
    
    y8toEnum fv99v28v99v31n p =
      T.ucguard
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fv99v28v99v31n
          (T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p 8))) (h p) (y9toEnum fv99v28v99v31n p)
      where
      
      h p = T.con0 T.mkNoSrcPos p DecimalNumber aDecimalNumber
      h p = y9toEnum fv99v28v99v31n p
      
    y8toEnum fv99v28v99v31n p = y9toEnum fv99v28v99v31n p
    
    y9toEnum fv99v28v99v31n p =
      T.ucguard
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fv99v28v99v31n
          (T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p 9))) (h p) (y10toEnum fv99v28v99v31n p)
      where
      
      h p = T.con0 T.mkNoSrcPos p LetterNumber aLetterNumber
      h p = y10toEnum fv99v28v99v31n p
      
    y9toEnum fv99v28v99v31n p = y10toEnum fv99v28v99v31n p
    
    y10toEnum fv99v28v99v31n p =
      T.ucguard
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fv99v28v99v31n
          (T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p 10))) (h p)
        (y11toEnum fv99v28v99v31n p)
      where
      
      h p = T.con0 T.mkNoSrcPos p OtherNumber aOtherNumber
      h p = y11toEnum fv99v28v99v31n p
      
    y10toEnum fv99v28v99v31n p = y11toEnum fv99v28v99v31n p
    
    y11toEnum fv99v28v99v31n p =
      T.ucguard
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fv99v28v99v31n
          (T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p 11))) (h p)
        (y12toEnum fv99v28v99v31n p)
      where
      
      h p = T.con0 T.mkNoSrcPos p ConnectorPunctuation aConnectorPunctuation
      h p = y12toEnum fv99v28v99v31n p
      
    y11toEnum fv99v28v99v31n p = y12toEnum fv99v28v99v31n p
    
    y12toEnum fv99v28v99v31n p =
      T.ucguard
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fv99v28v99v31n
          (T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p 12))) (h p)
        (y13toEnum fv99v28v99v31n p)
      where
      
      h p = T.con0 T.mkNoSrcPos p DashPunctuation aDashPunctuation
      h p = y13toEnum fv99v28v99v31n p
      
    y12toEnum fv99v28v99v31n p = y13toEnum fv99v28v99v31n p
    
    y13toEnum fv99v28v99v31n p =
      T.ucguard
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fv99v28v99v31n
          (T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p 13))) (h p)
        (y14toEnum fv99v28v99v31n p)
      where
      
      h p = T.con0 T.mkNoSrcPos p OpenPunctuation aOpenPunctuation
      h p = y14toEnum fv99v28v99v31n p
      
    y13toEnum fv99v28v99v31n p = y14toEnum fv99v28v99v31n p
    
    y14toEnum fv99v28v99v31n p =
      T.ucguard
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fv99v28v99v31n
          (T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p 14))) (h p)
        (y15toEnum fv99v28v99v31n p)
      where
      
      h p = T.con0 T.mkNoSrcPos p ClosePunctuation aClosePunctuation
      h p = y15toEnum fv99v28v99v31n p
      
    y14toEnum fv99v28v99v31n p = y15toEnum fv99v28v99v31n p
    
    y15toEnum fv99v28v99v31n p =
      T.ucguard
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fv99v28v99v31n
          (T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p 15))) (h p)
        (y16toEnum fv99v28v99v31n p)
      where
      
      h p = T.con0 T.mkNoSrcPos p InitialQuote aInitialQuote
      h p = y16toEnum fv99v28v99v31n p
      
    y15toEnum fv99v28v99v31n p = y16toEnum fv99v28v99v31n p
    
    y16toEnum fv99v28v99v31n p =
      T.ucguard
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fv99v28v99v31n
          (T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p 16))) (h p)
        (y17toEnum fv99v28v99v31n p)
      where
      
      h p = T.con0 T.mkNoSrcPos p FinalQuote aFinalQuote
      h p = y17toEnum fv99v28v99v31n p
      
    y16toEnum fv99v28v99v31n p = y17toEnum fv99v28v99v31n p
    
    y17toEnum fv99v28v99v31n p =
      T.ucguard
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fv99v28v99v31n
          (T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p 17))) (h p)
        (y18toEnum fv99v28v99v31n p)
      where
      
      h p = T.con0 T.mkNoSrcPos p OtherPunctuation aOtherPunctuation
      h p = y18toEnum fv99v28v99v31n p
      
    y17toEnum fv99v28v99v31n p = y18toEnum fv99v28v99v31n p
    
    y18toEnum fv99v28v99v31n p =
      T.ucguard
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fv99v28v99v31n
          (T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p 18))) (h p)
        (y19toEnum fv99v28v99v31n p)
      where
      
      h p = T.con0 T.mkNoSrcPos p MathSymbol aMathSymbol
      h p = y19toEnum fv99v28v99v31n p
      
    y18toEnum fv99v28v99v31n p = y19toEnum fv99v28v99v31n p
    
    y19toEnum fv99v28v99v31n p =
      T.ucguard
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fv99v28v99v31n
          (T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p 19))) (h p)
        (y20toEnum fv99v28v99v31n p)
      where
      
      h p = T.con0 T.mkNoSrcPos p CurrencySymbol aCurrencySymbol
      h p = y20toEnum fv99v28v99v31n p
      
    y19toEnum fv99v28v99v31n p = y20toEnum fv99v28v99v31n p
    
    y20toEnum fv99v28v99v31n p =
      T.ucguard
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fv99v28v99v31n
          (T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p 20))) (h p)
        (y21toEnum fv99v28v99v31n p)
      where
      
      h p = T.con0 T.mkNoSrcPos p ModifierSymbol aModifierSymbol
      h p = y21toEnum fv99v28v99v31n p
      
    y20toEnum fv99v28v99v31n p = y21toEnum fv99v28v99v31n p
    
    y21toEnum fv99v28v99v31n p =
      T.ucguard
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fv99v28v99v31n
          (T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p 21))) (h p)
        (y22toEnum fv99v28v99v31n p)
      where
      
      h p = T.con0 T.mkNoSrcPos p OtherSymbol aOtherSymbol
      h p = y22toEnum fv99v28v99v31n p
      
    y21toEnum fv99v28v99v31n p = y22toEnum fv99v28v99v31n p
    
    y22toEnum fv99v28v99v31n p =
      T.ucguard
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fv99v28v99v31n
          (T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p 22))) (h p)
        (y23toEnum fv99v28v99v31n p)
      where
      
      h p = T.con0 T.mkNoSrcPos p Space aSpace
      h p = y23toEnum fv99v28v99v31n p
      
    y22toEnum fv99v28v99v31n p = y23toEnum fv99v28v99v31n p
    
    y23toEnum fv99v28v99v31n p =
      T.ucguard
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fv99v28v99v31n
          (T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p 23))) (h p)
        (y24toEnum fv99v28v99v31n p)
      where
      
      h p = T.con0 T.mkNoSrcPos p LineSeparator aLineSeparator
      h p = y24toEnum fv99v28v99v31n p
      
    y23toEnum fv99v28v99v31n p = y24toEnum fv99v28v99v31n p
    
    y24toEnum fv99v28v99v31n p =
      T.ucguard
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fv99v28v99v31n
          (T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p 24))) (h p)
        (y25toEnum fv99v28v99v31n p)
      where
      
      h p = T.con0 T.mkNoSrcPos p ParagraphSeparator aParagraphSeparator
      h p = y25toEnum fv99v28v99v31n p
      
    y24toEnum fv99v28v99v31n p = y25toEnum fv99v28v99v31n p
    
    y25toEnum fv99v28v99v31n p =
      T.ucguard
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fv99v28v99v31n
          (T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p 25))) (h p)
        (y26toEnum fv99v28v99v31n p)
      where
      
      h p = T.con0 T.mkNoSrcPos p Control aControl
      h p = y26toEnum fv99v28v99v31n p
      
    y25toEnum fv99v28v99v31n p = y26toEnum fv99v28v99v31n p
    
    y26toEnum fv99v28v99v31n p =
      T.ucguard
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fv99v28v99v31n
          (T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p 26))) (h p)
        (y27toEnum fv99v28v99v31n p)
      where
      
      h p = T.con0 T.mkNoSrcPos p Format aFormat
      h p = y27toEnum fv99v28v99v31n p
      
    y26toEnum fv99v28v99v31n p = y27toEnum fv99v28v99v31n p
    
    y27toEnum fv99v28v99v31n p =
      T.ucguard
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fv99v28v99v31n
          (T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p 27))) (h p)
        (y28toEnum fv99v28v99v31n p)
      where
      
      h p = T.con0 T.mkNoSrcPos p Surrogate aSurrogate
      h p = y28toEnum fv99v28v99v31n p
      
    y27toEnum fv99v28v99v31n p = y28toEnum fv99v28v99v31n p
    
    y28toEnum fv99v28v99v31n p =
      T.ucguard
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fv99v28v99v31n
          (T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p 28))) (h p)
        (y29toEnum fv99v28v99v31n p)
      where
      
      h p = T.con0 T.mkNoSrcPos p PrivateUse aPrivateUse
      h p = y29toEnum fv99v28v99v31n p
      
    y28toEnum fv99v28v99v31n p = y29toEnum fv99v28v99v31n p
    
    y29toEnum fv99v28v99v31n p =
      T.ucguard
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fv99v28v99v31n
          (T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p 29))) (h p)
        (y30toEnum fv99v28v99v31n p)
      where
      
      h p = T.con0 T.mkNoSrcPos p NotAssigned aNotAssigned
      h p = y30toEnum fv99v28v99v31n p
      
    y29toEnum fv99v28v99v31n p = y30toEnum fv99v28v99v31n p
    
    y30toEnum _ p =
      T.uwrapForward p
        (Hat.Prelude.herror
          (T.fromLitString T.mkNoSrcPos p "toEnum: argument out of bounds") p)
    
  
  genumFrom penumFrom p =
    T.ufun1 a99v28v99v31enumFrom penumFrom p henumFrom
    where
    
    henumFrom fy1 p =
      T.uap2 T.mkNoSrcPos p (Hat.Prelude.genumFromTo T.mkNoSrcPos p) fy1
        (T.con0 T.mkNoSrcPos p NotAssigned aNotAssigned)
    
  
  genumFromThen penumFromThen p =
    T.ufun2 a99v28v99v31enumFromThen penumFromThen p henumFromThen
    where
    
    henumFromThen fy1 fy2 p =
      T.uap3 T.mkNoSrcPos p (Hat.Prelude.genumFromThenTo T.mkNoSrcPos p) fy1 fy2
        (T.ucif p
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!>= p)
            (T.uap1 T.mkNoSrcPos p (Hat.Prelude.gfromEnum T.mkNoSrcPos p) fy1)
            (T.uap1 T.mkNoSrcPos p (Hat.Prelude.gfromEnum T.mkNoSrcPos p) fy2))
          (T.con0 T.mkNoSrcPos p NotAssigned aNotAssigned)
          (T.con0 T.mkNoSrcPos p UppercaseLetter aUppercaseLetter))
    
  

instance Read (GeneralCategory)
  where
  
  greadsPrec preadsPrec p =
    T.ufun1 a99v34v99v37readsPrec preadsPrec p hreadsPrec
    where
    
    hreadsPrec fy1 p =
      T.uap2 T.mkNoSrcPos p (Hat.PreludeBasic.galt T.mkNoSrcPos p)
        (T.uwrapForward p
          (Hat.Prelude.hreadParen
            (T.con0 T.mkNoSrcPos p Hat.Prelude.False Hat.Prelude.aFalse)
            (T.uwrapForward p
              (Hat.PreludeBasic.hthenLex
                (T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gyield T.mkNoSrcPos p)
                  (T.con0 T.mkNoSrcPos p UppercaseLetter aUppercaseLetter))
                (T.fromLitString T.mkNoSrcPos p "UppercaseLetter") p)) p))
        (T.uap2 T.mkNoSrcPos p (Hat.PreludeBasic.galt T.mkNoSrcPos p)
          (T.uwrapForward p
            (Hat.Prelude.hreadParen
              (T.con0 T.mkNoSrcPos p Hat.Prelude.False Hat.Prelude.aFalse)
              (T.uwrapForward p
                (Hat.PreludeBasic.hthenLex
                  (T.uap1 T.mkNoSrcPos p
                    (Hat.PreludeBasic.gyield T.mkNoSrcPos p)
                    (T.con0 T.mkNoSrcPos p LowercaseLetter aLowercaseLetter))
                  (T.fromLitString T.mkNoSrcPos p "LowercaseLetter") p)) p))
          (T.uap2 T.mkNoSrcPos p (Hat.PreludeBasic.galt T.mkNoSrcPos p)
            (T.uwrapForward p
              (Hat.Prelude.hreadParen
                (T.con0 T.mkNoSrcPos p Hat.Prelude.False Hat.Prelude.aFalse)
                (T.uwrapForward p
                  (Hat.PreludeBasic.hthenLex
                    (T.uap1 T.mkNoSrcPos p
                      (Hat.PreludeBasic.gyield T.mkNoSrcPos p)
                      (T.con0 T.mkNoSrcPos p TitlecaseLetter aTitlecaseLetter))
                    (T.fromLitString T.mkNoSrcPos p "TitlecaseLetter") p)) p))
            (T.uap2 T.mkNoSrcPos p (Hat.PreludeBasic.galt T.mkNoSrcPos p)
              (T.uwrapForward p
                (Hat.Prelude.hreadParen
                  (T.con0 T.mkNoSrcPos p Hat.Prelude.False Hat.Prelude.aFalse)
                  (T.uwrapForward p
                    (Hat.PreludeBasic.hthenLex
                      (T.uap1 T.mkNoSrcPos p
                        (Hat.PreludeBasic.gyield T.mkNoSrcPos p)
                        (T.con0 T.mkNoSrcPos p ModifierLetter aModifierLetter))
                      (T.fromLitString T.mkNoSrcPos p "ModifierLetter") p)) p))
              (T.uap2 T.mkNoSrcPos p (Hat.PreludeBasic.galt T.mkNoSrcPos p)
                (T.uwrapForward p
                  (Hat.Prelude.hreadParen
                    (T.con0 T.mkNoSrcPos p Hat.Prelude.False Hat.Prelude.aFalse)
                    (T.uwrapForward p
                      (Hat.PreludeBasic.hthenLex
                        (T.uap1 T.mkNoSrcPos p
                          (Hat.PreludeBasic.gyield T.mkNoSrcPos p)
                          (T.con0 T.mkNoSrcPos p OtherLetter aOtherLetter))
                        (T.fromLitString T.mkNoSrcPos p "OtherLetter") p)) p))
                (T.uap2 T.mkNoSrcPos p (Hat.PreludeBasic.galt T.mkNoSrcPos p)
                  (T.uwrapForward p
                    (Hat.Prelude.hreadParen
                      (T.con0 T.mkNoSrcPos p Hat.Prelude.False
                        Hat.Prelude.aFalse)
                      (T.uwrapForward p
                        (Hat.PreludeBasic.hthenLex
                          (T.uap1 T.mkNoSrcPos p
                            (Hat.PreludeBasic.gyield T.mkNoSrcPos p)
                            (T.con0 T.mkNoSrcPos p NonSpacingMark
                              aNonSpacingMark))
                          (T.fromLitString T.mkNoSrcPos p "NonSpacingMark") p))
                      p))
                  (T.uap2 T.mkNoSrcPos p (Hat.PreludeBasic.galt T.mkNoSrcPos p)
                    (T.uwrapForward p
                      (Hat.Prelude.hreadParen
                        (T.con0 T.mkNoSrcPos p Hat.Prelude.False
                          Hat.Prelude.aFalse)
                        (T.uwrapForward p
                          (Hat.PreludeBasic.hthenLex
                            (T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gyield T.mkNoSrcPos p)
                              (T.con0 T.mkNoSrcPos p SpacingCombiningMark
                                aSpacingCombiningMark))
                            (T.fromLitString T.mkNoSrcPos p
                              "SpacingCombiningMark") p)) p))
                    (T.uap2 T.mkNoSrcPos p
                      (Hat.PreludeBasic.galt T.mkNoSrcPos p)
                      (T.uwrapForward p
                        (Hat.Prelude.hreadParen
                          (T.con0 T.mkNoSrcPos p Hat.Prelude.False
                            Hat.Prelude.aFalse)
                          (T.uwrapForward p
                            (Hat.PreludeBasic.hthenLex
                              (T.uap1 T.mkNoSrcPos p
                                (Hat.PreludeBasic.gyield T.mkNoSrcPos p)
                                (T.con0 T.mkNoSrcPos p EnclosingMark
                                  aEnclosingMark))
                              (T.fromLitString T.mkNoSrcPos p "EnclosingMark")
                              p)) p))
                      (T.uap2 T.mkNoSrcPos p
                        (Hat.PreludeBasic.galt T.mkNoSrcPos p)
                        (T.uwrapForward p
                          (Hat.Prelude.hreadParen
                            (T.con0 T.mkNoSrcPos p Hat.Prelude.False
                              Hat.Prelude.aFalse)
                            (T.uwrapForward p
                              (Hat.PreludeBasic.hthenLex
                                (T.uap1 T.mkNoSrcPos p
                                  (Hat.PreludeBasic.gyield T.mkNoSrcPos p)
                                  (T.con0 T.mkNoSrcPos p DecimalNumber
                                    aDecimalNumber))
                                (T.fromLitString T.mkNoSrcPos p "DecimalNumber")
                                p)) p))
                        (T.uap2 T.mkNoSrcPos p
                          (Hat.PreludeBasic.galt T.mkNoSrcPos p)
                          (T.uwrapForward p
                            (Hat.Prelude.hreadParen
                              (T.con0 T.mkNoSrcPos p Hat.Prelude.False
                                Hat.Prelude.aFalse)
                              (T.uwrapForward p
                                (Hat.PreludeBasic.hthenLex
                                  (T.uap1 T.mkNoSrcPos p
                                    (Hat.PreludeBasic.gyield T.mkNoSrcPos p)
                                    (T.con0 T.mkNoSrcPos p LetterNumber
                                      aLetterNumber))
                                  (T.fromLitString T.mkNoSrcPos p
                                    "LetterNumber") p)) p))
                          (T.uap2 T.mkNoSrcPos p
                            (Hat.PreludeBasic.galt T.mkNoSrcPos p)
                            (T.uwrapForward p
                              (Hat.Prelude.hreadParen
                                (T.con0 T.mkNoSrcPos p Hat.Prelude.False
                                  Hat.Prelude.aFalse)
                                (T.uwrapForward p
                                  (Hat.PreludeBasic.hthenLex
                                    (T.uap1 T.mkNoSrcPos p
                                      (Hat.PreludeBasic.gyield T.mkNoSrcPos p)
                                      (T.con0 T.mkNoSrcPos p OtherNumber
                                        aOtherNumber))
                                    (T.fromLitString T.mkNoSrcPos p
                                      "OtherNumber") p)) p))
                            (T.uap2 T.mkNoSrcPos p
                              (Hat.PreludeBasic.galt T.mkNoSrcPos p)
                              (T.uwrapForward p
                                (Hat.Prelude.hreadParen
                                  (T.con0 T.mkNoSrcPos p Hat.Prelude.False
                                    Hat.Prelude.aFalse)
                                  (T.uwrapForward p
                                    (Hat.PreludeBasic.hthenLex
                                      (T.uap1 T.mkNoSrcPos p
                                        (Hat.PreludeBasic.gyield T.mkNoSrcPos p)
                                        (T.con0 T.mkNoSrcPos p
                                          ConnectorPunctuation
                                          aConnectorPunctuation))
                                      (T.fromLitString T.mkNoSrcPos p
                                        "ConnectorPunctuation") p)) p))
                              (T.uap2 T.mkNoSrcPos p
                                (Hat.PreludeBasic.galt T.mkNoSrcPos p)
                                (T.uwrapForward p
                                  (Hat.Prelude.hreadParen
                                    (T.con0 T.mkNoSrcPos p Hat.Prelude.False
                                      Hat.Prelude.aFalse)
                                    (T.uwrapForward p
                                      (Hat.PreludeBasic.hthenLex
                                        (T.uap1 T.mkNoSrcPos p
                                          (Hat.PreludeBasic.gyield T.mkNoSrcPos
                                            p)
                                          (T.con0 T.mkNoSrcPos p DashPunctuation
                                            aDashPunctuation))
                                        (T.fromLitString T.mkNoSrcPos p
                                          "DashPunctuation") p)) p))
                                (T.uap2 T.mkNoSrcPos p
                                  (Hat.PreludeBasic.galt T.mkNoSrcPos p)
                                  (T.uwrapForward p
                                    (Hat.Prelude.hreadParen
                                      (T.con0 T.mkNoSrcPos p Hat.Prelude.False
                                        Hat.Prelude.aFalse)
                                      (T.uwrapForward p
                                        (Hat.PreludeBasic.hthenLex
                                          (T.uap1 T.mkNoSrcPos p
                                            (Hat.PreludeBasic.gyield
                                              T.mkNoSrcPos p)
                                            (T.con0 T.mkNoSrcPos p
                                              OpenPunctuation aOpenPunctuation))
                                          (T.fromLitString T.mkNoSrcPos p
                                            "OpenPunctuation") p)) p))
                                  (T.uap2 T.mkNoSrcPos p
                                    (Hat.PreludeBasic.galt T.mkNoSrcPos p)
                                    (T.uwrapForward p
                                      (Hat.Prelude.hreadParen
                                        (T.con0 T.mkNoSrcPos p Hat.Prelude.False
                                          Hat.Prelude.aFalse)
                                        (T.uwrapForward p
                                          (Hat.PreludeBasic.hthenLex
                                            (T.uap1 T.mkNoSrcPos p
                                              (Hat.PreludeBasic.gyield
                                                T.mkNoSrcPos p)
                                              (T.con0 T.mkNoSrcPos p
                                                ClosePunctuation
                                                aClosePunctuation))
                                            (T.fromLitString T.mkNoSrcPos p
                                              "ClosePunctuation") p)) p))
                                    (T.uap2 T.mkNoSrcPos p
                                      (Hat.PreludeBasic.galt T.mkNoSrcPos p)
                                      (T.uwrapForward p
                                        (Hat.Prelude.hreadParen
                                          (T.con0 T.mkNoSrcPos p
                                            Hat.Prelude.False
                                            Hat.Prelude.aFalse)
                                          (T.uwrapForward p
                                            (Hat.PreludeBasic.hthenLex
                                              (T.uap1 T.mkNoSrcPos p
                                                (Hat.PreludeBasic.gyield
                                                  T.mkNoSrcPos p)
                                                (T.con0 T.mkNoSrcPos p
                                                  InitialQuote aInitialQuote))
                                              (T.fromLitString T.mkNoSrcPos p
                                                "InitialQuote") p)) p))
                                      (T.uap2 T.mkNoSrcPos p
                                        (Hat.PreludeBasic.galt T.mkNoSrcPos p)
                                        (T.uwrapForward p
                                          (Hat.Prelude.hreadParen
                                            (T.con0 T.mkNoSrcPos p
                                              Hat.Prelude.False
                                              Hat.Prelude.aFalse)
                                            (T.uwrapForward p
                                              (Hat.PreludeBasic.hthenLex
                                                (T.uap1 T.mkNoSrcPos p
                                                  (Hat.PreludeBasic.gyield
                                                    T.mkNoSrcPos p)
                                                  (T.con0 T.mkNoSrcPos p
                                                    FinalQuote aFinalQuote))
                                                (T.fromLitString T.mkNoSrcPos p
                                                  "FinalQuote") p)) p))
                                        (T.uap2 T.mkNoSrcPos p
                                          (Hat.PreludeBasic.galt T.mkNoSrcPos p)
                                          (T.uwrapForward p
                                            (Hat.Prelude.hreadParen
                                              (T.con0 T.mkNoSrcPos p
                                                Hat.Prelude.False
                                                Hat.Prelude.aFalse)
                                              (T.uwrapForward p
                                                (Hat.PreludeBasic.hthenLex
                                                  (T.uap1 T.mkNoSrcPos p
                                                    (Hat.PreludeBasic.gyield
                                                      T.mkNoSrcPos p)
                                                    (T.con0 T.mkNoSrcPos p
                                                      OtherPunctuation
                                                      aOtherPunctuation))
                                                  (T.fromLitString T.mkNoSrcPos
                                                    p "OtherPunctuation") p))
                                              p))
                                          (T.uap2 T.mkNoSrcPos p
                                            (Hat.PreludeBasic.galt T.mkNoSrcPos
                                              p)
                                            (T.uwrapForward p
                                              (Hat.Prelude.hreadParen
                                                (T.con0 T.mkNoSrcPos p
                                                  Hat.Prelude.False
                                                  Hat.Prelude.aFalse)
                                                (T.uwrapForward p
                                                  (Hat.PreludeBasic.hthenLex
                                                    (T.uap1 T.mkNoSrcPos p
                                                      (Hat.PreludeBasic.gyield
                                                        T.mkNoSrcPos p)
                                                      (T.con0 T.mkNoSrcPos p
                                                        MathSymbol aMathSymbol))
                                                    (T.fromLitString
                                                      T.mkNoSrcPos p
                                                      "MathSymbol") p)) p))
                                            (T.uap2 T.mkNoSrcPos p
                                              (Hat.PreludeBasic.galt
                                                T.mkNoSrcPos p)
                                              (T.uwrapForward p
                                                (Hat.Prelude.hreadParen
                                                  (T.con0 T.mkNoSrcPos p
                                                    Hat.Prelude.False
                                                    Hat.Prelude.aFalse)
                                                  (T.uwrapForward p
                                                    (Hat.PreludeBasic.hthenLex
                                                      (T.uap1 T.mkNoSrcPos p
                                                        (Hat.PreludeBasic.gyield
                                                          T.mkNoSrcPos p)
                                                        (T.con0 T.mkNoSrcPos p
                                                          CurrencySymbol
                                                          aCurrencySymbol))
                                                      (T.fromLitString
                                                        T.mkNoSrcPos p
                                                        "CurrencySymbol") p))
                                                  p))
                                              (T.uap2 T.mkNoSrcPos p
                                                (Hat.PreludeBasic.galt
                                                  T.mkNoSrcPos p)
                                                (T.uwrapForward p
                                                  (Hat.Prelude.hreadParen
                                                    (T.con0 T.mkNoSrcPos p
                                                      Hat.Prelude.False
                                                      Hat.Prelude.aFalse)
                                                    (T.uwrapForward p
                                                      (Hat.PreludeBasic.hthenLex
                                                        (T.uap1 T.mkNoSrcPos p
                                                          (Hat.PreludeBasic.gyield
                                                            T.mkNoSrcPos p)
                                                          (T.con0 T.mkNoSrcPos p
                                                            ModifierSymbol
                                                            aModifierSymbol))
                                                        (T.fromLitString
                                                          T.mkNoSrcPos p
                                                          "ModifierSymbol") p))
                                                    p))
                                                (T.uap2 T.mkNoSrcPos p
                                                  (Hat.PreludeBasic.galt
                                                    T.mkNoSrcPos p)
                                                  (T.uwrapForward p
                                                    (Hat.Prelude.hreadParen
                                                      (T.con0 T.mkNoSrcPos p
                                                        Hat.Prelude.False
                                                        Hat.Prelude.aFalse)
                                                      (T.uwrapForward p
                                                        (Hat.PreludeBasic.hthenLex
                                                          (T.uap1 T.mkNoSrcPos p
                                                            (Hat.PreludeBasic.gyield
                                                              T.mkNoSrcPos p)
                                                            (T.con0 T.mkNoSrcPos
                                                              p OtherSymbol
                                                              aOtherSymbol))
                                                          (T.fromLitString
                                                            T.mkNoSrcPos p
                                                            "OtherSymbol") p))
                                                      p))
                                                  (T.uap2 T.mkNoSrcPos p
                                                    (Hat.PreludeBasic.galt
                                                      T.mkNoSrcPos p)
                                                    (T.uwrapForward p
                                                      (Hat.Prelude.hreadParen
                                                        (T.con0 T.mkNoSrcPos p
                                                          Hat.Prelude.False
                                                          Hat.Prelude.aFalse)
                                                        (T.uwrapForward p
                                                          (Hat.PreludeBasic.hthenLex
                                                            (T.uap1 T.mkNoSrcPos
                                                              p
                                                              (Hat.PreludeBasic.gyield
                                                                T.mkNoSrcPos p)
                                                              (T.con0
                                                                T.mkNoSrcPos p
                                                                Space aSpace))
                                                            (T.fromLitString
                                                              T.mkNoSrcPos p
                                                              "Space") p)) p))
                                                    (T.uap2 T.mkNoSrcPos p
                                                      (Hat.PreludeBasic.galt
                                                        T.mkNoSrcPos p)
                                                      (T.uwrapForward p
                                                        (Hat.Prelude.hreadParen
                                                          (T.con0 T.mkNoSrcPos p
                                                            Hat.Prelude.False
                                                            Hat.Prelude.aFalse)
                                                          (T.uwrapForward p
                                                            (Hat.PreludeBasic.hthenLex
                                                              (T.uap1
                                                                T.mkNoSrcPos p
                                                                (Hat.PreludeBasic.gyield
                                                                  T.mkNoSrcPos
                                                                  p)
                                                                (T.con0
                                                                  T.mkNoSrcPos p
                                                                  LineSeparator
                                                                  aLineSeparator))
                                                              (T.fromLitString
                                                                T.mkNoSrcPos p
                                                                "LineSeparator")
                                                              p)) p))
                                                      (T.uap2 T.mkNoSrcPos p
                                                        (Hat.PreludeBasic.galt
                                                          T.mkNoSrcPos p)
                                                        (T.uwrapForward p
                                                          (Hat.Prelude.hreadParen
                                                            (T.con0 T.mkNoSrcPos
                                                              p
                                                              Hat.Prelude.False
                                                              Hat.Prelude.aFalse)
                                                            (T.uwrapForward p
                                                              (Hat.PreludeBasic.hthenLex
                                                                (T.uap1
                                                                  T.mkNoSrcPos p
                                                                  (Hat.PreludeBasic.gyield
                                                                    T.mkNoSrcPos
                                                                    p)
                                                                  (T.con0
                                                                    T.mkNoSrcPos
                                                                    p
                                                                    ParagraphSeparator
                                                                    aParagraphSeparator))
                                                                (T.fromLitString
                                                                  T.mkNoSrcPos p
                                                                  "ParagraphSeparator")
                                                                p)) p))
                                                        (T.uap2 T.mkNoSrcPos p
                                                          (Hat.PreludeBasic.galt
                                                            T.mkNoSrcPos p)
                                                          (T.uwrapForward p
                                                            (Hat.Prelude.hreadParen
                                                              (T.con0
                                                                T.mkNoSrcPos p
                                                                Hat.Prelude.False
                                                                Hat.Prelude.aFalse)
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
                                                                      p Control
                                                                      aControl))
                                                                  (T.fromLitString
                                                                    T.mkNoSrcPos
                                                                    p "Control")
                                                                  p)) p))
                                                          (T.uap2 T.mkNoSrcPos p
                                                            (Hat.PreludeBasic.galt
                                                              T.mkNoSrcPos p)
                                                            (T.uwrapForward p
                                                              (Hat.Prelude.hreadParen
                                                                (T.con0
                                                                  T.mkNoSrcPos p
                                                                  Hat.Prelude.False
                                                                  Hat.Prelude.aFalse)
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
                                                                        p Format
                                                                        aFormat))
                                                                    (T.fromLitString
                                                                      T.mkNoSrcPos
                                                                      p
                                                                      "Format")
                                                                    p)) p))
                                                            (T.uap2 T.mkNoSrcPos
                                                              p
                                                              (Hat.PreludeBasic.galt
                                                                T.mkNoSrcPos p)
                                                              (T.uwrapForward p
                                                                (Hat.Prelude.hreadParen
                                                                  (T.con0
                                                                    T.mkNoSrcPos
                                                                    p
                                                                    Hat.Prelude.False
                                                                    Hat.Prelude.aFalse)
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
                                                                      p)) p))
                                                              (T.uap2
                                                                T.mkNoSrcPos p
                                                                (Hat.PreludeBasic.galt
                                                                  T.mkNoSrcPos
                                                                  p)
                                                                (T.uwrapForward
                                                                  p
                                                                  (Hat.Prelude.hreadParen
                                                                    (T.con0
                                                                      T.mkNoSrcPos
                                                                      p
                                                                      Hat.Prelude.False
                                                                      Hat.Prelude.aFalse)
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
                                                                        p)) p))
                                                                (T.uwrapForward
                                                                  p
                                                                  (Hat.Prelude.hreadParen
                                                                    (T.con0
                                                                      T.mkNoSrcPos
                                                                      p
                                                                      Hat.Prelude.False
                                                                      Hat.Prelude.aFalse)
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
    
  

instance Show (GeneralCategory)
  where
  
  gshowsPrec pshowsPrec p =
    T.ufun2 a99v40v99v43showsPrec pshowsPrec p hshowsPrec
    where
    
    hshowsPrec fy1 (T.R (UppercaseLetter) _) p =
      T.uap1 T.mkNoSrcPos p (Hat.Prelude.gshowString T.mkNoSrcPos p)
        (T.fromLitString T.mkNoSrcPos p "UppercaseLetter")
    hshowsPrec fy1 (T.R (LowercaseLetter) _) p =
      T.uap1 T.mkNoSrcPos p (Hat.Prelude.gshowString T.mkNoSrcPos p)
        (T.fromLitString T.mkNoSrcPos p "LowercaseLetter")
    hshowsPrec fy1 (T.R (TitlecaseLetter) _) p =
      T.uap1 T.mkNoSrcPos p (Hat.Prelude.gshowString T.mkNoSrcPos p)
        (T.fromLitString T.mkNoSrcPos p "TitlecaseLetter")
    hshowsPrec fy1 (T.R (ModifierLetter) _) p =
      T.uap1 T.mkNoSrcPos p (Hat.Prelude.gshowString T.mkNoSrcPos p)
        (T.fromLitString T.mkNoSrcPos p "ModifierLetter")
    hshowsPrec fy1 (T.R (OtherLetter) _) p =
      T.uap1 T.mkNoSrcPos p (Hat.Prelude.gshowString T.mkNoSrcPos p)
        (T.fromLitString T.mkNoSrcPos p "OtherLetter")
    hshowsPrec fy1 (T.R (NonSpacingMark) _) p =
      T.uap1 T.mkNoSrcPos p (Hat.Prelude.gshowString T.mkNoSrcPos p)
        (T.fromLitString T.mkNoSrcPos p "NonSpacingMark")
    hshowsPrec fy1 (T.R (SpacingCombiningMark) _) p =
      T.uap1 T.mkNoSrcPos p (Hat.Prelude.gshowString T.mkNoSrcPos p)
        (T.fromLitString T.mkNoSrcPos p "SpacingCombiningMark")
    hshowsPrec fy1 (T.R (EnclosingMark) _) p =
      T.uap1 T.mkNoSrcPos p (Hat.Prelude.gshowString T.mkNoSrcPos p)
        (T.fromLitString T.mkNoSrcPos p "EnclosingMark")
    hshowsPrec fy1 (T.R (DecimalNumber) _) p =
      T.uap1 T.mkNoSrcPos p (Hat.Prelude.gshowString T.mkNoSrcPos p)
        (T.fromLitString T.mkNoSrcPos p "DecimalNumber")
    hshowsPrec fy1 (T.R (LetterNumber) _) p =
      T.uap1 T.mkNoSrcPos p (Hat.Prelude.gshowString T.mkNoSrcPos p)
        (T.fromLitString T.mkNoSrcPos p "LetterNumber")
    hshowsPrec fy1 (T.R (OtherNumber) _) p =
      T.uap1 T.mkNoSrcPos p (Hat.Prelude.gshowString T.mkNoSrcPos p)
        (T.fromLitString T.mkNoSrcPos p "OtherNumber")
    hshowsPrec fy1 (T.R (ConnectorPunctuation) _) p =
      T.uap1 T.mkNoSrcPos p (Hat.Prelude.gshowString T.mkNoSrcPos p)
        (T.fromLitString T.mkNoSrcPos p "ConnectorPunctuation")
    hshowsPrec fy1 (T.R (DashPunctuation) _) p =
      T.uap1 T.mkNoSrcPos p (Hat.Prelude.gshowString T.mkNoSrcPos p)
        (T.fromLitString T.mkNoSrcPos p "DashPunctuation")
    hshowsPrec fy1 (T.R (OpenPunctuation) _) p =
      T.uap1 T.mkNoSrcPos p (Hat.Prelude.gshowString T.mkNoSrcPos p)
        (T.fromLitString T.mkNoSrcPos p "OpenPunctuation")
    hshowsPrec fy1 (T.R (ClosePunctuation) _) p =
      T.uap1 T.mkNoSrcPos p (Hat.Prelude.gshowString T.mkNoSrcPos p)
        (T.fromLitString T.mkNoSrcPos p "ClosePunctuation")
    hshowsPrec fy1 (T.R (InitialQuote) _) p =
      T.uap1 T.mkNoSrcPos p (Hat.Prelude.gshowString T.mkNoSrcPos p)
        (T.fromLitString T.mkNoSrcPos p "InitialQuote")
    hshowsPrec fy1 (T.R (FinalQuote) _) p =
      T.uap1 T.mkNoSrcPos p (Hat.Prelude.gshowString T.mkNoSrcPos p)
        (T.fromLitString T.mkNoSrcPos p "FinalQuote")
    hshowsPrec fy1 (T.R (OtherPunctuation) _) p =
      T.uap1 T.mkNoSrcPos p (Hat.Prelude.gshowString T.mkNoSrcPos p)
        (T.fromLitString T.mkNoSrcPos p "OtherPunctuation")
    hshowsPrec fy1 (T.R (MathSymbol) _) p =
      T.uap1 T.mkNoSrcPos p (Hat.Prelude.gshowString T.mkNoSrcPos p)
        (T.fromLitString T.mkNoSrcPos p "MathSymbol")
    hshowsPrec fy1 (T.R (CurrencySymbol) _) p =
      T.uap1 T.mkNoSrcPos p (Hat.Prelude.gshowString T.mkNoSrcPos p)
        (T.fromLitString T.mkNoSrcPos p "CurrencySymbol")
    hshowsPrec fy1 (T.R (ModifierSymbol) _) p =
      T.uap1 T.mkNoSrcPos p (Hat.Prelude.gshowString T.mkNoSrcPos p)
        (T.fromLitString T.mkNoSrcPos p "ModifierSymbol")
    hshowsPrec fy1 (T.R (OtherSymbol) _) p =
      T.uap1 T.mkNoSrcPos p (Hat.Prelude.gshowString T.mkNoSrcPos p)
        (T.fromLitString T.mkNoSrcPos p "OtherSymbol")
    hshowsPrec fy1 (T.R (Space) _) p =
      T.uap1 T.mkNoSrcPos p (Hat.Prelude.gshowString T.mkNoSrcPos p)
        (T.fromLitString T.mkNoSrcPos p "Space")
    hshowsPrec fy1 (T.R (LineSeparator) _) p =
      T.uap1 T.mkNoSrcPos p (Hat.Prelude.gshowString T.mkNoSrcPos p)
        (T.fromLitString T.mkNoSrcPos p "LineSeparator")
    hshowsPrec fy1 (T.R (ParagraphSeparator) _) p =
      T.uap1 T.mkNoSrcPos p (Hat.Prelude.gshowString T.mkNoSrcPos p)
        (T.fromLitString T.mkNoSrcPos p "ParagraphSeparator")
    hshowsPrec fy1 (T.R (Control) _) p =
      T.uap1 T.mkNoSrcPos p (Hat.Prelude.gshowString T.mkNoSrcPos p)
        (T.fromLitString T.mkNoSrcPos p "Control")
    hshowsPrec fy1 (T.R (Format) _) p =
      T.uap1 T.mkNoSrcPos p (Hat.Prelude.gshowString T.mkNoSrcPos p)
        (T.fromLitString T.mkNoSrcPos p "Format")
    hshowsPrec fy1 (T.R (Surrogate) _) p =
      T.uap1 T.mkNoSrcPos p (Hat.Prelude.gshowString T.mkNoSrcPos p)
        (T.fromLitString T.mkNoSrcPos p "Surrogate")
    hshowsPrec fy1 (T.R (PrivateUse) _) p =
      T.uap1 T.mkNoSrcPos p (Hat.Prelude.gshowString T.mkNoSrcPos p)
        (T.fromLitString T.mkNoSrcPos p "PrivateUse")
    hshowsPrec fy1 (T.R (NotAssigned) _) p =
      T.uap1 T.mkNoSrcPos p (Hat.Prelude.gshowString T.mkNoSrcPos p)
        (T.fromLitString T.mkNoSrcPos p "NotAssigned")
    hshowsPrec _ _ p = T.fatal p
    
  

instance Bounded (GeneralCategory)
  where
  
  gminBound pminBound p = T.uconstUse pminBound p sminBound
  
  sminBound =
    T.uconstDef T.mkRoot a99v46v99v52minBound
      (\ p -> T.con0 T.mkNoSrcPos p UppercaseLetter aUppercaseLetter)
  
  gmaxBound pmaxBound p = T.uconstUse pmaxBound p smaxBound
  
  smaxBound =
    T.uconstDef T.mkRoot a99v46v99v52maxBound
      (\ p -> T.con0 T.mkNoSrcPos p NotAssigned aNotAssigned)
  

instance Ix (GeneralCategory)
  where
  
  grange prange p =
    T.ufun1 a99v55v99v56range prange p hrange
    where
    
    hrange (T.R (T.Tuple2 fy1 fy2) _) p =
      T.uwrapForward p
        (Hat.PreludeBasic.hmap (glocalToEnum T.mkNoSrcPos p)
          (T.uap2 T.mkNoSrcPos p (Hat.Prelude.genumFromTo T.mkNoSrcPos p)
            (T.uwrapForward p (hlocalFromEnum fy1 p))
            (T.uwrapForward p (hlocalFromEnum fy2 p))) p)
      where
      
      glocalToEnum ::
        T.RefSrcPos -> T.RefExp -> T.R (T.Fun Hat.Prelude.Int GeneralCategory)
      
      hlocalToEnum :: (T.R Hat.Prelude.Int) -> T.RefExp -> T.R GeneralCategory
      
      glocalToEnum plocalToEnum p =
        T.ufun1 a99v55v99v56localToEnum plocalToEnum p hlocalToEnum
      
      alocalToEnum = a99v55v99v56localToEnum
      
      hlocalToEnum fv99v55v99v56n p =
        T.ucguard
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fv99v55v99v56n
            (T.uap1 T.mkNoSrcPos p
              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
              (T.conInteger T.mkNoSrcPos p 0))) (h p)
          (y1localToEnum fv99v55v99v56n p)
        where
        
        h p = T.con0 T.mkNoSrcPos p UppercaseLetter aUppercaseLetter
        h p = y1localToEnum fv99v55v99v56n p
        
      hlocalToEnum fv99v55v99v56n p = y1localToEnum fv99v55v99v56n p
      
      y1localToEnum fv99v55v99v56n p =
        T.ucguard
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fv99v55v99v56n
            (T.uap1 T.mkNoSrcPos p
              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
              (T.conInteger T.mkNoSrcPos p 1))) (h p)
          (y2localToEnum fv99v55v99v56n p)
        where
        
        h p = T.con0 T.mkNoSrcPos p LowercaseLetter aLowercaseLetter
        h p = y2localToEnum fv99v55v99v56n p
        
      y1localToEnum fv99v55v99v56n p = y2localToEnum fv99v55v99v56n p
      
      y2localToEnum fv99v55v99v56n p =
        T.ucguard
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fv99v55v99v56n
            (T.uap1 T.mkNoSrcPos p
              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
              (T.conInteger T.mkNoSrcPos p 2))) (h p)
          (y3localToEnum fv99v55v99v56n p)
        where
        
        h p = T.con0 T.mkNoSrcPos p TitlecaseLetter aTitlecaseLetter
        h p = y3localToEnum fv99v55v99v56n p
        
      y2localToEnum fv99v55v99v56n p = y3localToEnum fv99v55v99v56n p
      
      y3localToEnum fv99v55v99v56n p =
        T.ucguard
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fv99v55v99v56n
            (T.uap1 T.mkNoSrcPos p
              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
              (T.conInteger T.mkNoSrcPos p 3))) (h p)
          (y4localToEnum fv99v55v99v56n p)
        where
        
        h p = T.con0 T.mkNoSrcPos p ModifierLetter aModifierLetter
        h p = y4localToEnum fv99v55v99v56n p
        
      y3localToEnum fv99v55v99v56n p = y4localToEnum fv99v55v99v56n p
      
      y4localToEnum fv99v55v99v56n p =
        T.ucguard
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fv99v55v99v56n
            (T.uap1 T.mkNoSrcPos p
              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
              (T.conInteger T.mkNoSrcPos p 4))) (h p)
          (y5localToEnum fv99v55v99v56n p)
        where
        
        h p = T.con0 T.mkNoSrcPos p OtherLetter aOtherLetter
        h p = y5localToEnum fv99v55v99v56n p
        
      y4localToEnum fv99v55v99v56n p = y5localToEnum fv99v55v99v56n p
      
      y5localToEnum fv99v55v99v56n p =
        T.ucguard
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fv99v55v99v56n
            (T.uap1 T.mkNoSrcPos p
              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
              (T.conInteger T.mkNoSrcPos p 5))) (h p)
          (y6localToEnum fv99v55v99v56n p)
        where
        
        h p = T.con0 T.mkNoSrcPos p NonSpacingMark aNonSpacingMark
        h p = y6localToEnum fv99v55v99v56n p
        
      y5localToEnum fv99v55v99v56n p = y6localToEnum fv99v55v99v56n p
      
      y6localToEnum fv99v55v99v56n p =
        T.ucguard
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fv99v55v99v56n
            (T.uap1 T.mkNoSrcPos p
              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
              (T.conInteger T.mkNoSrcPos p 6))) (h p)
          (y7localToEnum fv99v55v99v56n p)
        where
        
        h p = T.con0 T.mkNoSrcPos p SpacingCombiningMark aSpacingCombiningMark
        h p = y7localToEnum fv99v55v99v56n p
        
      y6localToEnum fv99v55v99v56n p = y7localToEnum fv99v55v99v56n p
      
      y7localToEnum fv99v55v99v56n p =
        T.ucguard
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fv99v55v99v56n
            (T.uap1 T.mkNoSrcPos p
              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
              (T.conInteger T.mkNoSrcPos p 7))) (h p)
          (y8localToEnum fv99v55v99v56n p)
        where
        
        h p = T.con0 T.mkNoSrcPos p EnclosingMark aEnclosingMark
        h p = y8localToEnum fv99v55v99v56n p
        
      y7localToEnum fv99v55v99v56n p = y8localToEnum fv99v55v99v56n p
      
      y8localToEnum fv99v55v99v56n p =
        T.ucguard
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fv99v55v99v56n
            (T.uap1 T.mkNoSrcPos p
              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
              (T.conInteger T.mkNoSrcPos p 8))) (h p)
          (y9localToEnum fv99v55v99v56n p)
        where
        
        h p = T.con0 T.mkNoSrcPos p DecimalNumber aDecimalNumber
        h p = y9localToEnum fv99v55v99v56n p
        
      y8localToEnum fv99v55v99v56n p = y9localToEnum fv99v55v99v56n p
      
      y9localToEnum fv99v55v99v56n p =
        T.ucguard
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fv99v55v99v56n
            (T.uap1 T.mkNoSrcPos p
              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
              (T.conInteger T.mkNoSrcPos p 9))) (h p)
          (y10localToEnum fv99v55v99v56n p)
        where
        
        h p = T.con0 T.mkNoSrcPos p LetterNumber aLetterNumber
        h p = y10localToEnum fv99v55v99v56n p
        
      y9localToEnum fv99v55v99v56n p = y10localToEnum fv99v55v99v56n p
      
      y10localToEnum fv99v55v99v56n p =
        T.ucguard
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fv99v55v99v56n
            (T.uap1 T.mkNoSrcPos p
              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
              (T.conInteger T.mkNoSrcPos p 10))) (h p)
          (y11localToEnum fv99v55v99v56n p)
        where
        
        h p = T.con0 T.mkNoSrcPos p OtherNumber aOtherNumber
        h p = y11localToEnum fv99v55v99v56n p
        
      y10localToEnum fv99v55v99v56n p = y11localToEnum fv99v55v99v56n p
      
      y11localToEnum fv99v55v99v56n p =
        T.ucguard
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fv99v55v99v56n
            (T.uap1 T.mkNoSrcPos p
              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
              (T.conInteger T.mkNoSrcPos p 11))) (h p)
          (y12localToEnum fv99v55v99v56n p)
        where
        
        h p = T.con0 T.mkNoSrcPos p ConnectorPunctuation aConnectorPunctuation
        h p = y12localToEnum fv99v55v99v56n p
        
      y11localToEnum fv99v55v99v56n p = y12localToEnum fv99v55v99v56n p
      
      y12localToEnum fv99v55v99v56n p =
        T.ucguard
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fv99v55v99v56n
            (T.uap1 T.mkNoSrcPos p
              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
              (T.conInteger T.mkNoSrcPos p 12))) (h p)
          (y13localToEnum fv99v55v99v56n p)
        where
        
        h p = T.con0 T.mkNoSrcPos p DashPunctuation aDashPunctuation
        h p = y13localToEnum fv99v55v99v56n p
        
      y12localToEnum fv99v55v99v56n p = y13localToEnum fv99v55v99v56n p
      
      y13localToEnum fv99v55v99v56n p =
        T.ucguard
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fv99v55v99v56n
            (T.uap1 T.mkNoSrcPos p
              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
              (T.conInteger T.mkNoSrcPos p 13))) (h p)
          (y14localToEnum fv99v55v99v56n p)
        where
        
        h p = T.con0 T.mkNoSrcPos p OpenPunctuation aOpenPunctuation
        h p = y14localToEnum fv99v55v99v56n p
        
      y13localToEnum fv99v55v99v56n p = y14localToEnum fv99v55v99v56n p
      
      y14localToEnum fv99v55v99v56n p =
        T.ucguard
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fv99v55v99v56n
            (T.uap1 T.mkNoSrcPos p
              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
              (T.conInteger T.mkNoSrcPos p 14))) (h p)
          (y15localToEnum fv99v55v99v56n p)
        where
        
        h p = T.con0 T.mkNoSrcPos p ClosePunctuation aClosePunctuation
        h p = y15localToEnum fv99v55v99v56n p
        
      y14localToEnum fv99v55v99v56n p = y15localToEnum fv99v55v99v56n p
      
      y15localToEnum fv99v55v99v56n p =
        T.ucguard
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fv99v55v99v56n
            (T.uap1 T.mkNoSrcPos p
              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
              (T.conInteger T.mkNoSrcPos p 15))) (h p)
          (y16localToEnum fv99v55v99v56n p)
        where
        
        h p = T.con0 T.mkNoSrcPos p InitialQuote aInitialQuote
        h p = y16localToEnum fv99v55v99v56n p
        
      y15localToEnum fv99v55v99v56n p = y16localToEnum fv99v55v99v56n p
      
      y16localToEnum fv99v55v99v56n p =
        T.ucguard
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fv99v55v99v56n
            (T.uap1 T.mkNoSrcPos p
              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
              (T.conInteger T.mkNoSrcPos p 16))) (h p)
          (y17localToEnum fv99v55v99v56n p)
        where
        
        h p = T.con0 T.mkNoSrcPos p FinalQuote aFinalQuote
        h p = y17localToEnum fv99v55v99v56n p
        
      y16localToEnum fv99v55v99v56n p = y17localToEnum fv99v55v99v56n p
      
      y17localToEnum fv99v55v99v56n p =
        T.ucguard
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fv99v55v99v56n
            (T.uap1 T.mkNoSrcPos p
              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
              (T.conInteger T.mkNoSrcPos p 17))) (h p)
          (y18localToEnum fv99v55v99v56n p)
        where
        
        h p = T.con0 T.mkNoSrcPos p OtherPunctuation aOtherPunctuation
        h p = y18localToEnum fv99v55v99v56n p
        
      y17localToEnum fv99v55v99v56n p = y18localToEnum fv99v55v99v56n p
      
      y18localToEnum fv99v55v99v56n p =
        T.ucguard
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fv99v55v99v56n
            (T.uap1 T.mkNoSrcPos p
              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
              (T.conInteger T.mkNoSrcPos p 18))) (h p)
          (y19localToEnum fv99v55v99v56n p)
        where
        
        h p = T.con0 T.mkNoSrcPos p MathSymbol aMathSymbol
        h p = y19localToEnum fv99v55v99v56n p
        
      y18localToEnum fv99v55v99v56n p = y19localToEnum fv99v55v99v56n p
      
      y19localToEnum fv99v55v99v56n p =
        T.ucguard
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fv99v55v99v56n
            (T.uap1 T.mkNoSrcPos p
              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
              (T.conInteger T.mkNoSrcPos p 19))) (h p)
          (y20localToEnum fv99v55v99v56n p)
        where
        
        h p = T.con0 T.mkNoSrcPos p CurrencySymbol aCurrencySymbol
        h p = y20localToEnum fv99v55v99v56n p
        
      y19localToEnum fv99v55v99v56n p = y20localToEnum fv99v55v99v56n p
      
      y20localToEnum fv99v55v99v56n p =
        T.ucguard
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fv99v55v99v56n
            (T.uap1 T.mkNoSrcPos p
              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
              (T.conInteger T.mkNoSrcPos p 20))) (h p)
          (y21localToEnum fv99v55v99v56n p)
        where
        
        h p = T.con0 T.mkNoSrcPos p ModifierSymbol aModifierSymbol
        h p = y21localToEnum fv99v55v99v56n p
        
      y20localToEnum fv99v55v99v56n p = y21localToEnum fv99v55v99v56n p
      
      y21localToEnum fv99v55v99v56n p =
        T.ucguard
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fv99v55v99v56n
            (T.uap1 T.mkNoSrcPos p
              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
              (T.conInteger T.mkNoSrcPos p 21))) (h p)
          (y22localToEnum fv99v55v99v56n p)
        where
        
        h p = T.con0 T.mkNoSrcPos p OtherSymbol aOtherSymbol
        h p = y22localToEnum fv99v55v99v56n p
        
      y21localToEnum fv99v55v99v56n p = y22localToEnum fv99v55v99v56n p
      
      y22localToEnum fv99v55v99v56n p =
        T.ucguard
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fv99v55v99v56n
            (T.uap1 T.mkNoSrcPos p
              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
              (T.conInteger T.mkNoSrcPos p 22))) (h p)
          (y23localToEnum fv99v55v99v56n p)
        where
        
        h p = T.con0 T.mkNoSrcPos p Space aSpace
        h p = y23localToEnum fv99v55v99v56n p
        
      y22localToEnum fv99v55v99v56n p = y23localToEnum fv99v55v99v56n p
      
      y23localToEnum fv99v55v99v56n p =
        T.ucguard
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fv99v55v99v56n
            (T.uap1 T.mkNoSrcPos p
              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
              (T.conInteger T.mkNoSrcPos p 23))) (h p)
          (y24localToEnum fv99v55v99v56n p)
        where
        
        h p = T.con0 T.mkNoSrcPos p LineSeparator aLineSeparator
        h p = y24localToEnum fv99v55v99v56n p
        
      y23localToEnum fv99v55v99v56n p = y24localToEnum fv99v55v99v56n p
      
      y24localToEnum fv99v55v99v56n p =
        T.ucguard
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fv99v55v99v56n
            (T.uap1 T.mkNoSrcPos p
              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
              (T.conInteger T.mkNoSrcPos p 24))) (h p)
          (y25localToEnum fv99v55v99v56n p)
        where
        
        h p = T.con0 T.mkNoSrcPos p ParagraphSeparator aParagraphSeparator
        h p = y25localToEnum fv99v55v99v56n p
        
      y24localToEnum fv99v55v99v56n p = y25localToEnum fv99v55v99v56n p
      
      y25localToEnum fv99v55v99v56n p =
        T.ucguard
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fv99v55v99v56n
            (T.uap1 T.mkNoSrcPos p
              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
              (T.conInteger T.mkNoSrcPos p 25))) (h p)
          (y26localToEnum fv99v55v99v56n p)
        where
        
        h p = T.con0 T.mkNoSrcPos p Control aControl
        h p = y26localToEnum fv99v55v99v56n p
        
      y25localToEnum fv99v55v99v56n p = y26localToEnum fv99v55v99v56n p
      
      y26localToEnum fv99v55v99v56n p =
        T.ucguard
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fv99v55v99v56n
            (T.uap1 T.mkNoSrcPos p
              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
              (T.conInteger T.mkNoSrcPos p 26))) (h p)
          (y27localToEnum fv99v55v99v56n p)
        where
        
        h p = T.con0 T.mkNoSrcPos p Format aFormat
        h p = y27localToEnum fv99v55v99v56n p
        
      y26localToEnum fv99v55v99v56n p = y27localToEnum fv99v55v99v56n p
      
      y27localToEnum fv99v55v99v56n p =
        T.ucguard
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fv99v55v99v56n
            (T.uap1 T.mkNoSrcPos p
              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
              (T.conInteger T.mkNoSrcPos p 27))) (h p)
          (y28localToEnum fv99v55v99v56n p)
        where
        
        h p = T.con0 T.mkNoSrcPos p Surrogate aSurrogate
        h p = y28localToEnum fv99v55v99v56n p
        
      y27localToEnum fv99v55v99v56n p = y28localToEnum fv99v55v99v56n p
      
      y28localToEnum fv99v55v99v56n p =
        T.ucguard
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fv99v55v99v56n
            (T.uap1 T.mkNoSrcPos p
              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
              (T.conInteger T.mkNoSrcPos p 28))) (h p)
          (y29localToEnum fv99v55v99v56n p)
        where
        
        h p = T.con0 T.mkNoSrcPos p PrivateUse aPrivateUse
        h p = y29localToEnum fv99v55v99v56n p
        
      y28localToEnum fv99v55v99v56n p = y29localToEnum fv99v55v99v56n p
      
      y29localToEnum fv99v55v99v56n p =
        T.ucguard
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fv99v55v99v56n
            (T.uap1 T.mkNoSrcPos p
              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
              (T.conInteger T.mkNoSrcPos p 29))) (h p) (T.fatal p)
        where
        
        h p = T.con0 T.mkNoSrcPos p NotAssigned aNotAssigned
        h p = T.fatal p
        
      y29localToEnum _ p = T.fatal p
      
      glocalFromEnum ::
        T.RefSrcPos -> T.RefExp -> T.R (T.Fun GeneralCategory Hat.Prelude.Int)
      
      hlocalFromEnum :: (T.R GeneralCategory) -> T.RefExp -> T.R Hat.Prelude.Int
      
      glocalFromEnum plocalFromEnum p =
        T.ufun1 a99v55v99v56localFromEnum plocalFromEnum p hlocalFromEnum
      
      alocalFromEnum = a99v55v99v56localFromEnum
      
      hlocalFromEnum (T.R UppercaseLetter _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 0)
      hlocalFromEnum (T.R LowercaseLetter _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 1)
      hlocalFromEnum (T.R TitlecaseLetter _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 2)
      hlocalFromEnum (T.R ModifierLetter _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 3)
      hlocalFromEnum (T.R OtherLetter _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 4)
      hlocalFromEnum (T.R NonSpacingMark _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 5)
      hlocalFromEnum (T.R SpacingCombiningMark _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 6)
      hlocalFromEnum (T.R EnclosingMark _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 7)
      hlocalFromEnum (T.R DecimalNumber _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 8)
      hlocalFromEnum (T.R LetterNumber _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 9)
      hlocalFromEnum (T.R OtherNumber _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 10)
      hlocalFromEnum (T.R ConnectorPunctuation _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 11)
      hlocalFromEnum (T.R DashPunctuation _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 12)
      hlocalFromEnum (T.R OpenPunctuation _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 13)
      hlocalFromEnum (T.R ClosePunctuation _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 14)
      hlocalFromEnum (T.R InitialQuote _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 15)
      hlocalFromEnum (T.R FinalQuote _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 16)
      hlocalFromEnum (T.R OtherPunctuation _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 17)
      hlocalFromEnum (T.R MathSymbol _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 18)
      hlocalFromEnum (T.R CurrencySymbol _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 19)
      hlocalFromEnum (T.R ModifierSymbol _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 20)
      hlocalFromEnum (T.R OtherSymbol _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 21)
      hlocalFromEnum (T.R Space _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 22)
      hlocalFromEnum (T.R LineSeparator _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 23)
      hlocalFromEnum (T.R ParagraphSeparator _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 24)
      hlocalFromEnum (T.R Control _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 25)
      hlocalFromEnum (T.R Format _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 26)
      hlocalFromEnum (T.R Surrogate _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 27)
      hlocalFromEnum (T.R PrivateUse _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 28)
      hlocalFromEnum (T.R NotAssigned _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 29)
      hlocalFromEnum _ p = T.fatal p
      
    hrange _ p = T.fatal p
    
  
  gindex pindex p =
    T.ufun2 a99v55v99v56index pindex p hindex
    where
    
    hindex (T.R (T.Tuple2 fy1 fy2) _) fy3 p =
      T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!- p)
        (T.uwrapForward p (hlocalFromEnum fy3 p))
        (T.uwrapForward p (hlocalFromEnum fy1 p))
      where
      
      glocalFromEnum ::
        T.RefSrcPos -> T.RefExp -> T.R (T.Fun GeneralCategory Hat.Prelude.Int)
      
      hlocalFromEnum :: (T.R GeneralCategory) -> T.RefExp -> T.R Hat.Prelude.Int
      
      glocalFromEnum plocalFromEnum p =
        T.ufun1 a99v55v99v56localFromEnum plocalFromEnum p hlocalFromEnum
      
      alocalFromEnum = a99v55v99v56localFromEnum
      
      hlocalFromEnum (T.R UppercaseLetter _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 0)
      hlocalFromEnum (T.R LowercaseLetter _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 1)
      hlocalFromEnum (T.R TitlecaseLetter _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 2)
      hlocalFromEnum (T.R ModifierLetter _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 3)
      hlocalFromEnum (T.R OtherLetter _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 4)
      hlocalFromEnum (T.R NonSpacingMark _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 5)
      hlocalFromEnum (T.R SpacingCombiningMark _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 6)
      hlocalFromEnum (T.R EnclosingMark _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 7)
      hlocalFromEnum (T.R DecimalNumber _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 8)
      hlocalFromEnum (T.R LetterNumber _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 9)
      hlocalFromEnum (T.R OtherNumber _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 10)
      hlocalFromEnum (T.R ConnectorPunctuation _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 11)
      hlocalFromEnum (T.R DashPunctuation _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 12)
      hlocalFromEnum (T.R OpenPunctuation _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 13)
      hlocalFromEnum (T.R ClosePunctuation _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 14)
      hlocalFromEnum (T.R InitialQuote _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 15)
      hlocalFromEnum (T.R FinalQuote _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 16)
      hlocalFromEnum (T.R OtherPunctuation _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 17)
      hlocalFromEnum (T.R MathSymbol _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 18)
      hlocalFromEnum (T.R CurrencySymbol _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 19)
      hlocalFromEnum (T.R ModifierSymbol _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 20)
      hlocalFromEnum (T.R OtherSymbol _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 21)
      hlocalFromEnum (T.R Space _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 22)
      hlocalFromEnum (T.R LineSeparator _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 23)
      hlocalFromEnum (T.R ParagraphSeparator _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 24)
      hlocalFromEnum (T.R Control _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 25)
      hlocalFromEnum (T.R Format _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 26)
      hlocalFromEnum (T.R Surrogate _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 27)
      hlocalFromEnum (T.R PrivateUse _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 28)
      hlocalFromEnum (T.R NotAssigned _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 29)
      hlocalFromEnum _ p = T.fatal p
      
    hindex _ _ p = T.fatal p
    
  
  ginRange pinRange p =
    T.ufun2 a99v55v99v56inRange pinRange p hinRange
    where
    
    hinRange (T.R (T.Tuple2 fy1 fy2) _) fy3 p =
      T.uap2 T.mkNoSrcPos p (Hat.Ix.ginRange T.mkNoSrcPos p)
        (T.con2 T.mkNoSrcPos p T.Tuple2 T.aTuple2
          (T.uwrapForward p (hlocalFromEnum fy1 p))
          (T.uwrapForward p (hlocalFromEnum fy2 p)))
        (T.uwrapForward p (hlocalFromEnum fy3 p))
      where
      
      glocalFromEnum ::
        T.RefSrcPos -> T.RefExp -> T.R (T.Fun GeneralCategory Hat.Prelude.Int)
      
      hlocalFromEnum :: (T.R GeneralCategory) -> T.RefExp -> T.R Hat.Prelude.Int
      
      glocalFromEnum plocalFromEnum p =
        T.ufun1 a99v55v99v56localFromEnum plocalFromEnum p hlocalFromEnum
      
      alocalFromEnum = a99v55v99v56localFromEnum
      
      hlocalFromEnum (T.R UppercaseLetter _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 0)
      hlocalFromEnum (T.R LowercaseLetter _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 1)
      hlocalFromEnum (T.R TitlecaseLetter _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 2)
      hlocalFromEnum (T.R ModifierLetter _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 3)
      hlocalFromEnum (T.R OtherLetter _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 4)
      hlocalFromEnum (T.R NonSpacingMark _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 5)
      hlocalFromEnum (T.R SpacingCombiningMark _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 6)
      hlocalFromEnum (T.R EnclosingMark _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 7)
      hlocalFromEnum (T.R DecimalNumber _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 8)
      hlocalFromEnum (T.R LetterNumber _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 9)
      hlocalFromEnum (T.R OtherNumber _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 10)
      hlocalFromEnum (T.R ConnectorPunctuation _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 11)
      hlocalFromEnum (T.R DashPunctuation _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 12)
      hlocalFromEnum (T.R OpenPunctuation _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 13)
      hlocalFromEnum (T.R ClosePunctuation _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 14)
      hlocalFromEnum (T.R InitialQuote _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 15)
      hlocalFromEnum (T.R FinalQuote _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 16)
      hlocalFromEnum (T.R OtherPunctuation _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 17)
      hlocalFromEnum (T.R MathSymbol _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 18)
      hlocalFromEnum (T.R CurrencySymbol _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 19)
      hlocalFromEnum (T.R ModifierSymbol _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 20)
      hlocalFromEnum (T.R OtherSymbol _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 21)
      hlocalFromEnum (T.R Space _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 22)
      hlocalFromEnum (T.R LineSeparator _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 23)
      hlocalFromEnum (T.R ParagraphSeparator _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 24)
      hlocalFromEnum (T.R Control _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 25)
      hlocalFromEnum (T.R Format _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 26)
      hlocalFromEnum (T.R Surrogate _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 27)
      hlocalFromEnum (T.R PrivateUse _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 28)
      hlocalFromEnum (T.R NotAssigned _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 29)
      hlocalFromEnum _ p = T.fatal p
      
    hinRange _ _ p = T.fatal p
    
  

ggenCat :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun Int Int)

ggenCat pgenCat p = T.ufun1 agenCat pgenCat p hgenCat

hgenCat z1genCat kgenCat =
  T.fromInt kgenCat
    ((\_ i -> Prelude.fromEnum (Data.Char.generalCategory (Prelude.toEnum i))) Prelude.True
      (T.toInt kgenCat z1genCat))

ggeneralCategory :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun Char GeneralCategory)

hgeneralCategory :: (T.R Char) -> T.RefExp -> T.R GeneralCategory

ggeneralCategory pgeneralCategory p =
  T.ufun1 ageneralCategory pgeneralCategory p hgeneralCategory

hgeneralCategory fc p =
  T.uwrapForward p
    (((gtoEnum T.mkNoSrcPos p)
        *$
        (T.uwrapForward p
          (((ggenCat T.mkNoSrcPos p)
              *$
              (T.uap1 T.mkNoSrcPos p (gord T.mkNoSrcPos p) fc)) p))) p)

tData_Char = T.mkModule "Data.Char" "Data/Char.hs" Prelude.False

aUppercaseLetter =
  T.mkConstructor tData_Char 690011 690025 3 0 "UppercaseLetter"

aLowercaseLetter =
  T.mkConstructor tData_Char 700011 700025 3 0 "LowercaseLetter"

aTitlecaseLetter =
  T.mkConstructor tData_Char 710011 710025 3 0 "TitlecaseLetter"

aModifierLetter = T.mkConstructor tData_Char 720011 720024 3 0 "ModifierLetter"

aOtherLetter = T.mkConstructor tData_Char 730011 730021 3 0 "OtherLetter"

aNonSpacingMark = T.mkConstructor tData_Char 740011 740024 3 0 "NonSpacingMark"

aSpacingCombiningMark =
  T.mkConstructor tData_Char 750011 750030 3 0 "SpacingCombiningMark"

aEnclosingMark = T.mkConstructor tData_Char 760011 760023 3 0 "EnclosingMark"

aDecimalNumber = T.mkConstructor tData_Char 770011 770023 3 0 "DecimalNumber"

aLetterNumber = T.mkConstructor tData_Char 780011 780022 3 0 "LetterNumber"

aOtherNumber = T.mkConstructor tData_Char 790011 790021 3 0 "OtherNumber"

aConnectorPunctuation =
  T.mkConstructor tData_Char 800011 800030 3 0 "ConnectorPunctuation"

aDashPunctuation =
  T.mkConstructor tData_Char 810011 810025 3 0 "DashPunctuation"

aOpenPunctuation =
  T.mkConstructor tData_Char 820011 820025 3 0 "OpenPunctuation"

aClosePunctuation =
  T.mkConstructor tData_Char 830011 830026 3 0 "ClosePunctuation"

aInitialQuote = T.mkConstructor tData_Char 840011 840022 3 0 "InitialQuote"

aFinalQuote = T.mkConstructor tData_Char 850011 850020 3 0 "FinalQuote"

aOtherPunctuation =
  T.mkConstructor tData_Char 860011 860026 3 0 "OtherPunctuation"

aMathSymbol = T.mkConstructor tData_Char 870011 870020 3 0 "MathSymbol"

aCurrencySymbol = T.mkConstructor tData_Char 880011 880024 3 0 "CurrencySymbol"

aModifierSymbol = T.mkConstructor tData_Char 890011 890024 3 0 "ModifierSymbol"

aOtherSymbol = T.mkConstructor tData_Char 900011 900021 3 0 "OtherSymbol"

aSpace = T.mkConstructor tData_Char 910011 910015 3 0 "Space"

aLineSeparator = T.mkConstructor tData_Char 920011 920023 3 0 "LineSeparator"

aParagraphSeparator =
  T.mkConstructor tData_Char 930011 930028 3 0 "ParagraphSeparator"

aControl = T.mkConstructor tData_Char 940011 940017 3 0 "Control"

aFormat = T.mkConstructor tData_Char 950011 950016 3 0 "Format"

aSurrogate = T.mkConstructor tData_Char 960011 960019 3 0 "Surrogate"

aPrivateUse = T.mkConstructor tData_Char 970011 970020 3 0 "PrivateUse"

aNotAssigned = T.mkConstructor tData_Char 980011 980021 3 0 "NotAssigned"

aisLetter = T.mkVariable tData_Char 480001 490026 3 1 "isLetter" Prelude.False

aisMark = T.mkVariable tData_Char 500001 510024 3 1 "isMark" Prelude.False

aisNumber = T.mkVariable tData_Char 520001 530026 3 1 "isNumber" Prelude.False

aisPunctuation =
  T.mkVariable tData_Char 540001 550031 3 1 "isPunctuation" Prelude.False

aisSymbol = T.mkVariable tData_Char 560001 570026 3 1 "isSymbol" Prelude.False

aisSeparator =
  T.mkVariable tData_Char 580001 590029 3 1 "isSeparator" Prelude.False

aisAsciiUpper =
  T.mkVariable tData_Char 600001 610030 3 1 "isAsciiUpper" Prelude.False

aisAsciiLower =
  T.mkVariable tData_Char 620001 630030 3 1 "isAsciiLower" Prelude.False

atoTitle = T.mkVariable tData_Char 650001 660025 3 1 "toTitle" Prelude.False

agenCat = T.mkVariable tData_Char 1010001 1030022 3 1 "genCat" Prelude.False

ageneralCategory =
  T.mkVariable tData_Char 1060001 1060043 3 1 "generalCategory" Prelude.False

(+>>=#>=>>=$!==) = T.mkVariable tData_Char 990019 990020 3 2 "==" Prelude.False

a99v23v99v25compare =
  T.mkVariable tData_Char 990023 990025 3 2 "compare" Prelude.False

a99v28v99v31fromEnum =
  T.mkVariable tData_Char 990028 990031 3 1 "fromEnum" Prelude.False

a99v28v99v31toEnum =
  T.mkVariable tData_Char 990028 990031 3 1 "toEnum" Prelude.False

a99v28v99v31enumFrom =
  T.mkVariable tData_Char 990028 990031 3 1 "enumFrom" Prelude.False

a99v28v99v31enumFromThen =
  T.mkVariable tData_Char 990028 990031 3 2 "enumFromThen" Prelude.False

a99v34v99v37readsPrec =
  T.mkVariable tData_Char 990034 990037 3 1 "readsPrec" Prelude.False

a99v40v99v43showsPrec =
  T.mkVariable tData_Char 990040 990043 3 2 "showsPrec" Prelude.False

a99v46v99v52minBound =
  T.mkVariable tData_Char 990046 990052 3 0 "minBound" Prelude.False

a99v46v99v52maxBound =
  T.mkVariable tData_Char 990046 990052 3 0 "maxBound" Prelude.False

a99v55v99v56range =
  T.mkVariable tData_Char 990055 990056 3 1 "range" Prelude.False

a99v55v99v56index =
  T.mkVariable tData_Char 990055 990056 3 2 "index" Prelude.False

a99v55v99v56inRange =
  T.mkVariable tData_Char 990055 990056 3 2 "inRange" Prelude.False

a99v23v99v25localFromEnum =
  T.mkVariable tData_Char 990023 990025 3 1 "localFromEnum" Prelude.True

a99v55v99v56localToEnum =
  T.mkVariable tData_Char 990055 990056 3 1 "localToEnum" Prelude.True

a99v55v99v56localFromEnum =
  T.mkVariable tData_Char 990055 990056 3 1 "localFromEnum" Prelude.True
