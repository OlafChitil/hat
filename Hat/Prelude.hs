module Hat.Prelude
  (g_filter,a_filter,h_filter,g_foldr,a_foldr,h_foldr,gmap,amap,hmap,(!++),(+++)
    ,(*++),gfilter,afilter,hfilter,gconcat,aconcat,hconcat,ghead,ahead,hhead
    ,glast,alast,hlast,gtail,atail,htail,ginit,ainit,hinit,gnull,anull,hnull
    ,glength,alength,hlength,(!!!),(+!!),(*!!),gfoldl,afoldl,hfoldl,gfoldl1
    ,afoldl1,hfoldl1,gscanl,ascanl,hscanl,gscanl1,ascanl1,hscanl1,gfoldr,afoldr
    ,hfoldr,gfoldr1,afoldr1,hfoldr1,gscanr,ascanr,hscanr,gscanr1,ascanr1,hscanr1
    ,giterate,aiterate,hiterate,grepeat,arepeat,hrepeat,greplicate,areplicate
    ,hreplicate,gcycle,acycle,hcycle,gtake,atake,htake,gdrop,adrop,hdrop
    ,gsplitAt,asplitAt,hsplitAt,gtakeWhile,atakeWhile,htakeWhile,gdropWhile
    ,adropWhile,hdropWhile,gspan,aspan,hspan,gbreak,abreak,hbreak,glines,alines
    ,hlines,gwords,awords,hwords,gunlines,gunwords,aunwords,hunwords,greverse
    ,gand,gor,gany,aany,hany,gall,aall,hall,gelem,aelem,helem,gnotElem,anotElem
    ,hnotElem,glookup,alookup,hlookup,gsum,gproduct,gmaximum,amaximum,hmaximum
    ,gminimum,aminimum,hminimum,gconcatMap,aconcatMap,hconcatMap,gzip,gzip3
    ,gzipWith,azipWith,hzipWith,gzipWith3,azipWith3,hzipWith3,gunzip,gunzip3
    ,ReadS(),ReadS___1(),ReadS___2(),ShowS(),ShowS___1(),ShowS___2()
    ,Read(greadsPrec,greadList,sreadsPrec,sreadList),Show(gshowsPrec,gshow
      ,gshowList,sshowsPrec,sshow,sshowList),greads,gshows,gread,aread,hread
    ,glex,alex,hlex,gshowChar,gshowString,greadParen,areadParen,hreadParen
    ,gshowParen,ashowParen,hshowParen,FilePath(),IOError(),gioError,aioError
    ,hioError,guserError,auserError,huserError,gcatch,acatch,hcatch,gputChar
    ,aputChar,hputChar,gputStr,aputStr,hputStr,gputStrLn,aputStrLn,hputStrLn
    ,gprint,aprint,hprint,ggetChar,ggetLine,ggetContents,ginteract,ainteract
    ,hinteract,greadFile,areadFile,hreadFile,gwriteFile,awriteFile,hwriteFile
    ,gappendFile,aappendFile,happendFile,greadIO,areadIO,hreadIO,greadLn
    ,Bool(False,True),aFalse,aTrue,Maybe(Nothing,Just),aNothing,aJust
    ,Either(Left,Right),aLeft,aRight,Ordering(LT,EQ,GT),aLT,aEQ,aGT,Char()
    ,String(),Int(),Integer(),Float(),Double(),Rational(),IO(),Eq((!==),(!/=)
      ,(|==),(|/=)),Ord(gcompare,(!<),(!<=),(!>=),(!>),gmax,gmin,scompare,(|<)
      ,(|<=),(|>=),(|>),smax,smin),Enum(gsucc,gpred,gtoEnum,gfromEnum,genumFrom
      ,genumFromThen,genumFromTo,genumFromThenTo,ssucc,spred,stoEnum,sfromEnum
      ,senumFrom,senumFromThen,senumFromTo,senumFromThenTo),Bounded(gminBound
      ,gmaxBound,sminBound,smaxBound),Num((!+),(!-),(!*),gnegate,gabs,gsignum
      ,gfromInteger,(|+),(|-),(|*),snegate,sabs,ssignum,sfromInteger)
    ,Real(gtoRational,stoRational),Integral(gquot,grem,gdiv,gmod,gquotRem
      ,gdivMod,gtoInteger,squot,srem,sdiv,smod,squotRem,sdivMod,stoInteger)
    ,Fractional((!/),grecip,gfromRational,(|/),srecip,sfromRational)
    ,Floating(gpi,gexp,glog,gsqrt,(!**),glogBase,gsin,gcos,gtan,gasin,gacos
      ,gatan,gsinh,gcosh,gtanh,gasinh,gacosh,gatanh,spi,sexp,slog,ssqrt,(|**)
      ,slogBase,ssin,scos,stan,sasin,sacos,satan,ssinh,scosh,stanh,sasinh,sacosh
      ,satanh),RealFrac(gproperFraction,gtruncate,ground,gceiling,gfloor
      ,sproperFraction,struncate,sround,sceiling,sfloor),RealFloat(gfloatRadix
      ,gfloatDigits,gfloatRange,gdecodeFloat,gencodeFloat,gexponent,gsignificand
      ,gscaleFloat,gisNaN,gisInfinite,gisDenormalized,gisIEEE,gisNegativeZero
      ,gatan2,sfloatRadix,sfloatDigits,sfloatRange,sdecodeFloat,sencodeFloat
      ,sexponent,ssignificand,sscaleFloat,sisNaN,sisInfinite,sisDenormalized
      ,sisIEEE,sisNegativeZero,satan2),Monad((!>>=),(!>>),greturn,gfail,(|>>=)
      ,(|>>),sreturn,sfail),Functor(gfmap,sfmap),gmapM,amapM,hmapM,gmapM_,amapM_
    ,hmapM_,gsequence,gsequence_,(!=<<),(+=<<),(*=<<),gmaybe,amaybe,hmaybe
    ,geither,aeither,heither,(!&&),(+&&),(*&&),(!||),(+||),(*||),gnot,anot,hnot
    ,gotherwise,gsubtract,geven,aeven,heven,godd,ggcd,agcd,hgcd,glcm,alcm,hlcm
    ,(!^),(+^),(*^),(!^^),(+^^),(*^^),gfromIntegral,grealToFrac,gfst,afst,hfst
    ,gsnd,asnd,hsnd,gcurry,acurry,hcurry,guncurry,auncurry,huncurry,gid,aid,hid
    ,gconst,aconst,hconst,(!.),(+.),(*.),gflip,aflip,hflip,(!$),(+$),(*$),guntil
    ,auntil,huntil,gasTypeOf,gerror,aerror,herror,gundefined,gseq,aseq,hseq
    ,(!$!),(+$!),(*$!)) where

import qualified Prelude 
import Hat.Hack 
import qualified Hat.Hat as T 
import Hat.Hat  (WrapVal(wrapVal))
import Hat.PreludeBasic 

tPrelude = T.mkModule "Prelude" "Prelude.hs" Prelude.False