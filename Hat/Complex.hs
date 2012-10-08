module Hat.Complex
  (Complex((:+)),(+:+),grealPart,arealPart,hrealPart,gimagPart,aimagPart
    ,himagPart,gconjugate,aconjugate,hconjugate,gmkPolar,amkPolar,hmkPolar,gcis
    ,acis,hcis,gpolar,apolar,hpolar,gmagnitude,amagnitude,hmagnitude,gphase
    ,aphase,hphase) where

import qualified Hat.PreludeBasic 
import qualified Prelude 
import Hat.Hack 
import qualified Hat.Hat as T 
import Hat.Hat  (WrapVal(wrapVal))
import Hat.Prelude 

data Complex a = (:+) !(T.R a) !(T.R a)

instance T.WrapVal ((Complex a))
  where
  
  wrapVal pwrapVal (kwrapVal@((T.R _ z1wrapVal) :+ (T.R _ z2wrapVal))) p =
    T.R kwrapVal (T.mkValueApp2 p pwrapVal (+:+) z1wrapVal z2wrapVal)
  

instance Eq a => Eq ((Complex a))
  where
  
  (!==) (%==) p =
    T.ufun2 (++=%@=+=%>==) (%==) p (*==)
    where
    
    (*==) (T.R (fy1 :+ fy2) _) (T.R (fy3 :+ fy4) _) p =
      T.uwrapForward p
        (((T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fy1 fy3)
            Hat.Prelude.*&&
            (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fy2 fy4)) p)
    (*==) _ _ p = T.con0 T.mkNoSrcPos p Hat.Prelude.False Hat.Prelude.aFalse
    
  

instance Read a => Read ((Complex a))
  where
  
  greadsPrec preadsPrec p =
    T.ufun1 a6v41v6v44readsPrec preadsPrec p hreadsPrec
    where
    
    hreadsPrec fy1 p =
      T.uwrapForward p
        (Hat.Prelude.hreadParen
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!> p) fy1
            (T.uap1 T.mkNoSrcPos p
              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
              (T.conInteger T.mkNoSrcPos p 6)))
          (T.uap2 T.mkNoSrcPos p (Hat.PreludeBasic.gthenAp T.mkNoSrcPos p)
            (T.uwrapForward p
              (Hat.PreludeBasic.hthenLex
                (T.uap2 T.mkNoSrcPos p (Hat.PreludeBasic.gthenAp T.mkNoSrcPos p)
                  (T.uap1 T.mkNoSrcPos p
                    (Hat.PreludeBasic.gyield T.mkNoSrcPos p)
                    (T.pa0 (:+) T.cn2 T.mkNoSrcPos p (+:+)))
                  (T.uap1 T.mkNoSrcPos p (Hat.Prelude.greadsPrec T.mkNoSrcPos p)
                    (T.uap1 T.mkNoSrcPos p
                      (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                      (T.conInteger T.mkNoSrcPos p 7))))
                (T.fromLitString T.mkNoSrcPos p ":+") p))
            (T.uap1 T.mkNoSrcPos p (Hat.Prelude.greadsPrec T.mkNoSrcPos p)
              (T.uap1 T.mkNoSrcPos p
                (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                (T.conInteger T.mkNoSrcPos p 7)))) p)
    
  

instance Show a => Show ((Complex a))
  where
  
  gshowsPrec pshowsPrec p =
    T.ufun2 a6v46v6v49showsPrec pshowsPrec p hshowsPrec
    where
    
    hshowsPrec fy1 (T.R (fy2 :+ fy3) _) p =
      T.uwrapForward p
        (Hat.Prelude.hshowParen
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!> p) fy1
            (T.uap1 T.mkNoSrcPos p
              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
              (T.conInteger T.mkNoSrcPos p 6)))
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!. p)
            (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!. p)
              (T.uap2 T.mkNoSrcPos p (Hat.Prelude.gshowsPrec T.mkNoSrcPos p)
                (T.uap1 T.mkNoSrcPos p
                  (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                  (T.conInteger T.mkNoSrcPos p 7)) fy2)
              (T.uap1 T.mkNoSrcPos p (Hat.Prelude.gshowString T.mkNoSrcPos p)
                (T.fromLitString T.mkNoSrcPos p " :+ ")))
            (T.uap2 T.mkNoSrcPos p (Hat.Prelude.gshowsPrec T.mkNoSrcPos p)
              (T.uap1 T.mkNoSrcPos p
                (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                (T.conInteger T.mkNoSrcPos p 7)) fy3)) p)
    hshowsPrec _ _ p = T.fatal p
    
  

grealPart,gimagPart ::
  RealFloat a => T.RefSrcPos -> T.RefExp -> T.R (T.Fun (Complex a) a)

hrealPart :: RealFloat a => (T.R (Complex a)) -> T.RefExp -> T.R a

himagPart :: RealFloat a => (T.R (Complex a)) -> T.RefExp -> T.R a

grealPart prealPart p = T.ufun1 arealPart prealPart p hrealPart

hrealPart (T.R (fx :+ fy) _) p = T.projection T.mkNoSrcPos p fx
hrealPart _ p = T.fatal p

gimagPart pimagPart p = T.ufun1 aimagPart pimagPart p himagPart

himagPart (T.R (fx :+ fy) _) p = T.projection T.mkNoSrcPos p fy
himagPart _ p = T.fatal p

gconjugate ::
  RealFloat a => T.RefSrcPos -> T.RefExp -> T.R (T.Fun (Complex a) (Complex a))

hconjugate :: RealFloat a => (T.R (Complex a)) -> T.RefExp -> T.R (Complex a)

gconjugate pconjugate p = T.ufun1 aconjugate pconjugate p hconjugate

hconjugate (T.R (fx :+ fy) _) p =
  T.con2 T.mkNoSrcPos p (:+) (+:+) fx
    (T.uap1 T.mkNoSrcPos p (gnegate T.mkNoSrcPos p) fy)
hconjugate _ p = T.fatal p

gmkPolar ::
  RealFloat a => T.RefSrcPos -> T.RefExp -> T.R (T.Fun a (T.Fun a (Complex a)))

hmkPolar :: RealFloat a => (T.R a) -> (T.R a) -> T.RefExp -> T.R (Complex a)

gmkPolar pmkPolar p = T.ufun2 amkPolar pmkPolar p hmkPolar

hmkPolar fr ftheta p =
  T.con2 T.mkNoSrcPos p (:+) (+:+)
    (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !* p) fr
      (T.uap1 T.mkNoSrcPos p (gcos T.mkNoSrcPos p) ftheta))
    (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !* p) fr
      (T.uap1 T.mkNoSrcPos p (gsin T.mkNoSrcPos p) ftheta))

gcis :: RealFloat a => T.RefSrcPos -> T.RefExp -> T.R (T.Fun a (Complex a))

hcis :: RealFloat a => (T.R a) -> T.RefExp -> T.R (Complex a)

gcis pcis p = T.ufun1 acis pcis p hcis

hcis ftheta p =
  T.con2 T.mkNoSrcPos p (:+) (+:+)
    (T.uap1 T.mkNoSrcPos p (gcos T.mkNoSrcPos p) ftheta)
    (T.uap1 T.mkNoSrcPos p (gsin T.mkNoSrcPos p) ftheta)

gpolar ::
  RealFloat a =>
  T.RefSrcPos -> T.RefExp -> T.R (T.Fun (Complex a) (T.Tuple2 a a))

hpolar :: RealFloat a => (T.R (Complex a)) -> T.RefExp -> T.R (T.Tuple2 a a)

gpolar ppolar p = T.ufun1 apolar ppolar p hpolar

hpolar fz p =
  T.con2 T.mkNoSrcPos p T.Tuple2 T.aTuple2 (T.uwrapForward p (hmagnitude fz p))
    (T.uwrapForward p (hphase fz p))

gmagnitude ::
  RealFloat a => T.RefSrcPos -> T.RefExp -> T.R (T.Fun (Complex a) a)

hmagnitude :: RealFloat a => (T.R (Complex a)) -> T.RefExp -> T.R a

gmagnitude pmagnitude p = T.ufun1 amagnitude pmagnitude p hmagnitude

hmagnitude (T.R (fx :+ fy) _) p =
  T.uap2 T.mkNoSrcPos p (gscaleFloat T.mkNoSrcPos p) (gk T.mkNoSrcPos p)
    (T.uap1 T.mkNoSrcPos p (gsqrt T.mkNoSrcPos p)
      (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !+ p)
        (T.uwrapForward p
          (((T.uap2 T.mkNoSrcPos p (gscaleFloat T.mkNoSrcPos p)
                (gmk T.mkNoSrcPos p) fx)
              *^
              (T.uap1 T.mkNoSrcPos p
                  (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                  (T.conInteger T.mkNoSrcPos p 2)
                :: T.R Integer)) p))
        (T.uwrapForward p
          (((T.uap2 T.mkNoSrcPos p (gscaleFloat T.mkNoSrcPos p)
                (gmk T.mkNoSrcPos p) fy)
              *^
              (T.uap1 T.mkNoSrcPos p
                  (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                  (T.conInteger T.mkNoSrcPos p 2)
                :: T.R Integer)) p))))
  where
  
  gk pk p = T.uconstUse pk p sk
  
  sk =
    T.uconstDef p a28v11v28v43k
      (\ p ->
        T.uap2 T.mkNoSrcPos p (gmax T.mkNoSrcPos p)
          (T.uap1 T.mkNoSrcPos p (gexponent T.mkNoSrcPos p) fx)
          (T.uap1 T.mkNoSrcPos p (gexponent T.mkNoSrcPos p) fy))
  
  gmk pmk p = T.uconstUse pmk p smk
  
  smk =
    T.uconstDef p a29v11v29v18mk
      (\ p ->
        T.uap1 T.mkNoSrcPos p (gnegate T.mkNoSrcPos p) (gk T.mkNoSrcPos p))
  
hmagnitude _ p = T.fatal p

gphase :: RealFloat a => T.RefSrcPos -> T.RefExp -> T.R (T.Fun (Complex a) a)

hphase :: RealFloat a => (T.R (Complex a)) -> T.RefExp -> T.R a

gphase pphase p = T.ufun1 aphase pphase p hphase

hphase (z1phase@(T.R (fv32v8v32v8n :+ v32v13v32v13n) _)) p =
  T.ucguard
    (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fv32v8v32v8n
      (T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
        (T.conInteger T.mkNoSrcPos p 0))) (h v32v13v32v13n p)
    (y1phase z1phase p)
  where
  
  h fv32v13v32v13n p =
    T.ucguard
      (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fv32v13v32v13n
        (T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 0))) (h p) (y1phase z1phase p)
    where
    
    h p =
      T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
        (T.conInteger T.mkNoSrcPos p 0)
    h p = y1phase z1phase p
    
  h _ p = y1phase z1phase p
  
hphase z1phase p = y1phase z1phase p

y1phase (T.R (fx :+ fy) _) p =
  T.uap2 T.mkNoSrcPos p (gatan2 T.mkNoSrcPos p) fy fx
y1phase _ p = T.fatal p

instance RealFloat a => Num ((Complex a))
  where
  
  (!+) (%+) p =
    T.ufun2 (+%^=#$=%^=#$+) (%+) p (*+)
    where
    
    (*+) (T.R (fx :+ fy) _) (T.R (fx' :+ fy') _) p =
      T.con2 T.mkNoSrcPos p (:+) (+:+)
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !+ p) fx fx')
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !+ p) fy fy')
    (*+) _ _ p = T.fatal p
    
  
  (!-) (%-) p =
    T.ufun2 (+%@=#$=%@=#$-) (%-) p (*-)
    where
    
    (*-) (T.R (fx :+ fy) _) (T.R (fx' :+ fy') _) p =
      T.con2 T.mkNoSrcPos p (:+) (+:+)
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !- p) fx fx')
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !- p) fy fy')
    (*-) _ _ p = T.fatal p
    
  
  (!*) (%*) p =
    T.ufun2 (+%>=#$=%>=#$*) (%*) p (**)
    where
    
    (**) (T.R (fx :+ fy) _) (T.R (fx' :+ fy') _) p =
      T.con2 T.mkNoSrcPos p (:+) (+:+)
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !- p)
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !* p) fx fx')
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !* p) fy fy'))
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !+ p)
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !* p) fx fy')
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !* p) fy fx'))
    (**) _ _ p = T.fatal p
    
  
  gnegate pnegate p =
    T.ufun1 a40v5v40v41negate pnegate p hnegate
    where
    
    hnegate (T.R (fx :+ fy) _) p =
      T.con2 T.mkNoSrcPos p (:+) (+:+)
        (T.uap1 T.mkNoSrcPos p (gnegate T.mkNoSrcPos p) fx)
        (T.uap1 T.mkNoSrcPos p (gnegate T.mkNoSrcPos p) fy)
    hnegate _ p = T.fatal p
    
  
  gabs pabs p =
    T.ufun1 a41v5v41v29abs pabs p habs
    where
    
    habs fz p =
      T.con2 T.mkNoSrcPos p (:+) (+:+) (T.uwrapForward p (hmagnitude fz p))
        (T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 0))
    
  
  gsignum psignum p =
    T.ufun1 a42v5v43v56signum psignum p hsignum
    where
    
    hsignum fv42v12v42v12n p =
      T.ucguard
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fv42v12v42v12n
          (T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p 0))) (h p) (y1signum fv42v12v42v12n p)
      where
      
      h p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 0)
      h p = y1signum fv42v12v42v12n p
      
    hsignum fv42v12v42v12n p = y1signum fv42v12v42v12n p
    
    y1signum (fz@(T.R (fx :+ fy) _)) p =
      T.con2 T.mkNoSrcPos p (:+) (+:+)
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !/ p) fx (gr T.mkNoSrcPos p))
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !/ p) fy (gr T.mkNoSrcPos p))
      where
      
      gr pr p = T.uconstUse pr p sr
      
      sr =
        T.uconstDef p a43v42v43v56r (\ p -> T.uwrapForward p (hmagnitude fz p))
      
    y1signum _ p = T.fatal p
    
  
  gfromInteger pfromInteger p =
    T.ufun1 a44v5v44v39fromInteger pfromInteger p hfromInteger
    where
    
    hfromInteger fn p =
      T.con2 T.mkNoSrcPos p (:+) (+:+)
        (T.uap1 T.mkNoSrcPos p (gfromInteger T.mkNoSrcPos p) fn)
        (T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 0))
    
  

instance RealFloat a => Fractional ((Complex a))
  where
  
  (!/) (%/) p =
    T.ufun2 (+&^=#!=&^=#!/) (%/) p (*/)
    where
    
    (*/) (T.R (fx :+ fy) _) (T.R (fx' :+ fy') _) p =
      T.con2 T.mkNoSrcPos p (:+) (+:+)
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !/ p)
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !+ p)
            (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !* p) fx (gx'' T.mkNoSrcPos p))
            (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !* p) fy
              (gy'' T.mkNoSrcPos p))) (gd T.mkNoSrcPos p))
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !/ p)
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !- p)
            (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !* p) fy (gx'' T.mkNoSrcPos p))
            (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !* p) fx
              (gy'' T.mkNoSrcPos p))) (gd T.mkNoSrcPos p))
      where
      
      gx'' px'' p = T.uconstUse px'' p sx''
      
      sx'' =
        T.uconstDef p a49v5v49v25x''
          (\ p ->
            T.uap2 T.mkNoSrcPos p (gscaleFloat T.mkNoSrcPos p)
              (gk T.mkNoSrcPos p) fx')
      
      gy'' py'' p = T.uconstUse py'' p sy''
      
      sy'' =
        T.uconstDef p a50v5v50v25y''
          (\ p ->
            T.uap2 T.mkNoSrcPos p (gscaleFloat T.mkNoSrcPos p)
              (gk T.mkNoSrcPos p) fy')
      
      gk pk p = T.uconstUse pk p sk
      
      sk =
        T.uconstDef p a51v5v51v42k
          (\ p ->
            T.uap1 T.mkNoSrcPos p (gnegate T.mkNoSrcPos p)
              (T.uap2 T.mkNoSrcPos p (gmax T.mkNoSrcPos p)
                (T.uap1 T.mkNoSrcPos p (gexponent T.mkNoSrcPos p) fx')
                (T.uap1 T.mkNoSrcPos p (gexponent T.mkNoSrcPos p) fy')))
      
      gd pd p = T.uconstUse pd p sd
      
      sd =
        T.uconstDef p a52v5v52v25d
          (\ p ->
            T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !+ p)
              (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !* p) fx'
                (gx'' T.mkNoSrcPos p))
              (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !* p) fy'
                (gy'' T.mkNoSrcPos p)))
      
    (*/) _ _ p = T.fatal p
    
  
  gfromRational pfromRational p =
    T.ufun1 a54v3v54v39fromRational pfromRational p hfromRational
    where
    
    hfromRational fa p =
      T.con2 T.mkNoSrcPos p (:+) (+:+)
        (T.uap1 T.mkNoSrcPos p (gfromRational T.mkNoSrcPos p) fa)
        (T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 0))
    
  

instance RealFloat a => Floating ((Complex a))
  where
  
  gpi ppi p = T.uconstUse ppi p spi
  
  spi =
    T.uconstDef T.mkRoot a57v5v57v29pi
      (\ p ->
        T.con2 T.mkNoSrcPos p (:+) (+:+) (gpi T.mkNoSrcPos p)
          (T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p 0)))
  
  gexp pexp p =
    T.ufun1 a58v5v59v40exp pexp p hexp
    where
    
    hexp (T.R (fx :+ fy) _) p =
      T.con2 T.mkNoSrcPos p (:+) (+:+)
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !* p) (gexpx T.mkNoSrcPos p)
          (T.uap1 T.mkNoSrcPos p (gcos T.mkNoSrcPos p) fy))
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !* p) (gexpx T.mkNoSrcPos p)
          (T.uap1 T.mkNoSrcPos p (gsin T.mkNoSrcPos p) fy))
      where
      
      gexpx pexpx p = T.uconstUse pexpx p sexpx
      
      sexpx =
        T.uconstDef p a59v29v59v40expx
          (\ p -> T.uap1 T.mkNoSrcPos p (gexp T.mkNoSrcPos p) fx)
      
    hexp _ p = T.fatal p
    
  
  glog plog p =
    T.ufun1 a60v5v60v50log plog p hlog
    where
    
    hlog fz p =
      T.con2 T.mkNoSrcPos p (:+) (+:+)
        (T.uap1 T.mkNoSrcPos p (glog T.mkNoSrcPos p)
          (T.uwrapForward p (hmagnitude fz p))) (T.uwrapForward p (hphase fz p))
    
  
  gsqrt psqrt p =
    T.ufun1 a62v5v66v67sqrt psqrt p hsqrt
    where
    
    hsqrt fv62v10v62v10n p =
      T.ucguard
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fv62v10v62v10n
          (T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p 0))) (h p) (y1sqrt fv62v10v62v10n p)
      where
      
      h p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 0)
      h p = y1sqrt fv62v10v62v10n p
      
    hsqrt fv62v10v62v10n p = y1sqrt fv62v10v62v10n p
    
    y1sqrt (fz@(T.R (fx :+ fy) _)) p =
      T.con2 T.mkNoSrcPos p (:+) (+:+) (gu T.mkNoSrcPos p)
        (T.ucif p
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !< p) fy
            (T.uap1 T.mkNoSrcPos p
              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
              (T.conInteger T.mkNoSrcPos p 0)))
          (T.uap1 T.mkNoSrcPos p (gnegate T.mkNoSrcPos p) (gv T.mkNoSrcPos p))
          (gv T.mkNoSrcPos p))
      where
      
      gu pu p = T.uconstUse pu p su
      
      gv pu p = T.uconstUse pu p sv
      
      j64v29v64v33u =
        case
          T.ucif p
            (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !< p) fx
              (T.uap1 T.mkNoSrcPos p
                (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                (T.conInteger T.mkNoSrcPos p 0)))
            (T.con2 T.mkNoSrcPos p T.Tuple2 T.aTuple2 (gv' T.mkNoSrcPos p)
              (gu' T.mkNoSrcPos p))
            (T.con2 T.mkNoSrcPos p T.Tuple2 T.aTuple2 (gu' T.mkNoSrcPos p)
              (gv' T.mkNoSrcPos p)) of
          T.R (T.Tuple2 fu fv) ku -> (ku,fu,fv)
          _ -> T.fatal p
      
      su =
        T.uconstDef p a64v30v64v30u
          (\ _ -> case j64v29v64v33u of (ku,fu,fv) -> fu)
      
      sv =
        T.uconstDef p a64v32v64v32v
          (\ _ -> case j64v29v64v33u of (ku,fu,fv) -> fv)
      
      gv' pv' p = T.uconstUse pv' p sv'
      
      sv' =
        T.uconstDef p a65v29v65v49v'
          (\ p ->
            T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !/ p)
              (T.uap1 T.mkNoSrcPos p (gabs T.mkNoSrcPos p) fy)
              (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !* p) (gu' T.mkNoSrcPos p)
                (T.uap1 T.mkNoSrcPos p
                  (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                  (T.conInteger T.mkNoSrcPos p 2))))
      
      gu' pu' p = T.uconstUse pu' p su'
      
      su' =
        T.uconstDef p a66v29v66v67u'
          (\ p ->
            T.uap1 T.mkNoSrcPos p (gsqrt T.mkNoSrcPos p)
              (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !/ p)
                (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !+ p)
                  (T.uwrapForward p (hmagnitude fz p))
                  (T.uap1 T.mkNoSrcPos p (gabs T.mkNoSrcPos p) fx))
                (T.uap1 T.mkNoSrcPos p
                  (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                  (T.conInteger T.mkNoSrcPos p 2))))
      
    y1sqrt _ p = T.fatal p
    
  
  gsin psin p =
    T.ufun1 a68v5v68v54sin psin p hsin
    where
    
    hsin (T.R (fx :+ fy) _) p =
      T.con2 T.mkNoSrcPos p (:+) (+:+)
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !* p)
          (T.uap1 T.mkNoSrcPos p (gsin T.mkNoSrcPos p) fx)
          (T.uap1 T.mkNoSrcPos p (gcosh T.mkNoSrcPos p) fy))
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !* p)
          (T.uap1 T.mkNoSrcPos p (gcos T.mkNoSrcPos p) fx)
          (T.uap1 T.mkNoSrcPos p (gsinh T.mkNoSrcPos p) fy))
    hsin _ p = T.fatal p
    
  
  gcos pcos p =
    T.ufun1 a69v5v69v57cos pcos p hcos
    where
    
    hcos (T.R (fx :+ fy) _) p =
      T.con2 T.mkNoSrcPos p (:+) (+:+)
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !* p)
          (T.uap1 T.mkNoSrcPos p (gcos T.mkNoSrcPos p) fx)
          (T.uap1 T.mkNoSrcPos p (gcosh T.mkNoSrcPos p) fy))
        (T.uap1 T.mkNoSrcPos p (gnegate T.mkNoSrcPos p)
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !* p)
            (T.uap1 T.mkNoSrcPos p (gsin T.mkNoSrcPos p) fx)
            (T.uap1 T.mkNoSrcPos p (gsinh T.mkNoSrcPos p) fy)))
    hcos _ p = T.fatal p
    
  
  gtan ptan p =
    T.ufun1 a70v5v74v42tan ptan p htan
    where
    
    htan (T.R (fx :+ fy) _) p =
      T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !/ p)
        (T.con2 T.mkNoSrcPos p (:+) (+:+)
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !* p) (gsinx T.mkNoSrcPos p)
            (gcoshy T.mkNoSrcPos p))
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !* p) (gcosx T.mkNoSrcPos p)
            (gsinhy T.mkNoSrcPos p)))
        (T.con2 T.mkNoSrcPos p (:+) (+:+)
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !* p) (gcosx T.mkNoSrcPos p)
            (gcoshy T.mkNoSrcPos p))
          (T.uap1 T.mkNoSrcPos p (gnegate T.mkNoSrcPos p)
            (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !* p) (gsinx T.mkNoSrcPos p)
              (gsinhy T.mkNoSrcPos p))))
      where
      
      gsinx psinx p = T.uconstUse psinx p ssinx
      
      ssinx =
        T.uconstDef p a71v29v71v41sinx
          (\ p -> T.uap1 T.mkNoSrcPos p (gsin T.mkNoSrcPos p) fx)
      
      gcosx pcosx p = T.uconstUse pcosx p scosx
      
      scosx =
        T.uconstDef p a72v29v72v41cosx
          (\ p -> T.uap1 T.mkNoSrcPos p (gcos T.mkNoSrcPos p) fx)
      
      gsinhy psinhy p = T.uconstUse psinhy p ssinhy
      
      ssinhy =
        T.uconstDef p a73v29v73v42sinhy
          (\ p -> T.uap1 T.mkNoSrcPos p (gsinh T.mkNoSrcPos p) fy)
      
      gcoshy pcoshy p = T.uconstUse pcoshy p scoshy
      
      scoshy =
        T.uconstDef p a74v29v74v42coshy
          (\ p -> T.uap1 T.mkNoSrcPos p (gcosh T.mkNoSrcPos p) fy)
      
    htan _ p = T.fatal p
    
  
  gsinh psinh p =
    T.ufun1 a76v5v76v55sinh psinh p hsinh
    where
    
    hsinh (T.R (fx :+ fy) _) p =
      T.con2 T.mkNoSrcPos p (:+) (+:+)
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !* p)
          (T.uap1 T.mkNoSrcPos p (gcos T.mkNoSrcPos p) fy)
          (T.uap1 T.mkNoSrcPos p (gsinh T.mkNoSrcPos p) fx))
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !* p)
          (T.uap1 T.mkNoSrcPos p (gsin T.mkNoSrcPos p) fy)
          (T.uap1 T.mkNoSrcPos p (gcosh T.mkNoSrcPos p) fx))
    hsinh _ p = T.fatal p
    
  
  gcosh pcosh p =
    T.ufun1 a77v5v77v54cosh pcosh p hcosh
    where
    
    hcosh (T.R (fx :+ fy) _) p =
      T.con2 T.mkNoSrcPos p (:+) (+:+)
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !* p)
          (T.uap1 T.mkNoSrcPos p (gcos T.mkNoSrcPos p) fy)
          (T.uap1 T.mkNoSrcPos p (gcosh T.mkNoSrcPos p) fx))
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !* p)
          (T.uap1 T.mkNoSrcPos p (gsin T.mkNoSrcPos p) fy)
          (T.uap1 T.mkNoSrcPos p (gsinh T.mkNoSrcPos p) fx))
    hcosh _ p = T.fatal p
    
  
  gtanh ptanh p =
    T.ufun1 a78v5v82v42tanh ptanh p htanh
    where
    
    htanh (T.R (fx :+ fy) _) p =
      T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !/ p)
        (T.con2 T.mkNoSrcPos p (:+) (+:+)
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !* p) (gcosy T.mkNoSrcPos p)
            (gsinhx T.mkNoSrcPos p))
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !* p) (gsiny T.mkNoSrcPos p)
            (gcoshx T.mkNoSrcPos p)))
        (T.con2 T.mkNoSrcPos p (:+) (+:+)
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !* p) (gcosy T.mkNoSrcPos p)
            (gcoshx T.mkNoSrcPos p))
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !* p) (gsiny T.mkNoSrcPos p)
            (gsinhx T.mkNoSrcPos p)))
      where
      
      gsiny psiny p = T.uconstUse psiny p ssiny
      
      ssiny =
        T.uconstDef p a79v29v79v41siny
          (\ p -> T.uap1 T.mkNoSrcPos p (gsin T.mkNoSrcPos p) fy)
      
      gcosy pcosy p = T.uconstUse pcosy p scosy
      
      scosy =
        T.uconstDef p a80v29v80v41cosy
          (\ p -> T.uap1 T.mkNoSrcPos p (gcos T.mkNoSrcPos p) fy)
      
      gsinhx psinhx p = T.uconstUse psinhx p ssinhx
      
      ssinhx =
        T.uconstDef p a81v29v81v42sinhx
          (\ p -> T.uap1 T.mkNoSrcPos p (gsinh T.mkNoSrcPos p) fx)
      
      gcoshx pcoshx p = T.uconstUse pcoshx p scoshx
      
      scoshx =
        T.uconstDef p a82v29v82v42coshx
          (\ p -> T.uap1 T.mkNoSrcPos p (gcosh T.mkNoSrcPos p) fx)
      
    htanh _ p = T.fatal p
    
  
  gasin pasin p =
    T.ufun1 a84v5v85v70asin pasin p hasin
    where
    
    hasin (fz@(T.R (fx :+ fy) _)) p =
      T.con2 T.mkNoSrcPos p (:+) (+:+) (gy' T.mkNoSrcPos p)
        (T.uap1 T.mkNoSrcPos p (gnegate T.mkNoSrcPos p) (gx' T.mkNoSrcPos p))
      where
      
      gx' px' p = T.uconstUse px' p sx'
      
      gy' px' p = T.uconstUse px' p sy'
      
      j85v31v85v36x' =
        case
          T.uap1 T.mkNoSrcPos p (glog T.mkNoSrcPos p)
            (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !+ p)
              (T.con2 T.mkNoSrcPos p (:+) (+:+)
                (T.uap1 T.mkNoSrcPos p (gnegate T.mkNoSrcPos p) fy) fx)
              (T.uap1 T.mkNoSrcPos p (gsqrt T.mkNoSrcPos p)
                (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !- p)
                  (T.uap1 T.mkNoSrcPos p
                    (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                    (T.conInteger T.mkNoSrcPos p 1))
                  (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !* p) fz fz)))) of
          T.R (fx' :+ fy') kx' -> (kx',fx',fy')
          _ -> T.fatal p
      
      sx' =
        T.uconstDef p a85v31v85v32x'
          (\ _ -> case j85v31v85v36x' of (kx',fx',fy') -> fx')
      
      sy' =
        T.uconstDef p a85v35v85v36y'
          (\ _ -> case j85v31v85v36x' of (kx',fx',fy') -> fy')
      
    hasin _ p = T.fatal p
    
  
  gacos pacos p =
    T.ufun1 a86v5v88v54acos pacos p hacos
    where
    
    hacos (fz@(T.R (fx :+ fy) _)) p =
      T.con2 T.mkNoSrcPos p (:+) (+:+) (gy'' T.mkNoSrcPos p)
        (T.uap1 T.mkNoSrcPos p (gnegate T.mkNoSrcPos p) (gx'' T.mkNoSrcPos p))
      where
      
      gx'' px'' p = T.uconstUse px'' p sx''
      
      gy'' px'' p = T.uconstUse px'' p sy''
      
      j87v30v87v37x'' =
        case
          T.uap1 T.mkNoSrcPos p (glog T.mkNoSrcPos p)
            (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !+ p) fz
              (T.con2 T.mkNoSrcPos p (:+) (+:+)
                (T.uap1 T.mkNoSrcPos p (gnegate T.mkNoSrcPos p)
                  (gy' T.mkNoSrcPos p)) (gx' T.mkNoSrcPos p))) of
          T.R (fx'' :+ fy'') kx'' -> (kx'',fx'',fy'')
          _ -> T.fatal p
      
      sx'' =
        T.uconstDef p a87v30v87v32x''
          (\ _ -> case j87v30v87v37x'' of (kx'',fx'',fy'') -> fx'')
      
      sy'' =
        T.uconstDef p a87v35v87v37y''
          (\ _ -> case j87v30v87v37x'' of (kx'',fx'',fy'') -> fy'')
      
      gx' px' p = T.uconstUse px' p sx'
      
      gy' px' p = T.uconstUse px' p sy'
      
      j88v30v88v35x' =
        case
          T.uap1 T.mkNoSrcPos p (gsqrt T.mkNoSrcPos p)
            (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !- p)
              (T.uap1 T.mkNoSrcPos p
                (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                (T.conInteger T.mkNoSrcPos p 1))
              (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !* p) fz fz)) of
          T.R (fx' :+ fy') kx' -> (kx',fx',fy')
          _ -> T.fatal p
      
      sx' =
        T.uconstDef p a88v30v88v31x'
          (\ _ -> case j88v30v88v35x' of (kx',fx',fy') -> fx')
      
      sy' =
        T.uconstDef p a88v34v88v35y'
          (\ _ -> case j88v30v88v35x' of (kx',fx',fy') -> fy')
      
    hacos _ p = T.fatal p
    
  
  gatan patan p =
    T.ufun1 a89v5v90v68atan patan p hatan
    where
    
    hatan (fz@(T.R (fx :+ fy) _)) p =
      T.con2 T.mkNoSrcPos p (:+) (+:+) (gy' T.mkNoSrcPos p)
        (T.uap1 T.mkNoSrcPos p (gnegate T.mkNoSrcPos p) (gx' T.mkNoSrcPos p))
      where
      
      gx' px' p = T.uconstUse px' p sx'
      
      gy' px' p = T.uconstUse px' p sy'
      
      j90v30v90v35x' =
        case
          T.uap1 T.mkNoSrcPos p (glog T.mkNoSrcPos p)
            (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !/ p)
              (T.con2 T.mkNoSrcPos p (:+) (+:+)
                (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !- p)
                  (T.uap1 T.mkNoSrcPos p
                    (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                    (T.conInteger T.mkNoSrcPos p 1)) fy) fx)
              (T.uap1 T.mkNoSrcPos p (gsqrt T.mkNoSrcPos p)
                (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !+ p)
                  (T.uap1 T.mkNoSrcPos p
                    (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                    (T.conInteger T.mkNoSrcPos p 1))
                  (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !* p) fz fz)))) of
          T.R (fx' :+ fy') kx' -> (kx',fx',fy')
          _ -> T.fatal p
      
      sx' =
        T.uconstDef p a90v30v90v31x'
          (\ _ -> case j90v30v90v35x' of (kx',fx',fy') -> fx')
      
      sy' =
        T.uconstDef p a90v34v90v35y'
          (\ _ -> case j90v30v90v35x' of (kx',fx',fy') -> fy')
      
    hatan _ p = T.fatal p
    
  
  gasinh pasinh p =
    T.ufun1 a92v5v92v42asinh pasinh p hasinh
    where
    
    hasinh fz p =
      T.uap1 T.mkNoSrcPos p (glog T.mkNoSrcPos p)
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !+ p) fz
          (T.uap1 T.mkNoSrcPos p (gsqrt T.mkNoSrcPos p)
            (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !+ p)
              (T.uap1 T.mkNoSrcPos p
                (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                (T.conInteger T.mkNoSrcPos p 1))
              (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !* p) fz fz))))
    
  
  gacosh pacosh p =
    T.ufun1 a93v5v93v55acosh pacosh p hacosh
    where
    
    hacosh fz p =
      T.uap1 T.mkNoSrcPos p (glog T.mkNoSrcPos p)
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !+ p) fz
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !* p)
            (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !+ p) fz
              (T.uap1 T.mkNoSrcPos p
                (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                (T.conInteger T.mkNoSrcPos p 1)))
            (T.uap1 T.mkNoSrcPos p (gsqrt T.mkNoSrcPos p)
              (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !/ p)
                (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !- p) fz
                  (T.uap1 T.mkNoSrcPos p
                    (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                    (T.conInteger T.mkNoSrcPos p 1)))
                (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !+ p) fz
                  (T.uap1 T.mkNoSrcPos p
                    (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                    (T.conInteger T.mkNoSrcPos p 1)))))))
    
  
  gatanh patanh p =
    T.ufun1 a94v5v94v46atanh patanh p hatanh
    where
    
    hatanh fz p =
      T.uap1 T.mkNoSrcPos p (glog T.mkNoSrcPos p)
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !/ p)
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !+ p)
            (T.uap1 T.mkNoSrcPos p
              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
              (T.conInteger T.mkNoSrcPos p 1)) fz)
          (T.uap1 T.mkNoSrcPos p (gsqrt T.mkNoSrcPos p)
            (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !- p)
              (T.uap1 T.mkNoSrcPos p
                (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                (T.conInteger T.mkNoSrcPos p 1))
              (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !* p) fz fz))))
    
  

tComplex = T.mkModule "Complex" "Complex.hs" Prelude.False

(+:+) = T.mkConstructor tComplex 60021 60022 24 2 ":+"

arealPart = T.mkVariable tComplex 100001 100021 3 1 "realPart" Prelude.False

aimagPart = T.mkVariable tComplex 110001 110021 3 1 "imagPart" Prelude.False

aconjugate = T.mkVariable tComplex 140001 140028 3 1 "conjugate" Prelude.False

amkPolar = T.mkVariable tComplex 170001 170050 3 2 "mkPolar" Prelude.False

acis = T.mkVariable tComplex 200001 200036 3 1 "cis" Prelude.False

apolar = T.mkVariable tComplex 230001 230034 3 1 "polar" Prelude.False

amagnitude = T.mkVariable tComplex 260001 290018 3 1 "magnitude" Prelude.False

aphase = T.mkVariable tComplex 320001 330026 3 1 "phase" Prelude.False

(++=%@=+=%>==) = T.mkVariable tComplex 60038 60039 3 2 "==" Prelude.False

a6v41v6v44readsPrec =
  T.mkVariable tComplex 60041 60044 3 1 "readsPrec" Prelude.False

a6v46v6v49showsPrec =
  T.mkVariable tComplex 60046 60049 3 2 "showsPrec" Prelude.False

(+%^=#$=%^=#$+) = T.mkVariable tComplex 370012 370012 26 2 "+" Prelude.False

(+%@=#$=%@=#$-) = T.mkVariable tComplex 380012 380012 26 2 "-" Prelude.False

(+%>=#$=%>=#$*) = T.mkVariable tComplex 390012 390012 30 2 "*" Prelude.False

a40v5v40v41negate =
  T.mkVariable tComplex 400005 400041 3 1 "negate" Prelude.False

a41v5v41v29abs = T.mkVariable tComplex 410005 410029 3 1 "abs" Prelude.False

a42v5v43v56signum =
  T.mkVariable tComplex 420005 430056 3 1 "signum" Prelude.False

a44v5v44v39fromInteger =
  T.mkVariable tComplex 440005 440039 3 1 "fromInteger" Prelude.False

(+&^=#!=&^=#!/) = T.mkVariable tComplex 470010 470010 30 2 "/" Prelude.False

a54v3v54v39fromRational =
  T.mkVariable tComplex 540003 540039 3 1 "fromRational" Prelude.False

a57v5v57v29pi = T.mkVariable tComplex 570005 570029 3 0 "pi" Prelude.False

a58v5v59v40exp = T.mkVariable tComplex 580005 590040 3 1 "exp" Prelude.False

a60v5v60v50log = T.mkVariable tComplex 600005 600050 3 1 "log" Prelude.False

a62v5v66v67sqrt = T.mkVariable tComplex 620005 660067 3 1 "sqrt" Prelude.False

a68v5v68v54sin = T.mkVariable tComplex 680005 680054 3 1 "sin" Prelude.False

a69v5v69v57cos = T.mkVariable tComplex 690005 690057 3 1 "cos" Prelude.False

a70v5v74v42tan = T.mkVariable tComplex 700005 740042 3 1 "tan" Prelude.False

a76v5v76v55sinh = T.mkVariable tComplex 760005 760055 3 1 "sinh" Prelude.False

a77v5v77v54cosh = T.mkVariable tComplex 770005 770054 3 1 "cosh" Prelude.False

a78v5v82v42tanh = T.mkVariable tComplex 780005 820042 3 1 "tanh" Prelude.False

a84v5v85v70asin = T.mkVariable tComplex 840005 850070 3 1 "asin" Prelude.False

a86v5v88v54acos = T.mkVariable tComplex 860005 880054 3 1 "acos" Prelude.False

a89v5v90v68atan = T.mkVariable tComplex 890005 900068 3 1 "atan" Prelude.False

a92v5v92v42asinh = T.mkVariable tComplex 920005 920042 3 1 "asinh" Prelude.False

a93v5v93v55acosh = T.mkVariable tComplex 930005 930055 3 1 "acosh" Prelude.False

a94v5v94v46atanh = T.mkVariable tComplex 940005 940046 3 1 "atanh" Prelude.False

a28v11v28v43k = T.mkVariable tComplex 280011 280043 3 0 "k" Prelude.True

a29v11v29v18mk = T.mkVariable tComplex 290011 290018 3 0 "mk" Prelude.True

a43v42v43v56r = T.mkVariable tComplex 430042 430056 3 0 "r" Prelude.True

a49v5v49v25x'' = T.mkVariable tComplex 490005 490025 3 0 "x''" Prelude.True

a50v5v50v25y'' = T.mkVariable tComplex 500005 500025 3 0 "y''" Prelude.True

a51v5v51v42k = T.mkVariable tComplex 510005 510042 3 0 "k" Prelude.True

a52v5v52v25d = T.mkVariable tComplex 520005 520025 3 0 "d" Prelude.True

a59v29v59v40expx = T.mkVariable tComplex 590029 590040 3 0 "expx" Prelude.True

a64v30v64v30u = T.mkVariable tComplex 640030 640030 3 0 "u" Prelude.True

a64v32v64v32v = T.mkVariable tComplex 640032 640032 3 0 "v" Prelude.True

a65v29v65v49v' = T.mkVariable tComplex 650029 650049 3 0 "v'" Prelude.True

a66v29v66v67u' = T.mkVariable tComplex 660029 660067 3 0 "u'" Prelude.True

a71v29v71v41sinx = T.mkVariable tComplex 710029 710041 3 0 "sinx" Prelude.True

a72v29v72v41cosx = T.mkVariable tComplex 720029 720041 3 0 "cosx" Prelude.True

a73v29v73v42sinhy = T.mkVariable tComplex 730029 730042 3 0 "sinhy" Prelude.True

a74v29v74v42coshy = T.mkVariable tComplex 740029 740042 3 0 "coshy" Prelude.True

a79v29v79v41siny = T.mkVariable tComplex 790029 790041 3 0 "siny" Prelude.True

a80v29v80v41cosy = T.mkVariable tComplex 800029 800041 3 0 "cosy" Prelude.True

a81v29v81v42sinhx = T.mkVariable tComplex 810029 810042 3 0 "sinhx" Prelude.True

a82v29v82v42coshx = T.mkVariable tComplex 820029 820042 3 0 "coshx" Prelude.True

a85v31v85v32x' = T.mkVariable tComplex 850031 850032 3 0 "x'" Prelude.True

a85v35v85v36y' = T.mkVariable tComplex 850035 850036 3 0 "y'" Prelude.True

a87v30v87v32x'' = T.mkVariable tComplex 870030 870032 3 0 "x''" Prelude.True

a87v35v87v37y'' = T.mkVariable tComplex 870035 870037 3 0 "y''" Prelude.True

a88v30v88v31x' = T.mkVariable tComplex 880030 880031 3 0 "x'" Prelude.True

a88v34v88v35y' = T.mkVariable tComplex 880034 880035 3 0 "y'" Prelude.True

a90v30v90v31x' = T.mkVariable tComplex 900030 900031 3 0 "x'" Prelude.True

a90v34v90v35y' = T.mkVariable tComplex 900034 900035 3 0 "y'" Prelude.True
