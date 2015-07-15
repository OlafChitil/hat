module Hat.Complex
       (Complex((:+)), (+:+), grealPart, arealPart,
        hrealPart, gimagPart, aimagPart, himagPart,
        gconjugate, aconjugate, hconjugate, gmkPolar,
        amkPolar, hmkPolar, gcis, acis, hcis, gpolar, apolar,
        hpolar, gmagnitude, amagnitude, hmagnitude, gphase,
        aphase, hphase)
       where
import qualified Prelude
import qualified Hat.Hat as T
import qualified Hat.PreludeBasic
import qualified Hat.PreludeBuiltinTypes
import Hat.Prelude
 
data Complex a = !(T.R a) :+ !(T.R a)
 
instance T.WrapVal (Complex a) where
        wrapVal pwrapVal
          kwrapVal@((:+) (T.R _ z1wrapVal) (T.R _ z2wrapVal)) p
          = T.R kwrapVal
              (T.mkValueApp2 p pwrapVal (+:+) z1wrapVal z2wrapVal)
 
instance (Eq a) => Eq (Complex a) where
        (%==) !== p = T.ufun2 (++=%@=+=%>==) (%==) p (*==)
          where (T.R ((:+) fy1 fy2) _ *== T.R ((:+) fy3 fy4) _)
                  p
                  = T.uwrapForward p
                      ((Hat.PreludeBasic.*&&)
                         (T.uap2 T.mkNoSrcPos p
                            ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                            fy1
                            fy3)
                         (T.uwrapForward p
                            ((Hat.PreludeBasic.*&&)
                               (T.uap2 T.mkNoSrcPos p
                                  ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                                  fy2
                                  fy4)
                               (T.con0 T.mkNoSrcPos p
                                  Hat.PreludeBuiltinTypes.True
                                  Hat.PreludeBuiltinTypes.aTrue)
                               p))
                         p)
                (_ *== _) p
                  = T.con0 T.mkNoSrcPos p Hat.PreludeBuiltinTypes.False
                      Hat.PreludeBuiltinTypes.aFalse
 
instance (Read a) => Read (Complex a) where
        greadsPrec preadsPrec p
          = T.ufun1 c6v41v6v44readsPrec preadsPrec p hreadsPrec
          where hreadsPrec fy1 p
                  = T.uwrapForward p
                      (hreadParen
                         (T.uap2 T.mkNoSrcPos p ((!>) T.mkNoSrcPos p) fy1
                            (T.uap1 T.mkNoSrcPos p
                               (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                               (T.conInteger T.mkNoSrcPos p (6))))
                         (T.uap2 T.mkNoSrcPos p
                            (Hat.PreludeBasic.gthenAp T.mkNoSrcPos p)
                            (T.uwrapForward p
                               (Hat.PreludeBasic.hthenLex
                                  (T.uap2 T.mkNoSrcPos p
                                     (Hat.PreludeBasic.gthenAp T.mkNoSrcPos p)
                                     (T.uap1 T.mkNoSrcPos p
                                        (Hat.PreludeBasic.gyield T.mkNoSrcPos p)
                                        (T.pa0 (:+) T.cn2 T.mkNoSrcPos p (+:+)))
                                     (T.uap1 T.mkNoSrcPos p
                                        (greadsPrec T.mkNoSrcPos p)
                                        (T.uap1 T.mkNoSrcPos p
                                           (Hat.PreludeBasic.gfromInteger
                                              T.mkNoSrcPos
                                              p)
                                           (T.conInteger T.mkNoSrcPos p (7)))))
                                  (T.fromLitString T.mkNoSrcPos p ":+")
                                  p))
                            (T.uap1 T.mkNoSrcPos p (greadsPrec T.mkNoSrcPos p)
                               (T.uap1 T.mkNoSrcPos p
                                  (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                                  (T.conInteger T.mkNoSrcPos p (7)))))
                         p)
 
instance (Show a) => Show (Complex a) where
        gshowsPrec pshowsPrec p
          = T.ufun2 c6v46v6v49showsPrec pshowsPrec p hshowsPrec
          where hshowsPrec fy1 (T.R ((:+) fy2 fy3) _) p
                  = T.uwrapForward p
                      (hshowParen
                         (T.uap2 T.mkNoSrcPos p ((!>) T.mkNoSrcPos p) fy1
                            (T.uap1 T.mkNoSrcPos p
                               (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                               (T.conInteger T.mkNoSrcPos p (6))))
                         (T.uap2 T.mkNoSrcPos p ((!.) T.mkNoSrcPos p)
                            (T.uap2 T.mkNoSrcPos p ((!.) T.mkNoSrcPos p)
                               (T.uap2 T.mkNoSrcPos p
                                  (gshowsPrec T.mkNoSrcPos p)
                                  (T.uap1 T.mkNoSrcPos p
                                     (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos
                                        p)
                                     (T.conInteger T.mkNoSrcPos p (7)))
                                  fy2)
                               (T.uap1 T.mkNoSrcPos p
                                  (gshowString T.mkNoSrcPos p)
                                  (T.fromLitString T.mkNoSrcPos p " :+ ")))
                            (T.uap2 T.mkNoSrcPos p (gshowsPrec T.mkNoSrcPos p)
                               (T.uap1 T.mkNoSrcPos p
                                  (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                                  (T.conInteger T.mkNoSrcPos p (7)))
                               fy3))
                         p)
                hshowsPrec _ _ p = T.fatal p
 
grealPart, gimagPart ::
             (RealFloat a) =>
             T.RefSrcPos -> T.RefExp -> T.R (T.Fun (Complex a) a)
 
hrealPart ::
            (RealFloat a) => T.R (Complex a) -> T.RefExp -> T.R a
 
himagPart ::
            (RealFloat a) => T.R (Complex a) -> T.RefExp -> T.R a
grealPart prealPart p
  = T.ufun1 arealPart prealPart p hrealPart
hrealPart (T.R ((:+) fx fy) _) p
  = T.projection T.mkNoSrcPos p fx
hrealPart _ p = T.fatal p
gimagPart pimagPart p
  = T.ufun1 aimagPart pimagPart p himagPart
himagPart (T.R ((:+) fx fy) _) p
  = T.projection T.mkNoSrcPos p fy
himagPart _ p = T.fatal p
 
gconjugate ::
             (RealFloat a) =>
             T.RefSrcPos ->
               T.RefExp -> T.R (T.Fun (Complex a) (Complex a))
 
hconjugate ::
             (RealFloat a) =>
             T.R (Complex a) -> T.RefExp -> T.R (Complex a)
gconjugate pconjugate p
  = T.ufun1 aconjugate pconjugate p hconjugate
hconjugate (T.R ((:+) fx fy) _) p
  = T.con2 T.mkNoSrcPos p (:+) (+:+) fx
      (T.uap1 T.mkNoSrcPos p
         (Hat.PreludeBasic.gnegate T.mkNoSrcPos p)
         fy)
hconjugate _ p = T.fatal p
 
gmkPolar ::
           (RealFloat a) =>
           T.RefSrcPos ->
             T.RefExp -> T.R (T.Fun a (T.Fun a (Complex a)))
 
hmkPolar ::
           (RealFloat a) =>
           T.R a -> T.R a -> T.RefExp -> T.R (Complex a)
gmkPolar pmkPolar p
  = T.ufun2 amkPolar pmkPolar p hmkPolar
hmkPolar fr ftheta p
  = T.con2 T.mkNoSrcPos p (:+) (+:+)
      (T.uap2 T.mkNoSrcPos p ((!*) T.mkNoSrcPos p) fr
         (T.uap1 T.mkNoSrcPos p (gcos T.mkNoSrcPos p) ftheta))
      (T.uap2 T.mkNoSrcPos p ((!*) T.mkNoSrcPos p) fr
         (T.uap1 T.mkNoSrcPos p (gsin T.mkNoSrcPos p) ftheta))
 
gcis ::
       (RealFloat a) =>
       T.RefSrcPos -> T.RefExp -> T.R (T.Fun a (Complex a))
 
hcis ::
       (RealFloat a) => T.R a -> T.RefExp -> T.R (Complex a)
gcis pcis p = T.ufun1 acis pcis p hcis
hcis ftheta p
  = T.con2 T.mkNoSrcPos p (:+) (+:+)
      (T.uap1 T.mkNoSrcPos p (gcos T.mkNoSrcPos p) ftheta)
      (T.uap1 T.mkNoSrcPos p (gsin T.mkNoSrcPos p) ftheta)
 
gpolar ::
         (RealFloat a) =>
         T.RefSrcPos ->
           T.RefExp -> T.R (T.Fun (Complex a) (T.Tuple2 a a))
 
hpolar ::
         (RealFloat a) =>
         T.R (Complex a) -> T.RefExp -> T.R (T.Tuple2 a a)
gpolar ppolar p = T.ufun1 apolar ppolar p hpolar
hpolar fz p
  = T.con2 T.mkNoSrcPos p T.Tuple2 T.aTuple2
      (T.uwrapForward p (hmagnitude fz p))
      (T.uwrapForward p (hphase fz p))
 
gmagnitude ::
             (RealFloat a) =>
             T.RefSrcPos -> T.RefExp -> T.R (T.Fun (Complex a) a)
 
hmagnitude ::
             (RealFloat a) => T.R (Complex a) -> T.RefExp -> T.R a
gmagnitude pmagnitude p
  = T.ufun1 amagnitude pmagnitude p hmagnitude
hmagnitude (T.R ((:+) fx fy) _) p
  = T.uap2 T.mkNoSrcPos p (gscaleFloat T.mkNoSrcPos p)
      (gk T.mkNoSrcPos p)
      (T.uap1 T.mkNoSrcPos p (gsqrt T.mkNoSrcPos p)
         (T.uap2 T.mkNoSrcPos p ((!+) T.mkNoSrcPos p)
            (T.uwrapForward p
               ((*^)
                  (T.uap2 T.mkNoSrcPos p (gscaleFloat T.mkNoSrcPos p)
                     (gmk T.mkNoSrcPos p)
                     fx)
                  (T.uap1 T.mkNoSrcPos p
                     (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                     (T.conInteger T.mkNoSrcPos p (2))
                     :: T.R Integer)
                  p))
            (T.uwrapForward p
               ((*^)
                  (T.uap2 T.mkNoSrcPos p (gscaleFloat T.mkNoSrcPos p)
                     (gmk T.mkNoSrcPos p)
                     fy)
                  (T.uap1 T.mkNoSrcPos p
                     (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                     (T.conInteger T.mkNoSrcPos p (2))
                     :: T.R Integer)
                  p))))
  where gk pk p = T.uconstUse pk p sk
        sk
          = T.uconstDef p c28v11v28v44k
              (\ p ->
                 T.uap2 T.mkNoSrcPos p (gmax T.mkNoSrcPos p)
                   (T.uap1 T.mkNoSrcPos p (gexponent T.mkNoSrcPos p) fx)
                   (T.uap1 T.mkNoSrcPos p (gexponent T.mkNoSrcPos p)
                      fy))
        gmk pmk p = T.uconstUse pmk p smk
        smk
          = T.uconstDef p c29v11v29v18mk
              (\ p ->
                 T.uap1 T.mkNoSrcPos p
                   (Hat.PreludeBasic.gnegate T.mkNoSrcPos p)
                   (gk T.mkNoSrcPos p))
hmagnitude _ p = T.fatal p
 
gphase ::
         (RealFloat a) =>
         T.RefSrcPos -> T.RefExp -> T.R (T.Fun (Complex a) a)
 
hphase ::
         (RealFloat a) => T.R (Complex a) -> T.RefExp -> T.R a
gphase pphase p = T.ufun1 aphase pphase p hphase
hphase
  z1phase@(T.R ((:+) fv32v8v32v8n v32v13v32v13n) _) p
  = T.ucguard
      (T.uap2 T.mkNoSrcPos p
         ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
         fv32v8v32v8n
         (T.uap1 T.mkNoSrcPos p
            (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p (0))))
      (h32v1v32v18n v32v13v32v13n p)
      (y1phase z1phase p)
  where h32v1v32v18n fv32v13v32v13n p
          = T.ucguard
              (T.uap2 T.mkNoSrcPos p
                 ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                 fv32v13v32v13n
                 (T.uap1 T.mkNoSrcPos p
                    (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                    (T.conInteger T.mkNoSrcPos p (0))))
              (h32v1v32v18n p)
              (y1phase z1phase p)
          where h32v1v32v18n p
                  = T.uap1 T.mkNoSrcPos p
                      (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                      (T.conInteger T.mkNoSrcPos p (0))
                h32v1v32v18n p = y1phase z1phase p
        h32v1v32v18n _ p = y1phase z1phase p
hphase z1phase p = y1phase z1phase p
y1phase (T.R ((:+) fx fy) _) p
  = T.uap2 T.mkNoSrcPos p (gatan2 T.mkNoSrcPos p) fy fx
y1phase _ p = T.fatal p
 
instance (RealFloat a) => Num (Complex a) where
        (%+) !+ p = T.ufun2 (+%^=*=%^=&#+) (%+) p (*+)
          where ((T.R ((:+) fx fy) _) *+
                   (T.R ((:+) fx' fy') _))
                  p
                  = T.con2 T.mkNoSrcPos p (:+) (+:+)
                      (T.uap2 T.mkNoSrcPos p ((!+) T.mkNoSrcPos p) fx fx')
                      (T.uap2 T.mkNoSrcPos p ((!+) T.mkNoSrcPos p) fy fy')
                (_ *+ _) p = T.fatal p
        (%-) !- p = T.ufun2 (+%@=*=%@=&#-) (%-) p (*-)
          where ((T.R ((:+) fx fy) _) *-
                   (T.R ((:+) fx' fy') _))
                  p
                  = T.con2 T.mkNoSrcPos p (:+) (+:+)
                      (T.uap2 T.mkNoSrcPos p ((!-) T.mkNoSrcPos p) fx fx')
                      (T.uap2 T.mkNoSrcPos p ((!-) T.mkNoSrcPos p) fy fy')
                (_ *- _) p = T.fatal p
        (%*) !* p = T.ufun2 (+%>=*=%>=*#*) (%*) p (**)
          where ((T.R ((:+) fx fy) _) **
                   (T.R ((:+) fx' fy') _))
                  p
                  = T.con2 T.mkNoSrcPos p (:+) (+:+)
                      (T.uap2 T.mkNoSrcPos p ((!-) T.mkNoSrcPos p)
                         (T.uap2 T.mkNoSrcPos p ((!*) T.mkNoSrcPos p) fx fx')
                         (T.uap2 T.mkNoSrcPos p ((!*) T.mkNoSrcPos p) fy fy'))
                      (T.uap2 T.mkNoSrcPos p ((!+) T.mkNoSrcPos p)
                         (T.uap2 T.mkNoSrcPos p ((!*) T.mkNoSrcPos p) fx fy')
                         (T.uap2 T.mkNoSrcPos p ((!*) T.mkNoSrcPos p) fy fx'))
                (_ ** _) p = T.fatal p
        gnegate pnegate p
          = T.ufun1 c40v5v40v41negate pnegate p hnegate
          where hnegate (T.R ((:+) fx fy) _) p
                  = T.con2 T.mkNoSrcPos p (:+) (+:+)
                      (T.uap1 T.mkNoSrcPos p (gnegate T.mkNoSrcPos p) fx)
                      (T.uap1 T.mkNoSrcPos p (gnegate T.mkNoSrcPos p) fy)
                hnegate _ p = T.fatal p
        gabs pabs p = T.ufun1 c41v5v41v29abs pabs p habs
          where habs fz p
                  = T.con2 T.mkNoSrcPos p (:+) (+:+)
                      (T.uwrapForward p (hmagnitude fz p))
                      (T.uap1 T.mkNoSrcPos p
                         (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                         (T.conInteger T.mkNoSrcPos p (0)))
        gsignum psignum p
          = T.ufun1 c42v5v44v0signum psignum p hsignum
          where hsignum fv42v12v42v12n p
                  = T.ucguard
                      (T.uap2 T.mkNoSrcPos p
                         ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                         fv42v12v42v12n
                         (T.uap1 T.mkNoSrcPos p
                            (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                            (T.conInteger T.mkNoSrcPos p (0))))
                      (h42v5v42v17n p)
                      (y1signum fv42v12v42v12n p)
                  where h42v5v42v17n p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (0))
                        h42v5v42v17n p = y1signum fv42v12v42v12n p
                hsignum fv42v12v42v12n p = y1signum fv42v12v42v12n p
                y1signum fz@(T.R ((:+) fx fy) _) p
                  = T.con2 T.mkNoSrcPos p (:+) (+:+)
                      (T.uap2 T.mkNoSrcPos p ((!/) T.mkNoSrcPos p) fx
                         (gr T.mkNoSrcPos p))
                      (T.uap2 T.mkNoSrcPos p ((!/) T.mkNoSrcPos p) fy
                         (gr T.mkNoSrcPos p))
                  where gr pr p = T.uconstUse pr p sr
                        sr
                          = T.uconstDef p c43v42v43v56r
                              (\ p -> T.uwrapForward p (hmagnitude fz p))
                y1signum _ p = T.fatal p
        gfromInteger pfromInteger p
          = T.ufun1 c44v5v44v39fromInteger pfromInteger p
              hfromInteger
          where hfromInteger fn p
                  = T.con2 T.mkNoSrcPos p (:+) (+:+)
                      (T.uap1 T.mkNoSrcPos p (gfromInteger T.mkNoSrcPos p)
                         fn)
                      (T.uap1 T.mkNoSrcPos p
                         (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                         (T.conInteger T.mkNoSrcPos p (0)))
 
instance (RealFloat a) => Fractional (Complex a)
         where
        (%/) !/ p = T.ufun2 (+&^=%=&@=>/) (%/) p (*/)
          where ((T.R ((:+) fx fy) _) */
                   (T.R ((:+) fx' fy') _))
                  p
                  = T.con2 T.mkNoSrcPos p (:+) (+:+)
                      (T.uap2 T.mkNoSrcPos p ((!/) T.mkNoSrcPos p)
                         (T.uap2 T.mkNoSrcPos p ((!+) T.mkNoSrcPos p)
                            (T.uap2 T.mkNoSrcPos p ((!*) T.mkNoSrcPos p) fx
                               (gx'' T.mkNoSrcPos p))
                            (T.uap2 T.mkNoSrcPos p ((!*) T.mkNoSrcPos p) fy
                               (gy'' T.mkNoSrcPos p)))
                         (gd T.mkNoSrcPos p))
                      (T.uap2 T.mkNoSrcPos p ((!/) T.mkNoSrcPos p)
                         (T.uap2 T.mkNoSrcPos p ((!-) T.mkNoSrcPos p)
                            (T.uap2 T.mkNoSrcPos p ((!*) T.mkNoSrcPos p) fy
                               (gx'' T.mkNoSrcPos p))
                            (T.uap2 T.mkNoSrcPos p ((!*) T.mkNoSrcPos p) fx
                               (gy'' T.mkNoSrcPos p)))
                         (gd T.mkNoSrcPos p))
                  where gx'' px'' p = T.uconstUse px'' p sx''
                        sx''
                          = T.uconstDef p c49v5v49v25x''
                              (\ p ->
                                 T.uap2 T.mkNoSrcPos p
                                   (gscaleFloat T.mkNoSrcPos p)
                                   (gk T.mkNoSrcPos p)
                                   fx')
                        gy'' py'' p = T.uconstUse py'' p sy''
                        sy''
                          = T.uconstDef p c50v5v50v25y''
                              (\ p ->
                                 T.uap2 T.mkNoSrcPos p
                                   (gscaleFloat T.mkNoSrcPos p)
                                   (gk T.mkNoSrcPos p)
                                   fy')
                        gk pk p = T.uconstUse pk p sk
                        sk
                          = T.uconstDef p c51v5v51v43k
                              (\ p ->
                                 T.uap1 T.mkNoSrcPos p
                                   (Hat.PreludeBasic.gnegate T.mkNoSrcPos p)
                                   (T.uap2 T.mkNoSrcPos p (gmax T.mkNoSrcPos p)
                                      (T.uap1 T.mkNoSrcPos p
                                         (gexponent T.mkNoSrcPos p)
                                         fx')
                                      (T.uap1 T.mkNoSrcPos p
                                         (gexponent T.mkNoSrcPos p)
                                         fy')))
                        gd pd p = T.uconstUse pd p sd
                        sd
                          = T.uconstDef p c52v5v52v25d
                              (\ p ->
                                 T.uap2 T.mkNoSrcPos p ((!+) T.mkNoSrcPos p)
                                   (T.uap2 T.mkNoSrcPos p ((!*) T.mkNoSrcPos p)
                                      fx'
                                      (gx'' T.mkNoSrcPos p))
                                   (T.uap2 T.mkNoSrcPos p ((!*) T.mkNoSrcPos p)
                                      fy'
                                      (gy'' T.mkNoSrcPos p)))
                (_ */ _) p = T.fatal p
        gfromRational pfromRational p
          = T.ufun1 c54v3v54v39fromRational pfromRational p
              hfromRational
          where hfromRational fa p
                  = T.con2 T.mkNoSrcPos p (:+) (+:+)
                      (T.uap1 T.mkNoSrcPos p (gfromRational T.mkNoSrcPos p)
                         fa)
                      (T.uap1 T.mkNoSrcPos p
                         (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                         (T.conInteger T.mkNoSrcPos p (0)))
 
instance (RealFloat a) => Floating (Complex a) where
        gpi ppi p = T.uconstUse ppi p spi
        spi
          = T.uconstDef p c57v5v57v29pi
              (\ p ->
                 T.con2 T.mkNoSrcPos p (:+) (+:+) (gpi T.mkNoSrcPos p)
                   (T.uap1 T.mkNoSrcPos p
                      (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                      (T.conInteger T.mkNoSrcPos p (0))))
        gexp pexp p = T.ufun1 c58v5v59v27exp pexp p hexp
          where hexp (T.R ((:+) fx fy) _) p
                  = T.con2 T.mkNoSrcPos p (:+) (+:+)
                      (T.uap2 T.mkNoSrcPos p ((!*) T.mkNoSrcPos p)
                         (gexpx T.mkNoSrcPos p)
                         (T.uap1 T.mkNoSrcPos p (gcos T.mkNoSrcPos p) fy))
                      (T.uap2 T.mkNoSrcPos p ((!*) T.mkNoSrcPos p)
                         (gexpx T.mkNoSrcPos p)
                         (T.uap1 T.mkNoSrcPos p (gsin T.mkNoSrcPos p) fy))
                  where gexpx pexpx p = T.uconstUse pexpx p sexpx
                        sexpx
                          = T.uconstDef p c59v29v59v40expx
                              (\ p ->
                                 T.uap1 T.mkNoSrcPos p (gexp T.mkNoSrcPos p) fx)
                hexp _ p = T.fatal p
        glog plog p = T.ufun1 c60v5v60v50log plog p hlog
          where hlog fz p
                  = T.con2 T.mkNoSrcPos p (:+) (+:+)
                      (T.uap1 T.mkNoSrcPos p (glog T.mkNoSrcPos p)
                         (T.uwrapForward p (hmagnitude fz p)))
                      (T.uwrapForward p (hphase fz p))
        gsqrt psqrt p = T.ufun1 c62v5v68v0sqrt psqrt p hsqrt
          where hsqrt fv62v10v62v10n p
                  = T.ucguard
                      (T.uap2 T.mkNoSrcPos p
                         ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                         fv62v10v62v10n
                         (T.uap1 T.mkNoSrcPos p
                            (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                            (T.conInteger T.mkNoSrcPos p (0))))
                      (h62v5v62v23n p)
                      (y1sqrt fv62v10v62v10n p)
                  where h62v5v62v23n p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (0))
                        h62v5v62v23n p = y1sqrt fv62v10v62v10n p
                hsqrt fv62v10v62v10n p = y1sqrt fv62v10v62v10n p
                y1sqrt fz@(T.R ((:+) fx fy) _) p
                  = T.con2 T.mkNoSrcPos p (:+) (+:+)
                      (gu T.mkNoSrcPos p)
                      (T.ucif p
                         (T.uap2 T.mkNoSrcPos p ((!<) T.mkNoSrcPos p) fy
                            (T.uap1 T.mkNoSrcPos p
                               (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                               (T.conInteger T.mkNoSrcPos p (0))))
                         (T.uap1 T.mkNoSrcPos p
                            (Hat.PreludeBasic.gnegate T.mkNoSrcPos p)
                            (gv T.mkNoSrcPos p))
                         (gv T.mkNoSrcPos p))
                  where gu pu p = T.uconstUse pu p su
                        gv pv p = T.uconstUse pv p sv
                        su
                          = T.uconstDef p c64v29v64v70u
                              (\ _ ->
                                 case j64v29v64v70u of
                                     (ku, fu, fv) -> fu)
                        sv
                          = T.uconstDef p c64v29v64v70v
                              (\ _ ->
                                 case j64v29v64v70u of
                                     (ku, fu, fv) -> fv)
                        j64v29v64v70u
                          = case
                              T.ucif p
                                (T.uap2 T.mkNoSrcPos p ((!<) T.mkNoSrcPos p) fx
                                   (T.uap1 T.mkNoSrcPos p
                                      (Hat.PreludeBasic.gfromInteger
                                         T.mkNoSrcPos
                                         p)
                                      (T.conInteger T.mkNoSrcPos p (0))))
                                (T.con2 T.mkNoSrcPos p T.Tuple2 T.aTuple2
                                   (gv' T.mkNoSrcPos p)
                                   (gu' T.mkNoSrcPos p))
                                (T.con2 T.mkNoSrcPos p T.Tuple2 T.aTuple2
                                   (gu' T.mkNoSrcPos p)
                                   (gv' T.mkNoSrcPos p))
                              of
                                T.R (T.Tuple2 fu fv) ku -> (ku, fu, fv)
                                _ -> T.fatal p
                        gv' pv' p = T.uconstUse pv' p sv'
                        sv'
                          = T.uconstDef p c65v29v65v50v'
                              (\ p ->
                                 T.uap2 T.mkNoSrcPos p ((!/) T.mkNoSrcPos p)
                                   (T.uap1 T.mkNoSrcPos p (gabs T.mkNoSrcPos p)
                                      fy)
                                   (T.uap2 T.mkNoSrcPos p ((!*) T.mkNoSrcPos p)
                                      (gu' T.mkNoSrcPos p)
                                      (T.uap1 T.mkNoSrcPos p
                                         (Hat.PreludeBasic.gfromInteger
                                            T.mkNoSrcPos
                                            p)
                                         (T.conInteger T.mkNoSrcPos p (2)))))
                        gu' pu' p = T.uconstUse pu' p su'
                        su'
                          = T.uconstDef p c66v29v66v68u'
                              (\ p ->
                                 T.uap1 T.mkNoSrcPos p (gsqrt T.mkNoSrcPos p)
                                   (T.uap2 T.mkNoSrcPos p ((!/) T.mkNoSrcPos p)
                                      (T.uap2 T.mkNoSrcPos p
                                         ((!+) T.mkNoSrcPos p)
                                         (T.uwrapForward p (hmagnitude fz p))
                                         (T.uap1 T.mkNoSrcPos p
                                            (gabs T.mkNoSrcPos p)
                                            fx))
                                      (T.uap1 T.mkNoSrcPos p
                                         (Hat.PreludeBasic.gfromInteger
                                            T.mkNoSrcPos
                                            p)
                                         (T.conInteger T.mkNoSrcPos p (2)))))
                y1sqrt _ p = T.fatal p
        gsin psin p = T.ufun1 c68v5v68v54sin psin p hsin
          where hsin (T.R ((:+) fx fy) _) p
                  = T.con2 T.mkNoSrcPos p (:+) (+:+)
                      (T.uap2 T.mkNoSrcPos p ((!*) T.mkNoSrcPos p)
                         (T.uap1 T.mkNoSrcPos p (gsin T.mkNoSrcPos p) fx)
                         (T.uap1 T.mkNoSrcPos p (gcosh T.mkNoSrcPos p) fy))
                      (T.uap2 T.mkNoSrcPos p ((!*) T.mkNoSrcPos p)
                         (T.uap1 T.mkNoSrcPos p (gcos T.mkNoSrcPos p) fx)
                         (T.uap1 T.mkNoSrcPos p (gsinh T.mkNoSrcPos p) fy))
                hsin _ p = T.fatal p
        gcos pcos p = T.ufun1 c69v5v69v58cos pcos p hcos
          where hcos (T.R ((:+) fx fy) _) p
                  = T.con2 T.mkNoSrcPos p (:+) (+:+)
                      (T.uap2 T.mkNoSrcPos p ((!*) T.mkNoSrcPos p)
                         (T.uap1 T.mkNoSrcPos p (gcos T.mkNoSrcPos p) fx)
                         (T.uap1 T.mkNoSrcPos p (gcosh T.mkNoSrcPos p) fy))
                      (T.uap1 T.mkNoSrcPos p
                         (Hat.PreludeBasic.gnegate T.mkNoSrcPos p)
                         (T.uap2 T.mkNoSrcPos p ((!*) T.mkNoSrcPos p)
                            (T.uap1 T.mkNoSrcPos p (gsin T.mkNoSrcPos p) fx)
                            (T.uap1 T.mkNoSrcPos p (gsinh T.mkNoSrcPos p) fy)))
                hcos _ p = T.fatal p
        gtan ptan p = T.ufun1 c70v5v71v27tan ptan p htan
          where htan (T.R ((:+) fx fy) _) p
                  = T.uap2 T.mkNoSrcPos p ((!/) T.mkNoSrcPos p)
                      (T.con2 T.mkNoSrcPos p (:+) (+:+)
                         (T.uap2 T.mkNoSrcPos p ((!*) T.mkNoSrcPos p)
                            (gsinx T.mkNoSrcPos p)
                            (gcoshy T.mkNoSrcPos p))
                         (T.uap2 T.mkNoSrcPos p ((!*) T.mkNoSrcPos p)
                            (gcosx T.mkNoSrcPos p)
                            (gsinhy T.mkNoSrcPos p)))
                      (T.con2 T.mkNoSrcPos p (:+) (+:+)
                         (T.uap2 T.mkNoSrcPos p ((!*) T.mkNoSrcPos p)
                            (gcosx T.mkNoSrcPos p)
                            (gcoshy T.mkNoSrcPos p))
                         (T.uap1 T.mkNoSrcPos p
                            (Hat.PreludeBasic.gnegate T.mkNoSrcPos p)
                            (T.uap2 T.mkNoSrcPos p ((!*) T.mkNoSrcPos p)
                               (gsinx T.mkNoSrcPos p)
                               (gsinhy T.mkNoSrcPos p))))
                  where gsinx psinx p = T.uconstUse psinx p ssinx
                        ssinx
                          = T.uconstDef p c71v29v71v41sinx
                              (\ p ->
                                 T.uap1 T.mkNoSrcPos p (gsin T.mkNoSrcPos p) fx)
                        gcosx pcosx p = T.uconstUse pcosx p scosx
                        scosx
                          = T.uconstDef p c72v29v72v41cosx
                              (\ p ->
                                 T.uap1 T.mkNoSrcPos p (gcos T.mkNoSrcPos p) fx)
                        gsinhy psinhy p = T.uconstUse psinhy p ssinhy
                        ssinhy
                          = T.uconstDef p c73v29v73v42sinhy
                              (\ p ->
                                 T.uap1 T.mkNoSrcPos p (gsinh T.mkNoSrcPos p)
                                   fy)
                        gcoshy pcoshy p = T.uconstUse pcoshy p scoshy
                        scoshy
                          = T.uconstDef p c74v29v74v42coshy
                              (\ p ->
                                 T.uap1 T.mkNoSrcPos p (gcosh T.mkNoSrcPos p)
                                   fy)
                htan _ p = T.fatal p
        gsinh psinh p = T.ufun1 c76v5v76v55sinh psinh p hsinh
          where hsinh (T.R ((:+) fx fy) _) p
                  = T.con2 T.mkNoSrcPos p (:+) (+:+)
                      (T.uap2 T.mkNoSrcPos p ((!*) T.mkNoSrcPos p)
                         (T.uap1 T.mkNoSrcPos p (gcos T.mkNoSrcPos p) fy)
                         (T.uap1 T.mkNoSrcPos p (gsinh T.mkNoSrcPos p) fx))
                      (T.uap2 T.mkNoSrcPos p ((!*) T.mkNoSrcPos p)
                         (T.uap1 T.mkNoSrcPos p (gsin T.mkNoSrcPos p) fy)
                         (T.uap1 T.mkNoSrcPos p (gcosh T.mkNoSrcPos p) fx))
                hsinh _ p = T.fatal p
        gcosh pcosh p = T.ufun1 c77v5v77v54cosh pcosh p hcosh
          where hcosh (T.R ((:+) fx fy) _) p
                  = T.con2 T.mkNoSrcPos p (:+) (+:+)
                      (T.uap2 T.mkNoSrcPos p ((!*) T.mkNoSrcPos p)
                         (T.uap1 T.mkNoSrcPos p (gcos T.mkNoSrcPos p) fy)
                         (T.uap1 T.mkNoSrcPos p (gcosh T.mkNoSrcPos p) fx))
                      (T.uap2 T.mkNoSrcPos p ((!*) T.mkNoSrcPos p)
                         (T.uap1 T.mkNoSrcPos p (gsin T.mkNoSrcPos p) fy)
                         (T.uap1 T.mkNoSrcPos p (gsinh T.mkNoSrcPos p) fx))
                hcosh _ p = T.fatal p
        gtanh ptanh p = T.ufun1 c78v5v79v27tanh ptanh p htanh
          where htanh (T.R ((:+) fx fy) _) p
                  = T.uap2 T.mkNoSrcPos p ((!/) T.mkNoSrcPos p)
                      (T.con2 T.mkNoSrcPos p (:+) (+:+)
                         (T.uap2 T.mkNoSrcPos p ((!*) T.mkNoSrcPos p)
                            (gcosy T.mkNoSrcPos p)
                            (gsinhx T.mkNoSrcPos p))
                         (T.uap2 T.mkNoSrcPos p ((!*) T.mkNoSrcPos p)
                            (gsiny T.mkNoSrcPos p)
                            (gcoshx T.mkNoSrcPos p)))
                      (T.con2 T.mkNoSrcPos p (:+) (+:+)
                         (T.uap2 T.mkNoSrcPos p ((!*) T.mkNoSrcPos p)
                            (gcosy T.mkNoSrcPos p)
                            (gcoshx T.mkNoSrcPos p))
                         (T.uap2 T.mkNoSrcPos p ((!*) T.mkNoSrcPos p)
                            (gsiny T.mkNoSrcPos p)
                            (gsinhx T.mkNoSrcPos p)))
                  where gsiny psiny p = T.uconstUse psiny p ssiny
                        ssiny
                          = T.uconstDef p c79v29v79v41siny
                              (\ p ->
                                 T.uap1 T.mkNoSrcPos p (gsin T.mkNoSrcPos p) fy)
                        gcosy pcosy p = T.uconstUse pcosy p scosy
                        scosy
                          = T.uconstDef p c80v29v80v41cosy
                              (\ p ->
                                 T.uap1 T.mkNoSrcPos p (gcos T.mkNoSrcPos p) fy)
                        gsinhx psinhx p = T.uconstUse psinhx p ssinhx
                        ssinhx
                          = T.uconstDef p c81v29v81v42sinhx
                              (\ p ->
                                 T.uap1 T.mkNoSrcPos p (gsinh T.mkNoSrcPos p)
                                   fx)
                        gcoshx pcoshx p = T.uconstUse pcoshx p scoshx
                        scoshx
                          = T.uconstDef p c82v29v82v42coshx
                              (\ p ->
                                 T.uap1 T.mkNoSrcPos p (gcosh T.mkNoSrcPos p)
                                   fx)
                htanh _ p = T.fatal p
        gasin pasin p = T.ufun1 c84v5v85v27asin pasin p hasin
          where hasin fz@(T.R ((:+) fx fy) _) p
                  = T.con2 T.mkNoSrcPos p (:+) (+:+)
                      (gy' T.mkNoSrcPos p)
                      (T.uap1 T.mkNoSrcPos p
                         (Hat.PreludeBasic.gnegate T.mkNoSrcPos p)
                         (gx' T.mkNoSrcPos p))
                  where gx' px' p = T.uconstUse px' p sx'
                        gy' py' p = T.uconstUse py' p sy'
                        sx'
                          = T.uconstDef p c85v30v85v72x'
                              (\ _ ->
                                 case j85v30v85v72x' of
                                     (kx', fx', fy') -> fx')
                        sy'
                          = T.uconstDef p c85v30v85v72y'
                              (\ _ ->
                                 case j85v30v85v72x' of
                                     (kx', fx', fy') -> fy')
                        j85v30v85v72x'
                          = case
                              T.uap1 T.mkNoSrcPos p (glog T.mkNoSrcPos p)
                                (T.uap2 T.mkNoSrcPos p ((!+) T.mkNoSrcPos p)
                                   (T.con2 T.mkNoSrcPos p (:+) (+:+)
                                      (T.uap1 T.mkNoSrcPos p
                                         (Hat.PreludeBasic.gnegate T.mkNoSrcPos
                                            p)
                                         fy)
                                      fx)
                                   (T.uap1 T.mkNoSrcPos p (gsqrt T.mkNoSrcPos p)
                                      (T.uap2 T.mkNoSrcPos p
                                         ((!-) T.mkNoSrcPos p)
                                         (T.uap1 T.mkNoSrcPos p
                                            (Hat.PreludeBasic.gfromInteger
                                               T.mkNoSrcPos
                                               p)
                                            (T.conInteger T.mkNoSrcPos p (1)))
                                         (T.uap2 T.mkNoSrcPos p
                                            ((!*) T.mkNoSrcPos p)
                                            fz
                                            fz))))
                              of
                                (T.R ((:+) fx' fy') kx') -> (kx', fx', fy')
                                _ -> T.fatal p
                hasin _ p = T.fatal p
        gacos pacos p = T.ufun1 c86v5v87v27acos pacos p hacos
          where hacos fz@(T.R ((:+) fx fy) _) p
                  = T.con2 T.mkNoSrcPos p (:+) (+:+)
                      (gy'' T.mkNoSrcPos p)
                      (T.uap1 T.mkNoSrcPos p
                         (Hat.PreludeBasic.gnegate T.mkNoSrcPos p)
                         (gx'' T.mkNoSrcPos p))
                  where gx'' px'' p = T.uconstUse px'' p sx''
                        gy'' py'' p = T.uconstUse py'' p sy''
                        sx''
                          = T.uconstDef p c87v29v87v62x''
                              (\ _ ->
                                 case j87v29v87v62x'' of
                                     (kx'', fx'', fy'') -> fx'')
                        sy''
                          = T.uconstDef p c87v29v87v62y''
                              (\ _ ->
                                 case j87v29v87v62x'' of
                                     (kx'', fx'', fy'') -> fy'')
                        j87v29v87v62x''
                          = case
                              T.uap1 T.mkNoSrcPos p (glog T.mkNoSrcPos p)
                                (T.uap2 T.mkNoSrcPos p ((!+) T.mkNoSrcPos p) fz
                                   (T.con2 T.mkNoSrcPos p (:+) (+:+)
                                      (T.uap1 T.mkNoSrcPos p
                                         (Hat.PreludeBasic.gnegate T.mkNoSrcPos
                                            p)
                                         (gy' T.mkNoSrcPos p))
                                      (gx' T.mkNoSrcPos p)))
                              of
                                (T.R ((:+) fx'' fy'') kx'') -> (kx'', fx'',
                                                                fy'')
                                _ -> T.fatal p
                        gx' px' p = T.uconstUse px' p sx'
                        gy' py' p = T.uconstUse py' p sy'
                        sx'
                          = T.uconstDef p c88v29v88v55x'
                              (\ _ ->
                                 case j88v29v88v55x' of
                                     (kx', fx', fy') -> fx')
                        sy'
                          = T.uconstDef p c88v29v88v55y'
                              (\ _ ->
                                 case j88v29v88v55x' of
                                     (kx', fx', fy') -> fy')
                        j88v29v88v55x'
                          = case
                              T.uap1 T.mkNoSrcPos p (gsqrt T.mkNoSrcPos p)
                                (T.uap2 T.mkNoSrcPos p ((!-) T.mkNoSrcPos p)
                                   (T.uap1 T.mkNoSrcPos p
                                      (Hat.PreludeBasic.gfromInteger
                                         T.mkNoSrcPos
                                         p)
                                      (T.conInteger T.mkNoSrcPos p (1)))
                                   (T.uap2 T.mkNoSrcPos p ((!*) T.mkNoSrcPos p)
                                      fz
                                      fz))
                              of
                                (T.R ((:+) fx' fy') kx') -> (kx', fx', fy')
                                _ -> T.fatal p
                hacos _ p = T.fatal p
        gatan patan p = T.ufun1 c89v5v90v27atan patan p hatan
          where hatan fz@(T.R ((:+) fx fy) _) p
                  = T.con2 T.mkNoSrcPos p (:+) (+:+)
                      (gy' T.mkNoSrcPos p)
                      (T.uap1 T.mkNoSrcPos p
                         (Hat.PreludeBasic.gnegate T.mkNoSrcPos p)
                         (gx' T.mkNoSrcPos p))
                  where gx' px' p = T.uconstUse px' p sx'
                        gy' py' p = T.uconstUse py' p sy'
                        sx'
                          = T.uconstDef p c90v29v90v70x'
                              (\ _ ->
                                 case j90v29v90v70x' of
                                     (kx', fx', fy') -> fx')
                        sy'
                          = T.uconstDef p c90v29v90v70y'
                              (\ _ ->
                                 case j90v29v90v70x' of
                                     (kx', fx', fy') -> fy')
                        j90v29v90v70x'
                          = case
                              T.uap1 T.mkNoSrcPos p (glog T.mkNoSrcPos p)
                                (T.uap2 T.mkNoSrcPos p ((!/) T.mkNoSrcPos p)
                                   (T.con2 T.mkNoSrcPos p (:+) (+:+)
                                      (T.uap2 T.mkNoSrcPos p
                                         ((!-) T.mkNoSrcPos p)
                                         (T.uap1 T.mkNoSrcPos p
                                            (Hat.PreludeBasic.gfromInteger
                                               T.mkNoSrcPos
                                               p)
                                            (T.conInteger T.mkNoSrcPos p (1)))
                                         fy)
                                      fx)
                                   (T.uap1 T.mkNoSrcPos p (gsqrt T.mkNoSrcPos p)
                                      (T.uap2 T.mkNoSrcPos p
                                         ((!+) T.mkNoSrcPos p)
                                         (T.uap1 T.mkNoSrcPos p
                                            (Hat.PreludeBasic.gfromInteger
                                               T.mkNoSrcPos
                                               p)
                                            (T.conInteger T.mkNoSrcPos p (1)))
                                         (T.uap2 T.mkNoSrcPos p
                                            ((!*) T.mkNoSrcPos p)
                                            fz
                                            fz))))
                              of
                                (T.R ((:+) fx' fy') kx') -> (kx', fx', fy')
                                _ -> T.fatal p
                hatan _ p = T.fatal p
        gasinh pasinh p
          = T.ufun1 c92v5v92v44asinh pasinh p hasinh
          where hasinh fz p
                  = T.uap1 T.mkNoSrcPos p (glog T.mkNoSrcPos p)
                      (T.uap2 T.mkNoSrcPos p ((!+) T.mkNoSrcPos p) fz
                         (T.uap1 T.mkNoSrcPos p (gsqrt T.mkNoSrcPos p)
                            (T.uap2 T.mkNoSrcPos p ((!+) T.mkNoSrcPos p)
                               (T.uap1 T.mkNoSrcPos p
                                  (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                                  (T.conInteger T.mkNoSrcPos p (1)))
                               (T.uap2 T.mkNoSrcPos p ((!*) T.mkNoSrcPos p) fz
                                  fz))))
        gacosh pacosh p
          = T.ufun1 c93v5v93v58acosh pacosh p hacosh
          where hacosh fz p
                  = T.uap1 T.mkNoSrcPos p (glog T.mkNoSrcPos p)
                      (T.uap2 T.mkNoSrcPos p ((!+) T.mkNoSrcPos p) fz
                         (T.uap2 T.mkNoSrcPos p ((!*) T.mkNoSrcPos p)
                            (T.uap2 T.mkNoSrcPos p ((!+) T.mkNoSrcPos p) fz
                               (T.uap1 T.mkNoSrcPos p
                                  (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                                  (T.conInteger T.mkNoSrcPos p (1))))
                            (T.uap1 T.mkNoSrcPos p (gsqrt T.mkNoSrcPos p)
                               (T.uap2 T.mkNoSrcPos p ((!/) T.mkNoSrcPos p)
                                  (T.uap2 T.mkNoSrcPos p ((!-) T.mkNoSrcPos p)
                                     fz
                                     (T.uap1 T.mkNoSrcPos p
                                        (Hat.PreludeBasic.gfromInteger
                                           T.mkNoSrcPos
                                           p)
                                        (T.conInteger T.mkNoSrcPos p (1))))
                                  (T.uap2 T.mkNoSrcPos p ((!+) T.mkNoSrcPos p)
                                     fz
                                     (T.uap1 T.mkNoSrcPos p
                                        (Hat.PreludeBasic.gfromInteger
                                           T.mkNoSrcPos
                                           p)
                                        (T.conInteger T.mkNoSrcPos p (1))))))))
        gatanh patanh p
          = T.ufun1 c94v5v94v48atanh patanh p hatanh
          where hatanh fz p
                  = T.uap1 T.mkNoSrcPos p (glog T.mkNoSrcPos p)
                      (T.uap2 T.mkNoSrcPos p ((!/) T.mkNoSrcPos p)
                         (T.uap2 T.mkNoSrcPos p ((!+) T.mkNoSrcPos p)
                            (T.uap1 T.mkNoSrcPos p
                               (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                               (T.conInteger T.mkNoSrcPos p (1)))
                            fz)
                         (T.uap1 T.mkNoSrcPos p (gsqrt T.mkNoSrcPos p)
                            (T.uap2 T.mkNoSrcPos p ((!-) T.mkNoSrcPos p)
                               (T.uap1 T.mkNoSrcPos p
                                  (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                                  (T.conInteger T.mkNoSrcPos p (1)))
                               (T.uap2 T.mkNoSrcPos p ((!*) T.mkNoSrcPos p) fz
                                  fz))))
acis
  = T.mkVariable tComplex 200001 200036 3 (1) "cis"
      Prelude.False
aconjugate
  = T.mkVariable tComplex 140001 140029 3 (1)
      "conjugate"
      Prelude.False
aimagPart
  = T.mkVariable tComplex 110001 110021 3 (1)
      "imagPart"
      Prelude.False
amagnitude
  = T.mkVariable tComplex 260001 280009 3 (1)
      "magnitude"
      Prelude.False
amkPolar
  = T.mkVariable tComplex 170001 170050 3 (2) "mkPolar"
      Prelude.False
aphase
  = T.mkVariable tComplex 320001 330026 3 (1) "phase"
      Prelude.False
apolar
  = T.mkVariable tComplex 230001 230034 3 (1) "polar"
      Prelude.False
arealPart
  = T.mkVariable tComplex 100001 100021 3 (1)
      "realPart"
      Prelude.False
(+:+)
  = T.mkConstructor tComplex 60021 60022 24 (2) ":+"
(++=%@=+=%>==)
  = T.mkVariable tComplex 60038 60039 3 (-1) "=="
      Prelude.False
c6v41v6v44readsPrec
  = T.mkVariable tComplex 60041 60044 3 (-1)
      "readsPrec"
      Prelude.False
c6v46v6v49showsPrec
  = T.mkVariable tComplex 60046 60049 3 (-1)
      "showsPrec"
      Prelude.False
c28v11v28v44k
  = T.mkVariable tComplex 280011 280044 3 (0) "k"
      Prelude.True
c29v11v29v18mk
  = T.mkVariable tComplex 290011 290018 3 (0) "mk"
      Prelude.True
c43v42v43v56r
  = T.mkVariable tComplex 430042 430056 3 (0) "r"
      Prelude.True
c41v5v41v29abs
  = T.mkVariable tComplex 410005 410029 3 (-1) "abs"
      Prelude.False
c44v5v44v39fromInteger
  = T.mkVariable tComplex 440005 440039 3 (-1)
      "fromInteger"
      Prelude.False
c40v5v40v41negate
  = T.mkVariable tComplex 400005 400041 3 (-1) "negate"
      Prelude.False
c42v5v44v0signum
  = T.mkVariable tComplex 420005 440000 3 (-1) "signum"
      Prelude.False
(+%>=*=%>=*#*)
  = T.mkVariable tComplex 390005 390051 3 (-1) "*"
      Prelude.False
(+%^=*=%^=&#+)
  = T.mkVariable tComplex 370005 370041 3 (-1) "+"
      Prelude.False
(+%@=*=%@=&#-)
  = T.mkVariable tComplex 380005 380041 3 (-1) "-"
      Prelude.False
c52v5v52v25d
  = T.mkVariable tComplex 520005 520025 3 (0) "d"
      Prelude.True
c51v5v51v43k
  = T.mkVariable tComplex 510005 510043 3 (0) "k"
      Prelude.True
c49v5v49v25x''
  = T.mkVariable tComplex 490005 490025 3 (0) "x''"
      Prelude.True
c50v5v50v25y''
  = T.mkVariable tComplex 500005 500025 3 (0) "y''"
      Prelude.True
c54v3v54v39fromRational
  = T.mkVariable tComplex 540003 540039 3 (-1)
      "fromRational"
      Prelude.False
(+&^=%=&@=>/)
  = T.mkVariable tComplex 470003 480009 3 (-1) "/"
      Prelude.False
c59v29v59v40expx
  = T.mkVariable tComplex 590029 590040 3 (0) "expx"
      Prelude.True
c64v29v64v70u
  = T.mkVariable tComplex 640029 640070 3 (0) "u"
      Prelude.True
c66v29v66v68u'
  = T.mkVariable tComplex 660029 660068 3 (0) "u'"
      Prelude.True
c64v29v64v70v
  = T.mkVariable tComplex 640029 640070 3 (0) "v"
      Prelude.True
c65v29v65v50v'
  = T.mkVariable tComplex 650029 650050 3 (0) "v'"
      Prelude.True
c74v29v74v42coshy
  = T.mkVariable tComplex 740029 740042 3 (0) "coshy"
      Prelude.True
c72v29v72v41cosx
  = T.mkVariable tComplex 720029 720041 3 (0) "cosx"
      Prelude.True
c73v29v73v42sinhy
  = T.mkVariable tComplex 730029 730042 3 (0) "sinhy"
      Prelude.True
c71v29v71v41sinx
  = T.mkVariable tComplex 710029 710041 3 (0) "sinx"
      Prelude.True
c82v29v82v42coshx
  = T.mkVariable tComplex 820029 820042 3 (0) "coshx"
      Prelude.True
c80v29v80v41cosy
  = T.mkVariable tComplex 800029 800041 3 (0) "cosy"
      Prelude.True
c81v29v81v42sinhx
  = T.mkVariable tComplex 810029 810042 3 (0) "sinhx"
      Prelude.True
c79v29v79v41siny
  = T.mkVariable tComplex 790029 790041 3 (0) "siny"
      Prelude.True
c85v30v85v72x'
  = T.mkVariable tComplex 850030 850072 3 (0) "x'"
      Prelude.True
c85v30v85v72y'
  = T.mkVariable tComplex 850030 850072 3 (0) "y'"
      Prelude.True
c88v29v88v55x'
  = T.mkVariable tComplex 880029 880055 3 (0) "x'"
      Prelude.True
c87v29v87v62x''
  = T.mkVariable tComplex 870029 870062 3 (0) "x''"
      Prelude.True
c88v29v88v55y'
  = T.mkVariable tComplex 880029 880055 3 (0) "y'"
      Prelude.True
c87v29v87v62y''
  = T.mkVariable tComplex 870029 870062 3 (0) "y''"
      Prelude.True
c90v29v90v70x'
  = T.mkVariable tComplex 900029 900070 3 (0) "x'"
      Prelude.True
c90v29v90v70y'
  = T.mkVariable tComplex 900029 900070 3 (0) "y'"
      Prelude.True
c86v5v87v27acos
  = T.mkVariable tComplex 860005 870027 3 (-1) "acos"
      Prelude.False
c93v5v93v58acosh
  = T.mkVariable tComplex 930005 930058 3 (-1) "acosh"
      Prelude.False
c84v5v85v27asin
  = T.mkVariable tComplex 840005 850027 3 (-1) "asin"
      Prelude.False
c92v5v92v44asinh
  = T.mkVariable tComplex 920005 920044 3 (-1) "asinh"
      Prelude.False
c89v5v90v27atan
  = T.mkVariable tComplex 890005 900027 3 (-1) "atan"
      Prelude.False
c94v5v94v48atanh
  = T.mkVariable tComplex 940005 940048 3 (-1) "atanh"
      Prelude.False
c69v5v69v58cos
  = T.mkVariable tComplex 690005 690058 3 (-1) "cos"
      Prelude.False
c77v5v77v54cosh
  = T.mkVariable tComplex 770005 770054 3 (-1) "cosh"
      Prelude.False
c58v5v59v27exp
  = T.mkVariable tComplex 580005 590027 3 (-1) "exp"
      Prelude.False
c60v5v60v50log
  = T.mkVariable tComplex 600005 600050 3 (-1) "log"
      Prelude.False
c57v5v57v29pi
  = T.mkVariable tComplex 570005 570029 3 (-1) "pi"
      Prelude.False
c68v5v68v54sin
  = T.mkVariable tComplex 680005 680054 3 (-1) "sin"
      Prelude.False
c76v5v76v55sinh
  = T.mkVariable tComplex 760005 760055 3 (-1) "sinh"
      Prelude.False
c62v5v68v0sqrt
  = T.mkVariable tComplex 620005 680000 3 (-1) "sqrt"
      Prelude.False
c70v5v71v27tan
  = T.mkVariable tComplex 700005 710027 3 (-1) "tan"
      Prelude.False
c78v5v79v27tanh
  = T.mkVariable tComplex 780005 790027 3 (-1) "tanh"
      Prelude.False
p = T.mkRoot
tComplex
  = T.mkModule "Complex" "Complex.hs" Prelude.False