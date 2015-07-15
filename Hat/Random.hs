module Hat.Random
       (RandomGen(gnext, gsplit, ggenRange, snext, ssplit,
                  sgenRange),
        StdGen, gmkStdGen, amkStdGen, hmkStdGen,
        Random(grandom, grandomR, grandoms, grandomRs,
               grandomIO, grandomRIO, srandom, srandomR, srandoms,
               srandomRs, srandomIO, srandomRIO),
        ggetStdRandom, agetStdRandom, hgetStdRandom,
        ggetStdGen, gsetStdGen, asetStdGen, hsetStdGen,
        gnewStdGen)
       where
import qualified Prelude
import qualified Hat.Hat as T
import qualified Hat.PreludeBasic
import qualified Hat.PreludeBuiltinTypes
import Hat.Prelude
import Hat.PreludeBuiltinTypes as T
import Hat.RandomBuiltin as T
import qualified System.Random as Random
import Hat.Char
 
class RandomGen g where
         
        ggenRange ::
                  T.RefSrcPos ->
                    T.RefExp -> T.R (T.Fun g (T.Tuple2 Int Int))
         
        sgenRange :: T.R (T.Fun g (T.Tuple2 Int Int))
         
        gnext ::
              T.RefSrcPos ->
                T.RefExp -> T.R (T.Fun g (T.Tuple2 Int g))
         
        snext :: T.R (T.Fun g (T.Tuple2 Int g))
        snext = Hat.PreludeBasic.gundefined T.mkNoSrcPos p
         
        gsplit ::
               T.RefSrcPos ->
                 T.RefExp -> T.R (T.Fun g (T.Tuple2 g g))
         
        ssplit :: T.R (T.Fun g (T.Tuple2 g g))
        ssplit = Hat.PreludeBasic.gundefined T.mkNoSrcPos p
        ggenRange pgenRange p
          = T.ufun1 c24v3v24v34genRange pgenRange p hgenRange
          where hgenRange fg p
                  = T.con2 T.mkNoSrcPos p T.Tuple2 T.aTuple2
                      (gminBound T.mkNoSrcPos p)
                      (gmaxBound T.mkNoSrcPos p)
 
instance RandomGen StdGen where
        ggenRange pgenRange p
          = T.uconstUse pgenRange p sgenRange
        sgenRange
          = T.uconstDef p c31v3v31v31genRange
              (\ p -> gprimStdGenGenRange T.mkNoSrcPos p)
        gnext pnext p = T.uconstUse pnext p snext
        snext
          = T.uconstDef p c32v3v32v23next
              (\ p -> gprimStdGenNext T.mkNoSrcPos p)
        gsplit psplit p = T.uconstUse psplit p ssplit
        ssplit
          = T.uconstDef p c33v3v33v25split
              (\ p -> gprimStdGenSplit T.mkNoSrcPos p)
 
instance Read StdGen where
        greadsPrec preadsPrec p
          = T.uconstUse preadsPrec p sreadsPrec
        sreadsPrec
          = T.uconstDef p c36v3v36v33readsPrec
              (\ p -> gprimStdGenReadsPrec T.mkNoSrcPos p)
 
instance Show StdGen where
        gshowsPrec pshowsPrec p
          = T.uconstUse pshowsPrec p sshowsPrec
        sshowsPrec
          = T.uconstDef p c39v3v39v33showsPrec
              (\ p -> gprimStdGenShowsPrec T.mkNoSrcPos p)
 
gprimStdGenGenRange ::
                    T.RefSrcPos ->
                      T.RefExp -> T.R (T.Fun StdGen (T.Tuple2 Int Int))
gprimStdGenGenRange pprimStdGenGenRange p
  = T.ufun1 aprimStdGenGenRange pprimStdGenGenRange p
      hprimStdGenGenRange
hprimStdGenGenRange z1primStdGenGenRange
  kprimStdGenGenRange
  = T.fromTuple2 T.fromInt T.fromInt
      kprimStdGenGenRange
      (Random.genRange
         (T.toStdGen kprimStdGenGenRange
            z1primStdGenGenRange))
 
gprimStdGenNext ::
                T.RefSrcPos ->
                  T.RefExp -> T.R (T.Fun StdGen (T.Tuple2 Int StdGen))
gprimStdGenNext pprimStdGenNext p
  = T.ufun1 aprimStdGenNext pprimStdGenNext p
      hprimStdGenNext
hprimStdGenNext z1primStdGenNext kprimStdGenNext
  = T.fromTuple2 T.fromInt T.fromStdGen kprimStdGenNext
      (Random.next
         (T.toStdGen kprimStdGenNext z1primStdGenNext))
 
gprimStdGenSplit ::
                 T.RefSrcPos ->
                   T.RefExp ->
                     T.R (T.Fun StdGen (T.Tuple2 StdGen StdGen))
gprimStdGenSplit pprimStdGenSplit p
  = T.ufun1 aprimStdGenSplit pprimStdGenSplit p
      hprimStdGenSplit
hprimStdGenSplit z1primStdGenSplit kprimStdGenSplit
  = T.fromTuple2 T.fromStdGen T.fromStdGen
      kprimStdGenSplit
      (Random.split
         (T.toStdGen kprimStdGenSplit z1primStdGenSplit))
 
gprimStdGenReadsPrec ::
                     T.RefSrcPos ->
                       T.RefExp ->
                         T.R
                           (T.Fun Int
                              (T.Fun String (T.List (T.Tuple2 StdGen String))))
gprimStdGenReadsPrec pprimStdGenReadsPrec p
  = T.ufun2 aprimStdGenReadsPrec pprimStdGenReadsPrec p
      hprimStdGenReadsPrec
hprimStdGenReadsPrec z1primStdGenReadsPrec
  z2primStdGenReadsPrec kprimStdGenReadsPrec
  = T.fromList (T.fromTuple2 T.fromStdGen T.fromString)
      kprimStdGenReadsPrec
      (Prelude.readsPrec
         (T.toInt kprimStdGenReadsPrec z1primStdGenReadsPrec)
         (T.toString kprimStdGenReadsPrec
            z2primStdGenReadsPrec))
 
gprimStdGenShowsPrec ::
                     T.RefSrcPos ->
                       T.RefExp ->
                         T.R (T.Fun Int (T.Fun StdGen (T.Fun String String)))
gprimStdGenShowsPrec pprimStdGenShowsPrec p
  = T.ufun3 aprimStdGenShowsPrec pprimStdGenShowsPrec p
      hprimStdGenShowsPrec
hprimStdGenShowsPrec z1primStdGenShowsPrec
  z2primStdGenShowsPrec z3primStdGenShowsPrec
  kprimStdGenShowsPrec
  = T.fromString kprimStdGenShowsPrec
      (Prelude.showsPrec
         (T.toInt kprimStdGenShowsPrec z1primStdGenShowsPrec)
         (T.toStdGen kprimStdGenShowsPrec
            z2primStdGenShowsPrec)
         (T.toString kprimStdGenShowsPrec
            z3primStdGenShowsPrec))
 
gmkStdGen ::
          T.RefSrcPos -> T.RefExp -> T.R (T.Fun Int StdGen)
gmkStdGen pmkStdGen p
  = T.ufun1 amkStdGen pmkStdGen p hmkStdGen
hmkStdGen z1mkStdGen kmkStdGen
  = T.fromStdGen kmkStdGen
      (Random.mkStdGen (T.toInt kmkStdGen z1mkStdGen))
 
class Random a where
         
        grandomR ::
                   (RandomGen g) =>
                   T.RefSrcPos ->
                     T.RefExp ->
                       T.R (T.Fun (T.Tuple2 a a) (T.Fun g (T.Tuple2 a g)))
         
        srandomR ::
                   (RandomGen g) =>
                   T.R (T.Fun (T.Tuple2 a a) (T.Fun g (T.Tuple2 a g)))
        srandomR = Hat.PreludeBasic.gundefined T.mkNoSrcPos p
         
        grandom ::
                  (RandomGen g) =>
                  T.RefSrcPos ->
                    T.RefExp -> T.R (T.Fun g (T.Tuple2 a g))
         
        srandom ::
                  (RandomGen g) => T.R (T.Fun g (T.Tuple2 a g))
        srandom = Hat.PreludeBasic.gundefined T.mkNoSrcPos p
         
        grandomRs ::
                    (RandomGen g) =>
                    T.RefSrcPos ->
                      T.RefExp ->
                        T.R (T.Fun (T.Tuple2 a a) (T.Fun g (T.List a)))
         
        srandomRs ::
                    (RandomGen g) =>
                    T.R (T.Fun (T.Tuple2 a a) (T.Fun g (T.List a)))
         
        grandoms ::
                   (RandomGen g) =>
                   T.RefSrcPos -> T.RefExp -> T.R (T.Fun g (T.List a))
         
        srandoms :: (RandomGen g) => T.R (T.Fun g (T.List a))
         
        grandomRIO ::
                   T.RefSrcPos ->
                     T.RefExp -> T.R (T.Fun (T.Tuple2 a a) (IO a))
         
        srandomRIO :: T.R (T.Fun (T.Tuple2 a a) (IO a))
         
        grandomIO :: T.RefSrcPos -> T.RefExp -> T.R (IO a)
         
        srandomIO :: T.R (IO a)
        grandoms prandoms p
          = T.ufun1 c68v3v69v9randoms prandoms p hrandoms
          where hrandoms fg p
                  = T.con2 T.mkNoSrcPos p T.Cons T.aCons
                      (gx T.mkNoSrcPos p)
                      (T.uap1 T.mkNoSrcPos p (grandoms T.mkNoSrcPos p)
                         (gg' T.mkNoSrcPos p))
                  where gx px p = T.uconstUse px p sx
                        gg' pg' p = T.uconstUse pg' p sg'
                        sx
                          = T.uconstDef p c70v5v70v21x
                              (\ _ ->
                                 case j70v5v70v21x of
                                     (kx, fx, fg') -> fx)
                        sg'
                          = T.uconstDef p c70v5v70v21g'
                              (\ _ ->
                                 case j70v5v70v21x of
                                     (kx, fx, fg') -> fg')
                        j70v5v70v21x
                          = case
                              T.uap1 T.mkNoSrcPos p (grandom T.mkNoSrcPos p) fg
                              of
                                T.R (T.Tuple2 fx fg') kx -> (kx, fx, fg')
                                _ -> T.fatal p
        grandomRs prandomRs p
          = T.ufun2 c71v3v72v9randomRs prandomRs p hrandomRs
          where hrandomRs frange fg p
                  = T.con2 T.mkNoSrcPos p T.Cons T.aCons
                      (gx T.mkNoSrcPos p)
                      (T.uap2 T.mkNoSrcPos p (grandomRs T.mkNoSrcPos p)
                         frange
                         (gg' T.mkNoSrcPos p))
                  where gx px p = T.uconstUse px p sx
                        gg' pg' p = T.uconstUse pg' p sg'
                        sx
                          = T.uconstDef p c73v5v73v28x
                              (\ _ ->
                                 case j73v5v73v28x of
                                     (kx, fx, fg') -> fx)
                        sg'
                          = T.uconstDef p c73v5v73v28g'
                              (\ _ ->
                                 case j73v5v73v28x of
                                     (kx, fx, fg') -> fg')
                        j73v5v73v28x
                          = case
                              T.uap2 T.mkNoSrcPos p (grandomR T.mkNoSrcPos p)
                                frange
                                fg
                              of
                                T.R (T.Tuple2 fx fg') kx -> (kx, fx, fg')
                                _ -> T.fatal p
        grandomIO prandomIO p
          = T.uconstUse prandomIO p srandomIO
        srandomIO
          = T.uconstDef p c74v3v74v39randomIO
              (\ p ->
                 T.uwrapForward p
                   (hgetStdRandom (grandom T.mkNoSrcPos p) p))
        grandomRIO prandomRIO p
          = T.ufun1 c75v3v75v48randomRIO prandomRIO p
              hrandomRIO
          where hrandomRIO frange p
                  = T.uwrapForward p
                      (hgetStdRandom
                         (T.uap1 T.mkNoSrcPos p (grandomR T.mkNoSrcPos p)
                            frange)
                         p)
 
instance Random Int where
        grandomR prandomR p
          = T.ufun2 c78v3v78v66randomR prandomR p hrandomR
          where hrandomR (T.R (T.Tuple2 fa fb) _) fg p
                  = T.uwrapForward p
                      (hrandomIvalInteger
                         (T.con2 T.mkNoSrcPos p T.Tuple2 T.aTuple2
                            (T.uap1 T.mkNoSrcPos p (gtoInteger T.mkNoSrcPos p)
                               fa)
                            (T.uap1 T.mkNoSrcPos p (gtoInteger T.mkNoSrcPos p)
                               fb))
                         fg
                         p)
        grandom prandom p
          = T.ufun1 c79v3v79v49random prandom p hrandom
          where hrandom fg p
                  = T.uap2 T.mkNoSrcPos p (grandomR T.mkNoSrcPos p)
                      (T.con2 T.mkNoSrcPos p T.Tuple2 T.aTuple2
                         (gminBound T.mkNoSrcPos p)
                         (gmaxBound T.mkNoSrcPos p))
                      fg
 
instance Random Integer where
        grandomR prandomR p
          = T.ufun2 c82v3v82v43randomR prandomR p hrandomR
          where hrandomR fival fg p
                  = T.uwrapForward p (hrandomIvalInteger fival fg p)
        grandom prandom p
          = T.ufun1 c83v3v83v83random prandom p hrandom
          where hrandom fg p
                  = T.uap2 T.mkNoSrcPos p (grandomR T.mkNoSrcPos p)
                      (T.con2 T.mkNoSrcPos p T.Tuple2 T.aTuple2
                         (T.uap1 T.mkNoSrcPos p (gtoInteger T.mkNoSrcPos p)
                            (gminBound T.mkNoSrcPos p :: T.R Int))
                         (T.uap1 T.mkNoSrcPos p (gtoInteger T.mkNoSrcPos p)
                            (gmaxBound T.mkNoSrcPos p :: T.R Int)))
                      fg
 
instance Random Float where
        grandom prandom p
          = T.ufun1 c86v3v86v63random prandom p hrandom
          where hrandom fg p
                  = T.uwrapForward p
                      (hrandomIvalDouble
                         (T.con2 T.mkNoSrcPos p T.Tuple2 T.aTuple2
                            (T.uap1 T.mkNoSrcPos p
                               (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                               (T.conInteger T.mkNoSrcPos p (0))
                               :: T.R Double)
                            (T.uap1 T.mkNoSrcPos p
                               (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                               (T.conInteger T.mkNoSrcPos p (1))))
                         (grealToFrac T.mkNoSrcPos p)
                         fg
                         p)
        grandomR prandomR p
          = T.ufun2 c87v3v87v78randomR prandomR p hrandomR
          where hrandomR (T.R (T.Tuple2 fa fb) _) fg p
                  = T.uwrapForward p
                      (hrandomIvalDouble
                         (T.con2 T.mkNoSrcPos p T.Tuple2 T.aTuple2
                            (T.uap1 T.mkNoSrcPos p (grealToFrac T.mkNoSrcPos p)
                               fa)
                            (T.uap1 T.mkNoSrcPos p (grealToFrac T.mkNoSrcPos p)
                               fb))
                         (grealToFrac T.mkNoSrcPos p)
                         fg
                         p)
 
instance Random Double where
        grandomR prandomR p
          = T.ufun2 c90v3v90v45randomR prandomR p hrandomR
          where hrandomR fival fg p
                  = T.uwrapForward p
                      (hrandomIvalDouble fival (gid T.mkNoSrcPos p) fg p)
        grandom prandom p
          = T.ufun1 c91v3v91v42random prandom p hrandom
          where hrandom fg p
                  = T.uap2 T.mkNoSrcPos p (grandomR T.mkNoSrcPos p)
                      (T.con2 T.mkNoSrcPos p T.Tuple2 T.aTuple2
                         (T.uap1 T.mkNoSrcPos p
                            (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                            (T.conInteger T.mkNoSrcPos p (0))
                            :: T.R Double)
                         (T.uap1 T.mkNoSrcPos p
                            (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                            (T.conInteger T.mkNoSrcPos p (1))))
                      fg
 
instance Random Bool where
        grandomR prandomR p
          = T.ufun2 c94v3v98v9randomR prandomR p hrandomR
          where hrandomR (T.R (T.Tuple2 fa fb) _) fg p
                  = T.uccase T.mkNoSrcPos p
                      (let v95v5v98v0v1 (T.R (T.Tuple2 fx fg) _) p
                             = T.con2 T.mkNoSrcPos p T.Tuple2 T.aTuple2
                                 (T.uwrapForward p (hint2Bool fx p))
                                 fg
                         in v95v5v98v0v1)
                      (T.uwrapForward p
                         (hrandomIvalInteger
                            (T.con2 T.mkNoSrcPos p T.Tuple2 T.aTuple2
                               (T.uwrapForward p (hbool2Integer fa p))
                               (T.uwrapForward p (hbool2Integer fb p)))
                            fg
                            p))
                  where  
                        gbool2Integer ::
                                      T.RefSrcPos ->
                                        T.RefExp -> T.R (T.Fun Bool Integer)
                         
                        hbool2Integer :: T.R Bool -> T.RefExp -> T.R Integer
                        gbool2Integer pbool2Integer p
                          = T.ufun1 c100v5v101v26bool2Integer pbool2Integer p
                              hbool2Integer
                        abool2Integer = c100v5v101v26bool2Integer
                        hbool2Integer (T.R False _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (0))
                        hbool2Integer (T.R True _) p
                          = T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                              (T.conInteger T.mkNoSrcPos p (1))
                        hbool2Integer _ p = T.fatal p
                         
                        gint2Bool ::
                                  T.RefSrcPos ->
                                    T.RefExp -> T.R (T.Fun Int Bool)
                         
                        hint2Bool :: T.R Int -> T.RefExp -> T.R Bool
                        gint2Bool pint2Bool p
                          = T.ufun1 c104v5v105v22int2Bool pint2Bool p hint2Bool
                        aint2Bool = c104v5v105v22int2Bool
                        hint2Bool fv104v14v104v14n p
                          = T.ucguard
                              (T.uap2 T.mkNoSrcPos p
                                 ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                                 fv104v14v104v14n
                                 (T.uap1 T.mkNoSrcPos p
                                    (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos
                                       p)
                                    (T.conInteger T.mkNoSrcPos p (0))))
                              (h104v5v104v23n p)
                              (y1int2Bool fv104v14v104v14n p)
                          where h104v5v104v23n p
                                  = T.con0 T.mkNoSrcPos p False aFalse
                                h104v5v104v23n p = y1int2Bool fv104v14v104v14n p
                        hint2Bool fv104v14v104v14n p
                          = y1int2Bool fv104v14v104v14n p
                        y1int2Bool _ p = T.con0 T.mkNoSrcPos p True aTrue
        grandom prandom p
          = T.ufun1 c107v3v107v42random prandom p hrandom
          where hrandom fg p
                  = T.uap2 T.mkNoSrcPos p (grandomR T.mkNoSrcPos p)
                      (T.con2 T.mkNoSrcPos p T.Tuple2 T.aTuple2
                         (gminBound T.mkNoSrcPos p)
                         (gmaxBound T.mkNoSrcPos p))
                      fg
 
instance Random Char where
        grandomR prandomR p
          = T.ufun2 c110v3v113v0randomR prandomR p hrandomR
          where hrandomR (T.R (T.Tuple2 fa fb) _) fg p
                  = T.uccase T.mkNoSrcPos p
                      (let v111v7v113v0v1 (T.R (T.Tuple2 fx fg) _) p
                             = T.con2 T.mkNoSrcPos p T.Tuple2 T.aTuple2
                                 (T.uap1 T.mkNoSrcPos p (gchr T.mkNoSrcPos p)
                                    fx)
                                 fg
                         in v111v7v113v0v1)
                      (T.uwrapForward p
                         (hrandomIvalInteger
                            (T.con2 T.mkNoSrcPos p T.Tuple2 T.aTuple2
                               (T.uap1 T.mkNoSrcPos p
                                  (gtoInteger T.mkNoSrcPos p)
                                  (T.uap1 T.mkNoSrcPos p (gord T.mkNoSrcPos p)
                                     fa))
                               (T.uap1 T.mkNoSrcPos p
                                  (gtoInteger T.mkNoSrcPos p)
                                  (T.uap1 T.mkNoSrcPos p (gord T.mkNoSrcPos p)
                                     fb)))
                            fg
                            p))
        grandom prandom p
          = T.ufun1 c113v3v113v42random prandom p hrandom
          where hrandom fg p
                  = T.uap2 T.mkNoSrcPos p (grandomR T.mkNoSrcPos p)
                      (T.con2 T.mkNoSrcPos p T.Tuple2 T.aTuple2
                         (gminBound T.mkNoSrcPos p)
                         (gmaxBound T.mkNoSrcPos p))
                      fg
 
grandomIvalInteger ::
                     (RandomGen g, Num a) =>
                     T.RefSrcPos ->
                       T.RefExp ->
                         T.R
                           (T.Fun (T.Tuple2 Integer Integer)
                              (T.Fun g (T.Tuple2 a g)))
 
hrandomIvalInteger ::
                     (RandomGen g, Num a) =>
                     T.R (T.Tuple2 Integer Integer) ->
                       T.R g -> T.RefExp -> T.R (T.Tuple2 a g)
grandomIvalInteger prandomIvalInteger p
  = T.ufun2 arandomIvalInteger prandomIvalInteger p
      hrandomIvalInteger
hrandomIvalInteger (T.R (T.Tuple2 fl fh) _) frng p
  = T.ucguard
      (T.uap2 T.mkNoSrcPos p ((!>) T.mkNoSrcPos p) fl fh)
      (T.uwrapForward p
         (hrandomIvalInteger
            (T.con2 T.mkNoSrcPos p T.Tuple2 T.aTuple2 fh fl)
            frng
            p))
      (T.ucguard (gotherwise T.mkNoSrcPos p)
         (T.uccase T.mkNoSrcPos p
            (let v120v16v121v0v1 (T.R (T.Tuple2 fv frng') _) p
                   = T.con2 T.mkNoSrcPos p T.Tuple2 T.aTuple2
                       (T.uap1 T.mkNoSrcPos p (gfromInteger T.mkNoSrcPos p)
                          (T.uap2 T.mkNoSrcPos p ((!+) T.mkNoSrcPos p) fl
                             (T.uap2 T.mkNoSrcPos p (gmod T.mkNoSrcPos p) fv
                                (gk T.mkNoSrcPos p))))
                       frng'
               in v120v16v121v0v1)
            (T.uwrapForward p
               (hf (gn T.mkNoSrcPos p)
                  (T.uap1 T.mkNoSrcPos p
                     (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                     (T.conInteger T.mkNoSrcPos p (1)))
                  frng
                  p)))
         (T.fatal p))
  where gk pk p = T.uconstUse pk p sk
        sk
          = T.uconstDef p c122v8v122v20k
              (\ p ->
                 T.uap2 T.mkNoSrcPos p ((!+) T.mkNoSrcPos p)
                   (T.uap2 T.mkNoSrcPos p ((!-) T.mkNoSrcPos p) fh fl)
                   (T.uap1 T.mkNoSrcPos p
                      (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                      (T.conInteger T.mkNoSrcPos p (1))))
        gb pb p = T.uconstUse pb p sb
        sb
          = T.uconstDef p c123v8v123v21b
              (\ p ->
                 T.uap1 T.mkNoSrcPos p
                   (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                   (T.conInteger T.mkNoSrcPos p (2147483561)))
        gn pn p = T.uconstUse pn p sn
        sn
          = T.uconstDef p c124v8v124v23n
              (\ p ->
                 T.uwrapForward p
                   (hiLogBase (gb T.mkNoSrcPos p) (gk T.mkNoSrcPos p)
                      p))
        gf pf p = T.ufun3 c126v8v131v42f pf p hf
        af = c126v8v131v42f
        hf fv126v10v126v10n facc fg p
          = T.ucguard
              (T.uap2 T.mkNoSrcPos p
                 ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
                 fv126v10v126v10n
                 (T.uap1 T.mkNoSrcPos p
                    (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                    (T.conInteger T.mkNoSrcPos p (0))))
              (h126v8v126v27n facc fg p)
              (y1f fv126v10v126v10n facc fg p)
          where h126v8v126v27n facc fg p
                  = T.con2 T.mkNoSrcPos p T.Tuple2 T.aTuple2 facc fg
                h126v8v126v27n _ _ p = y1f fv126v10v126v10n facc fg p
        hf fv126v10v126v10n facc fg p
          = y1f fv126v10v126v10n facc fg p
        y1f fn facc fg p
          = let gx px p = T.uconstUse px p sx
                gg' pg' p = T.uconstUse pg' p sg'
                sx
                  = T.uconstDef p c129v12v129v28x
                      (\ _ ->
                         case j129v12v129v28x of
                             (kx, fx, fg') -> fx)
                sg'
                  = T.uconstDef p c129v12v129v28g'
                      (\ _ ->
                         case j129v12v129v28x of
                             (kx, fx, fg') -> fg')
                j129v12v129v28x
                  = case
                      T.uap1 T.mkNoSrcPos p (gnext T.mkNoSrcPos p) fg of
                        T.R (T.Tuple2 fx fg') kx -> (kx, fx, fg')
                        _ -> T.fatal p
              in
              T.uwrapForward p
                (hf
                   (T.uap2 T.mkNoSrcPos p ((!-) T.mkNoSrcPos p) fn
                      (T.uap1 T.mkNoSrcPos p
                         (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                         (T.conInteger T.mkNoSrcPos p (1))))
                   (T.uap2 T.mkNoSrcPos p ((!+) T.mkNoSrcPos p)
                      (T.uap1 T.mkNoSrcPos p (gfromInt T.mkNoSrcPos p)
                         (gx T.mkNoSrcPos p))
                      (T.uap2 T.mkNoSrcPos p ((!*) T.mkNoSrcPos p) facc
                         (gb T.mkNoSrcPos p)))
                   (gg' T.mkNoSrcPos p)
                   p)
 
grandomIvalDouble ::
                    (RandomGen g, Fractional a) =>
                    T.RefSrcPos ->
                      T.RefExp ->
                        T.R
                          (T.Fun (T.Tuple2 Double Double)
                             (T.Fun (T.Fun Double a) (T.Fun g (T.Tuple2 a g))))
 
hrandomIvalDouble ::
                    (RandomGen g, Fractional a) =>
                    T.R (T.Tuple2 Double Double) ->
                      T.R (T.Fun Double a) ->
                        T.R g -> T.RefExp -> T.R (T.Tuple2 a g)
grandomIvalDouble prandomIvalDouble p
  = T.ufun3 arandomIvalDouble prandomIvalDouble p
      hrandomIvalDouble
hrandomIvalDouble (T.R (T.Tuple2 fl fh) _)
  ffromDouble frng p
  = T.ucguard
      (T.uap2 T.mkNoSrcPos p ((!>) T.mkNoSrcPos p) fl fh)
      (T.uwrapForward p
         (hrandomIvalDouble
            (T.con2 T.mkNoSrcPos p T.Tuple2 T.aTuple2 fh fl)
            ffromDouble
            frng
            p))
      (T.ucguard (gotherwise T.mkNoSrcPos p)
         (T.uccase T.mkNoSrcPos p
            (let v137v8v147v0v1 (T.R (T.Tuple2 fx frng') _) p
                   = let gscaled_x pscaled_x p
                           = T.uconstUse pscaled_x p sscaled_x
                         sscaled_x
                           = T.uconstDef p c140v14v143v37scaled_x
                               (\ p ->
                                  T.uap2 T.mkNoSrcPos p ((!+) T.mkNoSrcPos p)
                                    (T.uap1 T.mkNoSrcPos p ffromDouble
                                       (T.uap2 T.mkNoSrcPos p
                                          ((!/) T.mkNoSrcPos p)
                                          (T.uap2 T.mkNoSrcPos p
                                             ((!+) T.mkNoSrcPos p)
                                             fl
                                             fh)
                                          (T.uap1 T.mkNoSrcPos p
                                             (Hat.PreludeBasic.gfromInteger
                                                T.mkNoSrcPos
                                                p)
                                             (T.conInteger T.mkNoSrcPos p
                                                (2)))))
                                    (T.uap2 T.mkNoSrcPos p ((!*) T.mkNoSrcPos p)
                                       (T.uap1 T.mkNoSrcPos p ffromDouble
                                          (T.uap2 T.mkNoSrcPos p
                                             ((!/) T.mkNoSrcPos p)
                                             (T.uap2 T.mkNoSrcPos p
                                                ((!-) T.mkNoSrcPos p)
                                                fh
                                                fl)
                                             (T.uap1 T.mkNoSrcPos p
                                                (grealToFrac T.mkNoSrcPos p)
                                                (gintRange T.mkNoSrcPos p))))
                                       (T.uap1 T.mkNoSrcPos p
                                          (gfromIntegral T.mkNoSrcPos p)
                                          (fx :: T.R Int))))
                       in
                       T.con2 T.mkNoSrcPos p T.Tuple2 T.aTuple2
                         (gscaled_x T.mkNoSrcPos p)
                         frng'
               in v137v8v147v0v1)
            (T.uwrapForward p
               (hrandomIvalInteger
                  (T.con2 T.mkNoSrcPos p T.Tuple2 T.aTuple2
                     (T.uap1 T.mkNoSrcPos p (gtoInteger T.mkNoSrcPos p)
                        (gminBound T.mkNoSrcPos p :: T.R Int))
                     (T.uap1 T.mkNoSrcPos p (gtoInteger T.mkNoSrcPos p)
                        (gmaxBound T.mkNoSrcPos p :: T.R Int)))
                  frng
                  p)))
         (T.fatal p))
 
gfromInt ::
           (Num a) =>
           T.RefSrcPos -> T.RefExp -> T.R (T.Fun Int a)
 
sfromInt :: (Num a) => T.R (T.Fun Int a)
gfromInt pfromInt p = T.uconstUse pfromInt p sfromInt
sfromInt
  = T.uconstDef p afromInt
      (\ p ->
         T.uap2 T.mkNoSrcPos p ((!.) T.mkNoSrcPos p)
           (gfromInteger T.mkNoSrcPos p)
           (gtoInteger T.mkNoSrcPos p))
 
gintRange :: T.RefSrcPos -> T.RefExp -> T.R Integer
 
sintRange :: T.R Integer
gintRange pintRange p
  = T.uconstUse pintRange p sintRange
sintRange
  = T.uconstDef p aintRange
      (\ p ->
         T.uap2 T.mkNoSrcPos p ((!-) T.mkNoSrcPos p)
           (T.uap1 T.mkNoSrcPos p (gtoInteger T.mkNoSrcPos p)
              (gmaxBound T.mkNoSrcPos p :: T.R Int))
           (T.uap1 T.mkNoSrcPos p (gtoInteger T.mkNoSrcPos p)
              (gminBound T.mkNoSrcPos p :: T.R Int)))
 
giLogBase ::
          T.RefSrcPos ->
            T.RefExp ->
              T.R (T.Fun Integer (T.Fun Integer Integer))
 
hiLogBase ::
          T.R Integer -> T.R Integer -> T.RefExp -> T.R Integer
giLogBase piLogBase p
  = T.ufun2 aiLogBase piLogBase p hiLogBase
hiLogBase fb fi p
  = T.ucif p
      (T.uap2 T.mkNoSrcPos p ((!<) T.mkNoSrcPos p) fi fb)
      (T.uap1 T.mkNoSrcPos p
         (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
         (T.conInteger T.mkNoSrcPos p (1)))
      (T.uap2 T.mkNoSrcPos p ((!+) T.mkNoSrcPos p)
         (T.uap1 T.mkNoSrcPos p
            (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p (1)))
         (T.uwrapForward p
            (hiLogBase fb
               (T.uap2 T.mkNoSrcPos p (gdiv T.mkNoSrcPos p) fi fb)
               p)))
 
gnewStdGen ::
           T.RefSrcPos -> T.RefExp -> T.R (IO StdGen)
gnewStdGen pnewStdGen p
  = T.uconstUse pnewStdGen p snewStdGen
snewStdGen
  = T.uconstDef p anewStdGen
      (\ p -> T.fromIO T.fromStdGen p Random.newStdGen)
 
gsetStdGen ::
           T.RefSrcPos ->
             T.RefExp -> T.R (T.Fun StdGen (IO T.Tuple0))
gsetStdGen psetStdGen p
  = T.ufun1 asetStdGen psetStdGen p hsetStdGen
hsetStdGen z1setStdGen ksetStdGen
  = T.fromIO T.fromTuple0 ksetStdGen
      (Random.setStdGen
         (T.toStdGen ksetStdGen z1setStdGen))
 
ggetStdGen ::
           T.RefSrcPos -> T.RefExp -> T.R (IO StdGen)
ggetStdGen pgetStdGen p
  = T.uconstUse pgetStdGen p sgetStdGen
sgetStdGen
  = T.uconstDef p agetStdGen
      (\ p -> T.fromIO T.fromStdGen p Random.getStdGen)
 
ggetStdRandom ::
              T.RefSrcPos ->
                T.RefExp ->
                  T.R (T.Fun (T.Fun StdGen (T.Tuple2 a StdGen)) (IO a))
ggetStdRandom pgetStdRandom p
  = T.ufun1 agetStdRandom pgetStdRandom p hgetStdRandom
hgetStdRandom z1getStdRandom kgetStdRandom
  = T.fromIO T.fromId kgetStdRandom
      (Random.getStdRandom
         ((T.toFun T.fromStdGen
             (T.toTuple2 T.toId T.toStdGen))
            kgetStdRandom
            z1getStdRandom))
afromInt
  = T.mkVariable tRandom 1480001 1480033 3 (0)
      "fromInt"
      Prelude.False
c24v3v24v34genRange
  = T.mkVariable tRandom 240003 240034 3 (-1)
      "genRange"
      Prelude.False
agetStdGen
  = T.mkVariable tRandom 1620001 1630016 3 (0)
      "getStdGen"
      Prelude.False
agetStdRandom
  = T.mkVariable tRandom 1640001 1650016 3 (1)
      "getStdRandom"
      Prelude.False
aiLogBase
  = T.mkVariable tRandom 1540001 1540062 3 (2)
      "iLogBase"
      Prelude.False
aintRange
  = T.mkVariable tRandom 1510001 1510065 3 (0)
      "intRange"
      Prelude.False
amkStdGen
  = T.mkVariable tRandom 530001 540012 3 (1) "mkStdGen"
      Prelude.False
anewStdGen
  = T.mkVariable tRandom 1580001 1590016 3 (0)
      "newStdGen"
      Prelude.False
aprimStdGenGenRange
  = T.mkVariable tRandom 410001 420022 3 (1)
      "primStdGenGenRange"
      Prelude.False
aprimStdGenNext
  = T.mkVariable tRandom 430001 440018 3 (1)
      "primStdGenNext"
      Prelude.False
aprimStdGenReadsPrec
  = T.mkVariable tRandom 470001 480023 3 (2)
      "primStdGenReadsPrec"
      Prelude.False
aprimStdGenShowsPrec
  = T.mkVariable tRandom 490001 500023 3 (3)
      "primStdGenShowsPrec"
      Prelude.False
aprimStdGenSplit
  = T.mkVariable tRandom 450001 460019 3 (1)
      "primStdGenSplit"
      Prelude.False
c74v3v74v39randomIO
  = T.mkVariable tRandom 740003 740039 3 (-1)
      "randomIO"
      Prelude.False
arandomIvalDouble
  = T.mkVariable tRandom 1340001 1470000 3 (3)
      "randomIvalDouble"
      Prelude.False
arandomIvalInteger
  = T.mkVariable tRandom 1180001 1210010 3 (2)
      "randomIvalInteger"
      Prelude.False
c75v3v75v48randomRIO
  = T.mkVariable tRandom 750003 750048 3 (-1)
      "randomRIO"
      Prelude.False
c71v3v72v9randomRs
  = T.mkVariable tRandom 710003 720009 3 (-1)
      "randomRs"
      Prelude.False
c68v3v69v9randoms
  = T.mkVariable tRandom 680003 690009 3 (-1) "randoms"
      Prelude.False
asetStdGen
  = T.mkVariable tRandom 1600001 1610016 3 (1)
      "setStdGen"
      Prelude.False
c31v3v31v31genRange
  = T.mkVariable tRandom 310003 310031 3 (-1)
      "genRange"
      Prelude.False
c32v3v32v23next
  = T.mkVariable tRandom 320003 320023 3 (-1) "next"
      Prelude.False
c33v3v33v25split
  = T.mkVariable tRandom 330003 330025 3 (-1) "split"
      Prelude.False
c36v3v36v33readsPrec
  = T.mkVariable tRandom 360003 360033 3 (-1)
      "readsPrec"
      Prelude.False
c39v3v39v33showsPrec
  = T.mkVariable tRandom 390003 390033 3 (-1)
      "showsPrec"
      Prelude.False
c70v5v70v21g'
  = T.mkVariable tRandom 700005 700021 3 (0) "g'"
      Prelude.True
c70v5v70v21x
  = T.mkVariable tRandom 700005 700021 3 (0) "x"
      Prelude.True
c73v5v73v28g'
  = T.mkVariable tRandom 730005 730028 3 (0) "g'"
      Prelude.True
c73v5v73v28x
  = T.mkVariable tRandom 730005 730028 3 (0) "x"
      Prelude.True
c79v3v79v49random
  = T.mkVariable tRandom 790003 790049 3 (-1) "random"
      Prelude.False
c78v3v78v66randomR
  = T.mkVariable tRandom 780003 780066 3 (-1) "randomR"
      Prelude.False
c83v3v83v83random
  = T.mkVariable tRandom 830003 830083 3 (-1) "random"
      Prelude.False
c82v3v82v43randomR
  = T.mkVariable tRandom 820003 820043 3 (-1) "randomR"
      Prelude.False
c86v3v86v63random
  = T.mkVariable tRandom 860003 860063 3 (-1) "random"
      Prelude.False
c87v3v87v78randomR
  = T.mkVariable tRandom 870003 870078 3 (-1) "randomR"
      Prelude.False
c91v3v91v42random
  = T.mkVariable tRandom 910003 910042 3 (-1) "random"
      Prelude.False
c90v3v90v45randomR
  = T.mkVariable tRandom 900003 900045 3 (-1) "randomR"
      Prelude.False
c100v5v101v26bool2Integer
  = T.mkVariable tRandom 1000005 1010026 3 (1)
      "bool2Integer"
      Prelude.True
c104v5v105v22int2Bool
  = T.mkVariable tRandom 1040005 1050022 3 (1)
      "int2Bool"
      Prelude.True
c107v3v107v42random
  = T.mkVariable tRandom 1070003 1070042 3 (-1)
      "random"
      Prelude.False
c94v3v98v9randomR
  = T.mkVariable tRandom 940003 980009 3 (-1) "randomR"
      Prelude.False
c113v3v113v42random
  = T.mkVariable tRandom 1130003 1130042 3 (-1)
      "random"
      Prelude.False
c110v3v113v0randomR
  = T.mkVariable tRandom 1100003 1130000 3 (-1)
      "randomR"
      Prelude.False
c123v8v123v21b
  = T.mkVariable tRandom 1230008 1230021 3 (0) "b"
      Prelude.True
c126v8v131v42f
  = T.mkVariable tRandom 1260008 1310042 3 (3) "f"
      Prelude.True
c122v8v122v20k
  = T.mkVariable tRandom 1220008 1220020 3 (0) "k"
      Prelude.True
c124v8v124v23n
  = T.mkVariable tRandom 1240008 1240023 3 (0) "n"
      Prelude.True
c129v12v129v28g'
  = T.mkVariable tRandom 1290012 1290028 3 (0) "g'"
      Prelude.True
c129v12v129v28x
  = T.mkVariable tRandom 1290012 1290028 3 (0) "x"
      Prelude.True
c140v14v143v37scaled_x
  = T.mkVariable tRandom 1400014 1430037 3 (0)
      "scaled_x"
      Prelude.True
p = T.mkRoot
tRandom
  = T.mkModule "Random" "Random.hs" Prelude.False