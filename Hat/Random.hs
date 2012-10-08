module Hat.Random
  (RandomGen(gnext,gsplit,ggenRange,snext,ssplit,sgenRange),StdGen(),gmkStdGen
    ,amkStdGen,hmkStdGen,Random(grandom,grandomR,grandoms,grandomRs,grandomIO
      ,grandomRIO,srandom,srandomR,srandoms,srandomRs,srandomIO,srandomRIO)
    ,ggetStdRandom,agetStdRandom,hgetStdRandom,ggetStdGen,gsetStdGen,asetStdGen
    ,hsetStdGen,gnewStdGen) where

import qualified Hat.PreludeBasic 
import qualified Prelude 
import Hat.Hack 
import qualified Hat.Hat as T 
import Hat.Hat  (WrapVal(wrapVal))
import Hat.Prelude 
import Hat.PreludeBuiltinTypes 
import Hat.RandomBuiltin 
import qualified System.Random as Random 
import Hat.Char 

class RandomGen g
  where
  
  ggenRange :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun g (T.Tuple2 Int Int))
  
  gnext :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun g (T.Tuple2 Int g))
  
  gsplit :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun g (T.Tuple2 g g))
  
  ggenRange pgenRange p =
    T.ufun1 a24v3v24v34genRange pgenRange p hgenRange
    where
    
    hgenRange fg p =
      T.con2 T.mkNoSrcPos p T.Tuple2 T.aTuple2 (gminBound T.mkNoSrcPos p)
        (gmaxBound T.mkNoSrcPos p)
    
  
  sgenRange :: T.R (T.Fun g (T.Tuple2 Int Int))
  
  snext :: T.R (T.Fun g (T.Tuple2 Int g))
  
  ssplit :: T.R (T.Fun g (T.Tuple2 g g))
  

instance RandomGen (StdGen)
  where
  
  ggenRange pgenRange p = T.uconstUse pgenRange p sgenRange
  
  sgenRange =
    T.uconstDef T.mkRoot a31v3v31v31genRange
      (\ p -> gprimStdGenGenRange T.mkNoSrcPos p)
  
  gnext pnext p = T.uconstUse pnext p snext
  
  snext =
    T.uconstDef T.mkRoot a32v3v32v23next (\ p -> gprimStdGenNext T.mkNoSrcPos p)
  
  gsplit psplit p = T.uconstUse psplit p ssplit
  
  ssplit =
    T.uconstDef T.mkRoot a33v3v33v25split
      (\ p -> gprimStdGenSplit T.mkNoSrcPos p)
  

instance Read (StdGen)
  where
  
  greadsPrec preadsPrec p = T.uconstUse preadsPrec p sreadsPrec
  
  sreadsPrec =
    T.uconstDef T.mkRoot a36v3v36v33readsPrec
      (\ p -> gprimStdGenReadsPrec T.mkNoSrcPos p)
  

instance Show (StdGen)
  where
  
  gshowsPrec pshowsPrec p = T.uconstUse pshowsPrec p sshowsPrec
  
  sshowsPrec =
    T.uconstDef T.mkRoot a39v3v39v33showsPrec
      (\ p -> gprimStdGenShowsPrec T.mkNoSrcPos p)
  

gprimStdGenGenRange ::
  T.RefSrcPos -> T.RefExp -> T.R (T.Fun StdGen (T.Tuple2 Int Int))

gprimStdGenGenRange pprimStdGenGenRange p =
  T.ufun1 aprimStdGenGenRange pprimStdGenGenRange p hprimStdGenGenRange

hprimStdGenGenRange z1primStdGenGenRange kprimStdGenGenRange =
  (T.fromTuple2 T.fromInt T.fromInt) kprimStdGenGenRange
    (Random.genRange (toStdGen kprimStdGenGenRange z1primStdGenGenRange))

gprimStdGenNext ::
  T.RefSrcPos -> T.RefExp -> T.R (T.Fun StdGen (T.Tuple2 Int StdGen))

gprimStdGenNext pprimStdGenNext p =
  T.ufun1 aprimStdGenNext pprimStdGenNext p hprimStdGenNext

hprimStdGenNext z1primStdGenNext kprimStdGenNext =
  (T.fromTuple2 T.fromInt fromStdGen) kprimStdGenNext
    (Random.next (toStdGen kprimStdGenNext z1primStdGenNext))

gprimStdGenSplit ::
  T.RefSrcPos -> T.RefExp -> T.R (T.Fun StdGen (T.Tuple2 StdGen StdGen))

gprimStdGenSplit pprimStdGenSplit p =
  T.ufun1 aprimStdGenSplit pprimStdGenSplit p hprimStdGenSplit

hprimStdGenSplit z1primStdGenSplit kprimStdGenSplit =
  (T.fromTuple2 fromStdGen fromStdGen) kprimStdGenSplit
    (Random.split (toStdGen kprimStdGenSplit z1primStdGenSplit))

gprimStdGenReadsPrec ::
  T.RefSrcPos ->
    T.RefExp -> T.R (T.Fun Int (T.Fun String (T.List (T.Tuple2 StdGen String))))

gprimStdGenReadsPrec pprimStdGenReadsPrec p =
  T.ufun2 aprimStdGenReadsPrec pprimStdGenReadsPrec p hprimStdGenReadsPrec

hprimStdGenReadsPrec z1primStdGenReadsPrec z2primStdGenReadsPrec
  kprimStdGenReadsPrec =
  (fromList (T.fromTuple2 fromStdGen fromString)) kprimStdGenReadsPrec
    (Prelude.readsPrec (T.toInt kprimStdGenReadsPrec z1primStdGenReadsPrec)
      (toString kprimStdGenReadsPrec z2primStdGenReadsPrec))

gprimStdGenShowsPrec ::
  T.RefSrcPos ->
    T.RefExp -> T.R (T.Fun Int (T.Fun StdGen (T.Fun String String)))

gprimStdGenShowsPrec pprimStdGenShowsPrec p =
  T.ufun3 aprimStdGenShowsPrec pprimStdGenShowsPrec p hprimStdGenShowsPrec

hprimStdGenShowsPrec z1primStdGenShowsPrec z2primStdGenShowsPrec
  z3primStdGenShowsPrec kprimStdGenShowsPrec =
  fromString kprimStdGenShowsPrec
    (Prelude.showsPrec (T.toInt kprimStdGenShowsPrec z1primStdGenShowsPrec)
      (toStdGen kprimStdGenShowsPrec z2primStdGenShowsPrec)
      (toString kprimStdGenShowsPrec z3primStdGenShowsPrec))

gmkStdGen :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun Int StdGen)

gmkStdGen pmkStdGen p = T.ufun1 amkStdGen pmkStdGen p hmkStdGen

hmkStdGen z1mkStdGen kmkStdGen =
  fromStdGen kmkStdGen (Random.mkStdGen (T.toInt kmkStdGen z1mkStdGen))

class Random a
  where
  
  grandomR ::
    RandomGen g =>
    T.RefSrcPos ->
      T.RefExp -> T.R (T.Fun (T.Tuple2 a a) (T.Fun g (T.Tuple2 a g)))
  
  grandom ::
    RandomGen g => T.RefSrcPos -> T.RefExp -> T.R (T.Fun g (T.Tuple2 a g))
  
  grandomRs ::
    RandomGen g =>
    T.RefSrcPos -> T.RefExp -> T.R (T.Fun (T.Tuple2 a a) (T.Fun g (T.List a)))
  
  grandoms :: RandomGen g => T.RefSrcPos -> T.RefExp -> T.R (T.Fun g (T.List a))
  
  grandomRIO :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun (T.Tuple2 a a) (IO a))
  
  grandomIO :: T.RefSrcPos -> T.RefExp -> T.R (IO a)
  
  grandoms prandoms p =
    T.ufun1 a68v3v70v21randoms prandoms p hrandoms
    where
    
    hrandoms fg p =
      T.con2 T.mkNoSrcPos p T.Cons T.aCons (gx T.mkNoSrcPos p)
        (T.uap1 T.mkNoSrcPos p (grandoms T.mkNoSrcPos p) (gg' T.mkNoSrcPos p))
      where
      
      gx px p = T.uconstUse px p sx
      
      gg' px p = T.uconstUse px p sg'
      
      j70v5v70v10x =
        case T.uap1 T.mkNoSrcPos p (grandom T.mkNoSrcPos p) fg of
          T.R (T.Tuple2 fx fg') kx -> (kx,fx,fg')
          _ -> T.fatal p
      
      sx =
        T.uconstDef p a70v6v70v6x
          (\ _ -> case j70v5v70v10x of (kx,fx,fg') -> fx)
      
      sg' =
        T.uconstDef p a70v8v70v9g'
          (\ _ -> case j70v5v70v10x of (kx,fx,fg') -> fg')
      
    
  
  grandomRs prandomRs p =
    T.ufun2 a71v3v73v28randomRs prandomRs p hrandomRs
    where
    
    hrandomRs frange fg p =
      T.con2 T.mkNoSrcPos p T.Cons T.aCons (gx T.mkNoSrcPos p)
        (T.uap2 T.mkNoSrcPos p (grandomRs T.mkNoSrcPos p) frange
          (gg' T.mkNoSrcPos p))
      where
      
      gx px p = T.uconstUse px p sx
      
      gg' px p = T.uconstUse px p sg'
      
      j73v5v73v10x =
        case T.uap2 T.mkNoSrcPos p (grandomR T.mkNoSrcPos p) frange fg of
          T.R (T.Tuple2 fx fg') kx -> (kx,fx,fg')
          _ -> T.fatal p
      
      sx =
        T.uconstDef p a73v6v73v6x
          (\ _ -> case j73v5v73v10x of (kx,fx,fg') -> fx)
      
      sg' =
        T.uconstDef p a73v8v73v9g'
          (\ _ -> case j73v5v73v10x of (kx,fx,fg') -> fg')
      
    
  
  grandomIO prandomIO p = T.uconstUse prandomIO p srandomIO
  
  srandomIO =
    T.uconstDef T.mkRoot a74v3v74v39randomIO
      (\ p -> T.uwrapForward p (hgetStdRandom (grandom T.mkNoSrcPos p) p))
  
  grandomRIO prandomRIO p =
    T.ufun1 a75v3v75v47randomRIO prandomRIO p hrandomRIO
    where
    
    hrandomRIO frange p =
      T.uwrapForward p
        (hgetStdRandom (T.uap1 T.mkNoSrcPos p (grandomR T.mkNoSrcPos p) frange)
          p)
    
  
  srandomR :: RandomGen g => T.R (T.Fun (T.Tuple2 a a) (T.Fun g (T.Tuple2 a g)))
  
  srandom :: RandomGen g => T.R (T.Fun g (T.Tuple2 a g))
  
  srandomRs :: RandomGen g => T.R (T.Fun (T.Tuple2 a a) (T.Fun g (T.List a)))
  
  srandoms :: RandomGen g => T.R (T.Fun g (T.List a))
  
  srandomRIO :: T.R (T.Fun (T.Tuple2 a a) (IO a))
  
  srandomIO :: T.R (IO a)
  

instance Random (Int)
  where
  
  grandomR prandomR p =
    T.ufun2 a78v3v78v66randomR prandomR p hrandomR
    where
    
    hrandomR (T.R (T.Tuple2 fa fb) _) fg p =
      T.uwrapForward p
        (hrandomIvalInteger
          (T.con2 T.mkNoSrcPos p T.Tuple2 T.aTuple2
            (T.uap1 T.mkNoSrcPos p (gtoInteger T.mkNoSrcPos p) fa)
            (T.uap1 T.mkNoSrcPos p (gtoInteger T.mkNoSrcPos p) fb)) fg p)
    hrandomR _ _ p = T.fatal p
    
  
  grandom prandom p =
    T.ufun1 a79v3v79v49random prandom p hrandom
    where
    
    hrandom fg p =
      T.uap2 T.mkNoSrcPos p (grandomR T.mkNoSrcPos p)
        (T.con2 T.mkNoSrcPos p T.Tuple2 T.aTuple2 (gminBound T.mkNoSrcPos p)
          (gmaxBound T.mkNoSrcPos p)) fg
    
  

instance Random (Integer)
  where
  
  grandomR prandomR p =
    T.ufun2 a82v3v82v43randomR prandomR p hrandomR
    where
    
    hrandomR fival fg p = T.uwrapForward p (hrandomIvalInteger fival fg p)
    
  
  grandom prandom p =
    T.ufun1 a83v3v83v83random prandom p hrandom
    where
    
    hrandom fg p =
      T.uap2 T.mkNoSrcPos p (grandomR T.mkNoSrcPos p)
        (T.con2 T.mkNoSrcPos p T.Tuple2 T.aTuple2
          (T.uap1 T.mkNoSrcPos p (gtoInteger T.mkNoSrcPos p)
            (gminBound T.mkNoSrcPos p :: T.R Int))
          (T.uap1 T.mkNoSrcPos p (gtoInteger T.mkNoSrcPos p)
            (gmaxBound T.mkNoSrcPos p :: T.R Int))) fg
    
  

instance Random (Float)
  where
  
  grandom prandom p =
    T.ufun1 a86v3v86v63random prandom p hrandom
    where
    
    hrandom fg p =
      T.uwrapForward p
        (hrandomIvalDouble
          (T.con2 T.mkNoSrcPos p T.Tuple2 T.aTuple2
            (T.uap1 T.mkNoSrcPos p
                (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                (T.conInteger T.mkNoSrcPos p 0)
              :: T.R Double)
            (T.uap1 T.mkNoSrcPos p
              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
              (T.conInteger T.mkNoSrcPos p 1))) (grealToFrac T.mkNoSrcPos p) fg
          p)
    
  
  grandomR prandomR p =
    T.ufun2 a87v3v87v78randomR prandomR p hrandomR
    where
    
    hrandomR (T.R (T.Tuple2 fa fb) _) fg p =
      T.uwrapForward p
        (hrandomIvalDouble
          (T.con2 T.mkNoSrcPos p T.Tuple2 T.aTuple2
            (T.uap1 T.mkNoSrcPos p (grealToFrac T.mkNoSrcPos p) fa)
            (T.uap1 T.mkNoSrcPos p (grealToFrac T.mkNoSrcPos p) fb))
          (grealToFrac T.mkNoSrcPos p) fg p)
    hrandomR _ _ p = T.fatal p
    
  

instance Random (Double)
  where
  
  grandomR prandomR p =
    T.ufun2 a90v3v90v45randomR prandomR p hrandomR
    where
    
    hrandomR fival fg p =
      T.uwrapForward p (hrandomIvalDouble fival (gid T.mkNoSrcPos p) fg p)
    
  
  grandom prandom p =
    T.ufun1 a91v3v91v42random prandom p hrandom
    where
    
    hrandom fg p =
      T.uap2 T.mkNoSrcPos p (grandomR T.mkNoSrcPos p)
        (T.con2 T.mkNoSrcPos p T.Tuple2 T.aTuple2
          (T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
              (T.conInteger T.mkNoSrcPos p 0)
            :: T.R Double)
          (T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p 1))) fg
    
  

instance Random (Bool)
  where
  
  grandomR prandomR p =
    T.ufun2 a94v3v105v22randomR prandomR p hrandomR
    where
    
    hrandomR (T.R (T.Tuple2 fa fb) _) fg p =
      T.uccase T.mkNoSrcPos p
        (let
          v95v5v97v33v1 (T.R (T.Tuple2 fx fg) _) p =
            T.con2 T.mkNoSrcPos p T.Tuple2 T.aTuple2
              (T.uwrapForward p (hint2Bool fx p)) fg
          v95v5v97v33v1 _ p = T.fatal p in (v95v5v97v33v1))
        (T.uwrapForward p
          (hrandomIvalInteger
            (T.con2 T.mkNoSrcPos p T.Tuple2 T.aTuple2
              (T.uwrapForward p (hbool2Integer fa p))
              (T.uwrapForward p (hbool2Integer fb p))) fg p))
      where
      
      gbool2Integer :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun Bool Integer)
      
      hbool2Integer :: (T.R Bool) -> T.RefExp -> T.R Integer
      
      gbool2Integer pbool2Integer p =
        T.ufun1 a100v5v101v26bool2Integer pbool2Integer p hbool2Integer
      
      abool2Integer = a100v5v101v26bool2Integer
      
      hbool2Integer (T.R False _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 0)
      hbool2Integer (T.R True _) p =
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 1)
      hbool2Integer _ p = T.fatal p
      
      gint2Bool :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun Int Bool)
      
      hint2Bool :: (T.R Int) -> T.RefExp -> T.R Bool
      
      gint2Bool pint2Bool p =
        T.ufun1 a104v5v105v22int2Bool pint2Bool p hint2Bool
      
      aint2Bool = a104v5v105v22int2Bool
      
      hint2Bool fv104v14v104v14n p =
        T.ucguard
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p)
            fv104v14v104v14n
            (T.uap1 T.mkNoSrcPos p
              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
              (T.conInteger T.mkNoSrcPos p 0))) (h p)
          (y1int2Bool fv104v14v104v14n p)
        where
        
        h p = T.con0 T.mkNoSrcPos p False aFalse
        h p = y1int2Bool fv104v14v104v14n p
        
      hint2Bool fv104v14v104v14n p = y1int2Bool fv104v14v104v14n p
      
      y1int2Bool _ p = T.con0 T.mkNoSrcPos p True aTrue
      
    hrandomR _ _ p = T.fatal p
    
  
  grandom prandom p =
    T.ufun1 a107v3v107v42random prandom p hrandom
    where
    
    hrandom fg p =
      T.uap2 T.mkNoSrcPos p (grandomR T.mkNoSrcPos p)
        (T.con2 T.mkNoSrcPos p T.Tuple2 T.aTuple2 (gminBound T.mkNoSrcPos p)
          (gmaxBound T.mkNoSrcPos p)) fg
    
  

instance Random (Char)
  where
  
  grandomR prandomR p =
    T.ufun2 a110v3v112v27randomR prandomR p hrandomR
    where
    
    hrandomR (T.R (T.Tuple2 fa fb) _) fg p =
      T.uccase T.mkNoSrcPos p
        (let
          v111v7v112v27v1 (T.R (T.Tuple2 fx fg) _) p =
            T.con2 T.mkNoSrcPos p T.Tuple2 T.aTuple2
              (T.uap1 T.mkNoSrcPos p (gchr T.mkNoSrcPos p) fx) fg
          v111v7v112v27v1 _ p = T.fatal p in (v111v7v112v27v1))
        (T.uwrapForward p
          (hrandomIvalInteger
            (T.con2 T.mkNoSrcPos p T.Tuple2 T.aTuple2
              (T.uap1 T.mkNoSrcPos p (gtoInteger T.mkNoSrcPos p)
                (T.uap1 T.mkNoSrcPos p (gord T.mkNoSrcPos p) fa))
              (T.uap1 T.mkNoSrcPos p (gtoInteger T.mkNoSrcPos p)
                (T.uap1 T.mkNoSrcPos p (gord T.mkNoSrcPos p) fb))) fg p))
    hrandomR _ _ p = T.fatal p
    
  
  grandom prandom p =
    T.ufun1 a113v3v113v42random prandom p hrandom
    where
    
    hrandom fg p =
      T.uap2 T.mkNoSrcPos p (grandomR T.mkNoSrcPos p)
        (T.con2 T.mkNoSrcPos p T.Tuple2 T.aTuple2 (gminBound T.mkNoSrcPos p)
          (gmaxBound T.mkNoSrcPos p)) fg
    
  

grandomIvalInteger ::
  (RandomGen g,Num a) =>
  T.RefSrcPos ->
    T.RefExp -> T.R (T.Fun (T.Tuple2 Integer Integer) (T.Fun g (T.Tuple2 a g)))

hrandomIvalInteger ::
  (RandomGen g,Num a) =>
  (T.R (T.Tuple2 Integer Integer)) -> (T.R g) -> T.RefExp -> T.R (T.Tuple2 a g)

grandomIvalInteger prandomIvalInteger p =
  T.ufun2 arandomIvalInteger prandomIvalInteger p hrandomIvalInteger

hrandomIvalInteger (T.R (T.Tuple2 fl fh) _) frng p =
  T.ucguard (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !> p) fl fh)
    (T.uwrapForward p
      (hrandomIvalInteger (T.con2 T.mkNoSrcPos p T.Tuple2 T.aTuple2 fh fl) frng
        p))
    (T.ucguard (gotherwise T.mkNoSrcPos p)
      (T.uccase T.mkNoSrcPos p
        (let
          v120v16v120v83v1 (T.R (T.Tuple2 fv frng') _) p =
            T.con2 T.mkNoSrcPos p T.Tuple2 T.aTuple2
              (T.uap1 T.mkNoSrcPos p (gfromInteger T.mkNoSrcPos p)
                (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !+ p) fl
                  (T.uap2 T.mkNoSrcPos p (gmod T.mkNoSrcPos p) fv
                    (gk T.mkNoSrcPos p)))) frng'
          v120v16v120v83v1 _ p = T.fatal p in (v120v16v120v83v1))
        (T.uwrapForward p
          (hf (gn T.mkNoSrcPos p)
            (T.uap1 T.mkNoSrcPos p
              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
              (T.conInteger T.mkNoSrcPos p 1)) frng p))) (T.fatal p))
  where
  
  gk pk p = T.uconstUse pk p sk
  
  sk =
    T.uconstDef p a122v8v122v20k
      (\ p ->
        T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !+ p)
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !- p) fh fl)
          (T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p 1)))
  
  gb pb p = T.uconstUse pb p sb
  
  sb =
    T.uconstDef p a123v8v123v21b
      (\ p ->
        T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 2147483561))
  
  gn pn p = T.uconstUse pn p sn
  
  sn =
    T.uconstDef p a124v8v124v23n
      (\ p ->
        T.uwrapForward p (hiLogBase (gb T.mkNoSrcPos p) (gk T.mkNoSrcPos p) p))
  
  gf pf p = T.ufun3 a126v8v131v42f pf p hf
  
  af = a126v8v131v42f
  
  hf fv126v10v126v10n facc fg p =
    T.ucguard
      (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!== p) fv126v10v126v10n
        (T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
          (T.conInteger T.mkNoSrcPos p 0))) (h facc fg p)
      (y1f fv126v10v126v10n facc fg p)
    where
    
    h facc fg p = T.con2 T.mkNoSrcPos p T.Tuple2 T.aTuple2 facc fg
    h _ _ p = y1f fv126v10v126v10n facc fg p
    
  hf fv126v10v126v10n facc fg p = y1f fv126v10v126v10n facc fg p
  
  y1f fn facc fg p =
    let
      gx px p = T.uconstUse px p sx
      gg' px p = T.uconstUse px p sg'
      j129v12v129v17x =
        case T.uap1 T.mkNoSrcPos p (gnext T.mkNoSrcPos p) fg of
          T.R (T.Tuple2 fx fg') kx -> (kx,fx,fg')
          _ -> T.fatal p
      sx =
        T.uconstDef p a129v13v129v13x
          (\ _ -> case j129v12v129v17x of (kx,fx,fg') -> fx)
      sg' =
        T.uconstDef p a129v15v129v16g'
          (\ _ -> case j129v12v129v17x of (kx,fx,fg') -> fg') in
      (T.uwrapForward p
        (hf
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !- p) fn
            (T.uap1 T.mkNoSrcPos p
              (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
              (T.conInteger T.mkNoSrcPos p 1)))
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !+ p)
            (T.uap1 T.mkNoSrcPos p (gfromInt T.mkNoSrcPos p)
              (gx T.mkNoSrcPos p))
            (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !* p) facc
              (gb T.mkNoSrcPos p))) (gg' T.mkNoSrcPos p) p))
  
hrandomIvalInteger _ _ p = T.fatal p

grandomIvalDouble ::
  (RandomGen g,Fractional a) =>
  T.RefSrcPos ->
    T.RefExp ->
      T.R
        (T.Fun (T.Tuple2 Double Double)
          (T.Fun (T.Fun Double a) (T.Fun g (T.Tuple2 a g))))

hrandomIvalDouble ::
  (RandomGen g,Fractional a) =>
  (T.R (T.Tuple2 Double Double)) ->
    (T.R (T.Fun Double a)) -> (T.R g) -> T.RefExp -> T.R (T.Tuple2 a g)

grandomIvalDouble prandomIvalDouble p =
  T.ufun3 arandomIvalDouble prandomIvalDouble p hrandomIvalDouble

hrandomIvalDouble (T.R (T.Tuple2 fl fh) _) ffromDouble frng p =
  T.ucguard (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !> p) fl fh)
    (T.uwrapForward p
      (hrandomIvalDouble (T.con2 T.mkNoSrcPos p T.Tuple2 T.aTuple2 fh fl)
        ffromDouble frng p))
    (T.ucguard (gotherwise T.mkNoSrcPos p)
      (T.uccase T.mkNoSrcPos p
        (let
          v137v8v145v28v1 (T.R (T.Tuple2 fx frng') _) p =
            let
              gscaled_x pscaled_x p = T.uconstUse pscaled_x p sscaled_x
              sscaled_x =
                T.uconstDef p a140v14v143v36scaled_x
                  (\ p ->
                    T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !+ p)
                      (T.uap1 T.mkNoSrcPos p ffromDouble
                        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !/ p)
                          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !+ p) fl fh)
                          (T.uap1 T.mkNoSrcPos p
                            (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                            (T.conInteger T.mkNoSrcPos p 2))))
                      (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !* p)
                        (T.uap1 T.mkNoSrcPos p ffromDouble
                          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !/ p)
                            (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !- p) fh fl)
                            (T.uap1 T.mkNoSrcPos p (grealToFrac T.mkNoSrcPos p)
                              (gintRange T.mkNoSrcPos p))))
                        (T.uap1 T.mkNoSrcPos p (gfromIntegral T.mkNoSrcPos p)
                          (fx :: T.R Int)))) in
              (T.con2 T.mkNoSrcPos p T.Tuple2 T.aTuple2
                (gscaled_x T.mkNoSrcPos p) frng')
          v137v8v145v28v1 _ p = T.fatal p in (v137v8v145v28v1))
        (T.uwrapForward p
          (hrandomIvalInteger
            (T.con2 T.mkNoSrcPos p T.Tuple2 T.aTuple2
              (T.uap1 T.mkNoSrcPos p (gtoInteger T.mkNoSrcPos p)
                (gminBound T.mkNoSrcPos p :: T.R Int))
              (T.uap1 T.mkNoSrcPos p (gtoInteger T.mkNoSrcPos p)
                (gmaxBound T.mkNoSrcPos p :: T.R Int))) frng p))) (T.fatal p))
hrandomIvalDouble _ _ _ p = T.fatal p

gfromInt :: Num a => T.RefSrcPos -> T.RefExp -> T.R (T.Fun Int a)

sfromInt :: Num a => T.R (T.Fun Int a)

gfromInt pfromInt p = T.uconstUse pfromInt p sfromInt

sfromInt =
  T.uconstDef T.mkRoot afromInt
    (\ p ->
      T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !. p) (gfromInteger T.mkNoSrcPos p)
        (gtoInteger T.mkNoSrcPos p))

gintRange :: T.RefSrcPos -> T.RefExp -> T.R Integer

sintRange :: T.R Integer

gintRange pintRange p = T.uconstUse pintRange p sintRange

sintRange =
  T.uconstDef T.mkRoot aintRange
    (\ p ->
      T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !- p)
        (T.uap1 T.mkNoSrcPos p (gtoInteger T.mkNoSrcPos p)
          (gmaxBound T.mkNoSrcPos p :: T.R Int))
        (T.uap1 T.mkNoSrcPos p (gtoInteger T.mkNoSrcPos p)
          (gminBound T.mkNoSrcPos p :: T.R Int)))

giLogBase ::
  T.RefSrcPos -> T.RefExp -> T.R (T.Fun Integer (T.Fun Integer Integer))

hiLogBase :: (T.R Integer) -> (T.R Integer) -> T.RefExp -> T.R Integer

giLogBase piLogBase p = T.ufun2 aiLogBase piLogBase p hiLogBase

hiLogBase fb fi p =
  T.ucif p (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !< p) fi fb)
    (T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
      (T.conInteger T.mkNoSrcPos p 1))
    (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !+ p)
      (T.uap1 T.mkNoSrcPos p (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
        (T.conInteger T.mkNoSrcPos p 1))
      (T.uwrapForward p
        (hiLogBase fb (T.uap2 T.mkNoSrcPos p (gdiv T.mkNoSrcPos p) fi fb) p)))

gnewStdGen :: T.RefSrcPos -> T.RefExp -> T.R (IO StdGen)

gnewStdGen pnewStdGen p = T.uconstUse pnewStdGen p snewStdGen

snewStdGen =
  T.uconstDef T.mkRoot anewStdGen
    (\ p -> (T.fromIO fromStdGen) p Random.newStdGen)

gsetStdGen :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun StdGen (IO T.Tuple0))

gsetStdGen psetStdGen p = T.ufun1 asetStdGen psetStdGen p hsetStdGen

hsetStdGen z1setStdGen ksetStdGen =
  (T.fromIO T.fromTuple0) ksetStdGen
    (Random.setStdGen (toStdGen ksetStdGen z1setStdGen))

ggetStdGen :: T.RefSrcPos -> T.RefExp -> T.R (IO StdGen)

ggetStdGen pgetStdGen p = T.uconstUse pgetStdGen p sgetStdGen

sgetStdGen =
  T.uconstDef T.mkRoot agetStdGen
    (\ p -> (T.fromIO fromStdGen) p Random.getStdGen)

ggetStdRandom ::
  T.RefSrcPos ->
    T.RefExp -> T.R (T.Fun (T.Fun StdGen (T.Tuple2 a StdGen)) (IO a))

ggetStdRandom pgetStdRandom p =
  T.ufun1 agetStdRandom pgetStdRandom p hgetStdRandom

hgetStdRandom z1getStdRandom kgetStdRandom =
  (T.fromIO T.fromId) kgetStdRandom
    (Random.getStdRandom
      ((toFun fromStdGen (T.toTuple2 T.toId toStdGen)) kgetStdRandom
        z1getStdRandom))

tRandom = T.mkModule "Random" "Random.hs" Prelude.False

aprimStdGenGenRange =
  T.mkVariable tRandom 410001 420041 3 1 "primStdGenGenRange" Prelude.False

aprimStdGenNext =
  T.mkVariable tRandom 430001 440040 3 1 "primStdGenNext" Prelude.False

aprimStdGenSplit =
  T.mkVariable tRandom 450001 460044 3 1 "primStdGenSplit" Prelude.False

aprimStdGenReadsPrec =
  T.mkVariable tRandom 470001 480042 3 2 "primStdGenReadsPrec" Prelude.False

aprimStdGenShowsPrec =
  T.mkVariable tRandom 490001 500057 3 3 "primStdGenShowsPrec" Prelude.False

amkStdGen = T.mkVariable tRandom 530001 540026 3 1 "mkStdGen" Prelude.False

arandomIvalInteger =
  T.mkVariable tRandom 1180001 1310042 3 2 "randomIvalInteger" Prelude.False

arandomIvalDouble =
  T.mkVariable tRandom 1340001 1450028 3 3 "randomIvalDouble" Prelude.False

afromInt = T.mkVariable tRandom 1480001 1480033 3 0 "fromInt" Prelude.False

aintRange = T.mkVariable tRandom 1510001 1510064 3 0 "intRange" Prelude.False

aiLogBase = T.mkVariable tRandom 1540001 1540061 3 2 "iLogBase" Prelude.False

anewStdGen = T.mkVariable tRandom 1580001 1590019 3 0 "newStdGen" Prelude.False

asetStdGen = T.mkVariable tRandom 1600001 1610029 3 1 "setStdGen" Prelude.False

agetStdGen = T.mkVariable tRandom 1620001 1630019 3 0 "getStdGen" Prelude.False

agetStdRandom =
  T.mkVariable tRandom 1640001 1650046 3 1 "getStdRandom" Prelude.False

a24v3v24v34genRange =
  T.mkVariable tRandom 240003 240034 3 1 "genRange" Prelude.False

a31v3v31v31genRange =
  T.mkVariable tRandom 310003 310031 3 0 "genRange" Prelude.False

a32v3v32v23next = T.mkVariable tRandom 320003 320023 3 0 "next" Prelude.False

a33v3v33v25split = T.mkVariable tRandom 330003 330025 3 0 "split" Prelude.False

a36v3v36v33readsPrec =
  T.mkVariable tRandom 360003 360033 3 0 "readsPrec" Prelude.False

a39v3v39v33showsPrec =
  T.mkVariable tRandom 390003 390033 3 0 "showsPrec" Prelude.False

a68v3v70v21randoms =
  T.mkVariable tRandom 680003 700021 3 1 "randoms" Prelude.False

a71v3v73v28randomRs =
  T.mkVariable tRandom 710003 730028 3 2 "randomRs" Prelude.False

a74v3v74v39randomIO =
  T.mkVariable tRandom 740003 740039 3 0 "randomIO" Prelude.False

a75v3v75v47randomRIO =
  T.mkVariable tRandom 750003 750047 3 1 "randomRIO" Prelude.False

a78v3v78v66randomR =
  T.mkVariable tRandom 780003 780066 3 2 "randomR" Prelude.False

a79v3v79v49random =
  T.mkVariable tRandom 790003 790049 3 1 "random" Prelude.False

a82v3v82v43randomR =
  T.mkVariable tRandom 820003 820043 3 2 "randomR" Prelude.False

a83v3v83v83random =
  T.mkVariable tRandom 830003 830083 3 1 "random" Prelude.False

a86v3v86v63random =
  T.mkVariable tRandom 860003 860063 3 1 "random" Prelude.False

a87v3v87v78randomR =
  T.mkVariable tRandom 870003 870078 3 2 "randomR" Prelude.False

a90v3v90v45randomR =
  T.mkVariable tRandom 900003 900045 3 2 "randomR" Prelude.False

a91v3v91v42random =
  T.mkVariable tRandom 910003 910042 3 1 "random" Prelude.False

a94v3v105v22randomR =
  T.mkVariable tRandom 940003 1050022 3 2 "randomR" Prelude.False

a107v3v107v42random =
  T.mkVariable tRandom 1070003 1070042 3 1 "random" Prelude.False

a110v3v112v27randomR =
  T.mkVariable tRandom 1100003 1120027 3 2 "randomR" Prelude.False

a113v3v113v42random =
  T.mkVariable tRandom 1130003 1130042 3 1 "random" Prelude.False

a70v6v70v6x = T.mkVariable tRandom 700006 700006 3 0 "x" Prelude.True

a70v8v70v9g' = T.mkVariable tRandom 700008 700009 3 0 "g'" Prelude.True

a73v6v73v6x = T.mkVariable tRandom 730006 730006 3 0 "x" Prelude.True

a73v8v73v9g' = T.mkVariable tRandom 730008 730009 3 0 "g'" Prelude.True

a100v5v101v26bool2Integer =
  T.mkVariable tRandom 1000005 1010026 3 1 "bool2Integer" Prelude.True

a104v5v105v22int2Bool =
  T.mkVariable tRandom 1040005 1050022 3 1 "int2Bool" Prelude.True

a122v8v122v20k = T.mkVariable tRandom 1220008 1220020 3 0 "k" Prelude.True

a123v8v123v21b = T.mkVariable tRandom 1230008 1230021 3 0 "b" Prelude.True

a124v8v124v23n = T.mkVariable tRandom 1240008 1240023 3 0 "n" Prelude.True

a126v8v131v42f = T.mkVariable tRandom 1260008 1310042 3 3 "f" Prelude.True

a129v13v129v13x = T.mkVariable tRandom 1290013 1290013 3 0 "x" Prelude.True

a129v15v129v16g' = T.mkVariable tRandom 1290015 1290016 3 0 "g'" Prelude.True

a140v14v143v36scaled_x =
  T.mkVariable tRandom 1400014 1430036 3 0 "scaled_x" Prelude.True
