module Hat.Monad
  (MonadPlus(gmzero,gmplus,smzero,smplus),gjoin,ajoin,hjoin,gguard,aguard,hguard
    ,gwhen,awhen,hwhen,gunless,aunless,hunless,gap,gmsum,amsum,hmsum,gfilterM
    ,afilterM,hfilterM,gmapAndUnzipM,amapAndUnzipM,hmapAndUnzipM,gzipWithM
    ,azipWithM,hzipWithM,gzipWithM_,azipWithM_,hzipWithM_,gfoldM,afoldM,hfoldM
    ,gliftM,aliftM,hliftM,gliftM2,aliftM2,hliftM2,gliftM3,aliftM3,hliftM3
    ,gliftM4,aliftM4,hliftM4,gliftM5,aliftM5,hliftM5,Monad((!>>=),(!>>),greturn
      ,gfail,(|>>=),(|>>),sreturn,sfail),Functor(gfmap,sfmap),gmapM,amapM,hmapM
    ,gmapM_,amapM_,hmapM_,gsequence,gsequence_,(!=<<),(+=<<),(*=<<)) where

import qualified Hat.PreludeBasic 
import qualified Prelude 
import Hat.Hack 
import qualified Hat.Hat as T 
import Hat.Hat  (WrapVal(wrapVal))
import Hat.Prelude 

class Monad m => MonadPlus m
  where
  
  gmzero :: T.RefSrcPos -> T.RefExp -> T.R (m a)
  
  gmplus :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun (m a) (T.Fun (m a) (m a)))
  
  smzero :: T.R (m a)
  
  smplus :: T.R (T.Fun (m a) (T.Fun (m a) (m a)))
  

instance MonadPlus (Maybe)
  where
  
  gmzero pmzero p = T.uconstUse pmzero p smzero
  
  smzero =
    T.uconstDef T.mkRoot a25v5v25v35mzero
      (\ p -> T.con0 T.mkNoSrcPos p Nothing aNothing)
  
  gmplus pmplus p =
    T.ufun2 a27v14v28v18mplus pmplus p hmplus
    where
    
    hmplus (T.R Nothing _) fys p = T.projection T.mkNoSrcPos p fys
    hmplus fxs fys p = T.projection T.mkNoSrcPos p fxs
    
  

instance MonadPlus (T.List)
  where
  
  gmzero pmzero p = T.uconstUse pmzero p smzero
  
  smzero =
    T.uconstDef T.mkRoot a31v5v31v15mzero
      (\ p -> T.con0 T.mkNoSrcPos p T.List T.aList)
  
  gmplus pmplus p = T.uconstUse pmplus p smplus
  
  smplus = T.uconstDef T.mkRoot a32v5v32v15mplus (\ p -> (T.mkNoSrcPos !++ p))
  

gmsum ::
  MonadPlus m => T.RefSrcPos -> T.RefExp -> T.R (T.Fun (T.List (m a)) (m a))

hmsum :: MonadPlus m => (T.R (T.List (m a))) -> T.RefExp -> T.R (m a)

gmsum pmsum p = T.ufun1 amsum pmsum p hmsum

hmsum fxs p =
  T.uwrapForward p
    (hfoldr (gmplus T.mkNoSrcPos p) (gmzero T.mkNoSrcPos p) fxs p)

gjoin :: Monad m => T.RefSrcPos -> T.RefExp -> T.R (T.Fun (m (m a)) (m a))

hjoin :: Monad m => (T.R (m (m a))) -> T.RefExp -> T.R (m a)

gjoin pjoin p = T.ufun1 ajoin pjoin p hjoin

hjoin fx p = T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !>>= p) fx (gid T.mkNoSrcPos p)

gwhen ::
  Monad m =>
  T.RefSrcPos -> T.RefExp -> T.R (T.Fun Bool (T.Fun (m T.Tuple0) (m T.Tuple0)))

hwhen ::
  Monad m => (T.R Bool) -> (T.R (m T.Tuple0)) -> T.RefExp -> T.R (m T.Tuple0)

gwhen pwhen p = T.ufun2 awhen pwhen p hwhen

hwhen fp fs p =
  T.ucif p fp (T.projection T.mkNoSrcPos p fs)
    (T.uap1 T.mkNoSrcPos p (greturn T.mkNoSrcPos p)
      (T.con0 T.mkNoSrcPos p T.Tuple0 T.aTuple0))

gunless ::
  Monad m =>
  T.RefSrcPos -> T.RefExp -> T.R (T.Fun Bool (T.Fun (m T.Tuple0) (m T.Tuple0)))

hunless ::
  Monad m => (T.R Bool) -> (T.R (m T.Tuple0)) -> T.RefExp -> T.R (m T.Tuple0)

gunless punless p = T.ufun2 aunless punless p hunless

hunless fp fs p = T.uwrapForward p (hwhen (T.uwrapForward p (hnot fp p)) fs p)

gap ::
  Monad m =>
  T.RefSrcPos -> T.RefExp -> T.R (T.Fun (m (T.Fun a b)) (T.Fun (m a) (m b)))

sap :: Monad m => T.R (T.Fun (m (T.Fun a b)) (T.Fun (m a) (m b)))

gap pap p = T.uconstUse pap p sap

sap =
  T.uconstDef T.mkRoot aap
    (\ p -> T.uwrapForward p (hliftM2 (T.mkNoSrcPos !$ p) p))

gguard ::
  MonadPlus m => T.RefSrcPos -> T.RefExp -> T.R (T.Fun Bool (m T.Tuple0))

hguard :: MonadPlus m => (T.R Bool) -> T.RefExp -> T.R (m T.Tuple0)

gguard pguard p = T.ufun1 aguard pguard p hguard

hguard fp p =
  T.ucif p fp
    (T.uap1 T.mkNoSrcPos p (greturn T.mkNoSrcPos p)
      (T.con0 T.mkNoSrcPos p T.Tuple0 T.aTuple0)) (gmzero T.mkNoSrcPos p)

gmapAndUnzipM ::
  Monad m =>
  T.RefSrcPos ->
    T.RefExp ->
      T.R
        (T.Fun (T.Fun a (m (T.Tuple2 b c)))
          (T.Fun (T.List a) (m (T.Tuple2 (T.List b) (T.List c)))))

hmapAndUnzipM ::
  Monad m =>
  (T.R (T.Fun a (m (T.Tuple2 b c)))) ->
    (T.R (T.List a)) -> T.RefExp -> T.R (m (T.Tuple2 (T.List b) (T.List c)))

gmapAndUnzipM pmapAndUnzipM p =
  T.ufun2 amapAndUnzipM pmapAndUnzipM p hmapAndUnzipM

hmapAndUnzipM ff fxs p =
  T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !>>= p)
    (T.uap1 T.mkNoSrcPos p (gsequence T.mkNoSrcPos p)
      (T.uwrapForward p (hmap ff fxs p)))
    (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !. p) (greturn T.mkNoSrcPos p)
      (gunzip T.mkNoSrcPos p))

gzipWithM ::
  Monad m =>
  T.RefSrcPos ->
    T.RefExp ->
      T.R
        (T.Fun (T.Fun a (T.Fun b (m c)))
          (T.Fun (T.List a) (T.Fun (T.List b) (m (T.List c)))))

hzipWithM ::
  Monad m =>
  (T.R (T.Fun a (T.Fun b (m c)))) ->
    (T.R (T.List a)) -> (T.R (T.List b)) -> T.RefExp -> T.R (m (T.List c))

gzipWithM pzipWithM p = T.ufun3 azipWithM pzipWithM p hzipWithM

hzipWithM ff fxs fys p =
  T.uap1 T.mkNoSrcPos p (gsequence T.mkNoSrcPos p)
    (T.uwrapForward p (hzipWith ff fxs fys p))

gzipWithM_ ::
  Monad m =>
  T.RefSrcPos ->
    T.RefExp ->
      T.R
        (T.Fun (T.Fun a (T.Fun b (m c)))
          (T.Fun (T.List a) (T.Fun (T.List b) (m T.Tuple0))))

hzipWithM_ ::
  Monad m =>
  (T.R (T.Fun a (T.Fun b (m c)))) ->
    (T.R (T.List a)) -> (T.R (T.List b)) -> T.RefExp -> T.R (m T.Tuple0)

gzipWithM_ pzipWithM_ p = T.ufun3 azipWithM_ pzipWithM_ p hzipWithM_

hzipWithM_ ff fxs fys p =
  T.uap1 T.mkNoSrcPos p (gsequence_ T.mkNoSrcPos p)
    (T.uwrapForward p (hzipWith ff fxs fys p))

gfoldM ::
  Monad m =>
  T.RefSrcPos ->
    T.RefExp ->
      T.R (T.Fun (T.Fun a (T.Fun b (m a))) (T.Fun a (T.Fun (T.List b) (m a))))

hfoldM ::
  Monad m =>
  (T.R (T.Fun a (T.Fun b (m a)))) ->
    (T.R a) -> (T.R (T.List b)) -> T.RefExp -> T.R (m a)

gfoldM pfoldM p = T.ufun3 afoldM pfoldM p hfoldM

hfoldM ff fa (T.R T.List _) p =
  T.uap1 T.mkNoSrcPos p (greturn T.mkNoSrcPos p) fa
hfoldM ff fa (T.R (T.Cons fx fxs) _) p =
  T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos !>>= p) (T.uap2 T.mkNoSrcPos p ff fa fx)
    (T.ufun1 T.mkLambda T.mkNoSrcPos p
      (\ fy p -> T.uwrapForward p (hfoldM ff fy fxs p)))
hfoldM _ _ _ p = T.fatal p

gfilterM ::
  Monad m =>
  T.RefSrcPos ->
    T.RefExp -> T.R (T.Fun (T.Fun a (m Bool)) (T.Fun (T.List a) (m (T.List a))))

hfilterM ::
  Monad m =>
  (T.R (T.Fun a (m Bool))) -> (T.R (T.List a)) -> T.RefExp -> T.R (m (T.List a))

gfilterM pfilterM p = T.ufun2 afilterM pfilterM p hfilterM

hfilterM fp (T.R T.List _) p =
  T.uap1 T.mkNoSrcPos p (greturn T.mkNoSrcPos p)
    (T.con0 T.mkNoSrcPos p T.List T.aList)
hfilterM fp (T.R (T.Cons fx fxs) _) p =
  T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!>>= p)
    (T.uap1 T.mkNoSrcPos p fp fx)
    (T.ufun1 T.mkDoLambda T.mkNoSrcPos p
      (\ fb p ->
        T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!>>= p)
          (T.uwrapForward p (hfilterM fp fxs p))
          (T.ufun1 T.mkDoLambda T.mkNoSrcPos p
            (\ fys p ->
              T.uap1 T.mkNoSrcPos p (greturn T.mkNoSrcPos p)
                (T.ucif p fb (T.con2 T.mkNoSrcPos p T.Cons T.aCons fx fys)
                  (T.projection T.mkNoSrcPos p fys))))))
hfilterM _ _ p = T.fatal p

gliftM ::
  Monad m =>
  T.RefSrcPos -> T.RefExp -> T.R (T.Fun (T.Fun a b) (T.Fun (m a) (m b)))

hliftM :: Monad m => (T.R (T.Fun a b)) -> T.RefExp -> T.R (T.Fun (m a) (m b))

gliftM pliftM p = T.ufun1 aliftM pliftM p hliftM

hliftM ff p =
  T.ufun1 T.mkLambda T.mkNoSrcPos p
    (\ fa p ->
      T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!>>= p) fa
        (T.ufun1 T.mkDoLambda T.mkNoSrcPos p
          (\ fa' p ->
            T.uap1 T.mkNoSrcPos p (greturn T.mkNoSrcPos p)
              (T.uap1 T.mkNoSrcPos p ff fa'))))

gliftM2 ::
  Monad m =>
  T.RefSrcPos ->
    T.RefExp ->
      T.R (T.Fun (T.Fun a (T.Fun b c)) (T.Fun (m a) (T.Fun (m b) (m c))))

hliftM2 ::
  Monad m =>
  (T.R (T.Fun a (T.Fun b c))) ->
    T.RefExp -> T.R (T.Fun (m a) (T.Fun (m b) (m c)))

gliftM2 pliftM2 p = T.ufun1 aliftM2 pliftM2 p hliftM2

hliftM2 ff p =
  T.ufun2 T.mkLambda T.mkNoSrcPos p
    (\ fa fb p ->
      T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!>>= p) fa
        (T.ufun1 T.mkDoLambda T.mkNoSrcPos p
          (\ fa' p ->
            T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!>>= p) fb
              (T.ufun1 T.mkDoLambda T.mkNoSrcPos p
                (\ fb' p ->
                  T.uap1 T.mkNoSrcPos p (greturn T.mkNoSrcPos p)
                    (T.uap2 T.mkNoSrcPos p ff fa' fb'))))))

gliftM3 ::
  Monad m =>
  T.RefSrcPos ->
    T.RefExp ->
      T.R
        (T.Fun (T.Fun a (T.Fun b (T.Fun c d)))
          (T.Fun (m a) (T.Fun (m b) (T.Fun (m c) (m d)))))

hliftM3 ::
  Monad m =>
  (T.R (T.Fun a (T.Fun b (T.Fun c d)))) ->
    T.RefExp -> T.R (T.Fun (m a) (T.Fun (m b) (T.Fun (m c) (m d))))

gliftM3 pliftM3 p = T.ufun1 aliftM3 pliftM3 p hliftM3

hliftM3 ff p =
  T.ufun3 T.mkLambda T.mkNoSrcPos p
    (\ fa fb fc p ->
      T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!>>= p) fa
        (T.ufun1 T.mkDoLambda T.mkNoSrcPos p
          (\ fa' p ->
            T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!>>= p) fb
              (T.ufun1 T.mkDoLambda T.mkNoSrcPos p
                (\ fb' p ->
                  T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!>>= p) fc
                    (T.ufun1 T.mkDoLambda T.mkNoSrcPos p
                      (\ fc' p ->
                        T.uap1 T.mkNoSrcPos p (greturn T.mkNoSrcPos p)
                          (T.uap3 T.mkNoSrcPos p ff fa' fb' fc'))))))))

gliftM4 ::
  Monad m =>
  T.RefSrcPos ->
    T.RefExp ->
      T.R
        (T.Fun (T.Fun a (T.Fun b (T.Fun c (T.Fun d e))))
          (T.Fun (m a) (T.Fun (m b) (T.Fun (m c) (T.Fun (m d) (m e))))))

hliftM4 ::
  Monad m =>
  (T.R (T.Fun a (T.Fun b (T.Fun c (T.Fun d e))))) ->
    T.RefExp ->
      T.R (T.Fun (m a) (T.Fun (m b) (T.Fun (m c) (T.Fun (m d) (m e)))))

gliftM4 pliftM4 p = T.ufun1 aliftM4 pliftM4 p hliftM4

hliftM4 ff p =
  T.ufun4 T.mkLambda T.mkNoSrcPos p
    (\ fa fb fc fd p ->
      T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!>>= p) fa
        (T.ufun1 T.mkDoLambda T.mkNoSrcPos p
          (\ fa' p ->
            T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!>>= p) fb
              (T.ufun1 T.mkDoLambda T.mkNoSrcPos p
                (\ fb' p ->
                  T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!>>= p) fc
                    (T.ufun1 T.mkDoLambda T.mkNoSrcPos p
                      (\ fc' p ->
                        T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!>>= p)
                          fd
                          (T.ufun1 T.mkDoLambda T.mkNoSrcPos p
                            (\ fd' p ->
                              T.uap1 T.mkNoSrcPos p (greturn T.mkNoSrcPos p)
                                (T.uap4 T.mkNoSrcPos p ff fa' fb' fc'
                                  fd'))))))))))

gliftM5 ::
  Monad m =>
  T.RefSrcPos ->
    T.RefExp ->
      T.R
        (T.Fun (T.Fun a (T.Fun b (T.Fun c (T.Fun d (T.Fun e f)))))
          (T.Fun (m a)
            (T.Fun (m b) (T.Fun (m c) (T.Fun (m d) (T.Fun (m e) (m f)))))))

hliftM5 ::
  Monad m =>
  (T.R (T.Fun a (T.Fun b (T.Fun c (T.Fun d (T.Fun e f)))))) ->
    T.RefExp ->
      T.R
        (T.Fun (m a)
          (T.Fun (m b) (T.Fun (m c) (T.Fun (m d) (T.Fun (m e) (m f))))))

gliftM5 pliftM5 p = T.ufun1 aliftM5 pliftM5 p hliftM5

hliftM5 ff p =
  T.ufun5 T.mkLambda T.mkNoSrcPos p
    (\ fa fb fc fd fe p ->
      T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!>>= p) fa
        (T.ufun1 T.mkDoLambda T.mkNoSrcPos p
          (\ fa' p ->
            T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!>>= p) fb
              (T.ufun1 T.mkDoLambda T.mkNoSrcPos p
                (\ fb' p ->
                  T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!>>= p) fc
                    (T.ufun1 T.mkDoLambda T.mkNoSrcPos p
                      (\ fc' p ->
                        T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos Hat.Prelude.!>>= p)
                          fd
                          (T.ufun1 T.mkDoLambda T.mkNoSrcPos p
                            (\ fd' p ->
                              T.uap2 T.mkNoSrcPos p
                                (T.mkNoSrcPos Hat.Prelude.!>>= p) fe
                                (T.ufun1 T.mkDoLambda T.mkNoSrcPos p
                                  (\ fe' p ->
                                    T.uap1 T.mkNoSrcPos p
                                      (greturn T.mkNoSrcPos p)
                                      (T.uap5 T.mkNoSrcPos p ff fa' fb' fc' fd'
                                        fe'))))))))))))

tMonad = T.mkModule "Monad" "Monad.hs" Prelude.False

amsum = T.mkVariable tMonad 390001 390032 3 1 "msum" Prelude.False

ajoin = T.mkVariable tMonad 420001 420028 3 1 "join" Prelude.False

awhen = T.mkVariable tMonad 450001 450047 3 2 "when" Prelude.False

aunless = T.mkVariable tMonad 480001 480034 3 2 "unless" Prelude.False

aap = T.mkVariable tMonad 510001 510029 3 0 "ap" Prelude.False

aguard = T.mkVariable tMonad 540001 540051 3 1 "guard" Prelude.False

amapAndUnzipM =
  T.mkVariable tMonad 570001 570058 3 2 "mapAndUnzipM" Prelude.False

azipWithM = T.mkVariable tMonad 600001 600045 3 3 "zipWithM" Prelude.False

azipWithM_ = T.mkVariable tMonad 630001 630047 3 3 "zipWithM_" Prelude.False

afoldM = T.mkVariable tMonad 660001 670049 3 3 "foldM" Prelude.False

afilterM = T.mkVariable tMonad 700001 730032 3 2 "filterM" Prelude.False

aliftM = T.mkVariable tMonad 770001 770052 3 1 "liftM" Prelude.False

aliftM2 = T.mkVariable tMonad 800001 800066 3 1 "liftM2" Prelude.False

aliftM3 = T.mkVariable tMonad 840001 850021 3 1 "liftM3" Prelude.False

aliftM4 = T.mkVariable tMonad 890001 900026 3 1 "liftM4" Prelude.False

aliftM5 = T.mkVariable tMonad 940001 950040 3 1 "liftM5" Prelude.False

a25v5v25v35mzero = T.mkVariable tMonad 250005 250035 3 0 "mzero" Prelude.False

a27v14v28v18mplus = T.mkVariable tMonad 270014 280018 3 2 "mplus" Prelude.False

a31v5v31v15mzero = T.mkVariable tMonad 310005 310015 3 0 "mzero" Prelude.False

a32v5v32v15mplus = T.mkVariable tMonad 320005 320015 3 0 "mplus" Prelude.False
