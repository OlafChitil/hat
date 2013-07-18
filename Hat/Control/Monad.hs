module Hat.Control.Monad
       (Functor(gfmap, sfmap),
        Monad((!>>=), (!>>), greturn, gfail, (|>>=), (|>>),
              sreturn, sfail),
        MonadPlus(gmzero, gmplus, smzero, smplus), gmapM,
        amapM, hmapM, gmapM_, amapM_, hmapM_, gforM, gforM_,
        gsequence, gsequence_, (!=<<), (+=<<), (*=<<),
        (!>=>), (+>=>), (*>=>), (!<=<), gforever, aforever,
        hforever, gvoid, gjoin, ajoin, hjoin, gmsum, amsum,
        hmsum, gfilterM, afilterM, hfilterM, gmapAndUnzipM,
        amapAndUnzipM, hmapAndUnzipM, gzipWithM, azipWithM,
        hzipWithM, gzipWithM_, azipWithM_, hzipWithM_,
        gfoldM, afoldM, hfoldM, gfoldM_, afoldM_, hfoldM_,
        greplicateM, areplicateM, hreplicateM, greplicateM_,
        areplicateM_, hreplicateM_, gguard, aguard, hguard,
        gwhen, awhen, hwhen, gunless, aunless, hunless,
        gliftM, aliftM, hliftM, gliftM2, aliftM2, hliftM2,
        gliftM3, aliftM3, hliftM3, gliftM4, aliftM4, hliftM4,
        gliftM5, aliftM5, hliftM5, gap)
       where
import qualified Prelude
import qualified Hat.Hat as T
import qualified Hat.PreludeBasic
import qualified Hat.PreludeBuiltinTypes
import Hat.Prelude
import Hat.Monad
 
gforM ::
        (Monad m) =>
        T.RefSrcPos ->
          T.RefExp ->
            T.R
              (T.Fun (T.List a)
                 (T.Fun (T.Fun a (m b)) (m (T.List b))))
 
sforM ::
        (Monad m) =>
        T.R
          (T.Fun (T.List a)
             (T.Fun (T.Fun a (m b)) (m (T.List b))))
gforM pforM p = T.uconstUse pforM p sforM
sforM
  = T.uconstDef p aforM
      (\ p ->
         T.uap1 T.mkNoSrcPos p (gflip T.mkNoSrcPos p)
           (gmapM T.mkNoSrcPos p))
 
gforM_ ::
         (Monad m) =>
         T.RefSrcPos ->
           T.RefExp ->
             T.R
               (T.Fun (T.List a)
                  (T.Fun (T.Fun a (m b)) (m T.Tuple0)))
 
sforM_ ::
         (Monad m) =>
         T.R
           (T.Fun (T.List a)
              (T.Fun (T.Fun a (m b)) (m T.Tuple0)))
gforM_ pforM_ p = T.uconstUse pforM_ p sforM_
sforM_
  = T.uconstDef p aforM_
      (\ p ->
         T.uap1 T.mkNoSrcPos p (gflip T.mkNoSrcPos p)
           (gmapM_ T.mkNoSrcPos p))
 
(!>=>) ::
         (Monad m) =>
         T.RefSrcPos ->
           T.RefExp ->
             T.R
               (T.Fun (T.Fun a (m b))
                  (T.Fun (T.Fun b (m c)) (T.Fun a (m c))))
 
(*>=>) ::
         (Monad m) =>
         T.R (T.Fun a (m b)) ->
           T.R (T.Fun b (m c)) ->
             T.RefExp -> T.R (T.Fun a (m c))
(%>=>) !>=> p = T.ufun2 (+>=>) (%>=>) p (*>=>)
(ff *>=> fg) p
  = T.ufun1 T.mkLambda T.mkNoSrcPos p
      (\ fx p ->
         T.uap2 T.mkNoSrcPos p ((!>>=) T.mkNoSrcPos p)
           (T.uap1 T.mkNoSrcPos p ff fx)
           fg)
 
(!<=<) ::
         (Monad m) =>
         T.RefSrcPos ->
           T.RefExp ->
             T.R
               (T.Fun (T.Fun b (m c))
                  (T.Fun (T.Fun a (m b)) (T.Fun a (m c))))
 
(|<=<) ::
         (Monad m) =>
         T.R
           (T.Fun (T.Fun b (m c))
              (T.Fun (T.Fun a (m b)) (T.Fun a (m c))))
(%<=<) !<=< p = T.uconstUse (%<=<) p (|<=<)
(|<=<)
  = T.uconstDef p (+<=<)
      (\ p ->
         T.uap1 T.mkNoSrcPos p (gflip T.mkNoSrcPos p)
           ((!>=>) T.mkNoSrcPos p))
 
gforever ::
           (Monad m) =>
           T.RefSrcPos -> T.RefExp -> T.R (T.Fun (m a) (m b))
 
hforever ::
           (Monad m) => T.R (m a) -> T.RefExp -> T.R (m b)
gforever pforever p
  = T.ufun1 aforever pforever p hforever
hforever fa p
  = let ga' pa' p = T.uconstUse pa' p sa'
        sa'
          = T.uconstDef p c31v19v31v30a'
              (\ p ->
                 T.uap2 T.mkNoSrcPos p ((!>>) T.mkNoSrcPos p) fa
                   (ga' T.mkNoSrcPos p))
      in ga' T.mkNoSrcPos p
 
gvoid ::
        (Functor f) =>
        T.RefSrcPos ->
          T.RefExp -> T.R (T.Fun (f a) (f T.Tuple0))
 
svoid ::
        (Functor f) => T.R (T.Fun (f a) (f T.Tuple0))
gvoid pvoid p = T.uconstUse pvoid p svoid
svoid
  = T.uconstDef p avoid
      (\ p ->
         T.uap1 T.mkNoSrcPos p (gfmap T.mkNoSrcPos p)
           (T.uap1 T.mkNoSrcPos p (gconst T.mkNoSrcPos p)
              (T.con0 T.mkNoSrcPos p T.Tuple0 T.aTuple0)))
 
gfoldM_ ::
          (Monad m) =>
          T.RefSrcPos ->
            T.RefExp ->
              T.R
                (T.Fun (T.Fun a (T.Fun b (m a)))
                   (T.Fun a (T.Fun (T.List b) (m T.Tuple0))))
 
hfoldM_ ::
          (Monad m) =>
          T.R (T.Fun a (T.Fun b (m a))) ->
            T.R a ->
              T.R (T.List b) -> T.RefExp -> T.R (m T.Tuple0)
gfoldM_ pfoldM_ p = T.ufun3 afoldM_ pfoldM_ p hfoldM_
hfoldM_ ff fa fxs p
  = T.uap2 T.mkNoSrcPos p ((!>>) T.mkNoSrcPos p)
      (T.uwrapForward p (hfoldM ff fa fxs p))
      (T.uap1 T.mkNoSrcPos p (greturn T.mkNoSrcPos p)
         (T.con0 T.mkNoSrcPos p T.Tuple0 T.aTuple0))
 
greplicateM ::
              (Monad m) =>
              T.RefSrcPos ->
                T.RefExp ->
                  T.R (T.Fun Int (T.Fun (m a) (m (T.List a))))
 
hreplicateM ::
              (Monad m) =>
              T.R Int ->
                T.R (m a) -> T.RefExp -> T.R (m (T.List a))
greplicateM preplicateM p
  = T.ufun2 areplicateM preplicateM p hreplicateM
hreplicateM fn fx p
  = T.uap1 T.mkNoSrcPos p (gsequence T.mkNoSrcPos p)
      (T.uwrapForward p (hreplicate fn fx p))
 
greplicateM_ ::
               (Monad m) =>
               T.RefSrcPos ->
                 T.RefExp ->
                   T.R (T.Fun Int (T.Fun (m a) (m T.Tuple0)))
 
hreplicateM_ ::
               (Monad m) =>
               T.R Int -> T.R (m a) -> T.RefExp -> T.R (m T.Tuple0)
greplicateM_ preplicateM_ p
  = T.ufun2 areplicateM_ preplicateM_ p hreplicateM_
hreplicateM_ fn fx p
  = T.uap1 T.mkNoSrcPos p (gsequence_ T.mkNoSrcPos p)
      (T.uwrapForward p (hreplicate fn fx p))
afoldM_
  = T.mkVariable tMonad 390001 390045 3 (3) "foldM_"
      Prelude.False
aforM
  = T.mkVariable tMonad 150001 150027 3 (0) "forM"
      Prelude.False
aforM_
  = T.mkVariable tMonad 190001 190028 3 (0) "forM_"
      Prelude.False
aforever
  = T.mkVariable tMonad 310001 310036 3 (1) "forever"
      Prelude.False
areplicateM
  = T.mkVariable tMonad 440001 440044 3 (2)
      "replicateM"
      Prelude.False
areplicateM_
  = T.mkVariable tMonad 480001 480045 3 (2)
      "replicateM_"
      Prelude.False
avoid
  = T.mkVariable tMonad 350001 350022 3 (0) "void"
      Prelude.False
(+<=<)
  = T.mkVariable tMonad 270001 270024 3 (0) "<=<"
      Prelude.False
(+>=>)
  = T.mkVariable tMonad 230001 230029 3 (2) ">=>"
      Prelude.False
c31v19v31v30a'
  = T.mkVariable tMonad 310019 310030 3 (0) "a'"
      Prelude.True
p = T.mkRoot
tMonad
  = T.mkModule "Control.Monad" "Control/Monad.hs"
      Prelude.False