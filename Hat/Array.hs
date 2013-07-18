module Hat.Array
       (module Hat.Ix, Array, garray, aarray, harray,
        glistArray, alistArray, hlistArray, (!!), (+!), (*!),
        gbounds, abounds, hbounds, gindices, gelems, aelems,
        helems, gassocs, aassocs, hassocs, gaccumArray,
        aaccumArray, haccumArray, (!//), (+//), (*//),
        gaccum, aaccum, haccum, gixmap, aixmap, hixmap)
       where
import qualified Prelude
import qualified Hat.Hat as T
import qualified Hat.PreludeBasic
import qualified Hat.PreludeBuiltinTypes
import Hat.Prelude
import Hat.Ix
import Hat.List ((!\\))
 
data Array a b = MkArray (T.R (T.Tuple2 a a))
                         (T.R (T.Fun a b))
 
instance T.WrapVal (Array a b) where
        wrapVal pwrapVal
          kwrapVal@(MkArray (T.R _ z1wrapVal)
                      (T.R _ z2wrapVal))
          p
          = T.R kwrapVal
              (T.mkValueApp2 p pwrapVal aMkArray z1wrapVal
                 z2wrapVal)
 
garray ::
         (Ix a) =>
         T.RefSrcPos ->
           T.RefExp ->
             T.R
               (T.Fun (T.Tuple2 a a)
                  (T.Fun (T.List (T.Tuple2 a b)) (Array a b)))
 
harray ::
         (Ix a) =>
         T.R (T.Tuple2 a a) ->
           T.R (T.List (T.Tuple2 a b)) ->
             T.RefExp -> T.R (Array a b)
garray parray p = T.ufun2 aarray parray p harray
harray fb fivs p
  = T.ucif p
      (T.uap1 T.mkNoSrcPos p (gand T.mkNoSrcPos p)
         (T.uap2 T.mkNoSrcPos p
            ((Hat.PreludeBasic.!>>=) T.mkNoSrcPos p)
            fivs
            (T.ufun1 T.mkDoLambda T.mkNoSrcPos p
               (\ (T.R (T.Tuple2 fi _) _) p ->
                  T.uap1 T.mkNoSrcPos p
                    (Hat.PreludeBasic.greturn T.mkNoSrcPos p)
                    (T.uap2 T.mkNoSrcPos p (ginRange T.mkNoSrcPos p) fb
                       fi)))))
      (T.con2 T.mkNoSrcPos p MkArray aMkArray fb
         (T.ufun1 T.mkLambda T.mkNoSrcPos p
            (\ fj p ->
               T.uccase T.mkNoSrcPos p
                 (let v20v29v26v0v1 (T.R (T.Cons fv (T.R T.Nil _)) _)
                        p = T.projection T.mkNoSrcPos p fv
                      v20v29v26v0v1 (T.R T.Nil _) p
                        = T.uwrapForward p
                            (herror
                               (T.fromLitString T.mkNoSrcPos p
                                  "Array.!: undefined array element")
                               p)
                      v20v29v26v0v1 _ p
                        = T.uwrapForward p
                            (herror
                               (T.fromLitString T.mkNoSrcPos p
                                  "Array.!: multiply defined array element")
                               p)
                    in v20v29v26v0v1)
                 (T.uap2 T.mkNoSrcPos p
                    ((Hat.PreludeBasic.!>>=) T.mkNoSrcPos p)
                    fivs
                    (T.ufun1 T.mkDoLambda T.mkNoSrcPos p
                       (\ (T.R (T.Tuple2 fi fv) _) p ->
                          T.uap2 T.mkNoSrcPos p
                            ((Hat.PreludeBasic.!>>) T.mkNoSrcPos p)
                            (T.uwrapForward p
                               (Hat.PreludeBasic.hguard
                                  (T.uap2 T.mkNoSrcPos p ((!==) T.mkNoSrcPos p)
                                     fi
                                     fj)
                                  p))
                            (T.uap1 T.mkNoSrcPos p
                               (Hat.PreludeBasic.greturn T.mkNoSrcPos p)
                               fv)))))))
      (T.uwrapForward p
         (herror
            (T.fromLitString T.mkNoSrcPos p
               "Array.array: out-of-range array association")
            p))
 
glistArray ::
             (Ix a) =>
             T.RefSrcPos ->
               T.RefExp ->
                 T.R
                   (T.Fun (T.Tuple2 a a) (T.Fun (T.List b) (Array a b)))
 
hlistArray ::
             (Ix a) =>
             T.R (T.Tuple2 a a) ->
               T.R (T.List b) -> T.RefExp -> T.R (Array a b)
glistArray plistArray p
  = T.ufun2 alistArray plistArray p hlistArray
hlistArray fb fvs p
  = T.uwrapForward p
      (harray fb
         (T.uwrapForward p
            (hzipWith
               (T.ufun2 T.mkLambda T.mkNoSrcPos p
                  (\ fa fb p ->
                     T.con2 T.mkNoSrcPos p T.Tuple2 T.aTuple2 fa fb))
               (T.uap1 T.mkNoSrcPos p (grange T.mkNoSrcPos p) fb)
               fvs
               p))
         p)
 
(!!) ::
       (Ix a) =>
       T.RefSrcPos ->
         T.RefExp -> T.R (T.Fun (Array a b) (T.Fun a b))
 
(*!) ::
       (Ix a) =>
       T.R (Array a b) -> T.RefExp -> T.R (T.Fun a b)
(%!) !! p = T.ufun1 (+!) (%!) p (*!)
(T.R (MkArray _ ff) _) *! p
  = T.projection T.mkNoSrcPos p ff
_ *! p = T.fatal p
 
gbounds ::
          (Ix a) =>
          T.RefSrcPos ->
            T.RefExp -> T.R (T.Fun (Array a b) (T.Tuple2 a a))
 
hbounds ::
          (Ix a) =>
          T.R (Array a b) -> T.RefExp -> T.R (T.Tuple2 a a)
gbounds pbounds p = T.ufun1 abounds pbounds p hbounds
hbounds (T.R (MkArray fb _) _) p
  = T.projection T.mkNoSrcPos p fb
hbounds _ p = T.fatal p
 
gindices ::
           (Ix a) =>
           T.RefSrcPos ->
             T.RefExp -> T.R (T.Fun (Array a b) (T.List a))
 
sindices ::
           (Ix a) => T.R (T.Fun (Array a b) (T.List a))
gindices pindices p = T.uconstUse pindices p sindices
sindices
  = T.uconstDef p aindices
      (\ p ->
         T.uap2 T.mkNoSrcPos p ((!.) T.mkNoSrcPos p)
           (grange T.mkNoSrcPos p)
           (gbounds T.mkNoSrcPos p))
 
gelems ::
         (Ix a) =>
         T.RefSrcPos ->
           T.RefExp -> T.R (T.Fun (Array a b) (T.List b))
 
helems ::
         (Ix a) =>
         T.R (Array a b) -> T.RefExp -> T.R (T.List b)
gelems pelems p = T.ufun1 aelems pelems p helems
helems fa p
  = T.uap2 T.mkNoSrcPos p
      ((Hat.PreludeBasic.!>>=) T.mkNoSrcPos p)
      (T.uap1 T.mkNoSrcPos p (gindices T.mkNoSrcPos p) fa)
      (T.ufun1 T.mkDoLambda T.mkNoSrcPos p
         (\ fi p ->
            T.uap1 T.mkNoSrcPos p
              (Hat.PreludeBasic.greturn T.mkNoSrcPos p)
              (T.uap1 T.mkNoSrcPos p (T.uwrapForward p ((*!) fa p))
                 fi)))
 
gassocs ::
          (Ix a) =>
          T.RefSrcPos ->
            T.RefExp ->
              T.R (T.Fun (Array a b) (T.List (T.Tuple2 a b)))
 
hassocs ::
          (Ix a) =>
          T.R (Array a b) ->
            T.RefExp -> T.R (T.List (T.Tuple2 a b))
gassocs passocs p = T.ufun1 aassocs passocs p hassocs
hassocs fa p
  = T.uap2 T.mkNoSrcPos p
      ((Hat.PreludeBasic.!>>=) T.mkNoSrcPos p)
      (T.uap1 T.mkNoSrcPos p (gindices T.mkNoSrcPos p) fa)
      (T.ufun1 T.mkDoLambda T.mkNoSrcPos p
         (\ fi p ->
            T.uap1 T.mkNoSrcPos p
              (Hat.PreludeBasic.greturn T.mkNoSrcPos p)
              (T.con2 T.mkNoSrcPos p T.Tuple2 T.aTuple2 fi
                 (T.uap1 T.mkNoSrcPos p (T.uwrapForward p ((*!) fa p))
                    fi))))
 
(!//) ::
        (Ix a) =>
        T.RefSrcPos ->
          T.RefExp ->
            T.R
              (T.Fun (Array a b)
                 (T.Fun (T.List (T.Tuple2 a b)) (Array a b)))
 
(*//) ::
        (Ix a) =>
        T.R (Array a b) ->
          T.R (T.List (T.Tuple2 a b)) ->
            T.RefExp -> T.R (Array a b)
(%//) !// p = T.ufun2 (+//) (%//) p (*//)
(fa *// fnew_ivs) p
  = T.uwrapForward p
      (harray (T.uwrapForward p (hbounds fa p))
         (T.uwrapForward p
            ((*++) (gold_ivs T.mkNoSrcPos p) fnew_ivs p))
         p)
  where gold_ivs pold_ivs p
          = T.uconstUse pold_ivs p sold_ivs
        sold_ivs
          = T.uconstDef p c53v20v54v64old_ivs
              (\ p ->
                 T.uap2 T.mkNoSrcPos p
                   ((Hat.PreludeBasic.!>>=) T.mkNoSrcPos p)
                   (T.uap1 T.mkNoSrcPos p (gindices T.mkNoSrcPos p) fa)
                   (T.ufun1 T.mkDoLambda T.mkNoSrcPos p
                      (\ fi p ->
                         T.uap2 T.mkNoSrcPos p
                           ((Hat.PreludeBasic.!>>) T.mkNoSrcPos p)
                           (T.uwrapForward p
                              (Hat.PreludeBasic.hguard
                                 (T.uap1 T.mkNoSrcPos p
                                    (T.uwrapForward p (hnotElem fi p))
                                    (gnew_is T.mkNoSrcPos p))
                                 p))
                           (T.uap1 T.mkNoSrcPos p
                              (Hat.PreludeBasic.greturn T.mkNoSrcPos p)
                              (T.con2 T.mkNoSrcPos p T.Tuple2 T.aTuple2 fi
                                 (T.uap1 T.mkNoSrcPos p
                                    (T.uwrapForward p ((*!) fa p))
                                    fi))))))
        gnew_is pnew_is p = T.uconstUse pnew_is p snew_is
        snew_is
          = T.uconstDef p c55v20v55v51new_is
              (\ p ->
                 T.uap2 T.mkNoSrcPos p
                   ((Hat.PreludeBasic.!>>=) T.mkNoSrcPos p)
                   fnew_ivs
                   (T.ufun1 T.mkDoLambda T.mkNoSrcPos p
                      (\ (T.R (T.Tuple2 fi _) _) p ->
                         T.uap1 T.mkNoSrcPos p
                           (Hat.PreludeBasic.greturn T.mkNoSrcPos p)
                           fi)))
 
gaccum ::
         (Ix a) =>
         T.RefSrcPos ->
           T.RefExp ->
             T.R
               (T.Fun (T.Fun b (T.Fun c b))
                  (T.Fun (Array a b)
                     (T.Fun (T.List (T.Tuple2 a c)) (Array a b))))
 
haccum ::
         (Ix a) =>
         T.R (T.Fun b (T.Fun c b)) ->
           T.RefExp ->
             T.R
               (T.Fun (Array a b)
                  (T.Fun (T.List (T.Tuple2 a c)) (Array a b)))
gaccum paccum p = T.ufun1 aaccum paccum p haccum
haccum ff p
  = T.uap1 T.mkNoSrcPos p (gfoldl T.mkNoSrcPos p)
      (T.ufun2 T.mkLambda T.mkNoSrcPos p
         (\ fa (T.R (T.Tuple2 fi fv) _) p ->
            T.uwrapForward p
              ((*//) fa
                 (T.fromExpList T.mkNoSrcPos p
                    [T.con2 T.mkNoSrcPos p T.Tuple2 T.aTuple2 fi
                       (T.uap2 T.mkNoSrcPos p ff
                          (T.uap1 T.mkNoSrcPos p (T.uwrapForward p ((*!) fa p))
                             fi)
                          fv)])
                 p)))
 
gaccumArray ::
              (Ix a) =>
              T.RefSrcPos ->
                T.RefExp ->
                  T.R
                    (T.Fun (T.Fun b (T.Fun c b))
                       (T.Fun b
                          (T.Fun (T.Tuple2 a a)
                             (T.Fun (T.List (T.Tuple2 a c)) (Array a b)))))
 
haccumArray ::
              (Ix a) =>
              T.R (T.Fun b (T.Fun c b)) ->
                T.R b ->
                  T.R (T.Tuple2 a a) ->
                    T.RefExp ->
                      T.R (T.Fun (T.List (T.Tuple2 a c)) (Array a b))
gaccumArray paccumArray p
  = T.ufun3 aaccumArray paccumArray p haccumArray
haccumArray ff fz fb p
  = T.uap1 T.mkNoSrcPos p
      (T.uwrapForward p (haccum ff p))
      (T.uwrapForward p
         (harray fb
            (T.uap2 T.mkNoSrcPos p
               ((Hat.PreludeBasic.!>>=) T.mkNoSrcPos p)
               (T.uap1 T.mkNoSrcPos p (grange T.mkNoSrcPos p) fb)
               (T.ufun1 T.mkDoLambda T.mkNoSrcPos p
                  (\ fi p ->
                     T.uap1 T.mkNoSrcPos p
                       (Hat.PreludeBasic.greturn T.mkNoSrcPos p)
                       (T.con2 T.mkNoSrcPos p T.Tuple2 T.aTuple2 fi fz))))
            p))
 
gixmap ::
         (Ix a, Ix b) =>
         T.RefSrcPos ->
           T.RefExp ->
             T.R
               (T.Fun (T.Tuple2 a a)
                  (T.Fun (T.Fun a b) (T.Fun (Array b c) (Array a c))))
 
hixmap ::
         (Ix a, Ix b) =>
         T.R (T.Tuple2 a a) ->
           T.R (T.Fun a b) ->
             T.R (Array b c) -> T.RefExp -> T.R (Array a c)
gixmap pixmap p = T.ufun3 aixmap pixmap p hixmap
hixmap fb ff fa p
  = T.uwrapForward p
      (harray fb
         (T.uap2 T.mkNoSrcPos p
            ((Hat.PreludeBasic.!>>=) T.mkNoSrcPos p)
            (T.uap1 T.mkNoSrcPos p (grange T.mkNoSrcPos p) fb)
            (T.ufun1 T.mkDoLambda T.mkNoSrcPos p
               (\ fi p ->
                  T.uap1 T.mkNoSrcPos p
                    (Hat.PreludeBasic.greturn T.mkNoSrcPos p)
                    (T.con2 T.mkNoSrcPos p T.Tuple2 T.aTuple2 fi
                       (T.uap1 T.mkNoSrcPos p (T.uwrapForward p ((*!) fa p))
                          (T.uap1 T.mkNoSrcPos p ff fi))))))
         p)
 
instance (Ix a) => Functor (Array a) where
        gfmap pfmap p = T.ufun2 c70v5v70v47fmap pfmap p hfmap
          where hfmap ffn (T.R (MkArray fb ff) _) p
                  = T.con2 T.mkNoSrcPos p MkArray aMkArray fb
                      (T.uap2 T.mkNoSrcPos p ((!.) T.mkNoSrcPos p) ffn ff)
                hfmap _ _ p = T.fatal p
 
instance (Ix a, Eq b) => Eq (Array a b) where
        (%==) !== p = T.ufun2 (+^%=*=^%=%+==) (%==) p (*==)
          where (fa *== fa') p
                  = T.uap2 T.mkNoSrcPos p ((!==) T.mkNoSrcPos p)
                      (T.uwrapForward p (hassocs fa p))
                      (T.uwrapForward p (hassocs fa' p))
 
instance (Ix a, Ord b) => Ord (Array a b) where
        (%<=) !<= p = T.ufun2 (+^+=*=^+=%+<=) (%<=) p (*<=)
          where (fa *<= fa') p
                  = T.uap2 T.mkNoSrcPos p ((!<=) T.mkNoSrcPos p)
                      (T.uwrapForward p (hassocs fa p))
                      (T.uwrapForward p (hassocs fa' p))
 
instance (Ix a, Show a, Show b) => Show (Array a b)
         where
        gshowsPrec pshowsPrec p
          = T.ufun2 c79v5v82v55showsPrec pshowsPrec p
              hshowsPrec
          where hshowsPrec fp fa p
                  = T.uwrapForward p
                      (hshowParen
                         (T.uap2 T.mkNoSrcPos p ((!>) T.mkNoSrcPos p) fp
                            (T.uap1 T.mkNoSrcPos p
                               (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                               (T.conInteger T.mkNoSrcPos p (9))))
                         (T.uap2 T.mkNoSrcPos p ((!.) T.mkNoSrcPos p)
                            (T.uap1 T.mkNoSrcPos p (gshowString T.mkNoSrcPos p)
                               (T.fromLitString T.mkNoSrcPos p "array "))
                            (T.uap2 T.mkNoSrcPos p ((!.) T.mkNoSrcPos p)
                               (T.uap1 T.mkNoSrcPos p (gshows T.mkNoSrcPos p)
                                  (T.uwrapForward p (hbounds fa p)))
                               (T.uap2 T.mkNoSrcPos p ((!.) T.mkNoSrcPos p)
                                  (T.uap1 T.mkNoSrcPos p
                                     (gshowChar T.mkNoSrcPos p)
                                     (T.conChar T.mkNoSrcPos p ' '))
                                  (T.uap1 T.mkNoSrcPos p (gshows T.mkNoSrcPos p)
                                     (T.uwrapForward p (hassocs fa p))))))
                         p)
 
instance (Ix a, Read a, Read b) => Read (Array a b)
         where
        greadsPrec preadsPrec p
          = T.ufun1 c85v5v88v64readsPrec preadsPrec p
              hreadsPrec
          where hreadsPrec fp p
                  = T.uwrapForward p
                      (hreadParen
                         (T.uap2 T.mkNoSrcPos p ((!>) T.mkNoSrcPos p) fp
                            (T.uap1 T.mkNoSrcPos p
                               (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                               (T.conInteger T.mkNoSrcPos p (9))))
                         (T.ufun1 T.mkLambda T.mkNoSrcPos p
                            (\ fr p ->
                               T.uap2 T.mkNoSrcPos p
                                 ((Hat.PreludeBasic.!>>=) T.mkNoSrcPos p)
                                 (T.uwrapForward p (hlex fr p))
                                 (T.ufun1 T.mkDoLambda T.mkNoSrcPos p
                                    (\ fv86v38v86v51v1 p ->
                                       T.uccase T.mkNoSrcPos p
                                         (let v86v38v86v51v1
                                                (T.R
                                                   (T.Tuple2
                                                      (T.R
                                                         (T.Cons (T.R 'a' _)
                                                            (T.R
                                                               (T.Cons
                                                                  (T.R 'r' _)
                                                                  (T.R
                                                                     (T.Cons
                                                                        (T.R 'r'
                                                                           _)
                                                                        (T.R
                                                                           (T.Cons
                                                                              (T.R
                                                                                 'a'
                                                                                 _)
                                                                              (T.R
                                                                                 (T.Cons
                                                                                    (T.R
                                                                                       'y'
                                                                                       _)
                                                                                    (T.R
                                                                                       T.Nil
                                                                                       _))
                                                                                 _))
                                                                           _))
                                                                     _))
                                                               _))
                                                         _)
                                                      fs)
                                                   _)
                                                p
                                                = T.uap2 T.mkNoSrcPos p
                                                    ((Hat.PreludeBasic.!>>=)
                                                       T.mkNoSrcPos
                                                       p)
                                                    (T.uap1 T.mkNoSrcPos p
                                                       (greads T.mkNoSrcPos p)
                                                       fs)
                                                    (T.ufun1 T.mkDoLambda
                                                       T.mkNoSrcPos
                                                       p
                                                       (\ (T.R (T.Tuple2 fb ft)
                                                             _)
                                                          p ->
                                                          T.uap2 T.mkNoSrcPos p
                                                            ((Hat.PreludeBasic.!>>=)
                                                               T.mkNoSrcPos
                                                               p)
                                                            (T.uap1 T.mkNoSrcPos
                                                               p
                                                               (greads
                                                                  T.mkNoSrcPos
                                                                  p)
                                                               ft)
                                                            (T.ufun1
                                                               T.mkDoLambda
                                                               T.mkNoSrcPos
                                                               p
                                                               (\ (T.R
                                                                     (T.Tuple2
                                                                        fas fu)
                                                                     _)
                                                                  p ->
                                                                  T.uap1
                                                                    T.mkNoSrcPos
                                                                    p
                                                                    (Hat.PreludeBasic.greturn
                                                                       T.mkNoSrcPos
                                                                       p)
                                                                    (T.con2
                                                                       T.mkNoSrcPos
                                                                       p
                                                                       T.Tuple2
                                                                       T.aTuple2
                                                                       (T.uwrapForward
                                                                          p
                                                                          (harray
                                                                             fb
                                                                             fas
                                                                             p))
                                                                       fu)))))
                                              v86v38v86v51v1 _ p
                                                = T.uap1 T.mkNoSrcPos p
                                                    (Hat.PreludeBasic.gfail
                                                       T.mkNoSrcPos
                                                       p)
                                                    (T.fromLitString
                                                       T.mkNoSrcPos
                                                       p
                                                       "pattern-match failure in do-expression")
                                            in v86v38v86v51v1)
                                         fv86v38v86v51v1))))
                         p)
aMkArray
  = T.mkConstructor tArray 130018 130024 3 (2)
      "MkArray"
aaccum
  = T.mkVariable tArray 590001 590065 3 (1) "accum"
      Prelude.False
aaccumArray
  = T.mkVariable tArray 630001 630065 3 (3)
      "accumArray"
      Prelude.False
aarray
  = T.mkVariable tArray 170001 270064 3 (2) "array"
      Prelude.False
aassocs
  = T.mkVariable tArray 480001 480052 3 (1) "assocs"
      Prelude.False
abounds
  = T.mkVariable tArray 390001 390026 3 (1) "bounds"
      Prelude.False
aelems
  = T.mkVariable tArray 450001 450047 3 (1) "elems"
      Prelude.False
aindices
  = T.mkVariable tArray 420001 420039 3 (0) "indices"
      Prelude.False
aixmap
  = T.mkVariable tArray 670001 670061 3 (3) "ixmap"
      Prelude.False
alistArray
  = T.mkVariable tArray 310001 310072 3 (2) "listArray"
      Prelude.False
(+!)
  = T.mkVariable tArray 350001 350026 38 (1) "!"
      Prelude.False
(+//)
  = T.mkVariable tArray 510001 520027 38 (2) "//"
      Prelude.False
c55v20v55v51new_is
  = T.mkVariable tArray 550020 550051 3 (0) "new_is"
      Prelude.True
c53v20v54v64old_ivs
  = T.mkVariable tArray 530020 540064 3 (0) "old_ivs"
      Prelude.True
c70v5v70v47fmap
  = T.mkVariable tArray 700005 700047 3 (-1) "fmap"
      Prelude.False
(+^%=*=^%=%+==)
  = T.mkVariable tArray 730005 730036 3 (-1) "=="
      Prelude.False
(+^+=*=^+=%+<=)
  = T.mkVariable tArray 760005 760036 3 (-1) "<="
      Prelude.False
c79v5v82v55showsPrec
  = T.mkVariable tArray 790005 820055 3 (-1)
      "showsPrec"
      Prelude.False
c85v5v88v64readsPrec
  = T.mkVariable tArray 850005 880064 3 (-1)
      "readsPrec"
      Prelude.False
p = T.mkRoot
tArray = T.mkModule "Array" "Array.hs" Prelude.False