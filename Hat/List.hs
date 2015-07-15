module Hat.List
       (gelemIndex, aelemIndex, helemIndex, gelemIndices,
        aelemIndices, helemIndices, gfind, afind, hfind,
        gfindIndex, afindIndex, hfindIndex, gfindIndices,
        afindIndices, hfindIndices, gnub, gnubBy, anubBy,
        hnubBy, gdelete, gdeleteBy, adeleteBy, hdeleteBy,
        (!\\), gdeleteFirstsBy, adeleteFirstsBy,
        hdeleteFirstsBy, gunion, gunionBy, aunionBy,
        hunionBy, gintersect, gintersectBy, aintersectBy,
        hintersectBy, gintersperse, aintersperse,
        hintersperse, gtranspose, atranspose, htranspose,
        gpartition, apartition, hpartition, ggroup, ggroupBy,
        agroupBy, hgroupBy, ginits, ainits, hinits, gtails,
        atails, htails, gisPrefixOf, aisPrefixOf,
        hisPrefixOf, gisSuffixOf, aisSuffixOf, hisSuffixOf,
        gmapAccumL, amapAccumL, hmapAccumL, gmapAccumR,
        amapAccumR, hmapAccumR, gsort, gsortBy, asortBy,
        hsortBy, ginsert, ginsertBy, ainsertBy, hinsertBy,
        gmaximumBy, amaximumBy, hmaximumBy, gminimumBy,
        aminimumBy, hminimumBy, ggenericLength,
        agenericLength, hgenericLength, ggenericTake,
        agenericTake, hgenericTake, ggenericDrop,
        agenericDrop, hgenericDrop, ggenericSplitAt,
        agenericSplitAt, hgenericSplitAt, ggenericIndex,
        agenericIndex, hgenericIndex, ggenericReplicate,
        agenericReplicate, hgenericReplicate, gzip4, gzip5,
        gzip6, gzip7, gzipWith4, azipWith4, hzipWith4,
        gzipWith5, azipWith5, hzipWith5, gzipWith6,
        azipWith6, hzipWith6, gzipWith7, azipWith7,
        hzipWith7, gunzip4, gunzip5, gunzip6, gunzip7,
        gunfoldr, aunfoldr, hunfoldr, gmap, amap, hmap,
        (!++), (+++), (*++), gconcat, aconcat, hconcat,
        gfilter, afilter, hfilter, ghead, ahead, hhead,
        glast, alast, hlast, gtail, atail, htail, ginit,
        ainit, hinit, gnull, anull, hnull, glength, alength,
        hlength, (!!!), (+!!), (*!!), gfoldl, afoldl, hfoldl,
        gfoldl1, afoldl1, hfoldl1, gscanl, ascanl, hscanl,
        gscanl1, ascanl1, hscanl1, gfoldr, afoldr, hfoldr,
        gfoldr1, afoldr1, hfoldr1, gscanr, ascanr, hscanr,
        gscanr1, ascanr1, hscanr1, giterate, aiterate,
        hiterate, grepeat, arepeat, hrepeat, greplicate,
        areplicate, hreplicate, gcycle, acycle, hcycle,
        gtake, atake, htake, gdrop, adrop, hdrop, gsplitAt,
        asplitAt, hsplitAt, gtakeWhile, atakeWhile,
        htakeWhile, gdropWhile, adropWhile, hdropWhile,
        gspan, aspan, hspan, gbreak, abreak, hbreak, glines,
        alines, hlines, gwords, awords, hwords, gunlines,
        gunwords, aunwords, hunwords, greverse, gand, gor,
        gany, aany, hany, gall, aall, hall, gelem, aelem,
        helem, gnotElem, anotElem, hnotElem, glookup,
        alookup, hlookup, gsum, gproduct, gmaximum, amaximum,
        hmaximum, gminimum, aminimum, hminimum, gconcatMap,
        aconcatMap, hconcatMap, gzip, gzip3, gzipWith,
        azipWith, hzipWith, gzipWith3, azipWith3, hzipWith3,
        gunzip, gunzip3)
       where
import qualified Prelude
import qualified Hat.Hat as T
import qualified Hat.PreludeBasic
import qualified Hat.PreludeBuiltinTypes
import Hat.Prelude
import Hat.Maybe
       (glistToMaybe, alistToMaybe, hlistToMaybe)
 
gelemIndex ::
             (Eq a) =>
             T.RefSrcPos ->
               T.RefExp ->
                 T.R (T.Fun a (T.Fun (T.List a) (Maybe Int)))
 
helemIndex ::
             (Eq a) =>
             T.R a ->
               T.RefExp -> T.R (T.Fun (T.List a) (Maybe Int))
gelemIndex pelemIndex p
  = T.ufun1 aelemIndex pelemIndex p helemIndex
helemIndex fx p
  = T.uwrapForward p
      (hfindIndex
         (T.uap1 T.mkNoSrcPos p ((!==) T.mkNoSrcPos p) fx)
         p)
 
gelemIndices ::
               (Eq a) =>
               T.RefSrcPos ->
                 T.RefExp ->
                   T.R (T.Fun a (T.Fun (T.List a) (T.List Int)))
 
helemIndices ::
               (Eq a) =>
               T.R a ->
                 T.RefExp -> T.R (T.Fun (T.List a) (T.List Int))
gelemIndices pelemIndices p
  = T.ufun1 aelemIndices pelemIndices p helemIndices
helemIndices fx p
  = T.uap1 T.mkNoSrcPos p (gfindIndices T.mkNoSrcPos p)
      (T.uap1 T.mkNoSrcPos p ((!==) T.mkNoSrcPos p) fx)
 
gfind ::
      T.RefSrcPos ->
        T.RefExp ->
          T.R
            (T.Fun (T.Fun a Bool) (T.Fun (T.List a) (Maybe a)))
 
hfind ::
      T.R (T.Fun a Bool) ->
        T.RefExp -> T.R (T.Fun (T.List a) (Maybe a))
gfind pfind p = T.ufun1 afind pfind p hfind
hfind fp p
  = T.uap2 T.mkNoSrcPos p ((!.) T.mkNoSrcPos p)
      (glistToMaybe T.mkNoSrcPos p)
      (T.uap1 T.mkNoSrcPos p (gfilter T.mkNoSrcPos p) fp)
 
gfindIndex ::
           T.RefSrcPos ->
             T.RefExp ->
               T.R
                 (T.Fun (T.Fun a Bool) (T.Fun (T.List a) (Maybe Int)))
 
hfindIndex ::
           T.R (T.Fun a Bool) ->
             T.RefExp -> T.R (T.Fun (T.List a) (Maybe Int))
gfindIndex pfindIndex p
  = T.ufun1 afindIndex pfindIndex p hfindIndex
hfindIndex fp p
  = T.uap2 T.mkNoSrcPos p ((!.) T.mkNoSrcPos p)
      (glistToMaybe T.mkNoSrcPos p)
      (T.uap1 T.mkNoSrcPos p (gfindIndices T.mkNoSrcPos p)
         fp)
 
gfindIndices ::
             T.RefSrcPos ->
               T.RefExp ->
                 T.R
                   (T.Fun (T.Fun a Bool)
                      (T.Fun (T.List a) (T.List Int)))
 
hfindIndices ::
             T.R (T.Fun a Bool) ->
               T.R (T.List a) -> T.RefExp -> T.R (T.List Int)
gfindIndices pfindIndices p
  = T.ufun2 afindIndices pfindIndices p hfindIndices
hfindIndices fp fxs p
  = T.uap2 T.mkNoSrcPos p
      ((Hat.PreludeBasic.!>>=) T.mkNoSrcPos p)
      (T.uap2 T.mkNoSrcPos p (gzip T.mkNoSrcPos p) fxs
         (T.uap1 T.mkNoSrcPos p
            (Hat.PreludeBasic.genumFrom T.mkNoSrcPos p)
            (T.uap1 T.mkNoSrcPos p
               (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
               (T.conInteger T.mkNoSrcPos p (0)))))
      (T.ufun1 T.mkDoLambda T.mkNoSrcPos p
         (\ (T.R (T.Tuple2 fx fi) _) p ->
            T.uap2 T.mkNoSrcPos p
              ((Hat.PreludeBasic.!>>) T.mkNoSrcPos p)
              (T.uwrapForward p
                 (Hat.PreludeBasic.hguard
                    (T.uap1 T.mkNoSrcPos p fp fx)
                    p))
              (T.uap1 T.mkNoSrcPos p
                 (Hat.PreludeBasic.greturn T.mkNoSrcPos p)
                 fi)))
 
gnub ::
       (Eq a) =>
       T.RefSrcPos ->
         T.RefExp -> T.R (T.Fun (T.List a) (T.List a))
 
snub :: (Eq a) => T.R (T.Fun (T.List a) (T.List a))
gnub pnub p = T.uconstUse pnub p snub
snub
  = T.uconstDef p anub
      (\ p ->
         T.uap1 T.mkNoSrcPos p (gnubBy T.mkNoSrcPos p)
           ((!==) T.mkNoSrcPos p))
 
gnubBy ::
       T.RefSrcPos ->
         T.RefExp ->
           T.R
             (T.Fun (T.Fun a (T.Fun a Bool))
                (T.Fun (T.List a) (T.List a)))
 
hnubBy ::
       T.R (T.Fun a (T.Fun a Bool)) ->
         T.R (T.List a) -> T.RefExp -> T.R (T.List a)
gnubBy pnubBy p = T.ufun2 anubBy pnubBy p hnubBy
hnubBy feq (T.R T.Nil _) p
  = T.con0 T.mkNoSrcPos p T.Nil T.aNil
hnubBy feq (T.R (T.Cons fx fxs) _) p
  = T.con2 T.mkNoSrcPos p T.Cons T.aCons fx
      (T.uwrapForward p
         (hnubBy feq
            (T.uwrapForward p
               (hfilter
                  (T.ufun1 T.mkLambda T.mkNoSrcPos p
                     (\ fy p ->
                        T.uwrapForward p
                          (hnot (T.uap2 T.mkNoSrcPos p feq fx fy) p)))
                  fxs
                  p))
            p))
hnubBy _ _ p = T.fatal p
 
gdelete ::
          (Eq a) =>
          T.RefSrcPos ->
            T.RefExp ->
              T.R (T.Fun a (T.Fun (T.List a) (T.List a)))
 
sdelete ::
          (Eq a) => T.R (T.Fun a (T.Fun (T.List a) (T.List a)))
gdelete pdelete p = T.uconstUse pdelete p sdelete
sdelete
  = T.uconstDef p adelete
      (\ p ->
         T.uap1 T.mkNoSrcPos p (gdeleteBy T.mkNoSrcPos p)
           ((!==) T.mkNoSrcPos p))
 
gdeleteBy ::
          T.RefSrcPos ->
            T.RefExp ->
              T.R
                (T.Fun (T.Fun a (T.Fun a Bool))
                   (T.Fun a (T.Fun (T.List a) (T.List a))))
 
hdeleteBy ::
          T.R (T.Fun a (T.Fun a Bool)) ->
            T.R a -> T.R (T.List a) -> T.RefExp -> T.R (T.List a)
gdeleteBy pdeleteBy p
  = T.ufun3 adeleteBy pdeleteBy p hdeleteBy
hdeleteBy feq fx (T.R T.Nil _) p
  = T.con0 T.mkNoSrcPos p T.Nil T.aNil
hdeleteBy feq fx (T.R (T.Cons fy fys) _) p
  = T.ucif p (T.uap2 T.mkNoSrcPos p feq fx fy)
      (T.projection T.mkNoSrcPos p fys)
      (T.con2 T.mkNoSrcPos p T.Cons T.aCons fy
         (T.uwrapForward p (hdeleteBy feq fx fys p)))
hdeleteBy _ _ _ p = T.fatal p
 
(!\\) ::
        (Eq a) =>
        T.RefSrcPos ->
          T.RefExp ->
            T.R (T.Fun (T.List a) (T.Fun (T.List a) (T.List a)))
 
(|\\) ::
        (Eq a) =>
        T.R (T.Fun (T.List a) (T.Fun (T.List a) (T.List a)))
(%\\) !\\ p = T.uconstUse (%\\) p (|\\)
(|\\)
  = T.uconstDef p (+\\)
      (\ p ->
         T.uap1 T.mkNoSrcPos p (gfoldl T.mkNoSrcPos p)
           (T.uap1 T.mkNoSrcPos p (gflip T.mkNoSrcPos p)
              (gdelete T.mkNoSrcPos p)))
 
gdeleteFirstsBy ::
                T.RefSrcPos ->
                  T.RefExp ->
                    T.R
                      (T.Fun (T.Fun a (T.Fun a Bool))
                         (T.Fun (T.List a) (T.Fun (T.List a) (T.List a))))
 
hdeleteFirstsBy ::
                T.R (T.Fun a (T.Fun a Bool)) ->
                  T.RefExp ->
                    T.R (T.Fun (T.List a) (T.Fun (T.List a) (T.List a)))
gdeleteFirstsBy pdeleteFirstsBy p
  = T.ufun1 adeleteFirstsBy pdeleteFirstsBy p
      hdeleteFirstsBy
hdeleteFirstsBy feq p
  = T.uap1 T.mkNoSrcPos p (gfoldl T.mkNoSrcPos p)
      (T.uap1 T.mkNoSrcPos p (gflip T.mkNoSrcPos p)
         (T.uap1 T.mkNoSrcPos p (gdeleteBy T.mkNoSrcPos p)
            feq))
 
gunion ::
         (Eq a) =>
         T.RefSrcPos ->
           T.RefExp ->
             T.R (T.Fun (T.List a) (T.Fun (T.List a) (T.List a)))
 
sunion ::
         (Eq a) =>
         T.R (T.Fun (T.List a) (T.Fun (T.List a) (T.List a)))
gunion punion p = T.uconstUse punion p sunion
sunion
  = T.uconstDef p aunion
      (\ p ->
         T.uap1 T.mkNoSrcPos p (gunionBy T.mkNoSrcPos p)
           ((!==) T.mkNoSrcPos p))
 
gunionBy ::
         T.RefSrcPos ->
           T.RefExp ->
             T.R
               (T.Fun (T.Fun a (T.Fun a Bool))
                  (T.Fun (T.List a) (T.Fun (T.List a) (T.List a))))
 
hunionBy ::
         T.R (T.Fun a (T.Fun a Bool)) ->
           T.R (T.List a) ->
             T.R (T.List a) -> T.RefExp -> T.R (T.List a)
gunionBy punionBy p
  = T.ufun3 aunionBy punionBy p hunionBy
hunionBy feq fxs fys p
  = T.uwrapForward p
      ((*++) fxs
         (T.uap2 T.mkNoSrcPos p
            (T.uwrapForward p (hdeleteFirstsBy feq p))
            (T.uwrapForward p (hnubBy feq fys p))
            fxs)
         p)
 
gintersect ::
             (Eq a) =>
             T.RefSrcPos ->
               T.RefExp ->
                 T.R (T.Fun (T.List a) (T.Fun (T.List a) (T.List a)))
 
sintersect ::
             (Eq a) =>
             T.R (T.Fun (T.List a) (T.Fun (T.List a) (T.List a)))
gintersect pintersect p
  = T.uconstUse pintersect p sintersect
sintersect
  = T.uconstDef p aintersect
      (\ p ->
         T.uap1 T.mkNoSrcPos p (gintersectBy T.mkNoSrcPos p)
           ((!==) T.mkNoSrcPos p))
 
gintersectBy ::
             T.RefSrcPos ->
               T.RefExp ->
                 T.R
                   (T.Fun (T.Fun a (T.Fun a Bool))
                      (T.Fun (T.List a) (T.Fun (T.List a) (T.List a))))
 
hintersectBy ::
             T.R (T.Fun a (T.Fun a Bool)) ->
               T.R (T.List a) ->
                 T.R (T.List a) -> T.RefExp -> T.R (T.List a)
gintersectBy pintersectBy p
  = T.ufun3 aintersectBy pintersectBy p hintersectBy
hintersectBy feq fxs fys p
  = T.uap2 T.mkNoSrcPos p
      ((Hat.PreludeBasic.!>>=) T.mkNoSrcPos p)
      fxs
      (T.ufun1 T.mkDoLambda T.mkNoSrcPos p
         (\ fx p ->
            T.uap2 T.mkNoSrcPos p
              ((Hat.PreludeBasic.!>>) T.mkNoSrcPos p)
              (T.uwrapForward p
                 (Hat.PreludeBasic.hguard
                    (T.uap1 T.mkNoSrcPos p
                       (T.uwrapForward p
                          (hany (T.uap1 T.mkNoSrcPos p feq fx) p))
                       fys)
                    p))
              (T.uap1 T.mkNoSrcPos p
                 (Hat.PreludeBasic.greturn T.mkNoSrcPos p)
                 fx)))
 
gintersperse ::
             T.RefSrcPos ->
               T.RefExp ->
                 T.R (T.Fun a (T.Fun (T.List a) (T.List a)))
 
hintersperse ::
             T.R a -> T.R (T.List a) -> T.RefExp -> T.R (T.List a)
gintersperse pintersperse p
  = T.ufun2 aintersperse pintersperse p hintersperse
hintersperse fsep (T.R T.Nil _) p
  = T.con0 T.mkNoSrcPos p T.Nil T.aNil
hintersperse fsep (T.R (T.Cons fx (T.R T.Nil _)) _) p
  = T.fromExpList T.mkNoSrcPos p [fx]
hintersperse fsep (T.R (T.Cons fx fxs) _) p
  = T.con2 T.mkNoSrcPos p T.Cons T.aCons fx
      (T.con2 T.mkNoSrcPos p T.Cons T.aCons fsep
         (T.uwrapForward p (hintersperse fsep fxs p)))
hintersperse _ _ p = T.fatal p
 
gtranspose ::
           T.RefSrcPos ->
             T.RefExp ->
               T.R (T.Fun (T.List (T.List a)) (T.List (T.List a)))
 
htranspose ::
           T.R (T.List (T.List a)) ->
             T.RefExp -> T.R (T.List (T.List a))
gtranspose ptranspose p
  = T.ufun1 atranspose ptranspose p htranspose
htranspose (T.R T.Nil _) p
  = T.con0 T.mkNoSrcPos p T.Nil T.aNil
htranspose (T.R (T.Cons (T.R T.Nil _) fxss) _) p
  = T.uwrapForward p (htranspose fxss p)
htranspose
  (T.R (T.Cons (T.R (T.Cons fx fxs) _) fxss) _) p
  = T.con2 T.mkNoSrcPos p T.Cons T.aCons
      (T.con2 T.mkNoSrcPos p T.Cons T.aCons fx
         (T.uap2 T.mkNoSrcPos p
            ((Hat.PreludeBasic.!>>=) T.mkNoSrcPos p)
            fxss
            (T.ufun1 T.mkDoLambda T.mkNoSrcPos p
               (\ fv93v38v93v45v1 p ->
                  T.uccase T.mkNoSrcPos p
                    (let v93v38v93v45v1 (T.R (T.Cons fh ft) _) p
                           = T.uap1 T.mkNoSrcPos p
                               (Hat.PreludeBasic.greturn T.mkNoSrcPos p)
                               fh
                         v93v38v93v45v1 _ p
                           = T.uap1 T.mkNoSrcPos p
                               (Hat.PreludeBasic.gfail T.mkNoSrcPos p)
                               (T.fromLitString T.mkNoSrcPos p
                                  "pattern-match failure in do-expression")
                       in v93v38v93v45v1)
                    fv93v38v93v45v1))))
      (T.uwrapForward p
         (htranspose
            (T.con2 T.mkNoSrcPos p T.Cons T.aCons fxs
               (T.uap2 T.mkNoSrcPos p
                  ((Hat.PreludeBasic.!>>=) T.mkNoSrcPos p)
                  fxss
                  (T.ufun1 T.mkDoLambda T.mkNoSrcPos p
                     (\ fv94v49v94v56v1 p ->
                        T.uccase T.mkNoSrcPos p
                          (let v94v49v94v56v1 (T.R (T.Cons fh ft) _) p
                                 = T.uap1 T.mkNoSrcPos p
                                     (Hat.PreludeBasic.greturn T.mkNoSrcPos p)
                                     ft
                               v94v49v94v56v1 _ p
                                 = T.uap1 T.mkNoSrcPos p
                                     (Hat.PreludeBasic.gfail T.mkNoSrcPos p)
                                     (T.fromLitString T.mkNoSrcPos p
                                        "pattern-match failure in do-expression")
                             in v94v49v94v56v1)
                          fv94v49v94v56v1))))
            p))
htranspose _ p = T.fatal p
 
gpartition ::
           T.RefSrcPos ->
             T.RefExp ->
               T.R
                 (T.Fun (T.Fun a Bool)
                    (T.Fun (T.List a) (T.Tuple2 (T.List a) (T.List a))))
 
hpartition ::
           T.R (T.Fun a Bool) ->
             T.R (T.List a) ->
               T.RefExp -> T.R (T.Tuple2 (T.List a) (T.List a))
gpartition ppartition p
  = T.ufun2 apartition ppartition p hpartition
hpartition fp fxs p
  = T.con2 T.mkNoSrcPos p T.Tuple2 T.aTuple2
      (T.uwrapForward p (hfilter fp fxs p))
      (T.uwrapForward p
         (hfilter
            (T.uap2 T.mkNoSrcPos p ((!.) T.mkNoSrcPos p)
               (gnot T.mkNoSrcPos p)
               fp)
            fxs
            p))
 
ggroup ::
         (Eq a) =>
         T.RefSrcPos ->
           T.RefExp ->
             T.R (T.Fun (T.List a) (T.List (T.List a)))
 
sgroup ::
         (Eq a) => T.R (T.Fun (T.List a) (T.List (T.List a)))
ggroup pgroup p = T.uconstUse pgroup p sgroup
sgroup
  = T.uconstDef p agroup
      (\ p ->
         T.uap1 T.mkNoSrcPos p (ggroupBy T.mkNoSrcPos p)
           ((!==) T.mkNoSrcPos p))
 
ggroupBy ::
         T.RefSrcPos ->
           T.RefExp ->
             T.R
               (T.Fun (T.Fun a (T.Fun a Bool))
                  (T.Fun (T.List a) (T.List (T.List a))))
 
hgroupBy ::
         T.R (T.Fun a (T.Fun a Bool)) ->
           T.R (T.List a) -> T.RefExp -> T.R (T.List (T.List a))
ggroupBy pgroupBy p
  = T.ufun2 agroupBy pgroupBy p hgroupBy
hgroupBy feq (T.R T.Nil _) p
  = T.con0 T.mkNoSrcPos p T.Nil T.aNil
hgroupBy feq (T.R (T.Cons fx fxs) _) p
  = T.con2 T.mkNoSrcPos p T.Cons T.aCons
      (T.con2 T.mkNoSrcPos p T.Cons T.aCons fx
         (gys T.mkNoSrcPos p))
      (T.uwrapForward p
         (hgroupBy feq (gzs T.mkNoSrcPos p) p))
  where gys pys p = T.uconstUse pys p sys
        gzs pzs p = T.uconstUse pzs p szs
        sys
          = T.uconstDef p c108v34v108v57ys
              (\ _ ->
                 case j108v34v108v57ys of
                     (kys, fys, fzs) -> fys)
        szs
          = T.uconstDef p c108v34v108v57zs
              (\ _ ->
                 case j108v34v108v57ys of
                     (kys, fys, fzs) -> fzs)
        j108v34v108v57ys
          = case
              T.uwrapForward p
                (hspan (T.uap1 T.mkNoSrcPos p feq fx) fxs p)
              of
                T.R (T.Tuple2 fys fzs) kys -> (kys, fys, fzs)
                _ -> T.fatal p
hgroupBy _ _ p = T.fatal p
 
ginits ::
       T.RefSrcPos ->
         T.RefExp ->
           T.R (T.Fun (T.List a) (T.List (T.List a)))
 
hinits ::
       T.R (T.List a) -> T.RefExp -> T.R (T.List (T.List a))
ginits pinits p = T.ufun1 ainits pinits p hinits
hinits (T.R T.Nil _) p
  = T.fromExpList T.mkNoSrcPos p
      [T.con0 T.mkNoSrcPos p T.Nil T.aNil]
hinits (T.R (T.Cons fx fxs) _) p
  = T.uwrapForward p
      ((*++)
         (T.fromExpList T.mkNoSrcPos p
            [T.con0 T.mkNoSrcPos p T.Nil T.aNil])
         (T.uwrapForward p
            (hmap (T.pa1 T.Cons T.cn1 T.mkNoSrcPos p T.aCons fx)
               (T.uwrapForward p (hinits fxs p))
               p))
         p)
hinits _ p = T.fatal p
 
gtails ::
       T.RefSrcPos ->
         T.RefExp ->
           T.R (T.Fun (T.List a) (T.List (T.List a)))
 
htails ::
       T.R (T.List a) -> T.RefExp -> T.R (T.List (T.List a))
gtails ptails p = T.ufun1 atails ptails p htails
htails (T.R T.Nil _) p
  = T.fromExpList T.mkNoSrcPos p
      [T.con0 T.mkNoSrcPos p T.Nil T.aNil]
htails fxxs@(T.R (T.Cons _ fxs) _) p
  = T.con2 T.mkNoSrcPos p T.Cons T.aCons fxxs
      (T.uwrapForward p (htails fxs p))
htails _ p = T.fatal p
 
gisPrefixOf ::
              (Eq a) =>
              T.RefSrcPos ->
                T.RefExp ->
                  T.R (T.Fun (T.List a) (T.Fun (T.List a) Bool))
 
hisPrefixOf ::
              (Eq a) =>
              T.R (T.List a) ->
                T.R (T.List a) -> T.RefExp -> T.R Bool
gisPrefixOf pisPrefixOf p
  = T.ufun2 aisPrefixOf pisPrefixOf p hisPrefixOf
hisPrefixOf (T.R T.Nil _) _ p
  = T.con0 T.mkNoSrcPos p True aTrue
hisPrefixOf _ (T.R T.Nil _) p
  = T.con0 T.mkNoSrcPos p False aFalse
hisPrefixOf (T.R (T.Cons fx fxs) _)
  (T.R (T.Cons fy fys) _) p
  = T.uwrapForward p
      ((*&&)
         (T.uap2 T.mkNoSrcPos p ((!==) T.mkNoSrcPos p) fx fy)
         (T.uwrapForward p (hisPrefixOf fxs fys p))
         p)
hisPrefixOf _ _ p = T.fatal p
 
gisSuffixOf ::
              (Eq a) =>
              T.RefSrcPos ->
                T.RefExp ->
                  T.R (T.Fun (T.List a) (T.Fun (T.List a) Bool))
 
hisSuffixOf ::
              (Eq a) =>
              T.R (T.List a) ->
                T.R (T.List a) -> T.RefExp -> T.R Bool
gisSuffixOf pisSuffixOf p
  = T.ufun2 aisSuffixOf pisSuffixOf p hisSuffixOf
hisSuffixOf fx fy p
  = T.uwrapForward p
      (hisPrefixOf
         (T.uap1 T.mkNoSrcPos p (greverse T.mkNoSrcPos p) fx)
         (T.uap1 T.mkNoSrcPos p (greverse T.mkNoSrcPos p) fy)
         p)
 
gmapAccumL ::
           T.RefSrcPos ->
             T.RefExp ->
               T.R
                 (T.Fun (T.Fun a (T.Fun b (T.Tuple2 a c)))
                    (T.Fun a (T.Fun (T.List b) (T.Tuple2 a (T.List c)))))
 
hmapAccumL ::
           T.R (T.Fun a (T.Fun b (T.Tuple2 a c))) ->
             T.R a ->
               T.R (T.List b) ->
                 T.RefExp -> T.R (T.Tuple2 a (T.List c))
gmapAccumL pmapAccumL p
  = T.ufun3 amapAccumL pmapAccumL p hmapAccumL
hmapAccumL ff fs (T.R T.Nil _) p
  = T.con2 T.mkNoSrcPos p T.Tuple2 T.aTuple2 fs
      (T.con0 T.mkNoSrcPos p T.Nil T.aNil)
hmapAccumL ff fs (T.R (T.Cons fx fxs) _) p
  = T.con2 T.mkNoSrcPos p T.Tuple2 T.aTuple2
      (gs'' T.mkNoSrcPos p)
      (T.con2 T.mkNoSrcPos p T.Cons T.aCons
         (gy T.mkNoSrcPos p)
         (gys T.mkNoSrcPos p))
  where gs' ps' p = T.uconstUse ps' p ss'
        gy py p = T.uconstUse py p sy
        ss'
          = T.uconstDef p c133v34v133v49s'
              (\ _ ->
                 case j133v34v133v49s' of
                     (ks', fs', fy) -> fs')
        sy
          = T.uconstDef p c133v34v133v49y
              (\ _ ->
                 case j133v34v133v49s' of
                     (ks', fs', fy) -> fy)
        j133v34v133v49s'
          = case T.uap2 T.mkNoSrcPos p ff fs fx of
                T.R (T.Tuple2 fs' fy) ks' -> (ks', fs', fy)
                _ -> T.fatal p
        gs'' ps'' p = T.uconstUse ps'' p ss''
        gys pys p = T.uconstUse pys p sys
        ss''
          = T.uconstDef p c134v34v134v61s''
              (\ _ ->
                 case j134v34v134v61s'' of
                     (ks'', fs'', fys) -> fs'')
        sys
          = T.uconstDef p c134v34v134v61ys
              (\ _ ->
                 case j134v34v134v61s'' of
                     (ks'', fs'', fys) -> fys)
        j134v34v134v61s''
          = case
              T.uwrapForward p
                (hmapAccumL ff (gs' T.mkNoSrcPos p) fxs p)
              of
                T.R (T.Tuple2 fs'' fys) ks'' -> (ks'', fs'', fys)
                _ -> T.fatal p
hmapAccumL _ _ _ p = T.fatal p
 
gmapAccumR ::
           T.RefSrcPos ->
             T.RefExp ->
               T.R
                 (T.Fun (T.Fun a (T.Fun b (T.Tuple2 a c)))
                    (T.Fun a (T.Fun (T.List b) (T.Tuple2 a (T.List c)))))
 
hmapAccumR ::
           T.R (T.Fun a (T.Fun b (T.Tuple2 a c))) ->
             T.R a ->
               T.R (T.List b) ->
                 T.RefExp -> T.R (T.Tuple2 a (T.List c))
gmapAccumR pmapAccumR p
  = T.ufun3 amapAccumR pmapAccumR p hmapAccumR
hmapAccumR ff fs (T.R T.Nil _) p
  = T.con2 T.mkNoSrcPos p T.Tuple2 T.aTuple2 fs
      (T.con0 T.mkNoSrcPos p T.Nil T.aNil)
hmapAccumR ff fs (T.R (T.Cons fx fxs) _) p
  = T.con2 T.mkNoSrcPos p T.Tuple2 T.aTuple2
      (gs'' T.mkNoSrcPos p)
      (T.con2 T.mkNoSrcPos p T.Cons T.aCons
         (gy T.mkNoSrcPos p)
         (gys T.mkNoSrcPos p))
  where gs'' ps'' p = T.uconstUse ps'' p ss''
        gy py p = T.uconstUse py p sy
        ss''
          = T.uconstDef p c139v34v139v50s''
              (\ _ ->
                 case j139v34v139v50s'' of
                     (ks'', fs'', fy) -> fs'')
        sy
          = T.uconstDef p c139v34v139v50y
              (\ _ ->
                 case j139v34v139v50s'' of
                     (ks'', fs'', fy) -> fy)
        j139v34v139v50s''
          = case
              T.uap2 T.mkNoSrcPos p ff (gs' T.mkNoSrcPos p) fx of
                T.R (T.Tuple2 fs'' fy) ks'' -> (ks'', fs'', fy)
                _ -> T.fatal p
        gs' ps' p = T.uconstUse ps' p ss'
        gys pys p = T.uconstUse pys p sys
        ss'
          = T.uconstDef p c140v34v140v60s'
              (\ _ ->
                 case j140v34v140v60s' of
                     (ks', fs', fys) -> fs')
        sys
          = T.uconstDef p c140v34v140v60ys
              (\ _ ->
                 case j140v34v140v60s' of
                     (ks', fs', fys) -> fys)
        j140v34v140v60s'
          = case T.uwrapForward p (hmapAccumR ff fs fxs p) of
                T.R (T.Tuple2 fs' fys) ks' -> (ks', fs', fys)
                _ -> T.fatal p
hmapAccumR _ _ _ p = T.fatal p
 
gunfoldr ::
         T.RefSrcPos ->
           T.RefExp ->
             T.R
               (T.Fun (T.Fun b (Maybe (T.Tuple2 a b)))
                  (T.Fun b (T.List a)))
 
hunfoldr ::
         T.R (T.Fun b (Maybe (T.Tuple2 a b))) ->
           T.R b -> T.RefExp -> T.R (T.List a)
gunfoldr punfoldr p
  = T.ufun2 aunfoldr punfoldr p hunfoldr
hunfoldr ff fb p
  = T.uccase T.mkNoSrcPos p
      (let v143v27v147v0v1 (T.R Nothing _) p
             = T.con0 T.mkNoSrcPos p T.Nil T.aNil
           v143v27v147v0v1
             (T.R (Just (T.R (T.Tuple2 fa fb) _)) _) p
             = T.con2 T.mkNoSrcPos p T.Cons T.aCons fa
                 (T.uwrapForward p (hunfoldr ff fb p))
           v143v27v147v0v1 _ p = T.fatal p
         in v143v27v147v0v1)
      (T.uap1 T.mkNoSrcPos p ff fb)
 
gsort ::
        (Ord a) =>
        T.RefSrcPos ->
          T.RefExp -> T.R (T.Fun (T.List a) (T.List a))
 
ssort :: (Ord a) => T.R (T.Fun (T.List a) (T.List a))
gsort psort p = T.uconstUse psort p ssort
ssort
  = T.uconstDef p asort
      (\ p ->
         T.uwrapForward p
           (hsortBy (gcompare T.mkNoSrcPos p) p))
 
gsortBy ::
        T.RefSrcPos ->
          T.RefExp ->
            T.R
              (T.Fun (T.Fun a (T.Fun a Ordering))
                 (T.Fun (T.List a) (T.List a)))
 
hsortBy ::
        T.R (T.Fun a (T.Fun a Ordering)) ->
          T.RefExp -> T.R (T.Fun (T.List a) (T.List a))
gsortBy psortBy p = T.ufun1 asortBy psortBy p hsortBy
hsortBy fcmp p
  = T.uap2 T.mkNoSrcPos p ((!.) T.mkNoSrcPos p)
      (gmergeAll T.mkNoSrcPos p)
      (gsequences T.mkNoSrcPos p)
  where gsequences psequences p
          = T.ufun1 c153v5v156v23sequences psequences p
              hsequences
        asequences = c153v5v156v23sequences
        hsequences
          (T.R (T.Cons fa (T.R (T.Cons fb fxs) _)) _) p
          = T.ucguard
              (T.uap2 T.mkNoSrcPos p ((!==) T.mkNoSrcPos p)
                 (T.uap2 T.mkNoSrcPos p fcmp fa fb)
                 (T.con0 T.mkNoSrcPos p GT aGT))
              (T.uwrapForward p
                 (hdescending fb (T.fromExpList T.mkNoSrcPos p [fa])
                    fxs
                    p))
              (T.ucguard (gotherwise T.mkNoSrcPos p)
                 (T.uwrapForward p
                    (hascending fb
                       (T.pa1 T.Cons T.cn1 T.mkNoSrcPos p T.aCons fa)
                       fxs
                       p))
                 (T.fatal p))
        hsequences fxs p = T.fromExpList T.mkNoSrcPos p [fxs]
        gdescending pdescending p
          = T.ufun3 c158v5v160v46descending pdescending p
              hdescending
        adescending = c158v5v160v46descending
        hdescending fa fas
          z3descending@(T.R (T.Cons fb fbs) _) p
          = T.ucguard
              (T.uap2 T.mkNoSrcPos p ((!==) T.mkNoSrcPos p)
                 (T.uap2 T.mkNoSrcPos p fcmp fa fb)
                 (T.con0 T.mkNoSrcPos p GT aGT))
              (T.uwrapForward p
                 (hdescending fb
                    (T.con2 T.mkNoSrcPos p T.Cons T.aCons fa fas)
                    fbs
                    p))
              (y1descending fa fas z3descending p)
        hdescending fa fas z3descending p
          = y1descending fa fas z3descending p
        y1descending fa fas fbs p
          = T.con2 T.mkNoSrcPos p T.Cons T.aCons
              (T.con2 T.mkNoSrcPos p T.Cons T.aCons fa fas)
              (T.uwrapForward p (hsequences fbs p))
        gascending pascending p
          = T.ufun3 c162v5v164v46ascending pascending p
              hascending
        aascending = c162v5v164v46ascending
        hascending fa fas z3ascending@(T.R (T.Cons fb fbs) _)
          p
          = T.ucguard
              (T.uap2 T.mkNoSrcPos p ((!/=) T.mkNoSrcPos p)
                 (T.uap2 T.mkNoSrcPos p fcmp fa fb)
                 (T.con0 T.mkNoSrcPos p GT aGT))
              (T.uwrapForward p
                 (hascending fb
                    (T.ufun1 T.mkLambda T.mkNoSrcPos p
                       (\ fys p ->
                          T.uap1 T.mkNoSrcPos p
                            (T.projection T.mkNoSrcPos p fas)
                            (T.con2 T.mkNoSrcPos p T.Cons T.aCons fa fys)))
                    fbs
                    p))
              (y1ascending fa fas z3ascending p)
        hascending fa fas z3ascending p
          = y1ascending fa fas z3ascending p
        y1ascending fa fas fbs p
          = T.con2 T.mkNoSrcPos p T.Cons T.aCons
              (T.uap1 T.mkNoSrcPos p fas
                 (T.fromExpList T.mkNoSrcPos p [fa]))
              (T.uwrapForward p (hsequences fbs p))
        gmergeAll pmergeAll p
          = T.ufun1 c166v5v167v43mergeAll pmergeAll p hmergeAll
        amergeAll = c166v5v167v43mergeAll
        hmergeAll (T.R (T.Cons fx (T.R T.Nil _)) _) p
          = T.projection T.mkNoSrcPos p fx
        hmergeAll fxs p
          = T.uwrapForward p
              (hmergeAll (T.uwrapForward p (hmergePairs fxs p)) p)
        gmergePairs pmergePairs p
          = T.ufun1 c169v5v170v28mergePairs pmergePairs p
              hmergePairs
        amergePairs = c169v5v170v28mergePairs
        hmergePairs
          (T.R (T.Cons fa (T.R (T.Cons fb fxs) _)) _) p
          = T.con2 T.mkNoSrcPos p T.Cons T.aCons
              (T.uwrapForward p (hmerge fa fb p))
              (T.uwrapForward p (hmergePairs fxs p))
        hmergePairs fxs p = T.projection T.mkNoSrcPos p fxs
        gmerge pmerge p
          = T.ufun2 c172v5v176v28merge pmerge p hmerge
        amerge = c172v5v176v28merge
        hmerge fas@(T.R (T.Cons fa fas') _)
          fbs@(T.R (T.Cons fb fbs') _) p
          = T.ucguard
              (T.uap2 T.mkNoSrcPos p ((!==) T.mkNoSrcPos p)
                 (T.uap2 T.mkNoSrcPos p fcmp fa fb)
                 (T.con0 T.mkNoSrcPos p GT aGT))
              (T.con2 T.mkNoSrcPos p T.Cons T.aCons fb
                 (T.uwrapForward p (hmerge fas fbs' p)))
              (T.ucguard (gotherwise T.mkNoSrcPos p)
                 (T.con2 T.mkNoSrcPos p T.Cons T.aCons fa
                    (T.uwrapForward p (hmerge fas' fbs p)))
                 (T.fatal p))
        hmerge (T.R T.Nil _) fbs p
          = T.projection T.mkNoSrcPos p fbs
        hmerge fas (T.R T.Nil _) p
          = T.projection T.mkNoSrcPos p fas
        hmerge _ _ p = T.fatal p
 
ginsert ::
          (Ord a) =>
          T.RefSrcPos ->
            T.RefExp ->
              T.R (T.Fun a (T.Fun (T.List a) (T.List a)))
 
sinsert ::
          (Ord a) =>
          T.R (T.Fun a (T.Fun (T.List a) (T.List a)))
ginsert pinsert p = T.uconstUse pinsert p sinsert
sinsert
  = T.uconstDef p ainsert
      (\ p ->
         T.uap1 T.mkNoSrcPos p (ginsertBy T.mkNoSrcPos p)
           (gcompare T.mkNoSrcPos p))
 
ginsertBy ::
          T.RefSrcPos ->
            T.RefExp ->
              T.R
                (T.Fun (T.Fun a (T.Fun a Ordering))
                   (T.Fun a (T.Fun (T.List a) (T.List a))))
 
hinsertBy ::
          T.R (T.Fun a (T.Fun a Ordering)) ->
            T.R a -> T.R (T.List a) -> T.RefExp -> T.R (T.List a)
ginsertBy pinsertBy p
  = T.ufun3 ainsertBy pinsertBy p hinsertBy
hinsertBy fcmp fx (T.R T.Nil _) p
  = T.fromExpList T.mkNoSrcPos p [fx]
hinsertBy fcmp fx fys@(T.R (T.Cons fy fys') _) p
  = T.uccase T.mkNoSrcPos p
      (let v184v28v188v0v1 (T.R GT _) p
             = T.con2 T.mkNoSrcPos p T.Cons T.aCons fy
                 (T.uwrapForward p (hinsertBy fcmp fx fys' p))
           v184v28v188v0v1 _ p
             = T.con2 T.mkNoSrcPos p T.Cons T.aCons fx fys
         in v184v28v188v0v1)
      (T.uap2 T.mkNoSrcPos p fcmp fx fy)
hinsertBy _ _ _ p = T.fatal p
 
gmaximumBy ::
           T.RefSrcPos ->
             T.RefExp ->
               T.R
                 (T.Fun (T.Fun a (T.Fun a Ordering))
                    (T.Fun (T.List a) a))
 
hmaximumBy ::
           T.R (T.Fun a (T.Fun a Ordering)) ->
             T.R (T.List a) -> T.RefExp -> T.R a
gmaximumBy pmaximumBy p
  = T.ufun2 amaximumBy pmaximumBy p hmaximumBy
hmaximumBy fcmp (T.R T.Nil _) p
  = T.uwrapForward p
      (herror
         (T.fromLitString T.mkNoSrcPos p
            "List.maximumBy: empty list")
         p)
hmaximumBy fcmp fxs p
  = T.uwrapForward p
      (hfoldl1 (gmax T.mkNoSrcPos p) fxs p)
  where gmax pmax p
          = T.ufun2 c192v28v196v0max pmax p hmax
        amax = c192v28v196v0max
        hmax fx fy p
          = T.uccase T.mkNoSrcPos p
              (let v192v38v196v0v1 (T.R GT _) p
                     = T.projection T.mkNoSrcPos p fx
                   v192v38v196v0v1 _ p = T.projection T.mkNoSrcPos p fy
                 in v192v38v196v0v1)
              (T.uap2 T.mkNoSrcPos p fcmp fx fy)
 
gminimumBy ::
           T.RefSrcPos ->
             T.RefExp ->
               T.R
                 (T.Fun (T.Fun a (T.Fun a Ordering))
                    (T.Fun (T.List a) a))
 
hminimumBy ::
           T.R (T.Fun a (T.Fun a Ordering)) ->
             T.R (T.List a) -> T.RefExp -> T.R a
gminimumBy pminimumBy p
  = T.ufun2 aminimumBy pminimumBy p hminimumBy
hminimumBy fcmp (T.R T.Nil _) p
  = T.uwrapForward p
      (herror
         (T.fromLitString T.mkNoSrcPos p
            "List.minimumBy: empty list")
         p)
hminimumBy fcmp fxs p
  = T.uwrapForward p
      (hfoldl1 (gmin T.mkNoSrcPos p) fxs p)
  where gmin pmin p
          = T.ufun2 c200v28v204v0min pmin p hmin
        amin = c200v28v204v0min
        hmin fx fy p
          = T.uccase T.mkNoSrcPos p
              (let v200v38v204v0v1 (T.R GT _) p
                     = T.projection T.mkNoSrcPos p fy
                   v200v38v204v0v1 _ p = T.projection T.mkNoSrcPos p fx
                 in v200v38v204v0v1)
              (T.uap2 T.mkNoSrcPos p fcmp fx fy)
 
ggenericLength ::
                 (Integral a) =>
                 T.RefSrcPos -> T.RefExp -> T.R (T.Fun (T.List b) a)
 
hgenericLength ::
                 (Integral a) => T.R (T.List b) -> T.RefExp -> T.R a
ggenericLength pgenericLength p
  = T.ufun1 agenericLength pgenericLength p
      hgenericLength
hgenericLength (T.R T.Nil _) p
  = T.uap1 T.mkNoSrcPos p
      (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
      (T.conInteger T.mkNoSrcPos p (0))
hgenericLength (T.R (T.Cons fx fxs) _) p
  = T.uap2 T.mkNoSrcPos p ((!+) T.mkNoSrcPos p)
      (T.uap1 T.mkNoSrcPos p
         (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
         (T.conInteger T.mkNoSrcPos p (1)))
      (T.uwrapForward p (hgenericLength fxs p))
hgenericLength _ p = T.fatal p
 
ggenericTake ::
               (Integral a) =>
               T.RefSrcPos ->
                 T.RefExp ->
                   T.R (T.Fun a (T.Fun (T.List b) (T.List b)))
 
hgenericTake ::
               (Integral a) =>
               T.R a -> T.R (T.List b) -> T.RefExp -> T.R (T.List b)
ggenericTake pgenericTake p
  = T.ufun2 agenericTake pgenericTake p hgenericTake
hgenericTake _ (T.R T.Nil _) p
  = T.con0 T.mkNoSrcPos p T.Nil T.aNil
hgenericTake fv210v13v210v13n v210v15v210v15n p
  = T.ucguard
      (T.uap2 T.mkNoSrcPos p
         ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
         fv210v13v210v13n
         (T.uap1 T.mkNoSrcPos p
            (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p (0))))
      (h210v1v210v29n v210v15v210v15n p)
      (y1genericTake fv210v13v210v13n v210v15v210v15n p)
  where h210v1v210v29n _ p
          = T.con0 T.mkNoSrcPos p T.Nil T.aNil
        h210v1v210v29n _ p
          = y1genericTake fv210v13v210v13n v210v15v210v15n p
hgenericTake fv210v13v210v13n v210v15v210v15n p
  = y1genericTake fv210v13v210v13n v210v15v210v15n p
y1genericTake fn (T.R (T.Cons fx fxs) _) p
  = T.ucguard
      (T.uap2 T.mkNoSrcPos p ((!>) T.mkNoSrcPos p) fn
         (T.uap1 T.mkNoSrcPos p
            (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p (0))))
      (T.con2 T.mkNoSrcPos p T.Cons T.aCons fx
         (T.uwrapForward p
            (hgenericTake
               (T.uap2 T.mkNoSrcPos p ((!-) T.mkNoSrcPos p) fn
                  (T.uap1 T.mkNoSrcPos p
                     (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                     (T.conInteger T.mkNoSrcPos p (1))))
               fxs
               p)))
      (T.ucguard (gotherwise T.mkNoSrcPos p)
         (T.uwrapForward p
            (herror
               (T.fromLitString T.mkNoSrcPos p
                  "List.genericTake: negative argument")
               p))
         (T.fatal p))
y1genericTake _ _ p = T.fatal p
 
ggenericDrop ::
               (Integral a) =>
               T.RefSrcPos ->
                 T.RefExp ->
                   T.R (T.Fun a (T.Fun (T.List b) (T.List b)))
 
hgenericDrop ::
               (Integral a) =>
               T.R a -> T.R (T.List b) -> T.RefExp -> T.R (T.List b)
ggenericDrop pgenericDrop p
  = T.ufun2 agenericDrop pgenericDrop p hgenericDrop
hgenericDrop fv216v13v216v13n fxs p
  = T.ucguard
      (T.uap2 T.mkNoSrcPos p
         ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
         fv216v13v216v13n
         (T.uap1 T.mkNoSrcPos p
            (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p (0))))
      (h216v1v216v29n fxs p)
      (y1genericDrop fv216v13v216v13n fxs p)
  where h216v1v216v29n fxs p
          = T.projection T.mkNoSrcPos p fxs
        h216v1v216v29n _ p
          = y1genericDrop fv216v13v216v13n fxs p
hgenericDrop fv216v13v216v13n fxs p
  = y1genericDrop fv216v13v216v13n fxs p
y1genericDrop _ (T.R T.Nil _) p
  = T.con0 T.mkNoSrcPos p T.Nil T.aNil
y1genericDrop fn (T.R (T.Cons _ fxs) _) p
  = T.ucguard
      (T.uap2 T.mkNoSrcPos p ((!>) T.mkNoSrcPos p) fn
         (T.uap1 T.mkNoSrcPos p
            (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p (0))))
      (T.uwrapForward p
         (hgenericDrop
            (T.uap2 T.mkNoSrcPos p ((!-) T.mkNoSrcPos p) fn
               (T.uap1 T.mkNoSrcPos p
                  (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                  (T.conInteger T.mkNoSrcPos p (1))))
            fxs
            p))
      (T.ucguard (gotherwise T.mkNoSrcPos p)
         (T.uwrapForward p
            (herror
               (T.fromLitString T.mkNoSrcPos p
                  "List.genericDrop: negative argument")
               p))
         (T.fatal p))
y1genericDrop _ _ p = T.fatal p
 
ggenericSplitAt ::
                  (Integral a) =>
                  T.RefSrcPos ->
                    T.RefExp ->
                      T.R
                        (T.Fun a
                           (T.Fun (T.List b) (T.Tuple2 (T.List b) (T.List b))))
 
hgenericSplitAt ::
                  (Integral a) =>
                  T.R a ->
                    T.R (T.List b) ->
                      T.RefExp -> T.R (T.Tuple2 (T.List b) (T.List b))
ggenericSplitAt pgenericSplitAt p
  = T.ufun2 agenericSplitAt pgenericSplitAt p
      hgenericSplitAt
hgenericSplitAt fv223v16v223v16n fxs p
  = T.ucguard
      (T.uap2 T.mkNoSrcPos p
         ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
         fv223v16v223v16n
         (T.uap1 T.mkNoSrcPos p
            (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p (0))))
      (h223v1v223v34n fxs p)
      (y1genericSplitAt fv223v16v223v16n fxs p)
  where h223v1v223v34n fxs p
          = T.con2 T.mkNoSrcPos p T.Tuple2 T.aTuple2
              (T.con0 T.mkNoSrcPos p T.Nil T.aNil)
              fxs
        h223v1v223v34n _ p
          = y1genericSplitAt fv223v16v223v16n fxs p
hgenericSplitAt fv223v16v223v16n fxs p
  = y1genericSplitAt fv223v16v223v16n fxs p
y1genericSplitAt _ (T.R T.Nil _) p
  = T.con2 T.mkNoSrcPos p T.Tuple2 T.aTuple2
      (T.con0 T.mkNoSrcPos p T.Nil T.aNil)
      (T.con0 T.mkNoSrcPos p T.Nil T.aNil)
y1genericSplitAt fn (T.R (T.Cons fx fxs) _) p
  = T.ucguard
      (T.uap2 T.mkNoSrcPos p ((!>) T.mkNoSrcPos p) fn
         (T.uap1 T.mkNoSrcPos p
            (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p (0))))
      (T.con2 T.mkNoSrcPos p T.Tuple2 T.aTuple2
         (T.con2 T.mkNoSrcPos p T.Cons T.aCons fx
            (gxs' T.mkNoSrcPos p))
         (gxs'' T.mkNoSrcPos p))
      (T.ucguard (gotherwise T.mkNoSrcPos p)
         (T.uwrapForward p
            (herror
               (T.fromLitString T.mkNoSrcPos p
                  "List.genericSplitAt: negative argument")
               p))
         (T.fatal p))
  where gxs' pxs' p = T.uconstUse pxs' p sxs'
        gxs'' pxs'' p = T.uconstUse pxs'' p sxs''
        sxs'
          = T.uconstDef p c228v14v228v50xs'
              (\ _ ->
                 case j228v14v228v50xs' of
                     (kxs', fxs', fxs'') -> fxs')
        sxs''
          = T.uconstDef p c228v14v228v50xs''
              (\ _ ->
                 case j228v14v228v50xs' of
                     (kxs', fxs', fxs'') -> fxs'')
        j228v14v228v50xs'
          = case
              T.uwrapForward p
                (hgenericSplitAt
                   (T.uap2 T.mkNoSrcPos p ((!-) T.mkNoSrcPos p) fn
                      (T.uap1 T.mkNoSrcPos p
                         (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                         (T.conInteger T.mkNoSrcPos p (1))))
                   fxs
                   p)
              of
                T.R (T.Tuple2 fxs' fxs'') kxs' -> (kxs', fxs', fxs'')
                _ -> T.fatal p
y1genericSplitAt _ _ p = T.fatal p
 
ggenericIndex ::
                (Integral a) =>
                T.RefSrcPos ->
                  T.RefExp -> T.R (T.Fun (T.List b) (T.Fun a b))
 
hgenericIndex ::
                (Integral a) =>
                T.R (T.List b) -> T.R a -> T.RefExp -> T.R b
ggenericIndex pgenericIndex p
  = T.ufun2 agenericIndex pgenericIndex p hgenericIndex
hgenericIndex z1genericIndex@(T.R (T.Cons fx _) _)
  fv231v21v231v21n p
  = T.ucguard
      (T.uap2 T.mkNoSrcPos p
         ((Hat.PreludeBasic.!==) T.mkNoSrcPos p)
         fv231v21v231v21n
         (T.uap1 T.mkNoSrcPos p
            (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p (0))))
      (h231v1v231v28n p)
      (y1genericIndex z1genericIndex fv231v21v231v21n p)
  where h231v1v231v28n p
          = T.projection T.mkNoSrcPos p fx
        h231v1v231v28n p
          = y1genericIndex z1genericIndex fv231v21v231v21n p
hgenericIndex z1genericIndex fv231v21v231v21n p
  = y1genericIndex z1genericIndex fv231v21v231v21n p
y1genericIndex (T.R (T.Cons _ fxs) _) fn p
  = T.ucguard
      (T.uap2 T.mkNoSrcPos p ((!>) T.mkNoSrcPos p) fn
         (T.uap1 T.mkNoSrcPos p
            (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p (0))))
      (T.uwrapForward p
         (hgenericIndex fxs
            (T.uap2 T.mkNoSrcPos p ((!-) T.mkNoSrcPos p) fn
               (T.uap1 T.mkNoSrcPos p
                  (Hat.PreludeBasic.gfromInteger T.mkNoSrcPos p)
                  (T.conInteger T.mkNoSrcPos p (1))))
            p))
      (T.ucguard (gotherwise T.mkNoSrcPos p)
         (T.uwrapForward p
            (herror
               (T.fromLitString T.mkNoSrcPos p
                  "List.genericIndex: negative argument")
               p))
         (T.fatal p))
y1genericIndex _ _ p
  = T.uwrapForward p
      (herror
         (T.fromLitString T.mkNoSrcPos p
            "List.genericIndex: index too large")
         p)
 
ggenericReplicate ::
                    (Integral a) =>
                    T.RefSrcPos ->
                      T.RefExp -> T.R (T.Fun a (T.Fun b (T.List b)))
 
hgenericReplicate ::
                    (Integral a) =>
                    T.R a -> T.R b -> T.RefExp -> T.R (T.List b)
ggenericReplicate pgenericReplicate p
  = T.ufun2 agenericReplicate pgenericReplicate p
      hgenericReplicate
hgenericReplicate fn fx p
  = T.uwrapForward p
      (hgenericTake fn (T.uwrapForward p (hrepeat fx p)) p)
 
gzip4 ::
      T.RefSrcPos ->
        T.RefExp ->
          T.R
            (T.Fun (T.List a)
               (T.Fun (T.List b)
                  (T.Fun (T.List c)
                     (T.Fun (T.List d) (T.List (T.Tuple4 a b c d))))))
 
szip4 ::
      T.R
        (T.Fun (T.List a)
           (T.Fun (T.List b)
              (T.Fun (T.List c)
                 (T.Fun (T.List d) (T.List (T.Tuple4 a b c d))))))
gzip4 pzip4 p = T.uconstUse pzip4 p szip4
szip4
  = T.uconstDef p azip4
      (\ p ->
         T.uap1 T.mkNoSrcPos p (gzipWith4 T.mkNoSrcPos p)
           (T.pa0 T.Tuple4 T.cn4 T.mkNoSrcPos p T.aTuple4))
 
gzip5 ::
      T.RefSrcPos ->
        T.RefExp ->
          T.R
            (T.Fun (T.List a)
               (T.Fun (T.List b)
                  (T.Fun (T.List c)
                     (T.Fun (T.List d)
                        (T.Fun (T.List e) (T.List (T.Tuple5 a b c d e)))))))
 
szip5 ::
      T.R
        (T.Fun (T.List a)
           (T.Fun (T.List b)
              (T.Fun (T.List c)
                 (T.Fun (T.List d)
                    (T.Fun (T.List e) (T.List (T.Tuple5 a b c d e)))))))
gzip5 pzip5 p = T.uconstUse pzip5 p szip5
szip5
  = T.uconstDef p azip5
      (\ p ->
         T.uap1 T.mkNoSrcPos p (gzipWith5 T.mkNoSrcPos p)
           (T.pa0 T.Tuple5 T.cn5 T.mkNoSrcPos p T.aTuple5))
 
gzip6 ::
      T.RefSrcPos ->
        T.RefExp ->
          T.R
            (T.Fun (T.List a)
               (T.Fun (T.List b)
                  (T.Fun (T.List c)
                     (T.Fun (T.List d)
                        (T.Fun (T.List e)
                           (T.Fun (T.List f)
                              (T.List (T.Tuple6 a b c d e f))))))))
 
szip6 ::
      T.R
        (T.Fun (T.List a)
           (T.Fun (T.List b)
              (T.Fun (T.List c)
                 (T.Fun (T.List d)
                    (T.Fun (T.List e)
                       (T.Fun (T.List f)
                          (T.List (T.Tuple6 a b c d e f))))))))
gzip6 pzip6 p = T.uconstUse pzip6 p szip6
szip6
  = T.uconstDef p azip6
      (\ p ->
         T.uap1 T.mkNoSrcPos p (gzipWith6 T.mkNoSrcPos p)
           (T.pa0 T.Tuple6 T.cn6 T.mkNoSrcPos p T.aTuple6))
 
gzip7 ::
      T.RefSrcPos ->
        T.RefExp ->
          T.R
            (T.Fun (T.List a)
               (T.Fun (T.List b)
                  (T.Fun (T.List c)
                     (T.Fun (T.List d)
                        (T.Fun (T.List e)
                           (T.Fun (T.List f)
                              (T.Fun (T.List g)
                                 (T.List (T.Tuple7 a b c d e f g)))))))))
 
szip7 ::
      T.R
        (T.Fun (T.List a)
           (T.Fun (T.List b)
              (T.Fun (T.List c)
                 (T.Fun (T.List d)
                    (T.Fun (T.List e)
                       (T.Fun (T.List f)
                          (T.Fun (T.List g)
                             (T.List (T.Tuple7 a b c d e f g)))))))))
gzip7 pzip7 p = T.uconstUse pzip7 p szip7
szip7
  = T.uconstDef p azip7
      (\ p ->
         T.uap1 T.mkNoSrcPos p (gzipWith7 T.mkNoSrcPos p)
           (T.pa0 T.Tuple7 T.cn7 T.mkNoSrcPos p T.aTuple7))
 
gzipWith4 ::
          T.RefSrcPos ->
            T.RefExp ->
              T.R
                (T.Fun (T.Fun a (T.Fun b (T.Fun c (T.Fun d e))))
                   (T.Fun (T.List a)
                      (T.Fun (T.List b)
                         (T.Fun (T.List c) (T.Fun (T.List d) (T.List e))))))
 
hzipWith4 ::
          T.R (T.Fun a (T.Fun b (T.Fun c (T.Fun d e)))) ->
            T.R (T.List a) ->
              T.R (T.List b) ->
                T.R (T.List c) ->
                  T.R (T.List d) -> T.RefExp -> T.R (T.List e)
gzipWith4 pzipWith4 p
  = T.ufun5 azipWith4 pzipWith4 p hzipWith4
hzipWith4 fz (T.R (T.Cons fa fas) _)
  (T.R (T.Cons fb fbs) _) (T.R (T.Cons fc fcs) _)
  (T.R (T.Cons fd fds) _) p
  = T.con2 T.mkNoSrcPos p T.Cons T.aCons
      (T.uap4 T.mkNoSrcPos p fz fa fb fc fd)
      (T.uwrapForward p (hzipWith4 fz fas fbs fcs fds p))
hzipWith4 _ _ _ _ _ p
  = T.con0 T.mkNoSrcPos p T.Nil T.aNil
 
gzipWith5 ::
          T.RefSrcPos ->
            T.RefExp ->
              T.R
                (T.Fun
                   (T.Fun a (T.Fun b (T.Fun c (T.Fun d (T.Fun e f)))))
                   (T.Fun (T.List a)
                      (T.Fun (T.List b)
                         (T.Fun (T.List c)
                            (T.Fun (T.List d) (T.Fun (T.List e) (T.List f)))))))
 
hzipWith5 ::
          T.R
            (T.Fun a (T.Fun b (T.Fun c (T.Fun d (T.Fun e f)))))
            ->
            T.R (T.List a) ->
              T.R (T.List b) ->
                T.R (T.List c) ->
                  T.R (T.List d) ->
                    T.R (T.List e) -> T.RefExp -> T.R (T.List f)
gzipWith5 pzipWith5 p
  = T.ufun6 azipWith5 pzipWith5 p hzipWith5
hzipWith5 fz (T.R (T.Cons fa fas) _)
  (T.R (T.Cons fb fbs) _) (T.R (T.Cons fc fcs) _)
  (T.R (T.Cons fd fds) _) (T.R (T.Cons fe fes) _) p
  = T.con2 T.mkNoSrcPos p T.Cons T.aCons
      (T.uap5 T.mkNoSrcPos p fz fa fb fc fd fe)
      (T.uap6 T.mkNoSrcPos p (gzipWith5 T.mkNoSrcPos p) fz
         fas
         fbs
         fcs
         fds
         fes)
hzipWith5 _ _ _ _ _ _ p
  = T.con0 T.mkNoSrcPos p T.Nil T.aNil
 
gzipWith6 ::
          T.RefSrcPos ->
            T.RefExp ->
              T.R
                (T.Fun
                   (T.Fun a
                      (T.Fun b (T.Fun c (T.Fun d (T.Fun e (T.Fun f g))))))
                   (T.Fun (T.List a)
                      (T.Fun (T.List b)
                         (T.Fun (T.List c)
                            (T.Fun (T.List d)
                               (T.Fun (T.List e)
                                  (T.Fun (T.List f) (T.List g))))))))
 
hzipWith6 ::
          T.R
            (T.Fun a
               (T.Fun b (T.Fun c (T.Fun d (T.Fun e (T.Fun f g))))))
            ->
            T.R (T.List a) ->
              T.R (T.List b) ->
                T.R (T.List c) ->
                  T.R (T.List d) ->
                    T.R (T.List e) ->
                      T.R (T.List f) -> T.RefExp -> T.R (T.List g)
gzipWith6 pzipWith6 p
  = T.ufun7 azipWith6 pzipWith6 p hzipWith6
hzipWith6 fz (T.R (T.Cons fa fas) _)
  (T.R (T.Cons fb fbs) _) (T.R (T.Cons fc fcs) _)
  (T.R (T.Cons fd fds) _) (T.R (T.Cons fe fes) _)
  (T.R (T.Cons ff ffs) _) p
  = T.con2 T.mkNoSrcPos p T.Cons T.aCons
      (T.uap6 T.mkNoSrcPos p fz fa fb fc fd fe ff)
      (T.uap7 T.mkNoSrcPos p (gzipWith6 T.mkNoSrcPos p) fz
         fas
         fbs
         fcs
         fds
         fes
         ffs)
hzipWith6 _ _ _ _ _ _ _ p
  = T.con0 T.mkNoSrcPos p T.Nil T.aNil
 
gzipWith7 ::
          T.RefSrcPos ->
            T.RefExp ->
              T.R
                (T.Fun
                   (T.Fun a
                      (T.Fun b
                         (T.Fun c (T.Fun d (T.Fun e (T.Fun f (T.Fun g h)))))))
                   (T.Fun (T.List a)
                      (T.Fun (T.List b)
                         (T.Fun (T.List c)
                            (T.Fun (T.List d)
                               (T.Fun (T.List e)
                                  (T.Fun (T.List f)
                                     (T.Fun (T.List g) (T.List h)))))))))
 
hzipWith7 ::
          T.R
            (T.Fun a
               (T.Fun b
                  (T.Fun c (T.Fun d (T.Fun e (T.Fun f (T.Fun g h)))))))
            ->
            T.R (T.List a) ->
              T.R (T.List b) ->
                T.R (T.List c) ->
                  T.R (T.List d) ->
                    T.R (T.List e) ->
                      T.R (T.List f) ->
                        T.R (T.List g) -> T.RefExp -> T.R (T.List h)
gzipWith7 pzipWith7 p
  = T.ufun8 azipWith7 pzipWith7 p hzipWith7
hzipWith7 fz (T.R (T.Cons fa fas) _)
  (T.R (T.Cons fb fbs) _) (T.R (T.Cons fc fcs) _)
  (T.R (T.Cons fd fds) _) (T.R (T.Cons fe fes) _)
  (T.R (T.Cons ff ffs) _) (T.R (T.Cons fg fgs) _) p
  = T.con2 T.mkNoSrcPos p T.Cons T.aCons
      (T.uap7 T.mkNoSrcPos p fz fa fb fc fd fe ff fg)
      (T.uap8 T.mkNoSrcPos p (gzipWith7 T.mkNoSrcPos p) fz
         fas
         fbs
         fcs
         fds
         fes
         ffs
         fgs)
hzipWith7 _ _ _ _ _ _ _ _ p
  = T.con0 T.mkNoSrcPos p T.Nil T.aNil
 
gunzip4 ::
        T.RefSrcPos ->
          T.RefExp ->
            T.R
              (T.Fun (T.List (T.Tuple4 a b c d))
                 (T.Tuple4 (T.List a) (T.List b) (T.List c)
                    (T.List d)))
 
sunzip4 ::
        T.R
          (T.Fun (T.List (T.Tuple4 a b c d))
             (T.Tuple4 (T.List a) (T.List b) (T.List c)
                (T.List d)))
gunzip4 punzip4 p = T.uconstUse punzip4 p sunzip4
sunzip4
  = T.uconstDef p aunzip4
      (\ p ->
         T.uap2 T.mkNoSrcPos p (gfoldr T.mkNoSrcPos p)
           (T.ufun2 T.mkLambda T.mkNoSrcPos p
              (\ (T.R (T.Tuple4 fa fb fc fd) _)
                 (T.R ~(T.Tuple4 fas fbs fcs fds) _) p ->
                 T.con4 T.mkNoSrcPos p T.Tuple4 T.aTuple4
                   (T.con2 T.mkNoSrcPos p T.Cons T.aCons fa fas)
                   (T.con2 T.mkNoSrcPos p T.Cons T.aCons fb fbs)
                   (T.con2 T.mkNoSrcPos p T.Cons T.aCons fc fcs)
                   (T.con2 T.mkNoSrcPos p T.Cons T.aCons fd fds)))
           (T.con4 T.mkNoSrcPos p T.Tuple4 T.aTuple4
              (T.con0 T.mkNoSrcPos p T.Nil T.aNil)
              (T.con0 T.mkNoSrcPos p T.Nil T.aNil)
              (T.con0 T.mkNoSrcPos p T.Nil T.aNil)
              (T.con0 T.mkNoSrcPos p T.Nil T.aNil)))
 
gunzip5 ::
        T.RefSrcPos ->
          T.RefExp ->
            T.R
              (T.Fun (T.List (T.Tuple5 a b c d e))
                 (T.Tuple5 (T.List a) (T.List b) (T.List c) (T.List d)
                    (T.List e)))
 
sunzip5 ::
        T.R
          (T.Fun (T.List (T.Tuple5 a b c d e))
             (T.Tuple5 (T.List a) (T.List b) (T.List c) (T.List d)
                (T.List e)))
gunzip5 punzip5 p = T.uconstUse punzip5 p sunzip5
sunzip5
  = T.uconstDef p aunzip5
      (\ p ->
         T.uap2 T.mkNoSrcPos p (gfoldr T.mkNoSrcPos p)
           (T.ufun2 T.mkLambda T.mkNoSrcPos p
              (\ (T.R (T.Tuple5 fa fb fc fd fe) _)
                 (T.R ~(T.Tuple5 fas fbs fcs fds fes) _) p ->
                 T.con5 T.mkNoSrcPos p T.Tuple5 T.aTuple5
                   (T.con2 T.mkNoSrcPos p T.Cons T.aCons fa fas)
                   (T.con2 T.mkNoSrcPos p T.Cons T.aCons fb fbs)
                   (T.con2 T.mkNoSrcPos p T.Cons T.aCons fc fcs)
                   (T.con2 T.mkNoSrcPos p T.Cons T.aCons fd fds)
                   (T.con2 T.mkNoSrcPos p T.Cons T.aCons fe fes)))
           (T.con5 T.mkNoSrcPos p T.Tuple5 T.aTuple5
              (T.con0 T.mkNoSrcPos p T.Nil T.aNil)
              (T.con0 T.mkNoSrcPos p T.Nil T.aNil)
              (T.con0 T.mkNoSrcPos p T.Nil T.aNil)
              (T.con0 T.mkNoSrcPos p T.Nil T.aNil)
              (T.con0 T.mkNoSrcPos p T.Nil T.aNil)))
 
gunzip6 ::
        T.RefSrcPos ->
          T.RefExp ->
            T.R
              (T.Fun (T.List (T.Tuple6 a b c d e f))
                 (T.Tuple6 (T.List a) (T.List b) (T.List c) (T.List d)
                    (T.List e)
                    (T.List f)))
 
sunzip6 ::
        T.R
          (T.Fun (T.List (T.Tuple6 a b c d e f))
             (T.Tuple6 (T.List a) (T.List b) (T.List c) (T.List d)
                (T.List e)
                (T.List f)))
gunzip6 punzip6 p = T.uconstUse punzip6 p sunzip6
sunzip6
  = T.uconstDef p aunzip6
      (\ p ->
         T.uap2 T.mkNoSrcPos p (gfoldr T.mkNoSrcPos p)
           (T.ufun2 T.mkLambda T.mkNoSrcPos p
              (\ (T.R (T.Tuple6 fa fb fc fd fe ff) _)
                 (T.R ~(T.Tuple6 fas fbs fcs fds fes ffs) _) p ->
                 T.con6 T.mkNoSrcPos p T.Tuple6 T.aTuple6
                   (T.con2 T.mkNoSrcPos p T.Cons T.aCons fa fas)
                   (T.con2 T.mkNoSrcPos p T.Cons T.aCons fb fbs)
                   (T.con2 T.mkNoSrcPos p T.Cons T.aCons fc fcs)
                   (T.con2 T.mkNoSrcPos p T.Cons T.aCons fd fds)
                   (T.con2 T.mkNoSrcPos p T.Cons T.aCons fe fes)
                   (T.con2 T.mkNoSrcPos p T.Cons T.aCons ff ffs)))
           (T.con6 T.mkNoSrcPos p T.Tuple6 T.aTuple6
              (T.con0 T.mkNoSrcPos p T.Nil T.aNil)
              (T.con0 T.mkNoSrcPos p T.Nil T.aNil)
              (T.con0 T.mkNoSrcPos p T.Nil T.aNil)
              (T.con0 T.mkNoSrcPos p T.Nil T.aNil)
              (T.con0 T.mkNoSrcPos p T.Nil T.aNil)
              (T.con0 T.mkNoSrcPos p T.Nil T.aNil)))
 
gunzip7 ::
        T.RefSrcPos ->
          T.RefExp ->
            T.R
              (T.Fun (T.List (T.Tuple7 a b c d e f g))
                 (T.Tuple7 (T.List a) (T.List b) (T.List c) (T.List d)
                    (T.List e)
                    (T.List f)
                    (T.List g)))
 
sunzip7 ::
        T.R
          (T.Fun (T.List (T.Tuple7 a b c d e f g))
             (T.Tuple7 (T.List a) (T.List b) (T.List c) (T.List d)
                (T.List e)
                (T.List f)
                (T.List g)))
gunzip7 punzip7 p = T.uconstUse punzip7 p sunzip7
sunzip7
  = T.uconstDef p aunzip7
      (\ p ->
         T.uap2 T.mkNoSrcPos p (gfoldr T.mkNoSrcPos p)
           (T.ufun2 T.mkLambda T.mkNoSrcPos p
              (\ (T.R (T.Tuple7 fa fb fc fd fe ff fg) _)
                 (T.R ~(T.Tuple7 fas fbs fcs fds fes ffs fgs) _) p ->
                 T.con7 T.mkNoSrcPos p T.Tuple7 T.aTuple7
                   (T.con2 T.mkNoSrcPos p T.Cons T.aCons fa fas)
                   (T.con2 T.mkNoSrcPos p T.Cons T.aCons fb fbs)
                   (T.con2 T.mkNoSrcPos p T.Cons T.aCons fc fcs)
                   (T.con2 T.mkNoSrcPos p T.Cons T.aCons fd fds)
                   (T.con2 T.mkNoSrcPos p T.Cons T.aCons fe fes)
                   (T.con2 T.mkNoSrcPos p T.Cons T.aCons ff ffs)
                   (T.con2 T.mkNoSrcPos p T.Cons T.aCons fg fgs)))
           (T.con7 T.mkNoSrcPos p T.Tuple7 T.aTuple7
              (T.con0 T.mkNoSrcPos p T.Nil T.aNil)
              (T.con0 T.mkNoSrcPos p T.Nil T.aNil)
              (T.con0 T.mkNoSrcPos p T.Nil T.aNil)
              (T.con0 T.mkNoSrcPos p T.Nil T.aNil)
              (T.con0 T.mkNoSrcPos p T.Nil T.aNil)
              (T.con0 T.mkNoSrcPos p T.Nil T.aNil)
              (T.con0 T.mkNoSrcPos p T.Nil T.aNil)))
adelete
  = T.mkVariable tList 560001 560040 3 (0) "delete"
      Prelude.False
adeleteBy
  = T.mkVariable tList 590001 600071 3 (3) "deleteBy"
      Prelude.False
adeleteFirstsBy
  = T.mkVariable tList 660001 660053 3 (1)
      "deleteFirstsBy"
      Prelude.False
aelemIndex
  = T.mkVariable tList 340001 340043 3 (1) "elemIndex"
      Prelude.False
aelemIndices
  = T.mkVariable tList 370001 370045 3 (1)
      "elemIndices"
      Prelude.False
afind
  = T.mkVariable tList 400001 400049 3 (1) "find"
      Prelude.False
afindIndex
  = T.mkVariable tList 430001 430054 3 (1) "findIndex"
      Prelude.False
afindIndices
  = T.mkVariable tList 460001 460061 3 (2)
      "findIndices"
      Prelude.False
agenericDrop
  = T.mkVariable tList 2160001 2200070 3 (2)
      "genericDrop"
      Prelude.False
agenericIndex
  = T.mkVariable tList 2310001 2350069 3 (2)
      "genericIndex"
      Prelude.False
agenericLength
  = T.mkVariable tList 2050001 2060047 3 (1)
      "genericLength"
      Prelude.False
agenericReplicate
  = T.mkVariable tList 2380001 2380051 3 (2)
      "genericReplicate"
      Prelude.False
agenericSplitAt
  = T.mkVariable tList 2230001 2300000 3 (2)
      "genericSplitAt"
      Prelude.False
agenericTake
  = T.mkVariable tList 2090001 2130070 3 (2)
      "genericTake"
      Prelude.False
agroup
  = T.mkVariable tList 1030001 1030039 3 (0) "group"
      Prelude.False
agroupBy
  = T.mkVariable tList 1060001 1120000 3 (2) "groupBy"
      Prelude.False
ainits
  = T.mkVariable tList 1130001 1140054 3 (1) "inits"
      Prelude.False
ainsert
  = T.mkVariable tList 1790001 1790042 3 (0) "insert"
      Prelude.False
ainsertBy
  = T.mkVariable tList 1820001 1880000 3 (3) "insertBy"
      Prelude.False
aintersect
  = T.mkVariable tList 750001 750043 3 (0) "intersect"
      Prelude.False
aintersectBy
  = T.mkVariable tList 780001 780055 3 (3)
      "intersectBy"
      Prelude.False
aintersperse
  = T.mkVariable tList 810001 830055 3 (2)
      "intersperse"
      Prelude.False
aisPrefixOf
  = T.mkVariable tList 1230001 1250054 3 (2)
      "isPrefixOf"
      Prelude.False
aisSuffixOf
  = T.mkVariable tList 1280001 1280059 3 (2)
      "isSuffixOf"
      Prelude.False
amapAccumL
  = T.mkVariable tList 1310001 1360000 3 (3)
      "mapAccumL"
      Prelude.False
amapAccumR
  = T.mkVariable tList 1370001 1420000 3 (3)
      "mapAccumR"
      Prelude.False
amaximumBy
  = T.mkVariable tList 1890001 1960000 3 (2)
      "maximumBy"
      Prelude.False
aminimumBy
  = T.mkVariable tList 1970001 2040000 3 (2)
      "minimumBy"
      Prelude.False
anub
  = T.mkVariable tList 490001 490037 3 (0) "nub"
      Prelude.False
anubBy
  = T.mkVariable tList 520001 530072 3 (2) "nubBy"
      Prelude.False
apartition
  = T.mkVariable tList 970001 970061 3 (2) "partition"
      Prelude.False
asort
  = T.mkVariable tList 1480001 1480041 3 (0) "sort"
      Prelude.False
asortBy
  = T.mkVariable tList 1510001 1520007 3 (1) "sortBy"
      Prelude.False
atails
  = T.mkVariable tList 1190001 1200041 3 (1) "tails"
      Prelude.False
atranspose
  = T.mkVariable tList 910001 940062 3 (1) "transpose"
      Prelude.False
aunfoldr
  = T.mkVariable tList 1430001 1470000 3 (2) "unfoldr"
      Prelude.False
aunion
  = T.mkVariable tList 690001 690039 3 (0) "union"
      Prelude.False
aunionBy
  = T.mkVariable tList 720001 720067 3 (3) "unionBy"
      Prelude.False
aunzip4
  = T.mkVariable tList 2780001 2800046 3 (0) "unzip4"
      Prelude.False
aunzip5
  = T.mkVariable tList 2830001 2850049 3 (0) "unzip5"
      Prelude.False
aunzip6
  = T.mkVariable tList 2880001 2900052 3 (0) "unzip6"
      Prelude.False
aunzip7
  = T.mkVariable tList 2930001 2950047 3 (0) "unzip7"
      Prelude.False
azip4
  = T.mkVariable tList 2410001 2410041 3 (0) "zip4"
      Prelude.False
azip5
  = T.mkVariable tList 2440001 2440042 3 (0) "zip5"
      Prelude.False
azip6
  = T.mkVariable tList 2480001 2480043 3 (0) "zip6"
      Prelude.False
azip7
  = T.mkVariable tList 2520001 2520044 3 (0) "zip7"
      Prelude.False
azipWith4
  = T.mkVariable tList 2550001 2570029 3 (5) "zipWith4"
      Prelude.False
azipWith5
  = T.mkVariable tList 2610001 2630029 3 (6) "zipWith5"
      Prelude.False
azipWith6
  = T.mkVariable tList 2670001 2690029 3 (7) "zipWith6"
      Prelude.False
azipWith7
  = T.mkVariable tList 2730001 2750029 3 (8) "zipWith7"
      Prelude.False
(+\\)
  = T.mkVariable tList 630001 630046 20 (0) "\\\\"
      Prelude.False
c108v34v108v57ys
  = T.mkVariable tList 1080034 1080057 3 (0) "ys"
      Prelude.True
c108v34v108v57zs
  = T.mkVariable tList 1080034 1080057 3 (0) "zs"
      Prelude.True
c133v34v133v49s'
  = T.mkVariable tList 1330034 1330049 3 (0) "s'"
      Prelude.True
c134v34v134v61s''
  = T.mkVariable tList 1340034 1340061 3 (0) "s''"
      Prelude.True
c133v34v133v49y
  = T.mkVariable tList 1330034 1330049 3 (0) "y"
      Prelude.True
c134v34v134v61ys
  = T.mkVariable tList 1340034 1340061 3 (0) "ys"
      Prelude.True
c140v34v140v60s'
  = T.mkVariable tList 1400034 1400060 3 (0) "s'"
      Prelude.True
c139v34v139v50s''
  = T.mkVariable tList 1390034 1390050 3 (0) "s''"
      Prelude.True
c139v34v139v50y
  = T.mkVariable tList 1390034 1390050 3 (0) "y"
      Prelude.True
c140v34v140v60ys
  = T.mkVariable tList 1400034 1400060 3 (0) "ys"
      Prelude.True
c162v5v164v46ascending
  = T.mkVariable tList 1620005 1640046 3 (3)
      "ascending"
      Prelude.True
c158v5v160v46descending
  = T.mkVariable tList 1580005 1600046 3 (3)
      "descending"
      Prelude.True
c172v5v176v28merge
  = T.mkVariable tList 1720005 1760028 3 (2) "merge"
      Prelude.True
c166v5v167v43mergeAll
  = T.mkVariable tList 1660005 1670043 3 (1) "mergeAll"
      Prelude.True
c169v5v170v28mergePairs
  = T.mkVariable tList 1690005 1700028 3 (1)
      "mergePairs"
      Prelude.True
c153v5v156v23sequences
  = T.mkVariable tList 1530005 1560023 3 (1)
      "sequences"
      Prelude.True
c192v28v196v0max
  = T.mkVariable tList 1920028 1960000 3 (2) "max"
      Prelude.True
c200v28v204v0min
  = T.mkVariable tList 2000028 2040000 3 (2) "min"
      Prelude.True
c228v14v228v50xs'
  = T.mkVariable tList 2280014 2280050 3 (0) "xs'"
      Prelude.True
c228v14v228v50xs''
  = T.mkVariable tList 2280014 2280050 3 (0) "xs''"
      Prelude.True
p = T.mkRoot
tList = T.mkModule "List" "List.hs" Prelude.False