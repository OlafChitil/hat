module Hat.Data.List
       ((!++), (+++), (*++), ghead, ahead, hhead, glast,
        alast, hlast, gtail, atail, htail, ginit, ainit,
        hinit, gnull, anull, hnull, glength, alength,
        hlength, gmap, amap, hmap, greverse, gintersperse,
        aintersperse, hintersperse, gintercalate,
        aintercalate, hintercalate, gtranspose, atranspose,
        htranspose, gsubsequences, asubsequences,
        hsubsequences, gpermutations, apermutations,
        hpermutations, gfoldl, afoldl, hfoldl, gfoldl',
        afoldl', hfoldl', gfoldl1, afoldl1, hfoldl1,
        gfoldl1', afoldl1', hfoldl1', gfoldr, afoldr, hfoldr,
        gfoldr1, afoldr1, hfoldr1, gconcat, aconcat, hconcat,
        gconcatMap, aconcatMap, hconcatMap, gand, gor, gany,
        aany, hany, gall, aall, hall, gsum, gproduct,
        gmaximum, amaximum, hmaximum, gminimum, aminimum,
        hminimum, gscanl, ascanl, hscanl, gscanl1, ascanl1,
        hscanl1, gscanr, ascanr, hscanr, gscanr1, ascanr1,
        hscanr1, gmapAccumL, amapAccumL, hmapAccumL,
        gmapAccumR, amapAccumR, hmapAccumR, giterate,
        aiterate, hiterate, grepeat, arepeat, hrepeat,
        greplicate, areplicate, hreplicate, gcycle, acycle,
        hcycle, gunfoldr, aunfoldr, hunfoldr, gtake, atake,
        htake, gdrop, adrop, hdrop, gsplitAt, asplitAt,
        hsplitAt, gtakeWhile, atakeWhile, htakeWhile,
        gdropWhile, adropWhile, hdropWhile, gspan, aspan,
        hspan, gbreak, abreak, hbreak, gstripPrefix,
        astripPrefix, hstripPrefix, ggroup, ginits, ainits,
        hinits, gtails, atails, htails, gisPrefixOf,
        aisPrefixOf, hisPrefixOf, gisSuffixOf, aisSuffixOf,
        hisSuffixOf, gisInfixOf, aisInfixOf, hisInfixOf,
        gelem, aelem, helem, gnotElem, anotElem, hnotElem,
        glookup, alookup, hlookup, gfind, afind, hfind,
        gfilter, afilter, hfilter, gpartition, apartition,
        hpartition, (!!!), (+!!), (*!!), gelemIndex,
        aelemIndex, helemIndex, gelemIndices, aelemIndices,
        helemIndices, gfindIndex, afindIndex, hfindIndex,
        gfindIndices, afindIndices, hfindIndices, gzip,
        gzip3, gzip4, gzip5, gzip6, gzip7, gzipWith,
        azipWith, hzipWith, gzipWith3, azipWith3, hzipWith3,
        gzipWith4, azipWith4, hzipWith4, gzipWith5,
        azipWith5, hzipWith5, gzipWith6, azipWith6,
        hzipWith6, gzipWith7, azipWith7, hzipWith7, gunzip,
        gunzip3, gunzip4, gunzip5, gunzip6, gunzip7, glines,
        alines, hlines, gwords, awords, hwords, gunlines,
        gunwords, aunwords, hunwords, gnub, gdelete, (!\\),
        gunion, gintersect, gsort, ginsert, gnubBy, anubBy,
        hnubBy, gdeleteBy, adeleteBy, hdeleteBy,
        gdeleteFirstsBy, adeleteFirstsBy, hdeleteFirstsBy,
        gunionBy, aunionBy, hunionBy, gintersectBy,
        aintersectBy, hintersectBy, ggroupBy, agroupBy,
        hgroupBy, gsortBy, asortBy, hsortBy, ginsertBy,
        ainsertBy, hinsertBy, gmaximumBy, amaximumBy,
        hmaximumBy, gminimumBy, aminimumBy, hminimumBy,
        ggenericLength, agenericLength, hgenericLength,
        ggenericTake, agenericTake, hgenericTake,
        ggenericDrop, agenericDrop, hgenericDrop,
        ggenericSplitAt, agenericSplitAt, hgenericSplitAt,
        ggenericIndex, agenericIndex, hgenericIndex,
        ggenericReplicate, agenericReplicate,
        hgenericReplicate)
       where
import qualified Prelude
import qualified Hat.Hat as T
import qualified Hat.PreludeBasic
import qualified Hat.PreludeBuiltinTypes
import Hat.Prelude
import Hat.List
 
gintercalate ::
             T.RefSrcPos ->
               T.RefExp ->
                 T.R
                   (T.Fun (T.List a)
                      (T.Fun (T.List (T.List a)) (T.List a)))
 
hintercalate ::
             T.R (T.List a) ->
               T.R (T.List (T.List a)) -> T.RefExp -> T.R (T.List a)
gintercalate pintercalate p
  = T.ufun2 aintercalate pintercalate p hintercalate
hintercalate fxs fxss p
  = T.uwrapForward p
      (hconcat (T.uwrapForward p (hintersperse fxs fxss p))
         p)
 
gsubsequences ::
              T.RefSrcPos ->
                T.RefExp ->
                  T.R (T.Fun (T.List a) (T.List (T.List a)))
 
hsubsequences ::
              T.R (T.List a) -> T.RefExp -> T.R (T.List (T.List a))
gsubsequences psubsequences p
  = T.ufun1 asubsequences psubsequences p hsubsequences
hsubsequences fxs p
  = T.con2 T.mkNoSrcPos p T.Cons T.aCons
      (T.con0 T.mkNoSrcPos p T.Nil T.aNil)
      (T.uwrapForward p (hnonEmptySubsequences fxs p))
 
gnonEmptySubsequences ::
                      T.RefSrcPos ->
                        T.RefExp ->
                          T.R (T.Fun (T.List a) (T.List (T.List a)))
 
hnonEmptySubsequences ::
                      T.R (T.List a) -> T.RefExp -> T.R (T.List (T.List a))
gnonEmptySubsequences pnonEmptySubsequences p
  = T.ufun1 anonEmptySubsequences pnonEmptySubsequences
      p
      hnonEmptySubsequences
hnonEmptySubsequences (T.R T.Nil _) p
  = T.con0 T.mkNoSrcPos p T.Nil T.aNil
hnonEmptySubsequences (T.R (T.Cons fx fxs) _) p
  = T.con2 T.mkNoSrcPos p T.Cons T.aCons
      (T.fromExpList T.mkNoSrcPos p [fx])
      (T.uwrapForward p
         (hfoldr (gf T.mkNoSrcPos p)
            (T.con0 T.mkNoSrcPos p T.Nil T.aNil)
            (T.uwrapForward p (hnonEmptySubsequences fxs p))
            p))
  where gf pf p = T.ufun2 c42v9v42v34f pf p hf
        af = c42v9v42v34f
        hf fys fr p
          = T.con2 T.mkNoSrcPos p T.Cons T.aCons fys
              (T.con2 T.mkNoSrcPos p T.Cons T.aCons
                 (T.con2 T.mkNoSrcPos p T.Cons T.aCons fx fys)
                 fr)
hnonEmptySubsequences _ p = T.fatal p
 
gpermutations ::
              T.RefSrcPos ->
                T.RefExp ->
                  T.R (T.Fun (T.List a) (T.List (T.List a)))
 
hpermutations ::
              T.R (T.List a) -> T.RefExp -> T.R (T.List (T.List a))
gpermutations ppermutations p
  = T.ufun1 apermutations ppermutations p hpermutations
hpermutations fxs0 p
  = T.con2 T.mkNoSrcPos p T.Cons T.aCons fxs0
      (T.uwrapForward p
         (hperms fxs0 (T.con0 T.mkNoSrcPos p T.Nil T.aNil) p))
  where gperms pperms p
          = T.ufun2 c50v5v58v0perms pperms p hperms
        aperms = c50v5v58v0perms
        hperms (T.R T.Nil _) _ p
          = T.con0 T.mkNoSrcPos p T.Nil T.aNil
        hperms (T.R (T.Cons ft fts) _) fis p
          = T.uwrapForward p
              (hfoldr (ginterleave T.mkNoSrcPos p)
                 (T.uwrapForward p
                    (hperms fts
                       (T.con2 T.mkNoSrcPos p T.Cons T.aCons ft fis)
                       p))
                 (T.uwrapForward p (hpermutations fis p))
                 p)
          where ginterleave pinterleave p
                  = T.ufun2 c52v13v52v75interleave pinterleave p
                      hinterleave
                ainterleave = c52v13v52v75interleave
                hinterleave fxs fr p
                  = let gzs pzs p = T.uconstUse pzs p szs
                        szs
                          = T.uconstDef p c52v42v52v69zs
                              (\ _ ->
                                 case j52v42v52v69zs of
                                     (kzs, fzs) -> fzs)
                        j52v42v52v69zs
                          = case
                              T.uwrapForward p
                                (hinterleave' (gid T.mkNoSrcPos p) fxs fr p)
                              of
                                T.R (T.Tuple2 _ fzs) kzs -> (kzs, fzs)
                                _ -> T.fatal p
                      in gzs T.mkNoSrcPos p
                ginterleave' pinterleave' p
                  = T.ufun3 c53v13v55v64interleave' pinterleave' p
                      hinterleave'
                ainterleave' = c53v13v55v64interleave'
                hinterleave' _ (T.R T.Nil _) fr p
                  = T.con2 T.mkNoSrcPos p T.Tuple2 T.aTuple2 fts fr
                hinterleave' ff (T.R (T.Cons fy fys) _) fr p
                  = let gus pus p = T.uconstUse pus p sus
                        gzs pzs p = T.uconstUse pzs p szs
                        sus
                          = T.uconstDef p c54v42v54v78us
                              (\ _ ->
                                 case j54v42v54v78us of
                                     (kus, fus, fzs) -> fus)
                        szs
                          = T.uconstDef p c54v42v54v78zs
                              (\ _ ->
                                 case j54v42v54v78us of
                                     (kus, fus, fzs) -> fzs)
                        j54v42v54v78us
                          = case
                              T.uwrapForward p
                                (hinterleave'
                                   (T.uap2 T.mkNoSrcPos p ((!.) T.mkNoSrcPos p)
                                      ff
                                      (T.pa1 T.Cons T.cn1 T.mkNoSrcPos p T.aCons
                                         fy))
                                   fys
                                   fr
                                   p)
                              of
                                T.R (T.Tuple2 fus fzs) kus -> (kus, fus, fzs)
                                _ -> T.fatal p
                      in
                      T.con2 T.mkNoSrcPos p T.Tuple2 T.aTuple2
                        (T.con2 T.mkNoSrcPos p T.Cons T.aCons fy
                           (gus T.mkNoSrcPos p))
                        (T.con2 T.mkNoSrcPos p T.Cons T.aCons
                           (T.uap1 T.mkNoSrcPos p ff
                              (T.con2 T.mkNoSrcPos p T.Cons T.aCons ft
                                 (T.con2 T.mkNoSrcPos p T.Cons T.aCons fy
                                    (gus T.mkNoSrcPos p))))
                           (gzs T.mkNoSrcPos p))
                hinterleave' _ _ _ p = T.fatal p
        hperms _ _ p = T.fatal p
 
gfoldl' ::
        T.RefSrcPos ->
          T.RefExp ->
            T.R
              (T.Fun (T.Fun a (T.Fun b a))
                 (T.Fun a (T.Fun (T.List b) a)))
 
hfoldl' ::
        T.R (T.Fun a (T.Fun b a)) ->
          T.R a -> T.R (T.List b) -> T.RefExp -> T.R a
gfoldl' pfoldl' p = T.ufun3 afoldl' pfoldl' p hfoldl'
hfoldl' ff fa (T.R T.Nil _) p
  = T.projection T.mkNoSrcPos p fa
hfoldl' ff fa (T.R (T.Cons fx fxs) _) p
  = let ga' pa' p = T.uconstUse pa' p sa'
        sa'
          = T.uconstDef p c60v25v60v34a'
              (\ p ->
                 T.uap2 T.mkNoSrcPos p
                   (T.projection T.mkNoSrcPos p ff)
                   fa
                   fx)
      in
      T.uwrapForward p
        (hseq (ga' T.mkNoSrcPos p)
           (T.uwrapForward p
              (hfoldl' ff (ga' T.mkNoSrcPos p) fxs p))
           p)
hfoldl' _ _ _ p = T.fatal p
 
gfoldl1' ::
         T.RefSrcPos ->
           T.RefExp ->
             T.R
               (T.Fun (T.Fun a (T.Fun a a)) (T.Fun (T.List a) a))
 
hfoldl1' ::
         T.R (T.Fun a (T.Fun a a)) ->
           T.R (T.List a) -> T.RefExp -> T.R a
gfoldl1' pfoldl1' p
  = T.ufun2 afoldl1' pfoldl1' p hfoldl1'
hfoldl1' ff (T.R (T.Cons fx fxs) _) p
  = T.uwrapForward p (hfoldl' ff fx fxs p)
hfoldl1' _ (T.R T.Nil _) p
  = T.uwrapForward p
      (herror
         (T.fromLitString T.mkNoSrcPos p
            "Prelude.foldl1': empty list")
         p)
hfoldl1' _ _ p = T.fatal p
 
gstripPrefix ::
               (Eq a) =>
               T.RefSrcPos ->
                 T.RefExp ->
                   T.R
                     (T.Fun (T.List a)
                        (T.Fun (T.List a) (Maybe (T.List a))))
 
hstripPrefix ::
               (Eq a) =>
               T.R (T.List a) ->
                 T.R (T.List a) -> T.RefExp -> T.R (Maybe (T.List a))
gstripPrefix pstripPrefix p
  = T.ufun2 astripPrefix pstripPrefix p hstripPrefix
hstripPrefix (T.R T.Nil _) fys p
  = T.con1 T.mkNoSrcPos p Just aJust fys
hstripPrefix z1stripPrefix@(T.R (T.Cons fx fxs) _)
  z2stripPrefix@(T.R (T.Cons fy fys) _) p
  = T.ucguard
      (T.uap2 T.mkNoSrcPos p ((!==) T.mkNoSrcPos p) fx fy)
      (T.uwrapForward p (hstripPrefix fxs fys p))
      (y1stripPrefix z1stripPrefix z2stripPrefix p)
hstripPrefix z1stripPrefix z2stripPrefix p
  = y1stripPrefix z1stripPrefix z2stripPrefix p
y1stripPrefix _ _ p
  = T.con0 T.mkNoSrcPos p Nothing aNothing
 
gisInfixOf ::
             (Eq a) =>
             T.RefSrcPos ->
               T.RefExp ->
                 T.R (T.Fun (T.List a) (T.Fun (T.List a) Bool))
 
hisInfixOf ::
             (Eq a) =>
             T.R (T.List a) ->
               T.R (T.List a) -> T.RefExp -> T.R Bool
gisInfixOf pisInfixOf p
  = T.ufun2 aisInfixOf pisInfixOf p hisInfixOf
hisInfixOf fneedle fhaystack p
  = T.uap1 T.mkNoSrcPos p
      (T.uwrapForward p
         (hany
            (T.uap1 T.mkNoSrcPos p (gisPrefixOf T.mkNoSrcPos p)
               fneedle)
            p))
      (T.uwrapForward p (htails fhaystack p))
afoldl'
  = T.mkVariable tList 590001 600061 3 (3) "foldl'"
      Prelude.False
afoldl1'
  = T.mkVariable tList 640001 650063 3 (2) "foldl1'"
      Prelude.False
aintercalate
  = T.mkVariable tList 270001 270048 3 (2)
      "intercalate"
      Prelude.False
aisInfixOf
  = T.mkVariable tList 900001 900068 3 (2) "isInfixOf"
      Prelude.False
anonEmptySubsequences
  = T.mkVariable tList 400001 470000 3 (1)
      "nonEmptySubsequences"
      Prelude.False
apermutations
  = T.mkVariable tList 480001 490007 3 (1)
      "permutations"
      Prelude.False
astripPrefix
  = T.mkVariable tList 760001 790025 3 (2)
      "stripPrefix"
      Prelude.False
asubsequences
  = T.mkVariable tList 330001 330055 3 (1)
      "subsequences"
      Prelude.False
c42v9v42v34f
  = T.mkVariable tList 420009 420034 3 (2) "f"
      Prelude.True
c50v5v58v0perms
  = T.mkVariable tList 500005 580000 3 (2) "perms"
      Prelude.True
c52v13v52v75interleave
  = T.mkVariable tList 520013 520075 3 (2) "interleave"
      Prelude.True
c53v13v55v64interleave'
  = T.mkVariable tList 530013 550064 3 (3)
      "interleave'"
      Prelude.True
c52v42v52v69zs
  = T.mkVariable tList 520042 520069 3 (0) "zs"
      Prelude.True
c54v42v54v78us
  = T.mkVariable tList 540042 540078 3 (0) "us"
      Prelude.True
c54v42v54v78zs
  = T.mkVariable tList 540042 540078 3 (0) "zs"
      Prelude.True
c60v25v60v34a'
  = T.mkVariable tList 600025 600034 3 (0) "a'"
      Prelude.True
p = T.mkRoot
tList
  = T.mkModule "Data.List" "Data/List.hs" Prelude.False