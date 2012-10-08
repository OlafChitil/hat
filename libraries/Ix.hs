module Ix ( Ix(range, index, inRange, rangeSize) ) where

class Ord a => Ix a  where
    range     :: (a,a) -> [a]
    index     :: (a,a) -> a -> Int
    inRange   :: (a,a) -> a -> Bool
    rangeSize :: (a,a) -> Int

    rangeSize b@(l,h) | null (range b) = 0
                      | otherwise      = index b h + 1 
-- NB: replacing "null (range b)" by  "not (l <= h)"
-- fails if the bounds are tuples.  For example,
--  (1,2) <= (2,1)
-- but the range is nevertheless empty
-- range ((1,2),(2,1)) = []

instance  Ix Char  where
    range (m,n) = [m..n]
    index b@(c,c') ci
        | inRange b ci  =  fromEnum ci - fromEnum c
        | otherwise     =  error "Ix.index: Index out of range."
    inRange (c,c') i    =  c <= i && i <= c'

instance  Ix Int  where
    range (m,n) = [m..n]
    index b@(m,n) i
        | inRange b i   =  i - m
        | otherwise     =  error "Ix.index: Index out of range."
    inRange (m,n) i     =  m <= i && i <= n

instance  Ix Integer  where
    range (m,n) = [m..n]
    index b@(m,n) i
        | inRange b i   =  fromInteger (i - m)
        | otherwise     =  error "Ix.index: Index out of range."
    inRange (m,n) i     =  m <= i && i <= n

instance Ix Bool where
  range (c,c') 	= [c .. c']

  index b@(c,c') ci
	| inRange b ci = fromEnum ci - fromEnum c
	| True         = error "Ix.Bool.index: Index out of range."
  inRange (c,c') ci    = c <= ci && ci <= c'  
  -- Assuming Enum and Ord agree on order for Bool

instance Ix Ordering where
  range (c,c') 	= [c .. c']

  index b@(c,c') ci
	| inRange b ci = fromEnum ci - fromEnum c
	| True         = error "Ix.Ordering.index: Index out of range."
  inRange (c,c') ci    = c <= ci && ci <= c'  
  -- Assuming Enum and Ord agree on order for Ordering

instance Ix () where
  range ((),()) = [()]
  index ((),()) () = 0
  inRange ((),()) () = True

instance  (Ix a, Ix b)  => Ix (a,b) where
         range ((l,l'),(u,u'))
                 = [(i,i') | i <- range (l,u), i' <- range (l',u')]
         index ((l,l'),(u,u')) (i,i')
                 =  index (l,u) i * rangeSize (l',u') + index (l',u') i'
         inRange ((l,l'),(u,u')) (i,i')
                 = inRange (l,u) i && inRange (l',u') i'

instance  (Ix a1, Ix a2, Ix a3) => Ix (a1,a2,a3)  where
    range ((l1,l2,l3),(u1,u2,u3)) =
          [(i1,i2,i3) | i1 <- range (l1,u1),
                        i2 <- range (l2,u2),
                        i3 <- range (l3,u3)]

    index ((l1,l2,l3),(u1,u2,u3)) (i1,i2,i3) =
        index (l3,u3) i3 + rangeSize (l3,u3) * (
         index (l2,u2) i2 + rangeSize (l2,u2) * (
           index (l1,u1) i1))

    inRange ((l1,l2,l3),(u1,u2,u3)) (i1,i2,i3) =
         inRange (l1,u1) i1
           && inRange (l2,u2) i2
           && inRange (l3,u3) i3

instance  (Ix a1, Ix a2, Ix a3, Ix a4) => Ix (a1,a2,a3,a4)  where
    range ((l1,l2,l3,l4),(u1,u2,u3,u4)) =
          [(i1,i2,i3,i4) | i1 <- range (l1,u1),
                           i2 <- range (l2,u2),
                           i3 <- range (l3,u3),
                           i4 <- range (l4,u4)]

    index ((l1,l2,l3,l4),(u1,u2,u3,u4)) (i1,i2,i3,i4) =
        index (l4,u4) i4 + rangeSize (l4,u4) * (
         index (l3,u3) i3 + rangeSize (l3,u3) * (
           index (l2,u2) i2 + rangeSize (l2,u2) * (
             index (l1,u1) i1)))

    inRange ((l1,l2,l3,l4),(u1,u2,u3,u4)) (i1,i2,i3,i4) =
         inRange (l1,u1) i1
           && inRange (l2,u2) i2
           && inRange (l3,u3) i3
           && inRange (l4,u4) i4

instance  (Ix a1, Ix a2, Ix a3, Ix a4, Ix a5) =>
	 Ix (a1,a2,a3,a4,a5)  where
    range ((l1,l2,l3,l4,l5),(u1,u2,u3,u4,u5)) =
          [(i1,i2,i3,i4,i5) | i1 <- range (l1,u1),
                              i2 <- range (l2,u2),
                              i3 <- range (l3,u3),
                              i4 <- range (l4,u4),
                              i5 <- range (l5,u5)]

    index ((l1,l2,l3,l4,l5),(u1,u2,u3,u4,u5)) (i1,i2,i3,i4,i5) =
        index (l5,u5) i5 + rangeSize (l5,u5) * (
         index (l4,u4) i4 + rangeSize (l4,u4) * (
          index (l3,u3) i3 + rangeSize (l3,u3) * (
           index (l2,u2) i2 + rangeSize (l2,u2) * (
            index (l1,u1) i1))))

    inRange ((l1,l2,l3,l4,l5),(u1,u2,u3,u4,u5)) (i1,i2,i3,i4,i5) =
         inRange (l1,u1) i1
           && inRange (l2,u2) i2
           && inRange (l3,u3) i3
           && inRange (l4,u4) i4
           && inRange (l5,u5) i5

instance  (Ix a1, Ix a2, Ix a3, Ix a4, Ix a5, Ix a6) =>
	 Ix (a1,a2,a3,a4,a5,a6)  where
    range ((l1,l2,l3,l4,l5,l6),(u1,u2,u3,u4,u5,u6)) =
          [(i1,i2,i3,i4,i5,i6) | i1 <- range (l1,u1),
                                 i2 <- range (l2,u2),
                                 i3 <- range (l3,u3),
                                 i4 <- range (l4,u4),
                                 i5 <- range (l5,u5),
                                 i6 <- range (l6,u6)]

    index ((l1,l2,l3,l4,l5,l6),(u1,u2,u3,u4,u5,u6)) (i1,i2,i3,i4,i5,i6) =
        index (l6,u6) i6 + rangeSize (l6,u6) * (
         index (l5,u5) i5 + rangeSize (l5,u5) * (
          index (l4,u4) i4 + rangeSize (l4,u4) * (
           index (l3,u3) i3 + rangeSize (l3,u3) * (
            index (l2,u2) i2 + rangeSize (l2,u2) * (
             index (l1,u1) i1)))))

    inRange ((l1,l2,l3,l4,l5,l6),(u1,u2,u3,u4,u5,u6)) (i1,i2,i3,i4,i5,i6) =
         inRange (l1,u1) i1
           && inRange (l2,u2) i2
           && inRange (l3,u3) i3
           && inRange (l4,u4) i4
           && inRange (l5,u5) i5
           && inRange (l6,u6) i6

instance  (Ix a1, Ix a2, Ix a3, Ix a4, Ix a5, Ix a6, Ix a7) =>
	 Ix (a1,a2,a3,a4,a5,a6,a7)  where
    range ((l1,l2,l3,l4,l5,l6,l7),(u1,u2,u3,u4,u5,u6,u7)) =
          [(i1,i2,i3,i4,i5,i6,i7) | i1 <- range (l1,u1),
                                    i2 <- range (l2,u2),
                                    i3 <- range (l3,u3),
                                    i4 <- range (l4,u4),
                                    i5 <- range (l5,u5),
                                    i6 <- range (l6,u6),
                                    i7 <- range (l7,u7)]

    index ((l1,l2,l3,l4,l5,l6,l7),(u1,u2,u3,u4,u5,u6,u7)) (i1,i2,i3,i4,i5,i6,i7) =
       index (l7,u7) i7 + rangeSize (l7,u7) * (
        index (l6,u6) i6 + rangeSize (l6,u6) * (
         index (l5,u5) i5 + rangeSize (l5,u5) * (
          index (l4,u4) i4 + rangeSize (l4,u4) * (
           index (l3,u3) i3 + rangeSize (l3,u3) * (
            index (l2,u2) i2 + rangeSize (l2,u2) * (
             index (l1,u1) i1))))))

    inRange ((l1,l2,l3,l4,l5,l6,l7),(u1,u2,u3,u4,u5,u6,u7)) (i1,i2,i3,i4,i5,i6,i7) =
         inRange (l1,u1) i1
           && inRange (l2,u2) i2
           && inRange (l3,u3) i3
           && inRange (l4,u4) i4
           && inRange (l5,u5) i5
           && inRange (l6,u6) i6
           && inRange (l7,u7) i7

instance  (Ix a1, Ix a2, Ix a3, Ix a4, Ix a5, Ix a6, Ix a7, Ix a8) =>
	 Ix (a1,a2,a3,a4,a5,a6,a7,a8)  where
    range ((l1,l2,l3,l4,l5,l6,l7,l8),(u1,u2,u3,u4,u5,u6,u7,u8)) =
          [(i1,i2,i3,i4,i5,i6,i7,i8) | i1 <- range (l1,u1),
                                    i2 <- range (l2,u2),
                                    i3 <- range (l3,u3),
                                    i4 <- range (l4,u4),
                                    i5 <- range (l5,u5),
                                    i6 <- range (l6,u6),
                                    i7 <- range (l7,u7),
                                    i8 <- range (l8,u8)]

    index ((l1,l2,l3,l4,l5,l6,l7,l8),(u1,u2,u3,u4,u5,u6,u7,u8))
          (i1,i2,i3,i4,i5,i6,i7,i8) =
       index (l8,u8) i8 + rangeSize (l8,u8) * (
        index (l7,u7) i7 + rangeSize (l7,u7) * (
         index (l6,u6) i6 + rangeSize (l6,u6) * (
          index (l5,u5) i5 + rangeSize (l5,u5) * (
           index (l4,u4) i4 + rangeSize (l4,u4) * (
            index (l3,u3) i3 + rangeSize (l3,u3) * (
             index (l2,u2) i2 + rangeSize (l2,u2) * (
              index (l1,u1) i1)))))))

    inRange ((l1,l2,l3,l4,l5,l6,l7,l8),(u1,u2,u3,u4,u5,u6,u7,u8))
            (i1,i2,i3,i4,i5,i6,i7,i8) =
         inRange (l1,u1) i1
           && inRange (l2,u2) i2
           && inRange (l3,u3) i3
           && inRange (l4,u4) i4
           && inRange (l5,u5) i5
           && inRange (l6,u6) i6
           && inRange (l7,u7) i7
           && inRange (l8,u8) i8

instance  (Ix a1, Ix a2, Ix a3, Ix a4, Ix a5, Ix a6, Ix a7, Ix a8, Ix a9) =>
	 Ix (a1,a2,a3,a4,a5,a6,a7,a8,a9)  where
    range ((l1,l2,l3,l4,l5,l6,l7,l8,l9),(u1,u2,u3,u4,u5,u6,u7,u8,u9)) =
          [(i1,i2,i3,i4,i5,i6,i7,i8,i9)
			 | i1 <- range (l1,u1),
                           i2 <- range (l2,u2),
                           i3 <- range (l3,u3),
                           i4 <- range (l4,u4),
                           i5 <- range (l5,u5),
                           i6 <- range (l6,u6),
                           i7 <- range (l7,u7),
                           i8 <- range (l8,u8),
                           i9 <- range (l9,u9)]

    index ((l1,l2,l3,l4,l5,l6,l7,l8,l9),(u1,u2,u3,u4,u5,u6,u7,u8,u9))
          (i1,i2,i3,i4,i5,i6,i7,i8,i9) =
            index (l9,u9) i9 + rangeSize (l9,u9) * (
             index (l8,u8) i8 + rangeSize (l8,u8) * (
              index (l7,u7) i7 + rangeSize (l7,u7) * (
               index (l6,u6) i6 + rangeSize (l6,u6) * (
                index (l5,u5) i5 + rangeSize (l5,u5) * (
                 index (l4,u4) i4 + rangeSize (l4,u4) * (
                  index (l3,u3) i3 + rangeSize (l3,u3) * (
                   index (l2,u2) i2 + rangeSize (l2,u2) * (
                    index (l1,u1) i1))))))))

    inRange ((l1,l2,l3,l4,l5,l6,l7,l8,l9),(u1,u2,u3,u4,u5,u6,u7,u8,u9))
            (i1,i2,i3,i4,i5,i6,i7,i8,i9) =
         inRange (l1,u1) i1
           && inRange (l2,u2) i2
           && inRange (l3,u3) i3
           && inRange (l4,u4) i4
           && inRange (l5,u5) i5
           && inRange (l6,u6) i6
           && inRange (l7,u7) i7
           && inRange (l8,u8) i8
           && inRange (l9,u9) i9

instance (Ix a1, Ix a2, Ix a3, Ix a4, Ix a5, Ix a6, Ix a7, Ix a8, Ix a9
	 ,Ix a10) =>
	 Ix (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10)  where
    range ((l1,l2,l3,l4,l5,l6,l7,l8,l9,l10)
	  ,(u1,u2,u3,u4,u5,u6,u7,u8,u9,u10)) =
          [(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10)
			 | i1 <- range (l1,u1),
                           i2 <- range (l2,u2),
                           i3 <- range (l3,u3),
                           i4 <- range (l4,u4),
                           i5 <- range (l5,u5),
                           i6 <- range (l6,u6),
                           i7 <- range (l7,u7),
                           i8 <- range (l8,u8),
                           i9 <- range (l9,u9),
                           i10 <- range (l10,u10)]

    index ((l1,l2,l3,l4,l5,l6,l7,l8,l9,l10)
	  ,(u1,u2,u3,u4,u5,u6,u7,u8,u9,u10))
          (i1,i2,i3,i4,i5,i6,i7,i8,i9,i10) =
           index (l10,u10) i10 + rangeSize (l10,u10) * (
            index (l9,u9) i9 + rangeSize (l9,u9) * (
             index (l8,u8) i8 + rangeSize (l8,u8) * (
              index (l7,u7) i7 + rangeSize (l7,u7) * (
               index (l6,u6) i6 + rangeSize (l6,u6) * (
                index (l5,u5) i5 + rangeSize (l5,u5) * (
                 index (l4,u4) i4 + rangeSize (l4,u4) * (
                  index (l3,u3) i3 + rangeSize (l3,u3) * (
                   index (l2,u2) i2 + rangeSize (l2,u2) * (
                    index (l1,u1) i1)))))))))

    inRange ((l1,l2,l3,l4,l5,l6,l7,l8,l9,l10)
	    ,(u1,u2,u3,u4,u5,u6,u7,u8,u9,u10))
            (i1,i2,i3,i4,i5,i6,i7,i8,i9,i10) =
         inRange (l1,u1) i1
           && inRange (l2,u2) i2
           && inRange (l3,u3) i3
           && inRange (l4,u4) i4
           && inRange (l5,u5) i5
           && inRange (l6,u6) i6
           && inRange (l7,u7) i7
           && inRange (l8,u8) i8
           && inRange (l9,u9) i9
           && inRange (l10,u10) i10

instance (Ix a1, Ix a2, Ix a3, Ix a4, Ix a5, Ix a6, Ix a7, Ix a8, Ix a9
	 ,Ix a10, Ix a11) =>
	 Ix (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11)  where
    range ((l1,l2,l3,l4,l5,l6,l7,l8,l9,l10,l11)
	  ,(u1,u2,u3,u4,u5,u6,u7,u8,u9,u10,u11)) =
          [(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11)
			 | i1 <- range (l1,u1),
                           i2 <- range (l2,u2),
                           i3 <- range (l3,u3),
                           i4 <- range (l4,u4),
                           i5 <- range (l5,u5),
                           i6 <- range (l6,u6),
                           i7 <- range (l7,u7),
                           i8 <- range (l8,u8),
                           i9 <- range (l9,u9),
                           i10 <- range (l10,u10),
                           i11 <- range (l11,u11)]

    index ((l1,l2,l3,l4,l5,l6,l7,l8,l9,l10,l11)
	  ,(u1,u2,u3,u4,u5,u6,u7,u8,u9,u10,u11))
          (i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11) =
          index (l11,u11) i11 + rangeSize (l11,u11) * (
           index (l10,u10) i10 + rangeSize (l10,u10) * (
            index (l9,u9) i9 + rangeSize (l9,u9) * (
             index (l8,u8) i8 + rangeSize (l8,u8) * (
              index (l7,u7) i7 + rangeSize (l7,u7) * (
               index (l6,u6) i6 + rangeSize (l6,u6) * (
                index (l5,u5) i5 + rangeSize (l5,u5) * (
                 index (l4,u4) i4 + rangeSize (l4,u4) * (
                  index (l3,u3) i3 + rangeSize (l3,u3) * (
                   index (l2,u2) i2 + rangeSize (l2,u2) * (
                    index (l1,u1) i1))))))))))

    inRange ((l1,l2,l3,l4,l5,l6,l7,l8,l9,l10,l11)
	    ,(u1,u2,u3,u4,u5,u6,u7,u8,u9,u10,u11))
            (i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11) =
         inRange (l1,u1) i1
           && inRange (l2,u2) i2
           && inRange (l3,u3) i3
           && inRange (l4,u4) i4
           && inRange (l5,u5) i5
           && inRange (l6,u6) i6
           && inRange (l7,u7) i7
           && inRange (l8,u8) i8
           && inRange (l9,u9) i9
           && inRange (l10,u10) i10
           && inRange (l11,u11) i11

instance (Ix a1, Ix a2, Ix a3, Ix a4, Ix a5, Ix a6, Ix a7, Ix a8, Ix a9
	 ,Ix a10, Ix a11, Ix a12) =>
	 Ix (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12)  where
    range ((l1,l2,l3,l4,l5,l6,l7,l8,l9,l10,l11,l12)
	  ,(u1,u2,u3,u4,u5,u6,u7,u8,u9,u10,u11,u12)) =
          [(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12)
			 | i1 <- range (l1,u1),
                           i2 <- range (l2,u2),
                           i3 <- range (l3,u3),
                           i4 <- range (l4,u4),
                           i5 <- range (l5,u5),
                           i6 <- range (l6,u6),
                           i7 <- range (l7,u7),
                           i8 <- range (l8,u8),
                           i9 <- range (l9,u9),
                           i10 <- range (l10,u10),
                           i11 <- range (l11,u11),
                           i12 <- range (l12,u12)]

    index ((l1,l2,l3,l4,l5,l6,l7,l8,l9,l10,l11,l12)
	  ,(u1,u2,u3,u4,u5,u6,u7,u8,u9,u10,u11,u12))
          (i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12) =
         index (l12,u12) i12 + rangeSize (l12,u12) * (
          index (l11,u11) i11 + rangeSize (l11,u11) * (
           index (l10,u10) i10 + rangeSize (l10,u10) * (
            index (l9,u9) i9 + rangeSize (l9,u9) * (
             index (l8,u8) i8 + rangeSize (l8,u8) * (
              index (l7,u7) i7 + rangeSize (l7,u7) * (
               index (l6,u6) i6 + rangeSize (l6,u6) * (
                index (l5,u5) i5 + rangeSize (l5,u5) * (
                 index (l4,u4) i4 + rangeSize (l4,u4) * (
                  index (l3,u3) i3 + rangeSize (l3,u3) * (
                   index (l2,u2) i2 + rangeSize (l2,u2) * (
                    index (l1,u1) i1)))))))))))

    inRange ((l1,l2,l3,l4,l5,l6,l7,l8,l9,l10,l11,l12)
	    ,(u1,u2,u3,u4,u5,u6,u7,u8,u9,u10,u11,u12))
            (i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12) =
         inRange (l1,u1) i1
           && inRange (l2,u2) i2
           && inRange (l3,u3) i3
           && inRange (l4,u4) i4
           && inRange (l5,u5) i5
           && inRange (l6,u6) i6
           && inRange (l7,u7) i7
           && inRange (l8,u8) i8
           && inRange (l9,u9) i9
           && inRange (l10,u10) i10
           && inRange (l11,u11) i11
           && inRange (l12,u12) i12

instance (Ix a1, Ix a2, Ix a3, Ix a4, Ix a5, Ix a6, Ix a7, Ix a8, Ix a9
	 ,Ix a10, Ix a11, Ix a12, Ix a13) =>
	 Ix (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13)  where
    range ((l1,l2,l3,l4,l5,l6,l7,l8,l9,l10,l11,l12,l13)
	  ,(u1,u2,u3,u4,u5,u6,u7,u8,u9,u10,u11,u12,u13)) =
          [(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13)
			 | i1 <- range (l1,u1),
                           i2 <- range (l2,u2),
                           i3 <- range (l3,u3),
                           i4 <- range (l4,u4),
                           i5 <- range (l5,u5),
                           i6 <- range (l6,u6),
                           i7 <- range (l7,u7),
                           i8 <- range (l8,u8),
                           i9 <- range (l9,u9),
                           i10 <- range (l10,u10),
                           i11 <- range (l11,u11),
                           i12 <- range (l12,u12),
                           i13 <- range (l13,u13)]

    index ((l1,l2,l3,l4,l5,l6,l7,l8,l9,l10,l11,l12,l13)
	  ,(u1,u2,u3,u4,u5,u6,u7,u8,u9,u10,u11,u12,u13))
          (i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13) =
        index (l13,u13) i13 + rangeSize (l13,u13) * (
         index (l12,u12) i12 + rangeSize (l12,u12) * (
          index (l11,u11) i11 + rangeSize (l11,u11) * (
           index (l10,u10) i10 + rangeSize (l10,u10) * (
            index (l9,u9) i9 + rangeSize (l9,u9) * (
             index (l8,u8) i8 + rangeSize (l8,u8) * (
              index (l7,u7) i7 + rangeSize (l7,u7) * (
               index (l6,u6) i6 + rangeSize (l6,u6) * (
                index (l5,u5) i5 + rangeSize (l5,u5) * (
                 index (l4,u4) i4 + rangeSize (l4,u4) * (
                  index (l3,u3) i3 + rangeSize (l3,u3) * (
                   index (l2,u2) i2 + rangeSize (l2,u2) * (
                    index (l1,u1) i1))))))))))))

    inRange ((l1,l2,l3,l4,l5,l6,l7,l8,l9,l10,l11,l12,l13)
	    ,(u1,u2,u3,u4,u5,u6,u7,u8,u9,u10,u11,u12,u13))
            (i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13) =
         inRange (l1,u1) i1
           && inRange (l2,u2) i2
           && inRange (l3,u3) i3
           && inRange (l4,u4) i4
           && inRange (l5,u5) i5
           && inRange (l6,u6) i6
           && inRange (l7,u7) i7
           && inRange (l8,u8) i8
           && inRange (l9,u9) i9
           && inRange (l10,u10) i10
           && inRange (l11,u11) i11
           && inRange (l12,u12) i12
           && inRange (l13,u13) i13

instance (Ix a1, Ix a2, Ix a3, Ix a4, Ix a5, Ix a6, Ix a7, Ix a8, Ix a9
	 ,Ix a10, Ix a11, Ix a12, Ix a13, Ix a14) =>
	 Ix (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14)  where
    range ((l1,l2,l3,l4,l5,l6,l7,l8,l9,l10,l11,l12,l13,l14)
	  ,(u1,u2,u3,u4,u5,u6,u7,u8,u9,u10,u11,u12,u13,u14)) =
          [(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14)
			 | i1 <- range (l1,u1),
                           i2 <- range (l2,u2),
                           i3 <- range (l3,u3),
                           i4 <- range (l4,u4),
                           i5 <- range (l5,u5),
                           i6 <- range (l6,u6),
                           i7 <- range (l7,u7),
                           i8 <- range (l8,u8),
                           i9 <- range (l9,u9),
                           i10 <- range (l10,u10),
                           i11 <- range (l11,u11),
                           i12 <- range (l12,u12),
                           i13 <- range (l13,u13),
                           i14 <- range (l14,u14)]

    index ((l1,l2,l3,l4,l5,l6,l7,l8,l9,l10,l11,l12,l13,l14)
	  ,(u1,u2,u3,u4,u5,u6,u7,u8,u9,u10,u11,u12,u13,u14))
          (i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14) =
       index (l14,u14) i14 + rangeSize (l14,u14) * (
        index (l13,u13) i13 + rangeSize (l13,u13) * (
         index (l12,u12) i12 + rangeSize (l12,u12) * (
          index (l11,u11) i11 + rangeSize (l11,u11) * (
           index (l10,u10) i10 + rangeSize (l10,u10) * (
            index (l9,u9) i9 + rangeSize (l9,u9) * (
             index (l8,u8) i8 + rangeSize (l8,u8) * (
              index (l7,u7) i7 + rangeSize (l7,u7) * (
               index (l6,u6) i6 + rangeSize (l6,u6) * (
                index (l5,u5) i5 + rangeSize (l5,u5) * (
                 index (l4,u4) i4 + rangeSize (l4,u4) * (
                  index (l3,u3) i3 + rangeSize (l3,u3) * (
                   index (l2,u2) i2 + rangeSize (l2,u2) * (
                    index (l1,u1) i1)))))))))))))

    inRange ((l1,l2,l3,l4,l5,l6,l7,l8,l9,l10,l11,l12,l13,l14)
	    ,(u1,u2,u3,u4,u5,u6,u7,u8,u9,u10,u11,u12,u13,u14))
            (i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14) =
         inRange (l1,u1) i1
           && inRange (l2,u2) i2
           && inRange (l3,u3) i3
           && inRange (l4,u4) i4
           && inRange (l5,u5) i5
           && inRange (l6,u6) i6
           && inRange (l7,u7) i7
           && inRange (l8,u8) i8
           && inRange (l9,u9) i9
           && inRange (l10,u10) i10
           && inRange (l11,u11) i11
           && inRange (l12,u12) i12
           && inRange (l13,u13) i13
           && inRange (l14,u14) i14

instance (Ix a1, Ix a2, Ix a3, Ix a4, Ix a5, Ix a6, Ix a7, Ix a8, Ix a9
	 ,Ix a10, Ix a11, Ix a12, Ix a13, Ix a14, Ix a15) =>
	 Ix (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15)  where
    range ((l1,l2,l3,l4,l5,l6,l7,l8,l9,l10,l11,l12,l13,l14,l15)
	  ,(u1,u2,u3,u4,u5,u6,u7,u8,u9,u10,u11,u12,u13,u14,u15)) =
          [(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15)
			 | i1 <- range (l1,u1),
                           i2 <- range (l2,u2),
                           i3 <- range (l3,u3),
                           i4 <- range (l4,u4),
                           i5 <- range (l5,u5),
                           i6 <- range (l6,u6),
                           i7 <- range (l7,u7),
                           i8 <- range (l8,u8),
                           i9 <- range (l9,u9),
                           i10 <- range (l10,u10),
                           i11 <- range (l11,u11),
                           i12 <- range (l12,u12),
                           i13 <- range (l13,u13),
                           i14 <- range (l14,u14),
                           i15 <- range (l15,u15)]

    index ((l1,l2,l3,l4,l5,l6,l7,l8,l9,l10,l11,l12,l13,l14,l15)
	  ,(u1,u2,u3,u4,u5,u6,u7,u8,u9,u10,u11,u12,u13,u14,u15))
          (i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15) =
      index (l15,u15) i15 + rangeSize (l15,u15) * (
       index (l14,u14) i14 + rangeSize (l14,u14) * (
        index (l13,u13) i13 + rangeSize (l13,u13) * (
         index (l12,u12) i12 + rangeSize (l12,u12) * (
          index (l11,u11) i11 + rangeSize (l11,u11) * (
           index (l10,u10) i10 + rangeSize (l10,u10) * (
            index (l9,u9) i9 + rangeSize (l9,u9) * (
             index (l8,u8) i8 + rangeSize (l8,u8) * (
              index (l7,u7) i7 + rangeSize (l7,u7) * (
               index (l6,u6) i6 + rangeSize (l6,u6) * (
                index (l5,u5) i5 + rangeSize (l5,u5) * (
                 index (l4,u4) i4 + rangeSize (l4,u4) * (
                  index (l3,u3) i3 + rangeSize (l3,u3) * (
                   index (l2,u2) i2 + rangeSize (l2,u2) * (
                    index (l1,u1) i1))))))))))))))

    inRange ((l1,l2,l3,l4,l5,l6,l7,l8,l9,l10,l11,l12,l13,l14,l15)
	    ,(u1,u2,u3,u4,u5,u6,u7,u8,u9,u10,u11,u12,u13,u14,u15))
            (i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15) =
         inRange (l1,u1) i1
           && inRange (l2,u2) i2
           && inRange (l3,u3) i3
           && inRange (l4,u4) i4
           && inRange (l5,u5) i5
           && inRange (l6,u6) i6
           && inRange (l7,u7) i7
           && inRange (l8,u8) i8
           && inRange (l9,u9) i9
           && inRange (l10,u10) i10
           && inRange (l11,u11) i11
           && inRange (l12,u12) i12
           && inRange (l13,u13) i13
           && inRange (l14,u14) i14
           && inRange (l15,u15) i15



