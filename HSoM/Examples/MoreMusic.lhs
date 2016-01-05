> module HSoM.Examples.MoreMusic where
> import Euterpea

> pr1, pr2 :: Pitch -> Music Pitch
> pr1 p =  tempo (5/6) 
>          (  tempo (4/3)  (  mkLn 1 p qn :+:
>                             tempo (3/2) (  mkLn 3 p en  :+:
>                                            mkLn 2 p sn  :+:
>                                            mkLn 1 p qn     ) :+:
>                             mkLn 1 p qn) :+:
>             tempo (3/2)  (  mkLn 6 p en))

> pr2 p = 
>    let  m1   = tempo (5/4) (tempo (3/2) m2 :+: m2)
>         m2   = mkLn 3 p en
>    in tempo (7/6) (  m1 :+:
>                      tempo (5/4) (mkLn 5 p en) :+:
>                      m1 :+:
>                      tempo (3/2) m2)

> mkLn :: Int -> p -> Dur -> Music p
> mkLn n p d = line $ take n $ repeat $ note d p

> pr12  :: Music Pitch
> pr12  = pr1 (C,4) :=: pr2 (G,4)


> trill :: Int -> Dur -> Music Pitch -> Music Pitch
> trill i sDur (Prim (Note tDur p)) =
>    if sDur >= tDur  then note tDur p
>                     else  note sDur p :+: 
>                           trill  (negate i) sDur 
>                                  (note (tDur-sDur) (trans i p))
> trill i d (Modify (Tempo r) m)  = tempo r (trill i (d*r) m)
> trill i d (Modify c m)          = Modify c (trill i d m)
> trill _ _ _                     = 
>       error "trill: input must be a single note."
> {-# LINE 702 "MoreMusic.lhs" #-}
> trill' :: Int -> Dur -> Music Pitch -> Music Pitch
> trill' i sDur m = trill (negate i) sDur (transpose i m)

> trilln :: Int -> Int -> Music Pitch -> Music Pitch
> trilln i nTimes m = trill i (dur m / fromIntegral nTimes) m

> trilln' :: Int -> Int -> Music Pitch -> Music Pitch
> trilln' i nTimes m = trilln (negate i) nTimes (transpose i m)

> roll  :: Dur -> Music Pitch -> Music Pitch
> rolln :: Int -> Music Pitch -> Music Pitch

> roll  dur    m = trill  0 dur m
> rolln nTimes m = trilln 0 nTimes m

> ssfMel :: Music Pitch
> ssfMel = line (l1 ++ l2 ++ l3 ++ l4)
>   where  l1  = [ trilln 2 5 (bf 6 en), ef 7 en, ef 6 en, ef 7 en ]
>          l2  = [ bf 6 sn, c  7 sn, bf 6 sn, g 6 sn, ef 6 en, bf 5 en ]
>          l3  = [ ef 6 sn, f 6 sn, g 6 sn, af 6 sn, bf 6 en, ef 7 en ]
>          l4  = [ trill 2 tn (bf 6 qn), bf 6 sn, denr ]

> starsAndStripes :: Music Pitch
> starsAndStripes = instrument Flute ssfMel

> grace :: Int -> Rational -> Music Pitch -> Music Pitch
> grace n r (Prim (Note d p))  =
>       note (r*d) (trans n p) :+: note ((1-r)*d) p
> grace n r _                  = 
>       error "grace: can only add a grace note to a note"

> grace2 ::  Int -> Rational -> 
>            Music Pitch -> Music Pitch -> Music Pitch
> grace2 n r (Prim (Note d1 p1)) (Prim (Note d2 p2)) =
>       note (d1-r*d2) p1 :+: note (r*d2) (trans n p2) :+: note d2 p2
> grace2 _ _ _ _  = 
>       error "grace2: can only add a grace note to a note"

> funkGroove :: Music Pitch
> funkGroove
>   =  let  p1  = perc LowTom         qn
>           p2  = perc AcousticSnare  en
>      in  tempo 3 $ instrument Percussion $ cut 8 $ forever
>          (  (  p1 :+: qnr :+: p2 :+: qnr :+: p2 :+:
>                p1 :+: p1 :+: qnr :+: p2 :+: enr)
>             :=: roll en (perc ClosedHiHat 2) )


> rep ::  (Music a -> Music a) -> (Music a -> Music a) -> Int 
>         -> Music a -> Music a
> rep f g 0 m  = rest 0
> rep f g n m  = m :=: g (rep f g (n-1) (f m))

> run,  cascade,  cascades,  final :: Music Pitch
> run', cascade', cascades', final' :: Music Pitch

> run       = rep (transpose 5) (offset tn) 8 (c 4 tn)
> cascade   = rep (transpose 4) (offset en) 8 run
> cascades  = rep  id           (offset sn) 2 cascade

> final = cascades :+: retro cascades
> run'       = rep (offset tn) (transpose 5) 8 (c 4 tn)
> cascade'   = rep (offset en) (transpose 4) 8 run'
> cascades'  = rep (offset sn)  id           2 cascade'
> final'     = cascades' :+: retro cascades'
