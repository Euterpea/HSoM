> module  HSoM.Examples.Interlude
>         (  childSong6,  --  :: Music Pitch,
>            prefix       --  :: [Music a] -> Music a)
>         )  where
> import Euterpea

> addDur       :: Dur -> [Dur -> Music a] -> Music a
> addDur d ns  =  let f n = n d
>                 in line (map f ns)

> graceNote :: Int -> Music Pitch -> Music Pitch
> graceNote n  (Prim (Note d p))  =
>           note (d/8) (trans n p) :+: note (7*d/8) p
> graceNote n  _                  = 
>           error "Can only add a grace note to a note."

> b1  = addDur dqn [b 2,   fs 3,  g 3,   fs 3]
> b2  = addDur dqn [b 2,   es 3,  fs 3,  es 3]
> b3  = addDur dqn [as 2,  fs 3,  g 3,   fs 3]

> bassLine =  times 3 b1 :+: times 2 b2 :+: 
>             times 4 b3 :+: times 5 b1

> mainVoice = times 3 v1 :+: v2

> v1   = v1a :+: graceNote (-1) (d 4 qn) :+: v1b                 --  bars 1-2
> v1a  = addDur en [a 4, e 4, d 4, fs 4, cs 4, b 3, e 4, b 3]
> v1b  = addDur en [cs 4, b 3]

> v2 = v2a :+: v2b :+: v2c :+: v2d :+: v2e :+: v2f :+: v2g

> v2a  =  line [  cs 4 (dhn+dhn), d 4 dhn, 
>                 f 4 hn, gs 4 qn, fs 4 (hn+en), g 4 en]     --  bars 7-11
> v2b  =  addDur en [  fs 4, e 4, cs 4, as 3] :+: a 3 dqn :+:
>         addDur en [  as 3, cs 4, fs 4, e 4, fs 4]          --  bars 12-13
> v2c  =  line [  g 4 en, as 4 en, cs 5 (hn+en), d 5 en, cs 5 en] :+:
>         e 4 en :+: enr :+: 
>         line [  as 4 en, a 4 en, g 4 en, d 4 qn, c 4 en, cs 4 en] 
>                                                            --  bars 14-16
> v2d  =  addDur en [  fs 4, cs 4, e 4, cs 4, 
>                      a 3, as 3, d 4, e 4, fs 4]            --  bars 17-18.5
> v2e  =  line [  graceNote 2 (e 4 qn), d 4 en, graceNote 2 (d 4 qn), cs 4 en,
>                 graceNote 1 (cs 4 qn), b 3 (en+hn), cs 4 en, b 3 en ]  
>                                                            --  bars 18.5-20
> v2f  =  line [  fs 4 en, a 4 en, b 4 (hn+qn), a 4 en, fs 4 en, e 4 qn,
>                 d 4 en, fs 4 en, e 4 hn, d 4 hn, fs 4 qn]  --  bars 21-23
> v2g  =  tempo (3/2) (line [cs 4 en, d 4 en, cs 4 en]) :+: 
>         b 3 (3*dhn+hn)                                     --  bars 24-28

> childSong6 :: Music Pitch
> childSong6 =  let t = (dhn/qn)*(69/120)
>               in instrument  RhodesPiano 
>                              (tempo t (bassLine :=: mainVoice))

> prefixes         :: [a] -> [[a]]
> prefixes []      =  []
> prefixes (x:xs)  =  let f pf = x:pf
>                     in [x] : map f (prefixes xs)

> prefix :: [Music a] -> Music a
> prefix mel =  let  m1  = line (concat (prefixes mel))
>                    m2  = transpose 12 (line (concat (prefixes (reverse mel))))
>                    m   = instrument Flute m1 :=: instrument VoiceOohs m2
>               in m :+: transpose 5 m :+: m

> mel1 = [c 4 en, e 4 sn, g 4 en, b 4 sn, a 4 en, f 4 sn, d 4 en, b 3 sn, c 4 en]
> mel2 = [c 4 sn, e 4 sn, g 4 sn, b 4 sn, a 4 sn, f 3 sn, d 4 sn, b 3 sn, c 4 sn]
