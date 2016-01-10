The first phrase of the flute part of "Stars and Stripes Forever."

> module HSoM.Examples.SSF where
> import Euterpea
> import HSoM.Examples.MoreMusic

> legato = Legato (11/10)
> staccato = Staccato (5/10)
>
> ssfMelody = line (m1 ++ m2 ++ m3 ++ m4)

> m1 = [trilln 2 5 (bf 5 en), Modify (Phrase [Art staccato]) (line [ef 6 en, ef 5 en, ef 6 en])]

> m2 = [Modify (Phrase [Art legato]) (line [bf 5 sn, c  6 sn, bf 5 sn, g  5 sn]),
>	    Modify (Phrase [Art staccato])    (line [ef 5 en, bf 4 en])]

> m3 = [Modify (Phrase [Art legato]) (line [ef 5 sn, f  5 sn, g  5 sn, af 5 sn]),
>	    Modify (Phrase [Art staccato])    (line [bf 5 en, ef 6 en])]

> m4 = [trill 2 tn (bf 5 qn), bf 5 sn, denr]

> ssf = Modify (Instrument Flute) ssfMelody
