> {-#  LANGUAGE Arrows  #-}

> module HSoM.Examples.AMandFM where
> import Euterpea
> import FRP.UISF.AuxFunctions

> tab1 = tableSinesN 4096 [1]

> tremolo ::   Clock c =>
>              Double -> Double -> SigFun c () Double
> tremolo tfrq dep = proc () -> do
>      trem  <- osc tab1 0 -< tfrq
>      outA  -< 1 + dep*trem

> amfmInst  :: Instr (Mono AudRate)
>        -- Dur -> AbsPitch -> Volume -> AudSF () Double
> amfmInst dur ap vol ps = 
>   let  f    = apToHz ap
>        v    = fromIntegral vol / 100
>        d    = fromRational dur
>        (tremAmt, tremFreq, vibAmt, vibFreq) =  
>            if length ps < 4 then (0, 1, 0, 1)
>            else (ps !! 0, ps !! 1, ps !! 2, ps !! 3)
>   in proc () -> do
>        asr <- envLineSeg [0,1,1,0] [d*0.01, d*0.98, d*0.01] -< ()
>        vibSig <- osc tab1 0 -< vibFreq
>        sineSig <- osc tab1 0 -< f + f*vibSig*vibAmt
>        tremEnv <- tremolo tremFreq tremAmt -< ()
>        outA -< (1-tremAmt) * asr * tremEnv * sineSig

> iMap :: InstrMap (Mono AudRate)
> iMap = [(CustomInstrument "AMFM", amfmInst)] 

> mkAMFM :: FilePath -> [Double] -> IO ()
> mkAMFM str p = writeWav str iMap $
>     instrument (CustomInstrument "AMFM") $ 
>     note 2 ((C,4::Octave), [Params p])

> amfm1 =  mkAMFM "amfm1.wav" [] -- neither trem nor vib
> amfm2 =  mkAMFM "amfm2.wav" [0.2,1.5,0.0,1.0] -- trem only
> amfm3 =  mkAMFM "amfm3.wav" [0.0,1.0,0.02,4.0] -- vib only
> amfm4 =  mkAMFM "amfm4.wav" [0.2,1.5,0.02,4.0] -- both

Vibrato and tremolo into the audible frequency range:

> amfm5 = mkAMFM "amfm5.wav" [0.2,50.0,0.0,1.0] -- trem only
> amfm6 = mkAMFM "amfm6.wav" [0.0,1.0,0.05,100.0] -- vib only
> amfm7 = mkAMFM "amfm7.wav" [0.2,50.0,0.05,100.0] -- both

> writeAll = sequence [amfm1, amfm2, amfm3, amfm4, amfm5, amfm6, amfm7]

