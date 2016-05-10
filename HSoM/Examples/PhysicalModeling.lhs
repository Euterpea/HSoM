> {-#  LANGUAGE Arrows  #-}

> module HSoM.Examples.PhysicalModeling where
> import Euterpea
> import FRP.UISF.AuxFunctions

> sineTable441 :: Table
> sineTable441 = tableSinesN 100 [1]

> s441 :: AudSF () Double
> s441 = proc () -> do
>          rec s <- delayLineT 100 sineTable441 -< s
>          outA -< s

> ts441 = outFile "s441.wav" 5 s441

> echo :: AudSF Double Double
> echo = proc s -> do
>          rec fb  <- delayLine 0.5 -< s + 0.7*fb
>          outA -< fb/3


> modVib :: Double -> Double -> AudSF Double Double
> modVib rate depth =
>   proc sin -> do
>     vib   <- osc sineTable 0  -< rate
>     sout  <- delayLine1 0.2   -< (sin,0.1+0.005*vib)
>     outA -< sout

> tModVib = outFile "modvib.wav" 6 $
>                   constA 440 >>> osc sineTable 0 >>> modVib 5 0.005

> sineTable :: Table
> sineTable = tableSinesN 4096 [1]



> flute ::  Time -> Double -> Double -> Double -> Double 
>           -> AudSF () Double
> flute dur amp fqc press breath = 
>   proc () -> do
>     env1   <- envLineSeg  [0, 1.1*press, press, press, 0] 
>                           [0.06, 0.2, dur-0.16, 0.02]  -< ()
>     env2   <- envLineSeg  [0, 1, 1, 0] 
>                           [0.01, dur-0.02, 0.01]       -< ()
>     envib  <- envLineSeg  [0, 0, 1, 1] 
>                           [0.5, 0.5, dur-1]            -< ()
>     flow   <- noiseWhite 42    -< ()
>     vib    <- osc sineTable 0  -< 5
>     let  emb = breath*flow*env1 + env1 + vib*0.1*envib
>     rec  flute  <- delayLine (1/fqc)    -< out
>          x      <- delayLine (1/fqc/2)  -< emb + flute*0.4
>          out    <- filterLowPassBW -< (x-x*x*x + flute*0.4, 2000)
>     outA -< out*amp*env2
    

> tFlute = outFile "tFlute.wav" 5 $ flute 5 0.3 440 0.99 0.2 
> tFlute2 = outFileNorm "tFlute2.wav" 5 $ flute 5 0.7 440 0.99 0.2 