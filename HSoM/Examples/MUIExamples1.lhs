> {-#  LANGUAGE Arrows, CPP  #-}

> module HSoM.Examples.MUIExamples1 where
> import Euterpea
> import Data.Maybe (mapMaybe)
> import HSoM
> import FRP.UISF
> import FRP.UISF.Graphics (withColor', rgbE, rectangleFilled)
> import FRP.UISF.Widget.Construction (mkWidget)


> ui0  ::  UISF () ()
> ui0  =   proc _ -> do
>     ap <- hiSlider 1 (0,100) 0 -< ()
>     display -< pitch ap

> mui0 = runMUI' ui0

> ui1 ::  UISF () ()
> ui1 =   setSize (150,150) $ 
>   proc _ -> do
>     ap <- title "Absolute Pitch" (hiSlider 1 (0,100) 0) -< ()
>     title "Pitch" display -< pitch ap

> mui1  =  runMUI' ui1

> ui2   ::  UISF () ()
> ui2   =   leftRight $
>   proc _ -> do
>     ap <- title "Absolute Pitch" (hiSlider 1 (0,100) 0) -< ()
>     title "Pitch" display -< pitch ap

> mui2  =  runMUI' ui2


> ui3  ::  UISF () ()
> ui3  =   proc _ -> do
>     devid <- selectOutput -< ()
>     ap <- title "Absolute Pitch" (hiSlider 1 (0,100) 0) -< ()
>     title "Pitch" display -< pitch ap
>     uap <- unique -< ap
>     midiOut -< (devid, fmap (\k-> [ANote 0 k 100 0.1]) uap)

> mui3  = runMUI' ui3


> ui4   :: UISF () ()
> ui4   = proc _ -> do
>     mi  <- selectInput   -< ()
>     mo  <- selectOutput  -< ()
>     m   <- midiIn        -< mi
>     midiOut -< (mo, m)

> mui4  = runMUI' ui4

> getDeviceIDs = topDown $
>   proc () -> do
>     mi    <- selectInput   -< ()
>     mo    <- selectOutput  -< ()
>     outA  -< (mi,mo)

> mui'4 = runMUI  (defaultMUIParams 
>                     {  uiTitle  = "MIDI Input / Output UI", 
>                        uiSize   = (200,200)})
>                 ui4

> ui5 ::  UISF () ()
> ui5 =   proc _ -> do
>     devid   <- selectOutput -< ()
>     ap      <- title "Absolute Pitch" (hiSlider 1 (0,100) 0) -< ()
>     title "Pitch" display -< pitch ap
>     f       <- title "Tempo" (hSlider (1,10) 1) -< ()
>     tick    <- timer -< 1/f
>     midiOut -< (devid, fmap (const [ANote 0 ap 100 0.1]) tick)

> colorSwatchUI :: UISF () ()
> colorSwatchUI = setSize (300, 220) $ pad (4,0,4,0) $ leftRight $ 
>     proc _ -> do
>         r <- newColorSlider "R" -< ()
>         g <- newColorSlider "G" -< ()
>         b <- newColorSlider "B" -< ()
>         e <- unique -< (r,g,b)
>         let rect = withColor' (rgbE r g b) (rectangleFilled ((0,0),d))
>         pad (4,8,0,0) $ canvas d -< fmap (const rect) e
>   where
>     d = (170,170)
>     newColorSlider l = title l $ withDisplay $ viSlider 16 (0,255) 0

> colorSwatch = runMUI' colorSwatchUI

