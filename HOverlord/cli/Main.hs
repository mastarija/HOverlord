module Main where

import qualified Graphics.X11.Types as X
import qualified Graphics.X11.Xlib.Image as X
import qualified Graphics.X11.Xlib.Display as X
import qualified Graphics.X11.Xlib.Screen as X
import Codec.Picture as CP
import Data.Function (on)

import Foreign.C.Types
import Data.Bits

main :: IO ()
main = do
    let (w,h) = (1920,1080)
    disp <- X.openDisplay ":0"
    let scr = X.defaultScreenOfDisplay disp
    root <- X.rootWindow disp (X.screenNumberOfScreen scr)
    img <- X.getImage disp root 0 0 w h (-1) X.xyPixmap

    let rgb :: CULong -> CP.PixelRGB8
        rgb v =
          let pixel s = fromIntegral $ 0xff .&. (v `shiftR` s)
          in CP.PixelRGB8 (pixel 16) (pixel 8) (pixel 0)

    let pixelRenderer :: Int -> Int -> CP.PixelRGB8
        pixelRenderer = (rgb .) . X.getPixel img `on` fromIntegral

    CP.writePng "./screenshot.png" $ CP.generateImage pixelRenderer (fromIntegral w) (fromIntegral h)

    X.destroyImage img
