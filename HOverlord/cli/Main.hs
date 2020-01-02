module Main where

import qualified Graphics.X11.Types as X
import qualified Graphics.X11.Xlib.Image as X
import qualified Graphics.X11.Xlib.Display as X
import qualified Graphics.X11.Xlib.Screen as X
import qualified Graphics.X11.Xlib.Types as X
import Codec.Picture as CP
import Data.Function (on)

import Foreign.C.Types
import Data.Bits

main :: IO ()
main = do
    disp <- X.openDisplay ":0"
    let  scrCnt = X.screenCount disp :: CInt
    mapM_ (getScreenScreenshot disp . fromIntegral) [0..scrCnt - 1]

getScreenScreenshot :: X.Display -> X.ScreenNumber -> IO ()
getScreenScreenshot disp scrNbr = do
    let scrNbr = X.defaultScreen disp
    let [w, h] = fromIntegral <$> [X.displayWidth disp scrNbr, X.displayHeight disp scrNbr]
    let scr = X.defaultScreenOfDisplay disp
    root <- X.rootWindow disp (X.screenNumberOfScreen scr)
    img <- X.getImage disp root 0 0 w h (-1) X.xyPixmap

    let rgb :: CULong -> CP.PixelRGB8
        rgb v =
          let pixel s = fromIntegral $ 0xff .&. (v `shiftR` s)
          in CP.PixelRGB8 (pixel 16) (pixel 8) (pixel 0)

    let pixelRenderer :: Int -> Int -> CP.PixelRGB8
        pixelRenderer = (rgb .) . X.getPixel img `on` fromIntegral

    CP.writePng ("./screenshot_" <> X.displayString disp <> "." <> show scrNbr <> ".png")
              $ CP.generateImage pixelRenderer (fromIntegral w) (fromIntegral h)

    X.destroyImage img
