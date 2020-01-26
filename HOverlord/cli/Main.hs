{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad.Extra(eitherM)
import Data.Bits
import Data.Function (on, (&))
import Foreign.C.Types
import System.Exit (ExitCode(..))
import System.Process (CreateProcess, shell, readCreateProcessWithExitCode)
import Text.RawString.QQ(r)
import qualified Codec.Picture as CP
import qualified Graphics.X11.Types as X
import qualified Graphics.X11.Xlib.Display as X
import qualified Graphics.X11.Xlib.Image as X
import qualified Graphics.X11.Xlib.Types as X


main :: IO ()
main = getScreenshot "test-screenshot"

getScreenshot :: String -> IO ()
getScreenshot imgPrefix = do
    -- Always use display/screen :0.0. All screens are concatenated inside this one
    disp <- X.openDisplay ":0"
    root <- X.rootWindow disp 0
    getScreenRects & eitherM
        print
        (mapM_ (getScreenScreenshot imgPrefix disp root) . zip [0..])

getScreenScreenshot :: String -> X.Display -> X.Window -> (Int, ScreenRect) -> IO ()
getScreenScreenshot imgPrefix disp root (scrIndex, ((w, h), (x, y))) = do
    img <- X.getImage disp root x y w h (-1) X.xyPixmap

    CP.writePng ("./" <> imgPrefix <> "_" <> show scrIndex <> ".png")
              $ CP.generateImage ((rgb .) . X.getPixel img `on` fromIntegral :: Int -> Int -> CP.PixelRGB8)
                                 (fromIntegral w)
                                 (fromIntegral h)
    X.destroyImage img
  where
    rgb :: CULong -> CP.PixelRGB8
    rgb v = let pixel s = fromIntegral $ 0xff .&. (v `shiftR` s)
            in CP.PixelRGB8 (pixel 16) (pixel 8) (pixel 0)

-- ((width, height), (startX, startY)) - specific screen rectangle in a collage image of all screens
type ScreenRect = ((CUInt, CUInt), (CInt, CInt))
data ScreenRectsErr = ScreenRectsErr String deriving Show

-- External xrandr call that returns list of all screen rectangles
getScreenRects :: IO (Either ScreenRectsErr [ScreenRect])
getScreenRects = do
    let scrRectsProc :: CreateProcess
        scrRectsProc = shell
          [r|    xrandr --current \
               | grep -oP '(?<!\d)\d*x\d*[+]\d*[+]\d*(?!\d)' \
               | awk -F '[x,+]' '{ print "(("$1","$2"),("$3","$4"))" }'
          |]
    (scrRectsEC, scrRectsStdout, scrRectsStderr) <- readCreateProcessWithExitCode scrRectsProc ""
    return $ case scrRectsEC of
        (ExitFailure _) ->  Left $ ScreenRectsErr scrRectsStderr
        ExitSuccess     ->  Right $ (read @ ScreenRect) <$> lines scrRectsStdout
