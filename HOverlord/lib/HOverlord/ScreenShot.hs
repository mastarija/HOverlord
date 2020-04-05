{-# LANGUAGE DeriveGeneric #-}
--
module HOverlord.ScreenShot where
--
import GHC.Generics ( Generic )
import Data.ByteString ( ByteString )
--

{-
  A @record@ representing coordinates and dimensions of real screen on
  a virtual screen.
-}
data Screen = Screen
  { x :: Int -- ^ @x@ coordinate
  , y :: Int -- ^ @y@ coordinate
  , w :: Int -- ^ width
  , h :: Int -- ^ height
  } deriving ( Eq, Show, Generic )

{-
  A @newtype@ wrapper around 'ByteString' used to represent screenshot
  data in @BMP@ format. This can be further processed into more
  appropriate image formats.
-}
newtype Bitmap = Bitmap
  { unBitmap :: ByteString -- ^ @BMP@ data
  } deriving ( Eq, Show, Generic )

{-
  Screen shot interface defined as a simple record.
-}
data ScreenShot = ScreenShot
  { screenEnum :: IO [Screen]                 -- ^ list all screens
  , screenShot :: Screen -> IO Bitmap         -- ^ take a screenshot
  , screenSave :: FilePath -> Bitmap -> IO () -- ^ save bitmap to disk
  }
