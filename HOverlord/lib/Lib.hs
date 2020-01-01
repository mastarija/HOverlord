{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ForeignFunctionInterface #-}
--
module Lib where
--
import Data.Bitmap.Base
import Data.Bitmap.IO
import Data.Bitmap.IO.File
import Data.Int ( Int32 )
import Data.Word ( Word8, Word16 )
import Data.ByteString ( ByteString )
import GHC.Generics ( Generic )
import Foreign.Storable ( Storable (..) )
import Foreign.Marshal.Alloc ( free )
import Foreign.C.String ( CString, peekCString )
import Foreign.C.Types ( CLong, CDouble, CULong )
import Foreign.Ptr ( Ptr, castPtr )
--

data BMP = BMP
  { image :: Ptr ByteString
  , bytes :: CULong
  } deriving ( Eq, Show, Generic )

data Test = Test
  { width   :: Double
  , height  :: Double
  } deriving ( Eq, Show, Generic )

instance Storable Test where
  sizeOf _ = 2 * sizeOf ( undefined :: Double )
  peek ptr = do
    w <- peek $ castPtr ptr
    h <- peekByteOff ( castPtr ptr ) ( sizeOf w )
    pure $ Test w h
  poke ptr ( Test w h ) = do
    poke ( castPtr ptr ) w
    pokeByteOff ( castPtr ptr ) ( sizeOf w ) h
  alignment _ = sizeOf ( undefined :: Double )

foreign import ccall "testRun" c_testRun :: IO ( Ptr Test )

testRun :: IO Test
testRun = c_testRun >>= peek

data CBitmap = CBitmap
  { bmType        :: Int32
  , bmWidth       :: Int32
  , bmHeight      :: Int32
  , bmWidthBytes  :: Int32
  , bmPlanes      :: Word16
  , bmBitsPixel   :: Word16
  , bmBits        :: Ptr Word8
  } deriving ( Eq, Show, Generic )

instance Storable CBitmap where
  sizeOf _  = 4 * sizeOf ( undefined :: Int32     )
            + 2 * sizeOf ( undefined :: Word16    )
            + 1 * sizeOf ( undefined :: Ptr Word8  )

  peek ptr  = CBitmap
    <$> peekByteOff pptr ( 0 * sInt32 )
    <*> peekByteOff pptr ( 1 * sInt32 )
    <*> peekByteOff pptr ( 2 * sInt32 )
    <*> peekByteOff pptr ( 3 * sInt32 )
    <*> peekByteOff pptr ( 3 * sInt32 + 0 * sWord16 )
    <*> peekByteOff pptr ( 3 * sInt32 + 1 * sWord16 )
    <*> peekByteOff pptr ( 3 * sInt32 + 2 * sWord16 )
    where pptr    = castPtr ptr
          sPtr    = sizeOf ( undefined :: Ptr Word8 )
          sInt32  = sizeOf ( undefined :: Int32 )
          sWord16 = sizeOf ( undefined :: Word16 )

  poke ptr CBitmap{..} = do
    pokeByteOff pptr ( 0 * sInt32               ) bmType
    pokeByteOff pptr ( 1 * sInt32               ) bmWidth
    pokeByteOff pptr ( 2 * sInt32               ) bmHeight
    pokeByteOff pptr ( 3 * sInt32               ) bmWidthBytes
    pokeByteOff pptr ( 3 * sInt32 + 0 * sWord16 ) bmPlanes
    pokeByteOff pptr ( 3 * sInt32 + 1 * sWord16 ) bmBitsPixel
    pokeByteOff pptr ( 3 * sInt32 + 2 * sWord16 ) bmBits
    where pptr    = castPtr ptr
          sPtr    = sizeOf ( undefined :: Ptr Word8 )
          sInt32  = sizeOf ( undefined :: Int32 )
          sWord16 = sizeOf ( undefined :: Word16 )

  alignment _ = sizeOf ( undefined :: Int32 )

foreign import ccall "TakeScreenShot" c_takeScreenShot :: IO ( Ptr CBitmap )

takeScreenShot :: IO CBitmap
takeScreenShot = c_takeScreenShot >>= peek

test :: IO ()
test = do
  img <- readRawData "../test.bin" ( ( 1366, 768 ), 3, PctWord8 ) :: IO ( IOBitmap Word8 )
  writeBitmap "test.bmp" img
