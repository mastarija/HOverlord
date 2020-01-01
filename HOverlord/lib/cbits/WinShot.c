#include <stdio.h>
#include <windows.h>
////////////////////////////////////////////////////////////////////////////////

typedef struct { void* image; unsigned long bytes; } BMP;

////////////////////////////////////////////////////////////////////////////////

DWORD ColorTableSize( BITMAPINFOHEADER* h ) {
  DWORD result        = 0;

  DWORD biClrUsed     = h->biClrUsed;
  WORD  biBitCount    = h->biBitCount;
  DWORD biCompression = h->biCompression;

  switch ( biBitCount ) {
    case 24:
      result = biClrUsed;
      break;
    case 16:
    case 32:
      if ( biCompression == BI_RGB )
        result = biClrUsed;
      else if ( biCompression == BI_BITFIELDS )
        result = 3;
      break;
    default: // for 0, 1, 2, 4, and 8
      if ( biClrUsed == 0 )
        result = ( 1 << biBitCount ); // 2^biBitCount
      else
        result = biClrUsed;
      break;
  }

  return result;
}

BMP* ScreenShot( int x, int y, int width, int height ) {
  BMP*              result            = NULL;

  HDC               hScreen           = GetDC( NULL );
  HDC               hScreenMemory     = CreateCompatibleDC( hScreen );

  HBITMAP           hBitmap           = CreateCompatibleBitmap( hScreen, width, height );
  HBITMAP           hBitmapOld        = SelectObject( hScreenMemory, hBitmap );

  BITMAPINFO*       pBitmapInfo       = ( BITMAPINFO* )       malloc( sizeof( BITMAPINFOHEADER ) );
  BITMAPFILEHEADER* pBitmapFileHeader = ( BITMAPFILEHEADER* ) malloc( sizeof( BITMAPFILEHEADER ) );

  BYTE*             pPixelBuffer      = NULL; // pixel buffer
  BYTE*             pBitmapBuffer     = NULL; // full bitmap file buffer

  DWORD             nColors           = 0;    // number of items in the Color Table

  DWORD             sBitmapInfo       = sizeof( BITMAPINFOHEADER );
  DWORD             sPixelBuffer      = 0;    // byte size of PIXEL buffer
  DWORD             sBitmapFileHeader = sizeof( BITMAPFILEHEADER );

  //////////////////////////////////////////////////////////////////////////////

  // copy pixels from "real" screen to "in memory" screen
  BitBlt( hScreenMemory, 0, 0, width, height, hScreen, x, y, SRCCOPY );

  //////////////////////////////////////////////////////////////////////////////

  // get BITMAPINFOHEADER data into BITMAPINFO structure
  pBitmapInfo->bmiHeader.biSize = sizeof( BITMAPINFOHEADER );
  if( !GetDIBits( hScreenMemory, hBitmap, 0, 0, NULL, pBitmapInfo, DIB_RGB_COLORS ) )
    goto FailExit01;

  // calculate number of additional bytes to allocate for Color Table
  nColors       = ColorTableSize( &pBitmapInfo->bmiHeader );
  sPixelBuffer  = pBitmapInfo->bmiHeader.biSizeImage;

  // calculate size of BITMAPINFO data structure
  if ( nColors > 0 )
    sBitmapInfo = sizeof( BITMAPINFOHEADER ) + sizeof( RGBQUAD ) * nColors;

  // reallocate BITMAPINFO data structure in case there is a Color Table
  if ( nColors > 0 )
    pBitmapInfo = realloc( pBitmapInfo, sBitmapInfo );

  // return null if reallocation failed
  if ( pBitmapInfo == NULL )
    goto FailExit01;

  // allocate enough bytes for storing pixels
  pPixelBuffer = malloc( sPixelBuffer );

  // return null if allocation failed
  if ( pPixelBuffer == NULL )
    goto FailExit01;

  // get full BITMAPINFO structure this time
  // return NULL if it fails
  if( !GetDIBits( hScreenMemory, hBitmap, 0, pBitmapInfo->bmiHeader.biHeight, pPixelBuffer, pBitmapInfo, DIB_RGB_COLORS ) )
    goto FailExit02;

  // initialize BITMAPFILEHEADER
  pBitmapFileHeader->bfType      = 0x4D42; // 0x4D B, 0x42 M : BM ( BitMap )
  pBitmapFileHeader->bfSize      = sBitmapFileHeader + sBitmapInfo + sPixelBuffer;
  pBitmapFileHeader->bfReserved1 = 0;
  pBitmapFileHeader->bfReserved2 = 0;
  pBitmapFileHeader->bfOffBits   = sBitmapFileHeader + sBitmapInfo;

  // allocate enough space to keep complete BMP file in memory
  pBitmapBuffer = malloc( sBitmapFileHeader + sBitmapInfo + sPixelBuffer );

  // return NULL if allocation failed
  if ( pBitmapBuffer == NULL )
    goto FailExit02;

  // copy BITMAPFILEHEADER into the bitmap buffer
  memcpy( pBitmapBuffer                                   , pBitmapFileHeader , sBitmapFileHeader );
  // copy BITMAPINFO into the bitmap buffer
  memcpy( pBitmapBuffer + sBitmapFileHeader               , pBitmapInfo       , sBitmapInfo       );
  // copy PIXELS into the bitmap buffer
  memcpy( pBitmapBuffer + sBitmapFileHeader + sBitmapInfo , pPixelBuffer      , sPixelBuffer      );

  // allocate enough memory for BMP structure
  result = malloc( sizeof( BMP ) );

  // return NULL if allocation fails
  // and clean up successfully allocated bitmap buffer ( such a shame )
  if ( result == NULL )
    goto FailExit03;

  result->image = pBitmapBuffer;
  result->bytes = sBitmapFileHeader + sBitmapInfo + sPixelBuffer;

  goto DONE;

  FailExit03:
    free( pBitmapBuffer );
  DONE:
  FailExit02:
    free( pPixelBuffer );
  FailExit01:
    free( pBitmapInfo );
    free( pBitmapFileHeader );
    DeleteDC( hScreenMemory );
    ReleaseDC( NULL, hScreen );
    DeleteObject( hBitmapOld );

  return result;
}

