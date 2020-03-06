#include <stdio.h>
#include <windows.h>
#include "WinShot.h"

////////////////////////////////////////////////////////////////////////////////

HBitmap* ScreenShot( int x, int y, int width, int height ) {
  HBitmap*          result            = NULL;

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

  // return null if reallocation failed for some reason
  if ( pBitmapInfo == NULL )
    goto FailExit01;

  // allocate enough bytes for storing pixels
  pPixelBuffer = malloc( sPixelBuffer );

  // return null if allocation failed
  if ( pPixelBuffer == NULL )
    goto FailExit01;

  // get full BITMAPINFO structure this time and return NULL if it fails
  if( !GetDIBits( hScreenMemory, hBitmap, 0, pBitmapInfo->bmiHeader.biHeight, pPixelBuffer, pBitmapInfo, DIB_RGB_COLORS ) )
    goto FailExit02;

  // initialize the BITMAPFILEHEADER
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
  result = malloc( sizeof( HBitmap ) );

  // return NULL if allocation fails
  // and clean up successfully allocated bitmap buffer ( such a shame )
  if ( result == NULL )
    goto FailExit03;

  // insert data into resulting HBitmap structure
  result->image = pBitmapBuffer;
  result->bytes = sBitmapFileHeader + sBitmapInfo + sPixelBuffer;

  // return result
  goto DONE;

  // memory cleanup (we have to be good peoples :)
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

  // return the HBitmap
  return result;
}

////////////////////////////////////////////////////////////////////////////////

/*
  This might be a complete mess, or it might work perfectly, I'm not sure. It
  was suprisingly hard to find a solid resource on the BitMap standard and ones
  that I did find were conflicting with each other on some things. So yeah, it
  works on my PC :)
*/
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

////////////////////////////////////////////////////////////////////////////////

HScreenList* EnumScreens() {
  int           cnt  = 0;     // number of detected screens
  HScreenList*  list = NULL;  // list of detected screens

  // get number of detected screens so we can allocate enough memory for a list
  EnumDisplayMonitors( NULL, NULL, ScreenCounter, (LPARAM) &cnt );

  // allocate enough memory to store screen info for every detected screen
  list = (HScreenList*) malloc( sizeof( int ) + cnt * sizeof( HScreen ) );

  // return null if memory allocation fails
  if ( list == NULL ) return NULL;

  // initialize the counter for ScreenInformer
  list->cnt = 0;

  // get position and size of each monitor into list
  EnumDisplayMonitors( NULL, NULL, ScreenInformer, (LPARAM) list );

  // return the pointer containing a list of detected screens
  return list;
}

BOOL ScreenCounter( HMONITOR hMonitor, HDC hDC, LPRECT pRect, LPARAM pCnt ) {
  int* pCounter = (int*) pCnt;  // cast the caster pointer
  (*pCounter)++;                // increase the counter value

  // continue iterating over screens
  return TRUE;
}

BOOL ScreenInformer( HMONITOR hMonitor, HDC hDC, LPRECT pRect, LPARAM pHScreenList ) {
  HScreenList*  list = (HScreenList*) pHScreenList; // cast to HScreenList
  int           cnt  = list->cnt;                   // get current index

  MONITORINFO info;
              info.cbSize = sizeof( MONITORINFO ); // needed for G...M...InfoA

  // get the screen info
  GetMonitorInfoA( hMonitor, &info );

  // store relevant screen info into the list
  list->screens[cnt].vsx = info.rcMonitor.left;
  list->screens[cnt].vsy = info.rcMonitor.top;
  list->screens[cnt].vsw = abs( info.rcMonitor.left - info.rcMonitor.right );
  list->screens[cnt].vsh = abs( info.rcMonitor.top  - info.rcMonitor.bottom );

  // increase the index
  list->cnt++;

  // continue iterating over screens
  return TRUE;
}
