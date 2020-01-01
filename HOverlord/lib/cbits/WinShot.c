#include <stdio.h>
#include <windows.h>
////////////////////////////////////////////////////////////////////////////////

typedef struct { void* image; unsigned long bytes; } BMP;

////////////////////////////////////////////////////////////////////////////////

BMP* ScreenShot() {
  int               x             = 0;
  int               y             = 0;

  int               width         = 1366;
  int               height        = 768;

  HDC               hScreen       = GetDC( NULL );
  HDC               hScreenMemory = CreateCompatibleDC( hScreen );

  HBITMAP           hBitmap       = CreateCompatibleBitmap( hScreen, width, height );
  HBITMAP           hBitmapOld    = SelectObject( hScreenMemory, hBitmap );

  BITMAPINFO*       pbinfo        = ( BITMAPINFO* ) malloc( sizeof( BITMAPINFOHEADER ) );
  BITMAPFILEHEADER* pbhead        = ( BITMAPFILEHEADER* ) malloc( sizeof( BITMAPFILEHEADER ) );
  BYTE*             ppixel        = NULL;
  BYTE*             bitmap        = NULL;

  DWORD             rgbcnt        = 0;
  DWORD             infosz        = 0;

  DWORD             fhsize        = 0;
  DWORD             bisize        = 0;
  DWORD             pxsize        = 0;

  //////////////////////////////////////////////////////////////////////////////

  // copy pixels from "real" screen to "in memory" screen
  BitBlt( hScreenMemory, 0, 0, width, height, hScreen, x, y, SRCCOPY );

  //////////////////////////////////////////////////////////////////////////////

  // get BITMAPINFOHEADER data into BITMAPINFO structure
  pbinfo->bmiHeader.biSize = sizeof( BITMAPINFOHEADER );
  if( !GetDIBits( hScreenMemory, hBitmap, 0, 0, NULL, pbinfo, DIB_RGB_COLORS ) )
    return NULL;

  // calculate number of additional bytes to allocate for Color Table
  switch ( pbinfo->bmiHeader.biBitCount ) {
    case 24:
      rgbcnt = pbinfo->bmiHeader.biClrUsed;
      break;
    case 16:
    case 32:
      if ( pbinfo->bmiHeader.biCompression == BI_RGB )
        rgbcnt = pbinfo->bmiHeader.biClrUsed;
      else if ( pbinfo->bmiHeader.biCompression == BI_BITFIELDS )
        rgbcnt = 3;
      break;
    default: // for 0, 1, 2, 4, and 8
      if ( pbinfo->bmiHeader.biClrUsed == 0 )
        rgbcnt = ( 1 << pbinfo->bmiHeader.biBitCount ); // 2^biBitCount
      else
        rgbcnt = pbinfo->bmiHeader.biClrUsed;
      break;
  }



  // reallocate BITMAPINFO data structure in case there is a Color Table
  if ( rgbcnt > 0 )
    infosz = sizeof( BITMAPINFOHEADER ) + sizeof( RGBQUAD ) * rgbcnt;
  else
    infosz = sizeof( BITMAPINFOHEADER );

  if ( rgbcnt > 0 )
    pbinfo = realloc( pbinfo, infosz );

  if ( pbinfo == NULL )
    return NULL;

  // allocate enough bytes for storing pixels
  ppixel = malloc( pbinfo->bmiHeader.biSizeImage );
  if ( ppixel == NULL )
    return NULL;

  // get full BITMAPINFO structure this time
  if( !GetDIBits( hScreenMemory, hBitmap, 0, pbinfo->bmiHeader.biHeight, ppixel, pbinfo, DIB_RGB_COLORS ) )
    return NULL;

  // initialize BITMAPFILEHEADER
  pbhead->bfType = 0x4D42; // 0x4D B, 0x42 M : BM ( bitmap )
  pbhead->bfSize = sizeof( BITMAPFILEHEADER ) + infosz + pbinfo->bmiHeader.biSizeImage;
  pbhead->bfReserved1 = 0;
  pbhead->bfReserved2 = 0;
  pbhead->bfOffBits   = sizeof( BITMAPFILEHEADER ) + infosz;

  // allocate enough space to keep complete BMP file in memory
  bitmap = malloc( pbhead->bfSize );

  // copy file header, bitmap info and pixels into single memory block
  memcpy( bitmap, pbhead, sizeof( BITMAPFILEHEADER ) );
  memcpy( bitmap + sizeof( BITMAPFILEHEADER ), pbinfo, infosz );
  memcpy( bitmap + sizeof( BITMAPFILEHEADER ) + infosz, ppixel, pbinfo->bmiHeader.biSizeImage );

  // start writing data to a file
  FILE* hfile = fopen( "test.bmp", "wb" );
  fwrite( bitmap, pbhead->bfSize, 1, hfile );

  return NULL;
}

