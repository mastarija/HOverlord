#include <stdio.h>
#include <windows.h>

struct Test
  { double width
  ; double height
  ;
  };

struct Test* testRun() {
  struct Test* result = (struct Test*) malloc( sizeof(struct Test) );
  result->width = 77.5;
  result->height = 88.8;
  return result;
}

BITMAP* TakeScreenShot () {
  int width  = 1366;
  int height = 768;

  HDC     hScreen       = GetDC( NULL );
  HDC     hMemoryScreen = CreateCompatibleDC( hScreen );

  HBITMAP hNewBit       = CreateCompatibleBitmap( hScreen, width, height );
  HBITMAP hOldBit       = SelectObject( hMemoryScreen, hNewBit );

  BOOL    bCopied       = BitBlt( hMemoryScreen, 0, 0, width, height, hScreen, 0, 0, SRCCOPY );

  HBITMAP result        = bCopied ? hNewBit : NULL;

  BITMAP* bitmap;
  GetObject( result, sizeof(bitmap), bitmap );

  SelectObject( hMemoryScreen, hOldBit );

  DeleteDC  ( hMemoryScreen );
  ReleaseDC ( NULL, hScreen );

  return bitmap;
}

int screenshot () {
  int width  = 1366;
  int height = 768;

  HDC     hScreen       = GetDC( NULL );
  HDC     hMemoryScreen = CreateCompatibleDC( hScreen );

  HBITMAP hNewBitmap    = CreateCompatibleBitmap( hScreen, width, height );
  HBITMAP hOldBitmap    = SelectObject( hMemoryScreen, hNewBitmap );

  BOOL    bCopied       = BitBlt( hMemoryScreen, 0, 0, width, height, hScreen, 0, 0, SRCCOPY );

  if ( !bCopied )
    printf( "failed to copy" );

  BITMAPINFO  BitmapInfo = { 0 };
              BitmapInfo.bmiHeader.biSize = sizeof( BitmapInfo.bmiHeader );

  if ( 0 == GetDIBits( hMemoryScreen, hNewBitmap, 0, 0, NULL, &BitmapInfo, DIB_RGB_COLORS ) )
    printf( "failed to get header" );

  // BYTE* pixels = new BYTE[BitmapInfo.bmiHeader.biSizeImage];
  BYTE* pixels = malloc( sizeof(BYTE) * BitmapInfo.bmiHeader.biSizeImage );
  BitmapInfo.bmiHeader.biBitCount = 32;
  BitmapInfo.bmiHeader.biCompression = BI_RGB;
  BitmapInfo.bmiHeader.biHeight = abs( BitmapInfo.bmiHeader.biHeight );

  if ( 0 == GetDIBits( hMemoryScreen, hNewBitmap, 0, BitmapInfo.bmiHeader.biHeight, pixels, &BitmapInfo, DIB_RGB_COLORS ) )
    printf( "failed to get bits" );

  FILE* hFile = fopen( "test.bin", "wb" );
  fwrite( pixels, sizeof(BYTE), BitmapInfo.bmiHeader.biSizeImage, hFile );
  fclose( hFile );

  // clean up
  SelectObject( hMemoryScreen, hOldBitmap );
  DeleteDC    ( hMemoryScreen );
  ReleaseDC   ( NULL, hScreen );
  DeleteObject( hNewBitmap );

  return 0;
}


// HBITMAP TakeScreenShot () {
//   int width  = 1366;
//   int height = 768;


//   HDC     hScreen       = GetDC( NULL );
//   HDC     hMemoryScreen = CreateCompatibleDC( hScreen );

//   HBITMAP hNewBit       = CreateCompatibleBitmap( hScreen, width, height );
//   HBITMAP hOldBit       = SelectObject( hMemoryScreen, hNewBit );

//   BOOL    bCopied       = BitBlt( hMemoryScreen, 0, 0, width, height, hScreen, 0, 0, SRCCOPY );

//   HBITMAP result        = bCopied ? hNewBit : NULL;

//   BITMAP  bitmap;
//   GetObject( result, sizeof(bitmap), (LPVoid)&bitmap );

//   SelectObject( hMemoryScreen, hOldBit );

//   DeleteDC  ( hMemoryScreen );
//   ReleaseDC ( NULL, hScreen );
// }

// void ClearScreenShot ( HBITMAP hNewBit ) {
//   DeleteObject( hNewBit );
// }
