#include <windows.h>

using namespace std;

extern "C" {
  HBITMAP TakeScreenShot () {
    int     width  = 1366;
    int     height = 768;

    HDC     hScreen       = GetDC( NULL );
    HDC     hMemoryScreen = CreateCompatibleDC( hScreen );

    HBITMAP hNewBit       = CreateCompatibleBitmap( hScreen, width, height );
    HBITMAP hOldBit       = SelectObject( hMemoryScreen, hNewBit );

    BOOL    bCopied       = BitBlt( hMemoryScreen, 0, 0, width, height, hScreen, 0, 0, SRCCOPY );

    HBITMAP result        = bCopied ? hNewBit : NULL;

    BITMAP  bitmap;
    GetObject( result, sizeof(bitmap), (LPVoid)&bitmap );

    SelectObject( hMemoryScreen, hOldBit );

    DeleteDC  ( hMemoryScreen );
    ReleaseDC ( NULL, hScreen );
  }

  void ClearScreenShot ( HBITMAP hNewBit ) {
    DeleteObject( hNewBit );
  }
}

