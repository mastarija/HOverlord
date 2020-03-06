#include <windows.h>

////////////////////////////////////////////////////////////////////////////////

/*
  Data structure used to pass information about an inmemory Bitmap. It contains
  a pointer to the Bitmap data and a number of bytes that make up the image (or
  how many bytes of data should one read from the image pointer to get the full
  image).
*/
typedef struct
  { void*         image; // pointer to the image data
    unsigned long bytes; // number of image bytes
  } HBitmap;

/*
  This data structure provides information about the virtual screens. It
  consists of X and Y coordinates of the virtual screens on a "mega screen" and
  their respective width and height.
*/
typedef struct
  { long vsx; // virtual screen x coordinate
    long vsy; // virtual screen y coordinate
    long vsw; // virtual screen width
    long vsh; // virtual screen height
  } HScreen;

/*
  A "dynamic" list of screen sizes. It contains a variable with number of
  elements in a list and a list of screen values. It is used to enumerate
  available screens.
*/
typedef struct
  { int     cnt;        // number of screens in a list
    HScreen screens[1]; // a list of screens
  } HScreenList;

/*
  Alias for "free" specialized for HBitmap. Used to free memory from Haskell.
*/
void FreeHBitmap( HBitmap* hBitmap );

/*
  Alias for "free" specialized for HScreenList. Used to free memory from
  Haskell.
*/
void FreeHScreenList( HScreenList* hBitmap );

/*
  Grabs an area of pixels defined by width, height and XY coordinates, and
  returns them in the bitmap format.
*/
HBitmap* ScreenShot( int x, int y, int width, int height );

/*
  Returns a list of all detected virtual screens. This is mostly a utility
  function intended to be used with ScreenShot.
*/
HScreenList* EnumScreens();

////////////////////////////////////////////////////////////////////////////////

/*
  Internal utility function used to generate a proper bitmap format.
*/
DWORD ColorTableSize( BITMAPINFOHEADER* h );

/*
  Internal utility function used to get the number of virtual screens through
  Win32 API.
*/
BOOL ScreenCounter( HMONITOR hMonitor, HDC hDC, LPRECT pRect, LPARAM pCnt );

/*
  Internal utility function used to get the list (HScreenList) of relevant
  information on each of virtual screens through Win32 API.
*/
BOOL ScreenInformer( HMONITOR hMonitor, HDC hDC, LPRECT pRect, LPARAM pSCRHelper );
