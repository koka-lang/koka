/* 
   Copyright 2012 Microsoft Corporation.

   This is free software; you can redistribute it and/or modify it under the
   terms of the Apache License, Version 2.0. A copy of the License can be
   found in the file "license.txt" at the root of this distribution.
*/


#include "cconsole.h"

/*------------------------------------------------------------------------
  Colors
------------------------------------------------------------------------*/
#define MaskRed       0x01
#define MaskGreen     0x02
#define MaskBlue      0x04
#define MaskHighlight 0x08
#define MaskRGB       (MaskRed | MaskGreen | MaskBlue)

#define MaxColor     15
#define DefaultColor (MaxColor+1)

/*------------------------------------------------------------------------
  Windows console functionality
------------------------------------------------------------------------*/
#if defined(__WIN32__) || defined(__MINGW32__) || defined(__CYGWIN__)

#include <windows.h>

#ifndef COMMON_LVB_REVERSE_VIDEO   
#define COMMON_LVB_REVERSE_VIDEO   0x4000 
#endif

#ifndef COMMON_LVB_UNDERSCORE      
#define COMMON_LVB_UNDERSCORE      0x8000 
#endif

#define FOREGROUND_MASK   (FOREGROUND_RED | FOREGROUND_BLUE | FOREGROUND_GREEN | FOREGROUND_INTENSITY)
#define BACKGROUND_MASK   (BACKGROUND_RED | BACKGROUND_BLUE | BACKGROUND_GREEN | BACKGROUND_INTENSITY)

#define DefaultAttributes (FOREGROUND_RED | FOREGROUND_BLUE | FOREGROUND_GREEN)

static WORD consoleAttributes   = DefaultAttributes;  /* last known attribute */
static WORD consoleInitialAttrs = DefaultAttributes;  /* attributes when application was initialized */
static WORD consoleInitialized  = 0;

static void consoleSetAttributes( WORD attributes )
{
  HANDLE hstdout = GetStdHandle(STD_OUTPUT_HANDLE); 
  if (hstdout!=INVALID_HANDLE_VALUE) {
	if (SetConsoleTextAttribute(hstdout, attributes)) {
      consoleAttributes = attributes;
	}
	/* CloseHandle(hstdout); */  /* Gives an error */
  } 
}

int consoleInit()
{
  /* save original settings */
  if (consoleInitialized <= 0) {
    CONSOLE_SCREEN_BUFFER_INFO consoleInfo; 
    HANDLE hstdout;
  
    hstdout = GetStdHandle(STD_OUTPUT_HANDLE); 
    if (hstdout!=INVALID_HANDLE_VALUE) {
	  if (GetConsoleScreenBufferInfo(hstdout, &consoleInfo)) {
        consoleAttributes  = consoleInfo.wAttributes;
        consoleInitialAttrs= consoleAttributes;
        consoleInitialized = 1;    
	  }
	  /* CloseHandle(hstdout); */  /* Gives an error */
    }
  }
  else {
    consoleInitialized++;
  }
  return (consoleInitialized != 0);
}

void consoleDone()
{
  /* restore original settings */
  if (consoleInitialized > 0) {
    consoleInitialized--;
    if (consoleInitialized == 0) {
      consoleSetAttributes( consoleInitialAttrs );
    }
  }
}

void consoleSetColor( int color )
{
  if (!consoleInitialized) return;
  if (color >= 0 && color <= MaxColor) {
    /* set the color */
    WORD attributes;
    WORD winColor = (color & MaskRed      ? FOREGROUND_RED : 0) |
                    (color & MaskGreen    ? FOREGROUND_GREEN : 0) |
                    (color & MaskBlue     ? FOREGROUND_BLUE : 0) |
                    (color & MaskHighlight? FOREGROUND_INTENSITY : 0);
    attributes = (consoleAttributes & ~FOREGROUND_MASK) | winColor;
    consoleSetAttributes(attributes);
  }
  else {
    /* default color */
    consoleSetAttributes( (consoleAttributes & ~FOREGROUND_MASK) | (consoleInitialAttrs & FOREGROUND_MASK) );
  }
}

void consoleSetBackColor( int color )
{
  if (!consoleInitialized) return;
  if (color >= 0 && color <= MaxColor) {
    /* set the color */
    WORD attributes;
    WORD winColor = (color & MaskRed      ? BACKGROUND_RED : 0) |
                    (color & MaskGreen    ? BACKGROUND_GREEN : 0) |
                    (color & MaskBlue     ? BACKGROUND_BLUE : 0) |
                    (color & MaskHighlight? BACKGROUND_INTENSITY : 0);
    attributes = (consoleAttributes & ~BACKGROUND_MASK) | winColor;
    consoleSetAttributes(attributes);
  }
  else {
    /* default color */
    consoleSetAttributes( (consoleAttributes & ~BACKGROUND_MASK) | (consoleInitialAttrs & BACKGROUND_MASK) );
  }
}

void consoleSetReverse( int rev )
{
  if (consoleInitialized) {
    WORD attributes = (consoleAttributes & ~COMMON_LVB_REVERSE_VIDEO) | (rev ? COMMON_LVB_REVERSE_VIDEO : 0);
    consoleSetAttributes(attributes);
  }
}

void consoleSetUnderline( int underline )
{
  if (consoleInitialized) {
    WORD attributes = (consoleAttributes & ~COMMON_LVB_UNDERSCORE) | (underline ? COMMON_LVB_UNDERSCORE : 0);
    consoleSetAttributes(attributes);
  }
}

int consoleGetState( void )
{
  return (int)(consoleAttributes);
}

void consoleSetState( int state )
{
  WORD attributes = (WORD)state;
  consoleSetAttributes(attributes);
}

const char* consoleGetProgramPath()
{
  static int cached = 0;
  static char programPath[256];
  static TCHAR tprogramPath[256];
  if (cached==0) {
    DWORD n = GetModuleFileName( NULL, tprogramPath, 256 );
    if (n < 0) n = 0;
    int i;
    for(i = 0; i < n; i++) {
      programPath[i] = (char)tprogramPath[i];
    }  
    programPath[i] = 0;
    cached = 1;
  }
  return programPath;
}

/*------------------------------------------------------------------------
   Unsupported console functionality
------------------------------------------------------------------------*/
#else

int consoleInit()
{
  return 0;
}

void consoleDone()
{ 
}

void consoleSetColor( int color )
{  
}

void consoleSetBackColor( int color )
{
}

void consoleSetReverse( int rev )
{
}

void consoleSetUnderline( int underline )
{
}

int consoleGetState( void )
{
  return (-1);
}

void consoleSetState( int state )
{
}

const char* consoleGetProgramPath()
{
  return "";
}
#endif
