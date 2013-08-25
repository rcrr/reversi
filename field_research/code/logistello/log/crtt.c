// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/* Textausgabe auf Text */

#ifndef PRECOMP
#include "main.h"
#endif

#include "crt.h"
#include "attr.h"

#include <stdarg.h>


void C_GOTOXY(int , int )
{
}

void C_FLUSH(void)
{
  fflush(stdout);
}


void C_PRINTF(char *format, ...)
{
  va_list ap;
  long p1, p2, p3, p4, p5, p6, p7, p8;


  va_start(ap, format);

  p1 = va_arg(ap, long);
  p2 = va_arg(ap, long);
  p3 = va_arg(ap, long);
  p4 = va_arg(ap, long);
  p5 = va_arg(ap, long);
  p6 = va_arg(ap, long);
  p7 = va_arg(ap, long);
  p8 = va_arg(ap, long);

  va_end(ap);

  printf(format, p1, p2, p3, p4, p5, p6, p7, p8);
}



void C_KOORAUS(SFPOS Pos, PARTEI)
{
  C_PRINTF("%c%c", 'a' + 7 - Pos % 8, '1' + Pos / 8);
}





