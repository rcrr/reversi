// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/* Textausgabe auf Grafik */

#ifndef PRECOMP
#include "main.h"
#endif

#include "crt.h"
#include "attr.h"


extern int fen;


void C_FLUSH(void) {}


void C_GOTOXY(int x, int y)
{
  cGotoXY(fen, x, y);
}



void C_PRINTF(char *format, ...)
{
  long *parpo;


  parpo = (long*)&format;

  cprintf(fen, format, *(parpo+1), *(parpo+2), *(parpo+3), *(parpo+4),
		       *(parpo+5), *(parpo+6), *(parpo+7), *(parpo+8));
}



void C_KOORAUS(SFPOS Pos, PARTEI Partei)
{
  C_PRINTF("%c%c", 'a' + 7 - Pos % 8, '1' + Pos / 8);
}


