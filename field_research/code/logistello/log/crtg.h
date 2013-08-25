// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/* implementierungsabhängige Ausgaben */

#ifndef CRTG_H
#define CRTG_H

#include "sboard.h"

void C_FLUSH	(void);
void C_GOTOXY	(int x, int y);
void C_PRINTF	(char *format, ...);
void C_KOORAUS	(SFPOS SfPos, PARTEI Partei);

#endif
