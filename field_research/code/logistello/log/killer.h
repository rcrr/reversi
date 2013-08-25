// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#ifndef KILLER_H
#define KILLER_H

#include "move.h"


#define KILLER	true	/* Killertabelle benutzen? */


void InitKiller		(KILLDAT *pKiller);
void InitKiller1	(KILLDAT *pKiller);
void FreeKiller		(KILLDAT *pKiller);
void KillerUpdate	(KILLDAT *pKiller, PARTEI Partei, SFPOS Zug, SFPOS BestZug);
void KillerAdjust	(KILLDAT *pKiller, SPFELD *psf);

#endif
