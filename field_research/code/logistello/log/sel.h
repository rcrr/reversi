// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#ifndef SEL_H
#define SEL_H

#include "move.h"

WERT	SelAlphaBeta	(ZUGIO *pzio, int RealeHoehe, PARTEI Partei, 
			 WERT al, WERT be, int LetzterZug);
WERT	SelAlphaBeta1	(ZUGIO *pzio, int RealeHoehe, int Tiefe, PARTEI Partei, 
			 WERT al, WERT be, int LetzterZug);
#endif
