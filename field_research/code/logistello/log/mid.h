// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#ifndef MID_H
#define MID_H

#include "move.h"

extern WERT AlphaBeta	(ZUGIO *pzio, int RealeHoehe, PARTEI Partei, 
			 WERT al, WERT be, int LetzterZug);
extern WERT AlphaBeta1	(ZUGIO *pzio, int RealeHoehe, PARTEI Partei, 
			 WERT al, WERT be, int LetzterZug, bool in_cut_search = false);


extern void FindPath	(ZUGIO *pzio, PARTEI Partei, SFPOS Zug, int al, int be, int Wert, ValType type);

#endif
