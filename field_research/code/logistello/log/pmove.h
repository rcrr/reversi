// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/* Prototypen von pmove.c */

#ifndef PMOVE_H
#define PMOVE_H

#include "move.h"


extern  void	InitCompzug(void);
extern  SFPOS	CompZug(ZUGIO *pzio);
extern  void	ZugDauer(SPFELD *, REAL Restzeit, REAL *pNorm, REAL *pMax);
extern	void	StatAus(COMZIO *pcio, ZUGIO *pzio);


#endif
