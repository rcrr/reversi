// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#ifndef PLAYM_H
#define PLAYM_H

#include "goodies.h"
#include "move.h"


#define STATE_WAIT	0
#define STATE_SEARCH	1

#define S_STATE_WAIT	2
#define S_STATE_ZUGERM	3
#define S_STATE_LIB	4


void SignalCheck(COMZIO *pcio, ZUGIO *pzio, bool no_count, bool block=false);

#endif
