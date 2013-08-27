// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#ifndef NEWEND_H
#define NEWEND_H

#include "move.h"

#define END_CHECK_DEPTH	6  // was 4
#define END_EVAL_DEPTH	11 // was 11
#define END_PERCENTILE  1.3

int EndIteration(
  ZUGIO *pzio,		/* pointer to global variables */
  PARTEI colour,	/* player to move              */
  int al, int be,	/* alpha-beta window           */
  int LastMove
);

void PrintIterStat(void);

#endif
