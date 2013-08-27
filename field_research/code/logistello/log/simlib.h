// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#ifndef SIMLIB_H
#define SIMLIB_H

#include "sboard.h"
#include "move.h"

typedef struct {

  bool	 UseLib;		/* true <=> use library	*/
  char	 LibName[100];		/* filename 		*/

  int    MaxDiscNum;
  int	 BoardNum;
  SPFELD *Boards;

} SIMLIB;


extern SIMLIB *InitSimLib(char *BiblName);
extern SFPOS   SimLibMove(SIMLIB *pbi, SPFELD *psf, PARTEI Partei);

#endif
