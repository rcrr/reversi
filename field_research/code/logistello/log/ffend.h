// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2


#ifndef FFEND_H
#define FFEND_H

#include "board.h"
#include "goodies.h"
#include "game.h"
#include "move.h"

#define FF_NUM  57


int FFEndAlphaBeta(
  ZUGIO		*pzio,		// pointer to global variables 
  PARTEI	Partei,		// player to move
  int		al,		// alpha-beta window
  int		be,
  int           last_move,
  SFPOS         *free_list
);


#endif
