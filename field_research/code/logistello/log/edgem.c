// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/* look at edges , 8.94 */

#ifndef PRECOMP
#include "main.h"
#endif

#include "sboard.h"
#include "crt.h"
#include "move.h"
#include "eval.h"
#include "killer.h"
#include "fpatt.h"


#define TEST_MODUS	MODUS_NORMAL
#define EVALFUNC	EvalA
#define PARFILE         "evala.par"
#define TABFILE         "oplay.tab"

#define GAMEFILE        "test.oko"

#define TESTNUM         54


void _abort(void) { exit(1); }

bool mask[100] = {

  0,0,0,0,0,0,0,0,0,0,
  0,1,1,1,1,1,1,1,1,0,
  0,1,1,0,0,0,0,1,1,0,
  0,1,0,0,0,0,0,0,1,0,
  0,1,0,0,0,0,0,0,1,0,
  0,1,0,0,0,0,0,0,1,0,
  0,1,0,0,0,0,0,0,1,0,
  0,1,1,0,0,0,0,1,1,0,
  0,1,1,1,1,1,1,1,1,0,
  0,0,0,0,0,0,0,0,0,0

};


int main(void)
{
  int i;
  FILE *fp;
  GAME game;
  SPFELD board;


  fp = fopen(GAMEFILE, "r");

  if (!fp) Error("file not found");


  FOREVER {

    if (!fReadPackedGame(fp, &game)) break;


    PlayGame(TESTNUM-4, &game, &board);

    FOR (i, 100) 
      if (!mask[i]) board.p[i] = 0;

    SfAus(&board, 0, 0);

    PlayGame(60, &game, &board);

    FOR (i, 100) 
      if (!mask[i]) board.p[i] = 0;

    SfAus(&board, 0, 0);

    printf("----------------\n");    

  }

  fclose(fp);

  return 0;
}

