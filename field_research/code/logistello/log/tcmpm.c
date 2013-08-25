// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/* find "close" game-starts / 6.95 */

#include "main.h"
#include <sys/time.h>

#include "playm.h"
#include "pmove.h"
#include "eval.h"
#include "sboard.h"

#include "goodies.h"
#include "filecom.h"
#include "crt.h"
#include "playgm.h"
#include "lib.h"
#include "fpatt.h"
#include "hash.h"

#define MAX_GAME_NUM 1000

void _abort(void) {}


int main(int argc, char **argv)
{
  int    i, j, res[2], ngames, nmax=-1, diff[MAX_GAME_NUM], or[MAX_GAME_NUM];
  FILE   *fp;
  SPFELD tab, board;
  GAME   game;
 

  FOR (j, MAX_GAME_NUM) or[j] = 0;


  for (i=1; argv[i]; i++) {

    if (!(fp=fopen(argv[i], "r"))) break;

    FOR (j, MAX_GAME_NUM) diff[j] = 0;

    ngames = 0;

    FOREVER {

      if (ngames >= MAX_GAME_NUM) Error("too many games");

      FOR (j, 2) {

        if (fTabEin(fp, &tab)) break;

        if (!Tab2Game(&tab, &game)) break;
        if (PlayGame(game.MoveNum, &game, &board)) break;
        res[j] = SfAnzBLACK(&board) - SfAnzWHITE(&board);

      }

      if (j < 2) break;


      if (sgn(res[0])-sgn(res[1]) != 0) diff[ngames]++;


      ngames++;

#if 0
if (ngames == 68) printf("%d %d %d\n", res[0], res[1], diff[ngames-1]);
#endif

    }

    if (ngames > nmax) nmax = ngames; 
    if (nmax != ngames) printf("different number of games\n");

    printf("%d pairs\n", ngames);

    FOR (j, ngames) printf("%d", diff[j] != 0);
    printf("\n");


    FOR (j, ngames) or[j] |= diff[j] != 0;
  
  }

  if (nmax < 0) Error("no tournament");

printf("\nOR:\n");

  FOR (j, nmax) printf("%d", or[j]);
  printf("\n");

  FOR (j, nmax) if (!or[j]) printf("%d ", j+1);
  printf("\n");

  return 0;
}
