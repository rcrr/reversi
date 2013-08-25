// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/* propagate distributions, 6.95 */

#include "main.h"

#include "sboard.h"
#include "board.h"
#include "eval.h"

#include "goodies.h"
#include "crt.h"
#include "distr.h"



#define DEPTH    4
#define MOVE_NUM 4


float variance;


void _abort(void) {}


DISTR gdistr[60];
SFPOS gmoves[60];
int   gmovenum;

DISTR DNegaMax(BRETT *pboard, int player, int depth, int height)
{
  int i, move, movenum;
  SFPOS movelist[70];
  DISTR distr0, distr[60];
  float val;
  SPFELD sf;
  DELTA delta;


  if (height == 0) {

    val = EvalASlow(pboard, player)/100000.0;

    if (val > +(SUP_MAX-0.1)) val = +(SUP_MAX-0.1);
    if (val < -(SUP_MAX-0.1)) val = -(SUP_MAX-0.1);
    
    distr[0] = DNormal(val, variance);
            
    return distr[0];
  }

  FOR (i, SUP_NUM) distr0.d[i] = 1.0;

  BrettSf(pboard, &sf);
  movenum = SfMoeglZuege(&sf, player, movelist);

  if (!movenum) {

    if (!SfMoeglZuege(&sf, GEGNER(player), movelist)) 
      Error("game end");
 
    distr[0] = DNegaMax(pboard, GEGNER(player), depth, height);

    FOR (i, SUP_NUM-1) distr0.d[i] *= 1.0 - distr[0].d[SUP_NUM-1-i-1];
    distr0.d[SUP_NUM-1] = 1.0;

  } else {


    FOR (move, movenum) {

      MAKE_MOVE(pboard, player, movelist[move], &delta);
      distr[move] = DNegaMax(pboard, GEGNER(player), depth+1, height-1);
      UNDO_MOVE(pboard, &delta);

      FOR (i, SUP_NUM-1) distr0.d[i] *= 1.0 - distr[move].d[SUP_NUM-1-i-1];
      distr0.d[SUP_NUM-1] = 1.0;
    }
  
  }


  if (depth == 0) {

    FOR (move, movenum) { 

      FOR (i, SUP_NUM-1) gdistr[move].d[i] = 1.0 - distr[move].d[SUP_NUM-1-i-1];
      gdistr[move].d[SUP_NUM-1] = 1.0;

      gmoves[move] = movelist[move];
    }

    gmovenum = movenum;

  }

  return distr0;
}


int main(int argc, char **argv)
{
  int    i, depth, move1, move2;
  float  m, v, max;
  DISTR  distr;
  SPFELD sf;
  BRETT  board;
  FILE   *fp;
/*
printf("%f %f %f %f %f\n", Phi(-2), Phi(-1), Phi(0), Phi(1), Phi(2));
exit(0);
*/

#if 0
  distr = DNormal(0.6, 0.3);
  MeanVar(&distr, &m, &v);
  printf("mean=%f var=%f\n", m, v);
 
  distr = DNormal(1, 1);
  MeanVar(&distr, &m, &v);
  printf("mean=%f var=%f\n", m, v);
#endif


  InitSetzen();

  strcpy(ParameterFile, "evala.par");

  fp = fopen("test.sfk", "r");
  if (!fp) Error("file?");

FOREVER {

  if (!fSfRead(fp, &sf, 1)) break;


  if (SfMoeglZuege(&sf, BLACK, gmoves) < 2) Error("too few moves");

  SfBrett(&sf, &board);

  SfAus(&sf, 0, 0);

  for (depth=DEPTH; depth <= DEPTH; depth++) {

printf("depth %d\n", depth);

    variance = 0.01;
    distr = DNegaMax(&board, BLACK, 0, depth);
    MeanVar(&distr, &m, &v);
    printf("mean=%f var=%f\n", m, v);

    DMax(gdistr, gmovenum);

    max = -1;

    FOR (i, gmovenum) {

      if (gdistr[i].pmax > max) { max = gdistr[i].pmax; move1 = gmoves[i]; }

      KoorAus(gmoves[i]); 
      MeanVar(&gdistr[i], &m, &v);
      printf(" %5.1f   mean=%f var=%f\n", gdistr[i].pmax * 100, m, v);

    }

    max = -1;


    variance = 0.5;
    distr = DNegaMax(&board, BLACK, 0, depth);
    MeanVar(&distr, &m, &v);
    printf("mean=%f var=%f\n", m, v);

    DMax(gdistr, gmovenum);

    max = -1;

    FOR (i, gmovenum) {

      if (gdistr[i].pmax > max) { max = gdistr[i].pmax; move2 = gmoves[i]; }

      KoorAus(gmoves[i]); 
      MeanVar(&gdistr[i], &m, &v);
      printf(" %5.1f   mean=%f var=%f\n", gdistr[i].pmax * 100, m, v);

    }

printf("---> "); KoorAus(move1); printf(" "); KoorAus(move2);

    if (move1 != move2) printf(" ***\a");
    printf("\n");


  }

#if 0
  distr = DNormal(m , v);
  FOR (i, SUP_NUM) printf("%.0f ", distr.d[i] * 100);
  printf("\n");
#endif
}
  return 0;
}
