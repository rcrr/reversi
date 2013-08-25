// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/* filter positions ---> corner move? <---, 6.95 */

#include "main.h"

#include "sboard.h"
#include "board.h"
#include "eval.h"

#include "goodies.h"
#include "crt.h"

#define S_LEN 300

int corners[4] = { A1, A8, H1, H8 };

void _abort(void) {}


int main(int argc, char **argv)
{
  char s[S_LEN];
  int    i, player;
  SPFELD board; 
  FILE   *fpboards, *fpevals;

#if 0

  if (argc != 3) Error("call: ofil sfk-file eval-file");

  fpboards = fopen(argv[1], "r");
  fpevals  = fopen(argv[2], "r");
  
  if (!fpboards || !fpevals) Error("file?");

  FOREVER {

    if (!fSfRead(fpboards, &board, 1)) break;

    if (!fgets(s, S_LEN-1, fpevals)) Error("no eval?");

    for (player = -1; player <= 1; player +=2) {

      FOR (i, 4) if (SfSetzen(&board, player, corners[i])) break;

      if (i < 4) break;

    }

    if (player <= 1) printf(s);
  }

#else

#define DMAX    (+1.0)

#define INT_NUM 10
#define DLEN    (DMAX/INT_NUM)
 
  int   basket;
  float a, b, v[9];
  struct { float v; int n; } d[INT_NUM];


  if (argc != 3) Error("call: ofil eval-file basket");

  FOR (i, INT_NUM) d[i].v = d[i].n = 0;

  fpevals  = fopen(argv[1], "r");

  if (sscanf(argv[2], "%d", &basket) != 1) Error("basket?");

  
  if (!fpevals) Error("evals?");

  FOREVER {

    if (!fgets(s, S_LEN-1, fpevals)) break;

    if (sscanf(s, "%f %f %f %f %f %f %f %f %f", 
      v, v+1, v+2, v+3, v+4, v+5, v+6, v+7, v+8) != 9) break;

    a = fabs(v[0]-v[2]);
    b = fabs(v[8]-v[4]);

    b = b*b;

    i = round(a/DLEN);
    if (i >= INT_NUM) i = INT_NUM-1;    

    if (i == basket) printf(s);
    d[i].v += b;  
    d[i].n ++;

  }

  if (basket < 0) 

  FOR (i, INT_NUM) printf("%2d %.2f  %.2f %d\n", 
    i, i*DLEN, sqrt(d[i].v/(d[i].n+0.01)), d[i].n);

#endif

  return 0;
}



