// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/* Spielfelddatei ausgeben / 2.4.92 */

#ifndef PRECOMP
#include "main.h"
#endif

 
#include "sboard.h"
#include "move.h"
#include "board.h"
#include "crt.h"


void _abort(void) { exit(1); }


int main(int argc, char **argv)
{
  int i;
  SPFELD sf;
  FILE   *fp;


  InitCrt();

  if (argc != 2) Error("usage: olook sfk-file");

  fp = fopen(argv[1], "r");

  if (!fp) Error("file not found\n");

  for (i=0;; i++) {

    if (fSfRead(fp, &sf, 1) != 1) break;

    printf("%d:\n\n-> ##\n\n", i+1);

    SfAus(&sf, BLACK, 0);


/*
SfBrett(&sfein[i], &Brett);
pb = &Brett;

FOR (j, 8) {
  FOR (k, 8) printf("%2d ", BR_LEER(pb, j*8+7-k));
  printf("\n");
}


printf("%"R_F" %"R_F" %"R_F"\n", MOBB(pb), MOBW(pb), MOBQ(pb));
*/
    printf("%d Steine, %d ", SfAnz(&sf), sf.Marke);

    if      (sf.Marke == MA_GEWONNEN) printf("SCHWARZ gewinnt");
    else if (sf.Marke == MA_VERLOREN) printf("WEISS gewinnt");
    else if (sf.Marke == MA_REMIS)    printf("remis");
    else if (sf.Marke >= MA_WKEIT && sf.Marke <= MA_WKEIT+100)
      printf("SCHWARZ gewinnt mit W'keit %.2f", (sf.Marke - MA_WKEIT)/100.0);
    else if (sf.Marke >= MA_DIFF && sf.Marke <= MA_DIFF+128)
      printf("#SCHWARZ-#WEISS = %d", sf.Marke - MA_DIFF - 64);
    else if (sf.Marke >= MA_WLD_MOVE && sf.Marke <= MA_WLD_MOVE+300) {
      
      SFPOS m;
      int   w;

      MoveWldDecode(sf.Marke, m, w);

      if (w >  0) printf("WIN  ");
      if (w == 0) printf("DRAW ");
      if (w <  0) printf("LOSS ");

      KoorAus(m);

    } else if (sf.Marke >= MA_VAL_MOVE && sf.Marke <= MA_VAL_MOVE+101*100) {
      
      SFPOS m;
      int   v;

      MoveValDecode(sf.Marke, m, v);

      printf("%d%% ", v+50);
      KoorAus(m);

    } else if (ZUG(sf.Marke - MA_ZUG)) KoorAus(sf.Marke - MA_ZUG); 
    else printf("unbekannte Marke!");

    printf("\n\n\n");
  
  }

  fclose(fp);
  return 0;
}
