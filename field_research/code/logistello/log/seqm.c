// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/* find move sequence 8.94 */

#ifndef PRECOMP
#include "main.h"
#endif

#include "lib.h"
#include "sboard.h"
#include "crt.h"
#include "goodies.h"
#include "game.h"
#include "tab.h"
#include "patt.h"


void _abort(void)
{}

GAME game[120000];
SPFELD Sf[120000];

int main(int argc, char **argv)
{
  int      i, num, gamenum, j;
  GAME     ga;
  FILE     *fp, *fpin, *fpout;
  char     file[200];
  SPFELD   tab, tab0, tab1, sf;

  if (argc != 3) {

error:

fprintf(stderr, "*** call: oseq lib-oko-file osp-file [-> osp-file.new\n");

    exit(20);
  }


  fpin = fopen(argv[2], "r");
  if (!fpin) Error("can't open osp-file");


  sprintf(file, "%s.new", argv[2]);
  fpout = fopen(file, "w");
  if (!fpout) Error("can't open osp-file.new");


  gamenum = 0;

  fp = fopen(argv[1], "r");
  if (!fp) Error("can't open lib-file");

  FOREVER {

    if (!fReadPackedGame(fp, &game[gamenum])) break;

    PlayGame(8, &game[gamenum], &Sf[gamenum]);
    gamenum++;
  }

  fclose(fp);

  FOREVER {

    if (fTabEin(fpin, &tab0)) break;

TabAus(&tab0);

    tab = tab0;

    FOR (i, 100) if (tab.p[i] >= NUM_DISP) tab.p[i] = LEER;

    num = SfAnz(&tab);


    FOR (i, gamenum) {    

      int k;
      SPFELD tboards[8];


      ga = game[i];

/*      PlayGame(num-4, &ga, &sf); */

/*TabAus(&tab); SfAus(&sf, BLACK, 0);*/

      Transform(&tab, tboards);


      FOR (k, 8) 
        if (SfGleich(&Sf[i], &tboards[k])) {

printf("."); fflush(stdout);

        ga.MoveNum = num-4;
        Game2Tab(&ga, &tab1);

#if 0      
        FOR (j, 100) {

          if (tab1.p[j] >= NUM_DISP) {
	    if (tab0.p[j] >= NUM_DISP) Error("???");
	    tab0.p[j] = tab1.p[j];
          }
        }
#endif

        fTabAus(fpout, &tab0);
        break;
      }

      if (k < 8) break;

    }

printf(":"); fflush(stdout);


  }

  fclose(fpin); fclose(fpout);

  return 0;
}


