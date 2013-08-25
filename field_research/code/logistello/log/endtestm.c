// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/* test new endgame 1.95 */

#include "main.h"
#include "sboard.h"
#include "crt.h"
#include "move.h"
#include "eval.h"
#include "killer.h"
#include "fpatt.h"


#define OSPFILE "test.osp"

#define TEST_MODUS	MODUS_NORMAL
#define EVALFUNC	EvalA
#define PARFILE         "evala.par"
#define TABFILE         "oplay.tab"

#define TESTANZ		5000
#define TIEFE		7


#define SFMAX		5000


int xx = 0;

int VERB = 1;



void _abort(void) { exit(1); }



void Check(COMZIO *pcio, ZUGIO *pzio, bool no_count) 
{
  Chk_Abort();
}



int main(int argc, char **argv)
{
  int   extranz=0, extrfalsch = 0;
  int   z, Tiefe, anz, i, j, k, r, r0, r1, begin, colour;
  uint4 Start, Ende, EinzelnClock, DanachClock, IterClock=0, 
	EinzelnKB=0, EinzelnKW=0, IterKB=0, IterKW=0;
  ZUGIO zio;
  BRETT Brett;
  SPFELD tab, sf;
  int  w;
  FILE *fp;
  GAME game;


  if (argc == 2) begin = atoi(argv[1]); else begin = 0;

  InitZug(&zio, EVALFUNC, Check, HASHBITS);

  strcpy(ParameterFile, PARFILE);
  strcpy(TableFile,     TABFILE);


  EinzelnClock = DanachClock = IterClock = 0;

  fp = fopen(OSPFILE, "r");

  if (!fp) Error("file?");

  for (i=0; i < TESTANZ; i++) {

    if (fTabEin(fp, &tab)) break;

    if (i < begin) continue;

    if (Tab2Game(&tab, &game)) { 

      colour = PlayGame(game.MoveNum, &game, &sf);
 
    } else {

      TabToSf(&tab, &sf);
      colour = tab.Marke;

    }

if (colour == BLACK) printf("-> ##\n\n");
else if (colour == WHITE) printf("-> ()\n\n");
else Error("colour?");

SfAus(&sf, colour, 0);

    Start = clock();

    InitIterAlphaBeta(&zio);



    FOREVER {

      int value;

      zio.Sf = sf;

      SfBrett(&zio.Sf, &zio.Brett);

      zio.DiscNum0 = SfAnz(&zio.Sf);

      zio.PathLen         = 0;
      zio.PathValue       = 0;
      zio.fertig          = false;
      zio.cio.timeout     = false;

      zio.Wert     = 0;
      zio.BestZug  = ZUG_UNBEKANNT;               /* bisher kein Zug gefunden */

#if KILLER
  KillerAdjust(&zio.killer, &zio.Sf);
#endif

      value = IterAlphaBeta(0, colour, -WERTGEWINN, +WERTGEWINN, ZUG_UNBEKANNT);

printf("hv=%2d%% ", round(EXPWERT(WERT_TO_REAL(value))*100.0)); fflush(stdout);

if (abs(value) >= WERTGEWINN) exit(0);

      UpdateWLD(&sf, colour);

      Ende  = clock();

printf(" t=%.1f\n", ((REAL)(Ende-Start)) / CLOCKS_PER_SEC);

#if 0
      { SPFELD sf;
        BrettSf(&zio.Brett, &sf);
        SfAus(&sf, 0, 0);
      }
#endif

    }


    EinzelnClock += Ende - Start;


  }


  fclose(fp);

  return 0;
}





