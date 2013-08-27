// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/* generate boards for selective search analysis, 3.94 */

#ifndef PRECOMP
#include "main.h"
#endif

/* SET BOARD_SAVE true IN mid.c! IF GEN == true */


#include "sboard.h"
#include "crt.h"
#include "move.h"
#include "eval.h"
#include "killer.h"

#include "featurem.h"



#define GEN	false


#define EVAL	BewA


#define QUIESCENCE	true
#define SELECTIVE	false
#define PERCENTILE	1.5

#define DEPTH	6

#define DEPTH1	4
#define DEPTH2	8

#define PACC	0.53

void _abort(void) { exit(1); }



void Check(COMZIO *pcio, ZUGIO *pzio, bool no_count) {}

int savedepth;

int main(int argc, char **argv)
{
  int    j, val1, val2, val20;
  ZUGIO  zio;
  SPFELD sf;
  FILE   *fp;
  SFPOS  moves[65];
  REAL   P1, P2;


  if (argc != 2) Error("call: ogen sfk-file");


  InitZug(&zio, EVAL, Check, HASHBITS);


  if (!(fp=fopen(argv[1], "r"))) Error("open?");

  FOREVER {

    if (!fSfRead(fp, &sf, 1)) break;



    if (!SfMoeglZuege(&sf, BLACK, moves)) continue;


    zio.Sf	 = sf;
    zio.BewMitte = zio.cio.mBewMitte;
    zio.Modus    = MODUS_NORMAL;
    zio.Partei   = BLACK;

    zio.Selective  = SELECTIVE;
    zio.Percentile = PERCENTILE;
    zio.Quiescence = QUIESCENCE;
 
    savedepth = 0;


#if GEN

printf("."); fflush(stdout);

    for (j=1; j <= DEPTH-1; j++) {	/* Hash & Killer füllen */

      zio.MaxTiefe     = j;
      zio.Partei       = BLACK;
      zio.LetzterZug   = ZUG_UNBEKANNT;

      Zugermittlung(&zio);
    }

    savedepth = zio.MaxTiefe = DEPTH;
    Zugermittlung(&zio);

#else


    for (j=1; j <= DEPTH1; j++) {

      zio.MaxTiefe     = j;
      zio.Partei       = BLACK;
      zio.LetzterZug   = ZUG_UNBEKANNT;

      Zugermittlung(&zio);
    }

    val1 = zio.Wert;

    for (j=DEPTH1+1; j <= DEPTH2; j++) {

      zio.MaxTiefe     = j;
      zio.Partei       = BLACK;
      zio.LetzterZug   = ZUG_UNBEKANNT;

      Zugermittlung(&zio);

      if (j == DEPTH2-1) val20 = zio.Wert;

    }

    val2 = zio.Wert;

    if (val1 != val2) 
      printf("### %.5f %.5f\n", WERT_TO_REAL(val1), WERT_TO_REAL(val2));

#define BO (0.3)

    if ((val1 < REAL_TO_WERT(-BO) && val2 > REAL_TO_WERT(BO)) ||
        (val2 < REAL_TO_WERT(-BO) && val1 > REAL_TO_WERT(BO))) {

      SfAus(&sf, 0, 0);

    }


    P1 = EXPWERT(WERT_TO_REAL(val20));
    P2 = EXPWERT(WERT_TO_REAL(val2));

    if (P1 <= PACC && P1 >= 1.0-PACC && P2 <= PACC && P2 >= 1.0-PACC) {

      printf("-> ##\n");
      SfAus(&sf, 0, 0);

      printf("%.2f%% %.2f%% %.2f%%\n", P1, P2, (P1+P2)/2);

    }


#endif



  }

  return 0;
}



