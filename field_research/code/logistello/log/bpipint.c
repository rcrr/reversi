// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/* BPIP interface */

#include "main.h"
#include "sboard.h"
#include "crt.h"
#include "move.h"
#include "eval.h"
#include "killer.h"
#include "goodies.h"
#include "eval.h"
#include "featurem.h"
#include "fpatt.h"
/*#include "time.h"*/
#include "mid.h"


#define DEPTH 2
#define EVAL  EvalASlow

void _abort(void) { exit(1); }

static void Check(COMZIO *pcio, ZUGIO *pzio, bool no_count) {}

static ZUGIO zio;


double logeval(SPFELD *psf, int to_move)
{
  static int first = 1;
  double value;

#if 0
SfAus(psf, 0, 0);
printf("%d\n", to_move);
#endif

printf("*"); fflush(stdout);

  if (first) {

    first = 0;

    strcpy(ParameterFile, "evala.par");
    strcpy(TableFile,     "oplay.tab");

    InitZug(&zio, EVAL, Check, HASHBITS, HASHMAXDEPTH);

    zio.cio.BewAnz = 0;

  }

  SfBrett(psf, &zio.Brett);

  zio.MaxTiefe   = 5;
  zio.Partei     = to_move;
  zio.LetzterZug = ZUG_UNBEKANNT;
  zio.SearchMode = SM_MIDGAME;
  zio.Quiescence = true;
  zio.Selective  = true;
  zio.Percentile = 1.5;
  zio.PathLen	 = 0;
  zio.PathValue	 = 0;
  zio.fertig   	 = false;
  zio.cio.timeout= false;

  zio.Wert	 = 0;
  zio.BestZug    = ZUG_UNBEKANNT;

  KillerAdjust(&zio.killer, psf);

  value = -WERT_TO_REAL(
      AlphaBeta1(&zio, DEPTH, to_move, -(WERTGEWINN+64), +(WERTGEWINN+64), ZUG_UNBEKANNT)
          );

#if 0
printf("v=%f\n", value);
#endif

  return -value;
}



