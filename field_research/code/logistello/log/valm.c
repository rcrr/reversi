// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

// evaluate positions given in sfk-file / 1.95

#include "main.h"
#include "lib.h"
#include "sboard.h"
#include "crt.h"
#include "goodies.h"
#include "killer.h"
#include "eval.h"
#include "hash.h"
#include "fpatt.h"
#include "pcstat.h"

#define OUT		false

#if 1

// this setting allows fast clearing of hashtabs

#define HBITS      10
#define HMAXDEPTH  64
#define H0BITS     10
#define H0MAXDEPTH 9

#else

// this setting allows fast deep evaluations

#define HBITS      20
#define HMAXDEPTH  64
#define H0BITS     18
#define H0MAXDEPTH 9

#endif


#if 1
#define EVALFUNC	EvalB
#define MPC_FILE        "pcstatl"
#define PARFILE         "evala.par"
#define TABFILE         "oplay.tab"
#else
#define EVALFUNC        EvalL
#define MPC_FILE        "pcstatl"
#define PARFILE         "evall.par"
#define TABFILE         "new_l.tab"
#endif

static bool  selective  = false;
static float percentile = 1.0;
static const bool QUIESCENCE = true;
static int   max_depth  = 10;
static bool  write_label = false;


static ZUGIO zio;

static char newfile[200];



void _abort(void) { exit(1); }

void Check(COMZIO *, ZUGIO *, bool, bool ) {}	/* Signal-Handler */




static int saved=0, gamenum=0;


int main(int argc, char **argv)
{
  char     outfile[300];
  int      argi, depth, newnum, d, i, maxdepth;
  LIBRARY  *pT=NULL;
  GAME     game;
  FILE     *fp;
  SPFELD   sf;
  char     *file;
  POSINFO  PI;
  FILE     *fpout;

  strcpy(PCStatFile, MPC_FILE);

  if (argc < 2) {

error: 

    Error(" call: oval [-sel p] [-depth n] [-label] sfk-file");

  }

  argi = 1;

  if (!strcmp(argv[argi], "-sel")) {

    if (!argv[argi+1] || sscanf(argv[argi+1], "%f", &percentile) != 1 || 
	percentile < 0.5 || percentile > 10)
      Error("p?");

    selective = true;
    argi += 2;

  }


  if (!strcmp(argv[argi], "-depth")) {

    if (!argv[argi+1] || sscanf(argv[argi+1], "%d", &max_depth) != 1 || 
	max_depth < 0 || max_depth > 18)
      Error("max_depth?");

    argi += 2;

  }

  if (!strcmp(argv[argi], "-label")) {

    write_label = true;
    argi++;
  }


  if (argc > argi+1) goto error;

  file = argv[argi];
  if (!file) goto error;


  fp = fopen(file, "r");
  if (!fp) { fprintf(stderr, "(%s) ", file); Error("can't open sfk-file"); }

  printf("sel=%d (p=%.3f) max_depth=%d\n", selective, percentile, max_depth);

  InitZug(&zio, EVALFUNC, Check, HBITS, HMAXDEPTH, H0BITS, H0MAXDEPTH);

  strcpy(ParameterFile, PARFILE);
  strcpy(TableFile, TABFILE);

/*
  sprintf(outfile, "%s.val", file);
  fpout = fopen(outfile, "w");
  if (!fpout) Error("can't open sfk-file.val");
*/



/* evaluate positions */


  for (newnum=0; ; newnum++) {

    if (!fSfRead(fp, &sf, 1)) break;

/*
SfAus(&sf, 0, 0);
printf("%d\n", player);
*/


    // evaluate position

    zio.Sf         = sf;
    zio.Partei     = BLACK;
    zio.LetzterZug = ZUG_UNBEKANNT;
    zio.Modus      = MODUS_NORMAL;

    zio.Selective  = selective;
    zio.Percentile1 = percentile;
    zio.Percentile2 = percentile;

    zio.Quiescence = QUIESCENCE;

    zio.ZugVorgabe = false;


#if KILLER
  KillerAdjust(&zio.killer, &zio.Sf);
#endif

    ClearHashTab(&zio.hash);  ClearHashTab(&zio.hash0);
 
    { BRETT Brett;
      SfBrett(&sf, &Brett);
      printf("%6.3f ", WERT_TO_REAL(zio.BewMitte(&Brett, BLACK)));
    }

    for (d=1; d <= max_depth; d++) {

      zio.MaxTiefe = d; Zugermittlung(&zio);

      printf("%6.3f ", WERT_TO_REAL(zio.Wert)); fflush(stdout);

    }
    
    if (write_label) {

      if      (sf.Marke == MA_GEWONNEN) printf("+1");
      else if (sf.Marke == MA_VERLOREN) printf("-1");
      else if (sf.Marke == MA_REMIS)    printf(" 0");
      else if (sf.Marke >= MA_WKEIT && sf.Marke <= MA_WKEIT+100)
	printf("%.2f", (sf.Marke - MA_WKEIT)/100.0);
      else if (sf.Marke >= MA_DIFF && sf.Marke <= MA_DIFF+128)
	printf("%d", sf.Marke - MA_DIFF - 64);
      else printf("illegal label");
      
    }

    printf("\n");
  }

  fclose(fp);

/*
  printf("\n%d position(s) evaluated\n", newnum);
*/
  return 0;
}

