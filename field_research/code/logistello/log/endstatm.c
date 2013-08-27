// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/* determine node counts for endgame, 3.95 */

#include "main.h"
#include "sboard.h"
#include "crt.h"
#include "move.h"
#include "eval.h"
#include "killer.h"
#include "fpatt.h"



#define EVALFUNC	EvalA
#define PARFILE         "evala.par"
#define TABFILE         "oplay.tab"

#define PRE_DEPTH	12
#define NUMBER          100

int VERB = 1;



void _abort(void) { exit(1); }



void Check(COMZIO *pcio, ZUGIO *pzio, bool no_count) 
{
  Chk_Abort();
}


double stddev(double x, double xx, int n)
{
  double r = xx / n - (x * x) / (n * n);

  if (r < 0) return 0;

  return sqrt(r);
}


int main(int argc, char **argv)
{
  int   extranz=0, extrfalsch = 0;
  int   z, Tiefe, anz, i, j, k, r, r0, r1, begin, colour;
  uint4 Start, Ende, Num, TimeB=0, TimeS=0, 
        NodesB=0, NodesS=0, SameMove=0, SameValue=0;
  int   ValueB, ValueS, MoveB, MoveS;
  double NodesBB=0, NodesSS=0;
  ZUGIO zio;
  BRETT Brett;
  SPFELD tab, sf;
  int  w;
  FILE *fp;
  GAME game;


  if (argc != 2) {

    Error("call: oendstat sfk-file");
    exit(20);
  }

  InitZug(&zio, EVALFUNC, Check, HASHBITS);

  strcpy(ParameterFile, PARFILE);
  strcpy(TableFile,     TABFILE);


  fp = fopen(argv[1], "r");

  if (!fp) Error("file?");

  for (Num=0; Num < NUMBER; Num++) {

    if (fSfRead(fp, &sf, 1) != 1) break;

printf("%d:\n", Num+1);
SfAus(&sf, BLACK, 0);


    {
      zio.Sf = sf;

      SfBrett(&zio.Sf, &zio.Brett);

      zio.DiscNum0 = SfAnz(&zio.Sf);

/* pre-iterations */

      zio.Selective       = true;
      zio.Percentile      = 1.5;
      zio.Partei          = BLACK;
      zio.Modus           = MODUS_NORMAL;
      zio.PathLen         = 0;
      zio.PathValue       = 0;
      zio.fertig          = false;
      zio.cio.timeout     = false;

      zio.Wert     = 0;
      zio.BestZug  = ZUG_UNBEKANNT;
      zio.cio.BewAnz = 0;

      ClearHashTab(&zio.hash);

#if KILLER
  KillerAdjust(&zio.killer, &zio.Sf);
#endif

      Start = clock();

      for (i=1; i <= PRE_DEPTH; i++) {
        zio.MaxTiefe = i;
        Zugermittlung(&zio);
      }

      Ende  = clock();

printf("prenodes = %d pretime = %d\n", 
zio.cio.BewAnz, round((float)(Ende-Start)/CLOCKS_PER_SEC));
printf("prevalue = %d %% premove = ", 
round(EXPWERT(WERT_TO_REAL(zio.Wert))*100));
KoorAus(zio.BestZug);
printf("\n");

/* brute-force endgame */

      zio.Selective       = false;
      zio.Modus           = MODUS_GEWINN;
      Zugermittlung(&zio);

      Ende  = clock();

printf("bnodes = %d btime = %d\n", 
zio.cio.BewAnz, round((float)(Ende-Start)/CLOCKS_PER_SEC));
printf("bvalue = %d bmove = ", zio.Wert); KoorAus(zio.BestZug); printf("\n"); 



#if ITER


/* pre-iterations */

      zio.Selective       = true;
      zio.Percentile      = 1.5;
      zio.Partei          = BLACK;
      zio.Modus           = MODUS_NORMAL;
      zio.PathLen         = 0;
      zio.PathValue       = 0;
      zio.fertig          = false;
      zio.cio.timeout     = false;

      zio.Wert     = 0;
      zio.BestZug  = ZUG_UNBEKANNT;
      zio.cio.BewAnz = 0;

      ClearHashTab(&zio.hash);

#if KILLER
  KillerAdjust(&zio.killer, &zio.Sf);
#endif

      Start = clock();

      for (i=1; i <= PRE_DEPTH; i++) {
        zio.MaxTiefe = i;
        Zugermittlung(&zio);
      }

      Ende = clock();

printf("prenodes = %d pretime = %d\n", zio.cio.BewAnz, (Ende-Start)/CLOCKS_PER_SEC);


/* iter endgame */

      zio.Modus           = MODUS_ITER;
      zio.al = 0; zio.be = 1;
      Zugermittlung(&zio);

      Ende  = clock();

printf("inodes = %d itime = %d                                     \n", 
zio.cio.BewAnz, (Ende-Start)/CLOCKS_PER_SEC);
printf("ival = %d imove = ", zio.Wert); KoorAus(zio.BestZug); printf("\n");

#endif

    }
  }




  fclose(fp);

  return 0;
}
