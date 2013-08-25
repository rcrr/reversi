// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/* determine node counts 3.95 */

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

#define DEPTH		12
#define NUMBER          50

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

    Error("call: onodes sfk-file");
    exit(20);
  }

  InitZug(&zio, EVALFUNC, Check, HASHBITS);

  strcpy(ParameterFile, PARFILE);
  strcpy(TableFile,     TABFILE);


  fp = fopen(argv[1], "r");

  if (!fp) Error("file?");

  for (Num=0; Num < NUMBER; Num++) {

    if (fSfRead(fp, &sf, 1) != 1) break;


SfAus(&sf, BLACK, 0);


    {
      zio.Sf = sf;

      SfBrett(&zio.Sf, &zio.Brett);

      zio.DiscNum0 = SfAnz(&zio.Sf);

      zio.Selective       = false;
      zio.Quiescence      = true;
      zio.Partei          = BLACK;
      zio.MaxTiefe        = DEPTH;
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

      for (i=1; i <= DEPTH; i++) {
        zio.MaxTiefe = i;
        Zugermittlung(&zio);
      }

      Ende  = clock();

      ValueB = zio.Wert;
      MoveB  = zio.BestZug;
      TimeB += Ende-Start;
      NodesB += zio.cio.BewAnz;
      NodesBB += (double)zio.cio.BewAnz * (double)zio.cio.BewAnz;

/*
printf("hv=%2d%% ", round(EXPWERT(WERT_TO_REAL(value))*100.0)); fflush(stdout);
printf(" t=%.1f\n", ((REAL)(Ende-Start)) / CLOCKS_PER_SEC);
*/


      zio.Selective       = true;
      zio.Percentile      = 1.5;
      zio.Quiescence      = true;
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

      for (i=1; i <= DEPTH; i++) {
        zio.MaxTiefe = i;
        Zugermittlung(&zio);
      }

      Ende  = clock();

      ValueS = zio.Wert;
      MoveS  = zio.BestZug;
      TimeS += Ende-Start;
      NodesS += zio.cio.BewAnz;
      NodesSS += (double)zio.cio.BewAnz * (double)zio.cio.BewAnz;

      SameMove += MoveB == MoveS;
      SameValue += ValueB == ValueS;
    }

  printf("%3d: NodesB=%.1f (%.1f) TimeB=%.1f |  NodesS=%.1f (%.1f) TimeS=%.1f    SameMove=%d SameValue=%d\n", 
    Num+1, 
    (double)NodesB / (Num+1),
    stddev(NodesB, NodesBB, Num+1),
    (double)TimeB / (CLOCKS_PER_SEC * (Num+1)), 

    (double)NodesS / (Num+1), 
    stddev(NodesS, NodesSS, Num+1),
    (double)TimeS / (CLOCKS_PER_SEC * (Num+1)), 

    SameMove, SameValue);
  

  }




  fclose(fp);

  return 0;
}
