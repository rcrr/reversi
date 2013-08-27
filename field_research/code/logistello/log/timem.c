// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/* Zeit messen , 10.92 */

#ifndef PRECOMP
#include "main.h"
#endif


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
#include "hash.h"

int z1=0;


#if 1

#define DATEI "xxx.sfk"

#else

#define DATEI "osp/knapp.sfk"

#endif

#define DEV_DEPTH       1

#define TESTANZ		250	/*3000*/
#define SFMAX		20000

#ifdef AMIGA
#define TESTDATEI	"ram:"DATEI
#else
#define TESTDATEI	DATEI
/*#define TESTDATEI	"osfk/"DATEI*/
#endif

#define EVAL	        EvalA

#define SELECTIVE	true
#define PERCENTILE	1.5
#define QUIESCENCE	true



int VERB = 1;

int z0;
int xx=0;

SPFELD *SFelder=0;


void _abort(void) { exit(1); }



void Check(COMZIO *pcio, ZUGIO *pzio, bool no_count) {}

#define PRI printf("i=%d\n", i)


#define WERTEMAX	1000

int werte[WERTEMAX];

#if 0

#define ZUG_AUS_ANZ 20

void PathOut(SFPOS *p, int anz)
{ 
  int i;


  printf("(");

  FOR (i, min(anz, ZUG_AUS_ANZ)) { 
    KoorAus(p[i]);
    if (i < ZUG_AUS_ANZ-1) printf(" ");
  }

  for (i=anz; i < ZUG_AUS_ANZ; i++) {

    printf("  ");
    if (i < ZUG_AUS_ANZ-1) printf(" ");
  }

  printf(")");
}

#endif


int compint(const void *a, const void *b)
{
  return *((int*) b) - *((int*) a);
}
 

int zanz=0;
int a0;



int main(int argc, char **argv)
{
  int    z, Tiefe, anz=0, i, j, k, r, r0, r1, a, GesamtAnz, fanz=0, fueber=0, funter=0, sw=0, premove, wrong=0, s;
  clock_t Start, Ende;
  int    pre_depth;
  uint4  IterClock=0, EinzelnKB=0, EinzelnKW=0, IterKB=0, IterKW=0;
  ZUGIO  zio;
  BRETT  Brett;
  SFPOS  moves[65];
  int    total=0;
  ZEIT   ZeitVor, ZeitNach;

#if 1

/* determine best-move frequency */

#define MAX_D 11

  if (argc != 2) Error("otime osp-file\n");

  { FILE *fp;
    char s[400], t[400], *p;
    int bl, wh, BestMove, num=0, move_num, mn;
    int guess_best[MAX_D+1], guess_win[MAX_D+1], guess_diff[MAX_D+1];
    int max_num, vals[100], moves[64], values[64];
    SPFELD sf;

  strcpy(ParameterFile, "evala.par");
  strcpy(TableFile,     "oplay.tab");

  InitZug(&zio, EVAL, Check, HASHBITS, HASHMAXDEPTH);

  { BRETT Brett;
  SfGrund(&sf);
  SfBrett(&sf, &Brett);
  EVAL(&Brett, BLACK);
  }

  zio.cio.BewAnz = 0;

  FOR (i, MAX_D+1) guess_best[i] = guess_win[i] = guess_diff[i] = 0;

fp = fopen(argv[1], "r");
  
    Start = clock();

FOREVER { 

    if (fTabEin(fp, &sf)) break;

    if (sf.Marke == BLACK) printf("-> ##\n"); else printf("-> ()\n");

    SfAus(&sf, 0, 0);

    FOREVER {

      if (!(fgets(s, 300, fp))) Error("file?");

      if (s[0] == '[') break;

    }

    move_num = 0;

    p = s;

    FOREVER {

      moves[move_num] = (p[1]-'a') + 1 + 10 * (p[2] - '1' + 1);
      if (sscanf(p+4, "%d", &values[move_num]) != 1) Error("value?");

      move_num++;

      while (*p && *p != ' ') p++;

      p++;

      if (*p != '[') break;
    }

    max_num = -100; mn = 0;

    FOR (i, move_num) { 

      printf("["); KoorAus(moves[i]); printf(":%d] ", values[i]); 

      vals[moves[i]] = values[i];

      if (values[i] > max_num) { max_num = values[i]; mn = 1; }
      else if (values[i] >= max_num) mn++;
    }

    printf("\n\n best=%d (#=%d)\n", max_num, mn);


    SfBrett(&sf, &zio.Brett);

    zio.Sf         = sf;
    zio.MaxTiefe   = pre_depth;
    zio.Partei     = sf.Marke;
    zio.LetzterZug = ZUG_UNBEKANNT;
    zio.Modus      = MODUS_NORMAL;
    zio.Quiescence = false;
    zio.Selective  = false;
    zio.Percentile = 1.5;
    zio.PathLen	   = 0;
    zio.PathValue  = 0;
    zio.fertig     = false;
    zio.cio.timeout= false;

    zio.Wert	 = 0;
    zio.BestZug  = ZUG_UNBEKANNT;	

    KillerAdjust(&zio.killer, &sf);

    zio.ZugVorgabe = false;


    for (j=1; j <= MAX_D; j++) {

      zio.MaxTiefe = j;
      Zugermittlung(&zio);
      
      printf("[%d: ", j); KoorAus(zio.BestZug); printf("] "); fflush(stdout);


      if (vals[zio.BestZug] == max_num) guess_best[j]++;
      if (vals[zio.BestZug] >= 0)       guess_win[j] += 1 + (vals[zio.BestZug] > 0); 

      guess_diff[j] += max_num - vals[zio.BestZug];
    }

    num++;

    printf("\nnum=%d time=%.2f\n", num, (double)(clock()-Start)/(CLOCKS_PER_SEC*num));

    for (i=1; i <= MAX_D; i++) 

      printf("[%2d: best=%d(%.1f%%) win=%.1f(%.1f%%) diff=%d(%.2f)]\n", 
	     i, guess_best[i], (100.0*guess_best[i])/num, 
	     guess_win[i]/2.0, (50.0*guess_win[i])/num,
	     guess_diff[i], ((double)guess_diff[i])/num);

}

fclose(fp);
exit(0);
}}

#endif



#if 0

/* determine endgame values for all moves */

#define MAX_D 10

  if (argc != 2) Error("otime osp-file\n");

  { FILE *fp;
    char s[400], t[400];
    SFPOS moves[64];
    static int move_num, bl, wh, BestMove, num=0, guess[MAX_D+1];
    SPFELD sf, sf0;

  strcpy(ParameterFile, "evala.par");
  strcpy(TableFile,     "oplay.tab");

  InitZug(&zio, EVAL, Check, HASHBITS, HASHMAXDEPTH);

  zio.cio.BewAnz = 0;

fp = fopen(argv[1], "r");
  
FOREVER { 

    if (fTabEin(fp, &sf0)) break;

    if (sf0.Marke == BLACK) printf("\n-> ##\n"); else printf("\n-> ()\n");
    SfAus(&sf0, 0, 0);

    move_num = SfMoeglZuege(&sf0, sf0.Marke, moves);

    if (!move_num) continue;


    FOR (i, move_num) {

      sf = sf0;
      if (!SfSetzen(&sf, sf0.Marke, moves[i])) Error("move?");

    SfBrett(&sf, &zio.Brett);

    zio.Sf         = sf;
    zio.Partei     = GEGNER(sf.Marke);
    zio.LetzterZug = ZUG_UNBEKANNT;
    zio.Modus      = MODUS_NORMAL;
    zio.Quiescence = true;
    zio.Selective  = true;
    zio.Percentile = 1.5;
    zio.PathLen	   = 0;
    zio.PathValue  = 0;
    zio.fertig     = false;
    zio.cio.timeout= false;

    zio.Wert	 = 0;
    zio.BestZug  = ZUG_UNBEKANNT;	

    KillerAdjust(&zio.killer, &sf);

    zio.ZugVorgabe = false;

    for (j=1; j <= 8; j++) {

      zio.MaxTiefe = j;
      Zugermittlung(&zio);

    }

    zio.Sf         = sf;
    zio.Partei     = GEGNER(sf.Marke);
    zio.LetzterZug = ZUG_UNBEKANNT;
    zio.Modus      = MODUS_DIFFERENZ;
    zio.Quiescence = true;
    zio.Selective  = false;
    zio.Percentile = 1.5;
    zio.PathLen	   = 0;
    zio.PathValue  = 0;
    zio.fertig     = false;
    zio.cio.timeout= false;
    
    Zugermittlung(&zio);

    printf("["); KoorAus(moves[i]); printf(":%d] ", -zio.Wert); fflush(stdout);

    }
    
    printf("\n");
}

fclose(fp);
exit(0);
}}

#endif


#if 0


  if (argc != 3) Error("otime sfk-file depth\n");

  if (sscanf(argv[2], "%d", &pre_depth) != 1) Error("depth?");

  SFelder = (SPFELD*) malloc(SFMAX * sizeof(SPFELD));
  
  if (!SFelder) Error("Speicher?");

/*
strcpy(BewKDatei, "par1");
printf("par1\n");
*/
  GesamtAnz = SfRead(SFelder, SFMAX, argv[1]);

  if (!GesamtAnz) Error("file?");

#if 0

/* generate sfk-file with alternating label */

  { FILE *fp=fopen("yyy.sfk", "w");
    int v;

    if (!fp) Error("???");

    v = 1;

    FOR (i, GesamtAnz) {

      if (SFelder[i].Marke == MA_WKEIT + 50) continue;

      if ((SFelder[i].Marke-MA_WKEIT-50)/abs(SFelder[i].Marke-MA_WKEIT-50) != v) 
        continue;

      fSfWrite(fp, &SFelder[i], 1);

      v = -v;

    }
    fclose(fp);
    exit(0);
  }
#endif



  strcpy(ParameterFile, "evala.par");
  strcpy(TableFile,     "oplay.tab");

  InitZug(&zio, EVAL, Check, HASHBITS, HASHMAXDEPTH);

  zio.cio.BewAnz = 0;

#if 0

/* solve positions */

  { FILE *fp;

fp = fopen("aaa.sfk", "w");
  
FOR (i, GesamtAnz) 
{ int x;

    SfBrett(&SFelder[i], &zio.Brett);

    zio.MaxTiefe   = 5;
      zio.Partei     = BLACK;
      zio.LetzterZug = ZUG_UNBEKANNT;
      zio.SearchMode = SM_ENDGAME;
      zio.Quiescence = false;
      zio.Selective  = false;
  zio.PathLen		= 0;
  zio.PathValue	= 0;
  zio.fertig   	= false;
  zio.cio.timeout	= false;

  zio.Wert	 = 0;
  zio.BestZug  = ZUG_UNBEKANNT;		/* bisher kein Zug gefunden */

      KillerAdjust(&zio.killer, &zio.Sf);

  Start = clock();

      x = EndAlphaBeta1(&zio, BLACK, -1, 1, ZUG_UNBEKANNT);
 
  total += clock()-Start;


  if (x) x = x / abs(x);

  printf("%d", x); fflush(stdout);


  SFelder[i].Marke = MA_WKEIT + 50 + 49*x;

  fSfWrite(fp, &SFelder[i], 1);

}

fclose(fp);
exit(0);
}
#endif





#if 0

/* determine 1 ply evaluation deviations */

  { FILE *fp;
    int  delta_num[64], delta_sum[64], delta_min[64], delta_max[64],
         val, high_val, path[64], player;
    BRETT board;
    int best_move, path_len;

    FOR (i, 64) delta_num[i] = delta_sum[i] = 0;

FOR (i, GesamtAnz) 
{ int x;

#if 0
SfAus(&SFelder[i], 0, 0);
#endif

    zio.Sf         = SFelder[i];
    zio.MaxTiefe   = pre_depth;
    zio.Partei     = BLACK;
    zio.LetzterZug = ZUG_UNBEKANNT;
    zio.Modus      = MODUS_NORMAL;
    zio.Quiescence = true;
    zio.Selective  = true;
    zio.Percentile = 1.5;
    zio.PathLen	   = 0;
    zio.PathValue  = 0;
    zio.fertig     = false;
    zio.cio.timeout= false;

    zio.Wert	 = 0;
    zio.BestZug  = ZUG_UNBEKANNT;	

    KillerAdjust(&zio.killer, &SFelder[i]);

    Start = clock();

    zio.ZugVorgabe = false;

    for (j=1; j <= pre_depth; j++) {

      zio.MaxTiefe = j;
      Zugermittlung(&zio);

    }

    total += clock()-Start;

    printf("len=%d: ", zio.PathLen);

    FOR (j, zio.PathLen) KoorAus(zio.Path[j]);
    printf("\n");

    if (zio.PathLen >= pre_depth) {
      
      FOR (j, zio.PathLen) path[j] = zio.Path[j];
      path_len = zio.PathLen;

      player = BLACK;

      ClearHashTab(&zio.hash);

      FOR (j, pre_depth) {

        zio.MaxTiefe = min(DEV_DEPTH, pre_depth-j); 

#if 0
printf("d0=%d %d\n", zio.MaxTiefe, pre_depth);
#endif
        
        zio.Partei = player;
        zio.ZugVorgabe = false;
        Zugermittlung(&zio);
        best_move = zio.BestZug;
        high_val = zio.Wert;

        zio.ZugVorgabe = true;
        zio.VorgabeAnz = 1;
        zio.Zuege[0]   = path[j];
        Zugermittlung(&zio);
        
        val = zio.Wert;

if (val > high_val) {

#if 1
SfAus(&zio.Sf, 0, 0); printf(" %d\n", player);
KoorAus(best_move); KoorAus(path[j]);
printf(">>> %d %d => %d\n", high_val, val, high_val - val);

#endif



}


#if 0
printf(">>> %d %d => %d\n", high_val, val, high_val - val);
#endif


        delta_sum[j] += high_val - val;
        delta_num[j]++;

        if (!SfSetzen(&zio.Sf, player, path[j])) Error("move?");
       
        player = GEGNER(player);
      }
    }

    if ((i & 31) == 0) {

      FOR (j, pre_depth) 
        printf("%d: %d %d %f\n", j, delta_num[j], delta_sum[j], 
               (double)delta_sum[j]/(delta_num[j]+0.0001));
      printf("\n");

    }

  }
}

exit(0);
}
#endif


#endif











#if 0

if (0)
  { SPFELD sf;
    BRETT  board;
 
    SfGrund(&sf);
    SfBrett(&sf, &board);
    a0 = EVAL(&board, BLACK);
  }

  Start = clock();


z0=z1=0;
  
  s = 0;

  k = 0;

  FOR (i, TESTANZ) {

    int v, res;


    while (k < GesamtAnz && SFelder[k].Marke == MA_WKEIT+50) k++;

    if (k >= GesamtAnz) break;


    SfBrett(&SFelder[i], &Brett);
   
    v = EvalA(&Brett, BLACK);

    if (v > 0) v = +1;
    if (v < 0) v = -1;

    if (SFelder[i].Marke > MA_WKEIT+50) res = 1; else res = -1;

    s += abs(v-res);

  }


printf("%d boards, s=%.1f\n", i, 100.0*s/2.0/i);

  printf("\n%.2f sek total, num=%d\n", (float)(total)/CLOCKS_PER_SEC, anz);


  return 0;
}



#if 0

    if (i >= 0 && SfAnz(&SFelder[i]) <= 64 && 
        SfMoeglZuege(&SFelder[i], BLACK, moves) >= 2)  {


      SfBrett (&SFelder[i], &Brett);
      SfBrett (&SFelder[i], &zio.Brett);

anz++;

#if 0
      ClearHashTab(&zio.hash);
#endif

      zio.Selective  = SELECTIVE;
      zio.Percentile = PERCENTILE;
      zio.Quiescence = QUIESCENCE;

      zio.Sf	   = SFelder[i];
      zio.BewMitte = zio.cio.mBewMitte;

      zio.Modus    = MODUS_GEWINN;


#if 0

      for (j=1; j <= pre_depth; j++)  {

        zio.MaxTiefe	 = j;
        zio.Partei	 = BLACK;
        zio.LetzterZug   = ZUG_UNBEKANNT;

        Zugermittlung(&zio);        

/*
        PathOut(&zio.Path, zio.PathLen);
*/

      }

#endif

#if 0
{ int x;

      zio.MaxTiefe   = 5;
      zio.Partei     = BLACK;
      zio.LetzterZug = ZUG_UNBEKANNT;
      zio.SearchMode = SM_ENDGAME;
      zio.Quiescence = false;
      zio.Selective  = false;
  zio.PathLen		= 0;
  zio.PathValue	= 0;
  zio.fertig   	= false;
  zio.cio.timeout	= false;

  zio.Wert	 = 0;
  zio.BestZug  = ZUG_UNBEKANNT;		/* bisher kein Zug gefunden */

      KillerAdjust(&zio.killer, &zio.Sf);

  Start = clock();

      x = EndAlphaBeta1(&zio, BLACK, -1, 1, ZUG_UNBEKANNT);
 
  total += clock()-Start;


  if (x) x = x / abs(x);

  printf("%d", x); fflush(stdout);

#endif

}


{ uint1 board[91];
  int x, y;

  memset(board, 3, 91);

  for (x=1; x <= 8; x++)
   
    for (y=1; y <= 8; y++)

      board[x+y*9] = SFelder[i].p[x + y*10] + 1;

  Start = clock();

  PrepareToSolve(board);
  x = EndSolve(board, -1, 1, BLACK + 1, 64-SfAnz(&SFelder[i]), 
           SfAnzBLACK(&SFelder[i]) - SfAnzWHITE(&SFelder[i]), 0);

  total += clock()-Start;

  if (x) x = x / abs(x);

  printf("%d", x); fflush(stdout);
}

#endif



#endif
