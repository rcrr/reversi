// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/* evaluate positions given in alt-file / 4.94 */

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
#include "speed.h"

#define OUT		false
#define VERBOSE         0

#define EVALFUNC	EvalL
#define PARFILE         "evala.par"
#define TABFILE         "oplay.tab"
#define PCSTATFILE      "pcstatl"


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

static bool  selective   = true;
static bool  time_limit  = false;
static float percentile1 = 1.0;
static float percentile2 = 1.5;
static bool  quiescence  = true;

#define WIN_LIMIT       58   // percent (only for statistics)

#define BAD_BREAK        1   // break early if best alternative is bad 
#define BAD_PERC        32   // ~ -4 discs

#define EXT_NUM         36    // >= EXT_NUM discs  => extend time by
#define EXT_FAC         (1.5) // EXT_FAC

static int incr[33] = {     /* search depth increment dependent on discnumber */

  0,0,

#if 0

/*  4 */ 2,2,2,2,
/* 12 */ 2,2,2,2,
/* 20 */ 2,0,0,0,
/* 28 */ 0,0,0,2,
/* 36 */ 2,2,2,2,
/* 44 */ 2,2,2,2,
/* 52 */ 2,2,2,2,
/* 60 */ 2,2,2

#else

/*  4 */ 0,0,0,0,
/* 12 */ 0,0,0,0,
/* 20 */ 0,0,0,0,
/* 28 */ 0,0,0,0,
/* 36 */ 0,0,0,0,
/* 44 */ 0,0,0,0,
/* 52 */ 0,0,0,0,
/* 60 */ 0,0,0,


#endif

};



static ZUGIO zio;


void _abort(void) { exit(1); }

void Check(COMZIO *, ZUGIO *, bool, bool) {}	/* Signal-Handler */




int saved=0, gamenum=0;


/* evaluate vertices recursively */


void EvalPos(
  LIBRARY  *pT, 
  POSINFO  *pPI,
  NewGame  &ngame,
  SPFELD   *psf,
  int      player,
  int      depth_time,
  int      *pbestmove,
  int      *pvalue,
  bool     time_limit
)
{
  int    movenum=0, mn, player1, i, j, d;
  SPFELD sf;
  MOVEDATA MoveDat[65];
  SFPOS  moves[65];


  if (pT) {


/* collect made moves in MoveDat */
  
    movenum = 0;

    sf = *psf;

    if (pPI->MoveIndex < 0) {			/* branch */

      if (!pPI->pNode->SonNum) Error("no son?");

      player1 = 0;

      FOR (i, pPI->pNode->SonNum) {

        MoveDat[movenum++].Move = pPI->pNode->Sons[i].Moves[0].get_move();
        player1 = pPI->pNode->Sons[i].Moves[0].get_player();
      }


    } else {					/* path */

      MoveDat[movenum++].Move = 
	pPI->pNode->Sons[pPI->MoveIndex].Moves[pPI->PathIndex].get_move();

      player1 =
        pPI->pNode->Sons[pPI->MoveIndex].Moves[pPI->PathIndex].get_player();
   
    }


#if OUT
    { 
      printf("\nPosition:\n");
      SfAus(psf, 0, 0);
      player1 = PlayGame(pGame->MoveNum, pGame, &sf);
      printf("%d\n", player1);

      printf("lib moves: [ ");
      FOR (i, movenum) { KoorAus(MoveDat[i].Move); printf(" "); }
      printf("]\n");


      if (!SfGleich(&sf, psf)) {
	fWriteGame(stdout, pGame);
	SfAus(&sf, 0, 0);
	SfAus(psf, 0, 0);
	Error("boards !=");
      }
    }
#endif


    if (player != player1) Error("different players");

  }


  if (movenum < (mn=SfMoeglZuegeE(psf, player, moves))) {


    // not all moves seen


    zio.VorgabeAnz = 0;

    FOR (i, mn) {

      FOR (j, movenum) if (moves[i] == MoveDat[j].Move) break;

      if (j >= movenum) zio.Zuege[zio.VorgabeAnz++] = moves[i];

    }


    if (zio.VorgabeAnz != mn - movenum) { 

      ngame.f_write(stdout);

      FOR (i, mn) { KoorAus(moves[i]); printf(" "); }
      printf("\n");

      FOR (i, movenum) { KoorAus(MoveDat[i].Move); printf(" "); }
      printf("\n");

      Error("wrong move number");
    }

    cout << SfAnz(psf) << " discs" << endl;

    cout << "moves seen:      " << flush;
    FOR (i, movenum) { KoorAus(MoveDat[i].Move); printf(" "); }
    printf("\n");
    
    cout << "moves to search: " << flush;
    FOR (i, zio.VorgabeAnz) { KoorAus(zio.Zuege[i]); printf(" "); }
    printf("\n");

    zio.Sf          = *psf;
    zio.Partei      = player;
    zio.LetzterZug  = ZUG_UNBEKANNT;
    zio.Modus       = MODUS_NORMAL;

    zio.Selective   = selective;
    zio.Percentile1 = percentile1;
    zio.Percentile2 = percentile2;
    zio.Quiescence  = quiescence;

    zio.ZugVorgabe = true;


#if KILLER
    KillerAdjust(&zio.killer, &zio.Sf);
#endif

    zio.hash.clear(); zio.hash0.clear();


    if (time_limit) {
      
      // time-limit

      ZEIT z0;

      int values[64];
      int best_moves[64];
      bool extended = false;
      int seconds = depth_time;
      int disc_num = SfAnz(&zio.Sf);

      if (disc_num >= EXT_NUM) seconds = my_round(seconds*EXT_FAC);

      printf("time=%d sec\n", seconds);

      for (d=1;; d++) {

	zio.MaxTiefe = d; Zugermittlung(&zio);
	
	ZEIT z1;
	CPUZeit(&z1);

	if (d <= 1) CPUZeit(&z0); // omit init. phase
	
	float used = ZeitDiff(&z1, &z0);

	printf("%2d ", d);
	printf("%5.1f ", used); KoorAus(zio.BestZug); 
	printf(" %+7.3f %d%%\n", 
	       WERT_TO_REAL(zio.Wert),
	       int(rint(EXPWERT(zio.Wert/WERTFAKTOR)*100)));
	
	values[d]     = zio.Wert;
	best_moves[d] = zio.BestZug;
 
	if (extended) break;  // only one extension
	
	if (d < 2) continue;

	if (disc_num+d >= 63) break;  // >= 64: MPC doesn't return ...

	if (used >= 0.66 * seconds) {

	  if ((values[d] > 0) ^ (values[d-1] > 0)) { 
	    cout << " different sign -> extend" << endl;
	    extended = true; continue; 
	  }
	  
	  if (best_moves[d] != best_moves[d-1]) { 
	    cout << " different move -> extend " << endl;
	    extended = true; continue; 
	  }
	  
	  break;
	}

#if BAD_BREAK

	if (used >= 0.33 * seconds) {

	  if (int(rint(EXPWERT(values[d]/WERTFAKTOR)*100))   <= BAD_PERC &&
	      int(rint(EXPWERT(values[d-1]/WERTFAKTOR)*100)) <= BAD_PERC) {

	    cout << " too bad -> break" << endl;
	    break;
	  }
#endif
	
	}
      }

      int value = (values[d] + values[d-1])/2;
      *pvalue = value;
      *pbestmove = best_moves[d];

      cout << "result: " << flush;
      KoorAus(*pbestmove); fflush(stdout);
      cout << " " << WERT_TO_REAL(*pvalue) << endl;

    } else {

      // depth-limit

      for (d=1; d <= depth_time; d++) {

	printf("%2d ", d); fflush(stdout);

	zio.MaxTiefe = d; Zugermittlung(&zio);

	if (d == depth_time-1) *pvalue = zio.Wert;

	/*KoorAus(zio.BestZug); printf("%d: %d\n", d, zio.Wert);*/

      }

      if ((*pvalue < 0) ^ (zio.Wert < 0)) { 

	// one more iteration, if sign of values differ */ 
	
	printf("%2d ", d); fflush(stdout);
	
	*pvalue = zio.Wert;
	
	zio.MaxTiefe = depth_time+1; Zugermittlung(&zio);
	
	printf("+++ ");
      }
      
      
      *pbestmove = zio.BestZug;
      printf(": %d %d -> ", *pvalue, zio.Wert);
      *pvalue = (*pvalue + zio.Wert)/2;
      printf("%d\n", *pvalue);

    }

  } else { *pbestmove = ZUG_UNBEKANNT; *pvalue = 0; }

}

 
// perfect = true  => save results (or approximations)

static void EvaluateBoard(
  SPFELD &sf, int depth, int perfect_disc_num, int &best_move, int &value, int &value0
) 
{
  static int won=0, drawn=0, lost=0, num=0;

  zio.VorgabeAnz = 0;
  zio.Sf         = sf;
  zio.Partei     = BLACK;
  zio.LetzterZug = ZUG_UNBEKANNT;
  
  zio.Modus      = MODUS_NORMAL;

  zio.Selective   = selective;
  zio.Percentile1 = percentile1;
  zio.Percentile2 = percentile2;
  zio.Quiescence  = quiescence;
  
  zio.ZugVorgabe = false;

#if KILLER
  KillerAdjust(&zio.killer, &zio.Sf);
#endif

  BRETT bo;
  SfBrett(&sf, &bo);
  value0 = my_round(WERT_TO_REAL(EVALFUNC(&bo, BLACK)) * DISCFAKTOR);

  if (SfAnz(&sf) >= perfect_disc_num) {

    // move ordering

    for (int d=1; d <= min(64-perfect_disc_num-3,6); d++) {
      zio.MaxTiefe = d; 
      Zugermittlung(&zio);
    }

    zio.Selective = false;

    zio.Modus = MODUS_DIFFERENZ;

    Zugermittlung(&zio);

    if (!ZUG(zio.BestZug)) { 

      zio.BestZug = ZUG_PASSEN;

      SfAus(&sf, 0, 0);
      KoorAus(zio.BestZug); printf("\n");
      
      //      Error("invalid move");

    }

    best_move = zio.BestZug;
    value = zio.Wert;

    // printf("%d v=%d move=(%d)\n", SfAnz(&sf), zio.Wert, zio.BestZug); 

#if OUT    
    printf("val=%d val0=%d\n", value, value0); fflush(stdout);
#endif
    
    if (zio.Wert <  0) lost++;
    if (zio.Wert >  0) won++;
    if (zio.Wert == 0) drawn++;
    num++;

    if ((num % 10) == 0) printf("n=%d w=%d d=%d l=%d\n", num, won, drawn, lost);


  } else {

    int d, val0=0;

    for (d=1; d <= depth; d++) {
      zio.MaxTiefe = d; 
      Zugermittlung(&zio);
      if (d == depth-1) val0 = zio.Wert;
    }

    if (!ZUG(zio.BestZug)) { 

      zio.BestZug = ZUG_PASSEN;

      SfAus(&sf, 0, 0);
      KoorAus(zio.BestZug); printf("\n");
      
      //      Error("invalid move");

    }

    best_move = zio.BestZug;

    // average value returned by d,d-1 search

    value = my_round(WERT_TO_REAL(0.5*(zio.Wert+val0)) * DISCFAKTOR);

#if OUT    
    printf("val0=%d val1=%d -> val=%d val0=%d\n", val0, zio.Wert, value, value0);
    fflush(stdout);
#endif
    
    if (value >  64) value =  64;
    if (value < -64) value = -64;

    if      (value < 0) lost++;
    else if (value > 0) won++;
    else                drawn++;

    num++;

    if ((num % 10) == 0) printf("n=%d w=%d d=%d l=%d\n", num, won, drawn, lost);

/*KoorAus(zio.BestZug); printf("%d: %d\n", d, zio.Wert);*/

  }
}



int main(int argc, char **argv)
{
  char     outfile[300];
  int      argi, depth_time, newnum;
  LIBRARY  *pT=NULL;
  NewGame  ngame;
  FILE     *fp;
  SPFELD   sf;
  char     *file;
  POSINFO  PI;
  FILE     *fpout;
  bool     frand=false;

  if (argc < 3) {

error: 

    Error(" call: oeval searchdepth/time [-brute] [-time] [-small] [-o old-lib] new-pos [-rand]\n\n\
      searchdepth/time: tree search look ahead or search time in seconds\n\
      -brute:         brute force search\n\
      -time:          timelimit instead of depth\n\
      -small:         use small hashtables -> faster\n\
      old-lib:        old library (oko-file)\n\
      new-pos:        new positions (oko-file) to evaluate\n\
      -rand:          choose length of prefix randomly\n\
                      (generate probcut examples)\n\
      -sfk  searchdepth wld-disc-num sfk-file: evaluate positions -> *.val (incl.moves!)\n\n\
      -sfk2 searchdepth perfect-disc-num sfk-file: evaluate positions, store pairs if some condition is met -> *.val\n\
      -sfktac searchdepth perfect-disc-num sfk-file: detect boards with bad evaluation -> *.tac/*.qui\n\n
 save evaluated positions in new-pos.eval");


  }



  argi = 1;


  if (!strcmp(argv[argi], "-sfk")) {

    Error("-sfk not implemented");

#if 0

    int depth;

    if (argc != 5) goto error;

    if (sscanf(argv[++argi], "%d", &depth) != 1 || depth < 1 || depth > 20) goto error;
    if (sscanf(argv[++argi], "%d", &wld_disc_num) != 1 || wld_disc_num < 40 || wld_disc_num > 63) goto error;
    
    file = argv[++argi];
    
    FILE *fpin = fopen(file, "r");
    if (!fpin) Error("can't open sfk-file");

    char outfile[1000];

    sprintf(outfile, "%s.val", file);

    FILE *fpout = fopen(outfile, "w");
    if (!fpout) Error("can't open output-file");

    InitZug(&zio, EVALFUNC, Check, HBITS, HMAXDEPTH, H0BITS, H0MAXDEPTH);

    strcpy(ParameterFile, PARFILE);
    strcpy(TableFile, TABFILE);
    strcpy(PCStatFile, PCSTATFILE);

    SPFELD sf;

    FOREVER {

      if (!fSfRead(fpin, &sf, 1)) break;
      EvaluateBoard(sf, depth, wld_disc_num, false);
      if (fSfWrite(fpout, &sf, 1)) Error("write error");
    }  

    fclose(fpin);
    fclose(fpout);

#endif

    return 0;
  }

  if (!strcmp(argv[argi], "-sfk2")) {

    int depth;
    int perfect_disc_num;

    if (argc != 5) goto error;

    if (sscanf(argv[++argi], "%d", &depth) != 1 || depth < 1 || depth > 20) goto error;
    if (sscanf(argv[++argi], "%d", &perfect_disc_num) != 1 || 
	perfect_disc_num < 44 || perfect_disc_num > 63) goto error;
    
    file = argv[++argi];
    
    FILE *fpin = fopen(file, "r");
    if (!fpin) Error("can't open sfk-file");

    char outfile[1000];
    sprintf(outfile, "%s.val", file);

    FILE *fpout = fopen(outfile, "w");
    if (!fpout) Error("can't open output-file");

    InitZug(&zio, EVALFUNC, Check, HBITS, HMAXDEPTH, H0BITS, H0MAXDEPTH);

    ParameterFile = PARFILE;
    TableFile = TABFILE;
    PCStatFile = PCSTATFILE;

    SPFELD sf;

    FOREVER {
      
      int best_move, value0, value;

      if (!fSfRead(fpin, &sf, 1)) break;
      EvaluateBoard(sf, depth, perfect_disc_num, best_move, value, value0);

      // different sign, or close and bad value0
      // store all ...
      
      if (1 || value * value0 < 0 || 
	  (value * value0 == 0 && value+value0 != 0) ||
	  (abs(value0-value) >= 1 && (1 || abs(value0) <= 4 || abs(value) <= 4))) {

	//	printf("!!!!\n");

	sf.Marke = MA_DIFF + 64 + value;
	if (fSfWrite(fpout, &sf, 1)) Error("write error");
	
	if (best_move != ZUG_PASSEN) {
	  if (!SfSetzen(&sf, BLACK, best_move)) Error("illegal best move!");
	}
	
	SfInvert(&sf);
	sf.Marke = MA_DIFF + 64 - value;
	if (fSfWrite(fpout, &sf, 1)) Error("write error");
	fflush(fpout);
      }
    }  

    fclose(fpin);
    fclose(fpout);

    return 0;
  }

  if (!strcmp(argv[argi], "-sfktac")) {

    int depth;
    int perfect_disc_num;

    if (argc != 5) goto error;

    if (sscanf(argv[++argi], "%d", &depth) != 1 || depth < 1 || depth > 20) goto error;
    if (sscanf(argv[++argi], "%d", &perfect_disc_num) != 1 || 
	perfect_disc_num < 30 || perfect_disc_num > 63) goto error;
    
    file = argv[++argi];
    
    FILE *fpin = fopen(file, "r");
    if (!fpin) Error("can't open sfk-file");

    char outfile[1000];

    sprintf(outfile, "%s.tac", file);

    FILE *fptac = fopen(outfile, "w");
    if (!fptac) Error("can't open output-file");

    sprintf(outfile, "%s.qui", file);
    FILE *fpqui = fopen(outfile, "w");
    if (!fpqui) Error("can't open output-file");

    InitZug(&zio, EVALFUNC, Check, HBITS, HMAXDEPTH, H0BITS, H0MAXDEPTH);

    ParameterFile = PARFILE;
    TableFile = TABFILE;
    PCStatFile = PCSTATFILE;

    SPFELD sf;

    FOREVER {
      
      int best_move, value0, value;

      if (!fSfRead(fpin, &sf, 1)) break;

      if (SfAnz(&sf) < perfect_disc_num) {

	EvaluateBoard(sf, depth, perfect_disc_num, best_move, value, value0);

	// value0 = sf.Marke - MA_DIFF - 64;

	// printf("label=%d\n", value0);

	if (abs(value - value0) >= 5) {
	  /*
            value * value0 < 0 || 
	    (value * value0 == 0 && value+value0 != 0) ||
	    (abs(value0-value) >= 1 && (abs(value0) <= 4 || abs(value) <= 4))) {
	  */
	  printf("!!!!\n");

	  // sf.Marke = MA_DIFF + 64 + value;
	  if (fSfWrite(fptac, &sf, 1)) Error("write error");
	  fflush(fptac);

	} else {

	  if (fSfWrite(fpqui, &sf, 1)) Error("write error");
	  fflush(fpqui);
	}
      }
    }  

    fclose(fpin);
    fclose(fptac);
    fclose(fpqui);

    return 0;
  }


  if (sscanf(argv[argi], "%d", &depth_time) != 1 || depth_time < 1) goto error;
  
  argi++;

  if (!strcmp(argv[argi], "-brute")) {
    selective = false;
    argi++;
  }

  if (!strcmp(argv[argi], "-time")) {
    time_limit = true;
    argi++;
  }

  int hashbits0, hashmaxdepth0, hashbits, hashmaxdepth;

  if (argv[argi] && !strcmp(argv[argi], "-small")) {

    argi++;

    hashbits0     = 14;
    hashmaxdepth0 =  6;
    hashbits      = 16;
    hashmaxdepth  = 64;

  } else {

    hashbits0     = 18;
    hashmaxdepth0 =  9;
    hashbits      = 20;
    hashmaxdepth  = 64;
  }


  if (argv[argi] && !strcmp(argv[argi], "-o")) {

    argi++;
    file = argv[argi++];

    if (!file) goto error;

    fp = fopen(file, "r");
    if (!fp) Error("can't open old-lib");

    /* read old gametree */

    pT = NewLibrary();

    FOREVER {

      if (!ngame.f_read_packed(fp)) break;

      if (!AppendGameToLibrary(pT, ngame)) { printf("."); fflush(stdout); }

    }

    fclose(fp);

    printf("\nOld library: %d game(s)\n", pT->GameNum);

  }

  InitZug(&zio, EVALFUNC, Check, hashbits, hashmaxdepth, hashbits0, hashmaxdepth0);

  ParameterFile = PARFILE;
  TableFile = TABFILE;
  PCStatFile = PCSTATFILE;


  file = argv[argi++];
  if (!file) goto error;

  fp = fopen(file, "r");
  if (!fp) { fprintf(stderr, "(%s) ", file); Error("can't open new-pos"); }


  sprintf(outfile, "%s.eval", file);
  fpout = fopen(outfile, "w");
  if (!fpout) Error("can't open new-pos.eval");


  if (argv[argi]) 
    if (!strcmp(argv[argi], "-rand")) frand = true;
    else goto error;


  // evaluate new positions


  printf("[ selective=%d, percentile1=%.2f, percentile2=%.2f, quiescence=%d]\n",
	 selective, percentile1, percentile2, quiescence);

  random(); random();

  for (newnum=0; ; newnum++) {

    int value, player, bestmove;


    if (!ngame.f_read_packed(fp)) break;

    if (frand) {

      if (random() % 1000 > 16) continue;
      ngame.set_move_num((random() % (min(41, ngame.get_move_num()))) + 1);
      printf("%2d: ", ngame.get_move_num()); ngame.f_write(stdout);
      player = ngame.play(ngame.get_move_num(), sf);
      if (!player) continue;

    }
      
    player = ngame.play(ngame.get_move_num(), sf);

    if (!player) {
      printf("game finished\n");
      continue;
    }
 
    if (pT && !SearchPosition(pT, ngame, &PI)) {

      ngame.f_write(stdout);

      fprintf(stderr, "+++ path not found\n");
      continue;
    }

    cout << endl;

#if VERBOSE
    if (player == BLACK) cout << "-> ##" << endl; else cout << "-> ()" << endl;
    SfAus(&sf, 0, 0);
#endif

    if (time_limit) {
      EvalPos(pT, &PI, ngame, &sf, player, depth_time,
	      &bestmove, &value, time_limit);
    } else {
      EvalPos(pT, &PI, ngame, &sf, player, depth_time+incr[SfAnz(&sf)/2],
	      &bestmove, &value, time_limit);
    }

    if (ZUG(bestmove)) {

      if (player == WHITE) value = -value;

      ngame.set_value(my_round(100*EXPWERT(WERT_TO_REAL(value)))-50);

      KoorAus(bestmove);

      if (time_limit) {
	printf(" v=%d (time=%d)\n", ngame.get_value(), depth_time);	
      } else {
	printf(" v=%d (depth=%d)\n", ngame.get_value(), depth_time+incr[SfAnz(&sf)/2]);
      }

      ngame.get_pm(ngame.get_move_num()).set(player, bestmove);
      ngame.set_move_num(ngame.get_move_num() + 1); 

      ngame.f_write_packed(fpout); fflush(fpout);

    } else {

#if VERBOSE
      printf("no further moves\n");
#endif

    }

  }

  fclose(fp); fclose(fpout);

  printf("\n%d position(s) evaluated\n", newnum);

  return 0;
}
