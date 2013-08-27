// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/* Computerzug, 1.8.91, 7.2.92, 93, 94 */

#include "main.h"
#include "pmove.h"
#include "move.h"
#include "eval.h"
#include "playm.h"
#include "killer.h"
#include "sboard.h"
#include "hash.h"
#include "newend.h"

#include "int.h"
#include "crtg.h"
#include "goodies.h"

#define SEL_WIDE_WINDOW 0  // 1 2x slower

#define EXACT_TIME      200000 // (was 20000) >= -> perform exact endgame search

#define NIV_INCR        (0.20)
#define NIV_STEPS       12
#define ENDEXTR		(1.5)	/* endgame extrapolation factor   */
#define MIDEXTR		(1.5)	/* (1.5) not all moves have to be seen */

#define AT_LEAST	0.63	/* (0.63) time that will be used */

/* bf search for a win when p>=WIN_P */

#define WIN_P           (1.01)  /* val >= : search for a win */

#define SCHRITT		2	/* window width in perfect search */

#define MAXTIEFE0	1
#define MAXTIEFE_STEP   1       // (1)
#define MIN_EXTR_TIEFE	4

#define MIN_TIEFE	6       // midgame-search at least with this depth

#define WLD_DIFF_BRUTE  10      // !selective
#define WLD_DIFF_SEL    2       // was 2 *7!!! then 5, 4, 3, 2

#define DIFF_GOOD0	4   
#define DEPTH0     	1

#define DIFF_GOOD1      8
#define DEPTH1	        2

#define DIFF_GOOD2	12
#define DEPTH2   	3

#define DIFF_GOOD3      16
#define DEPTH3   	4


#define MINDEST_ZEIT	0.1


#define ZEIT_ANTEIL 	0.7 /* Zeitanteil, nach dem offensichtlicher Zug */
			    /* geprüft wird				 */

#define KEINE_ZEIT	60.0	/* darüber wird ZEITFAKTOR benutzt	*/
#define ZEITFAKTOR	1.33	/* gibt max. Zeitüberschreitung an	*/

#define ZUG_AUS_ANZ	3	/* Züge in Variantenausgabe		*/


#define ENDFAC		0	/* time for midgame pre-iterations	*/
/* does not work for forced exact search ... */



/* hand tailored time table */

// max. at 36

static float Zeiten[31] = {
/* 4 */    50.0, 50.0, 50.0, 80.0, 100.0, 100.0, 
/* 16 */   130.0, 
/* 18 */   140.0, 
/* 20 */   200.0, 
/* 22 */   200.0,
/* 24 */   200.0, 
/* 26 */   200.0, 
/* 28 */   200.0, 
/* 30 */   200.0, 
/* 32 */   200.0,
/* 34 */   210.0, 
/* 36 */   240.0,
/* 38 */   240.0,
/* 40 */   210.0,      
/* 42 */   100.0,
/* 44 */   40, 20.0, 10.0, 5.0, 3.0, 2.0, 1.0, 1.0, 1.0, 1.0,
/* 64 */   1.0
};

#if 0
// previous
// max. at 38

static float Zeiten[31] = {
/* 4 */    50.0, 50.0, 50.0, 60.0, 70.0, 80.0, 
/* 16 */   120.0, 
/* 18 */   130.0, 
/* 20 */   150.0, 
/* 22 */   150.0,
/* 24 */   150.0, 
/* 26 */   150.0, 
/* 28 */   150.0, 
/* 30 */   150.0, 
/* 32 */   160.0,
/* 34 */   170.0, 
/* 36 */   230.0,
/* 38 */   230.0,
/* 40 */   180.0,      
/* 42 */   180.0,
/* 44 */    80.0, 30.0, 10.0, 5.0, 3.0, 2.0, 1.0, 1.0, 1.0, 1.0,
/* 64 */   1.0
};

// was /* 44 */   130.0, 90.0, 30.0, 20.0, 3.0, 2.0, 1.0, 1.0, 1.0, 1.0,

#endif


/* normierte Zeitanteile zum Ende hin */

static REAL Zeitanteil[30];	

 
void InitCompzug(void)
{
  float summe = 0.0;
  int   i;
  bool  schonmal = false;


  if (schonmal) Error("InitCompzug 2mal aufgerufen");

  FOR (i, 30) summe += Zeiten[i];


  FOR (i, 30) { 
    Zeitanteil[i] = Zeiten[i] / summe;
    summe -= Zeiten[i];
  }


#if 0

  float total = 1800.0, extr_total = total;

  FOR (i, 30) {

    cout << (i+2)*2 << " "
	 << Zeitanteil[i] << " "
         << Zeitanteil[i] * total << " " 
         << Zeitanteil[i] * extr_total * 1.33 << endl;

    total -= Zeitanteil[i] * total;
    extr_total -= Zeitanteil[i] * extr_total * 1.33;

  }

#endif


  schonmal = true;
}




/* Spielwert ausgeben */

void WertAus(WERT w, ValType type, int al, int be)
{
  REAL prob, rw;
  

  if (type == ENDGAME) {

    if      (w >= be) printf(" (   >= %+-3d  )", w);
    else if (w <= al) printf(" (   <= %+-3d  )", w);
    else	      printf(" (   == %+-3d  )", w);

  } else {

    if      (w >  WERTGEWINN) printf(" (   >= %+-3d  )", w - WERTGEWINN);
    else if (w < -WERTGEWINN) printf(" (   <= %+-3d  )", w + WERTGEWINN);  
    else {

      rw = WERT_TO_REAL(w);
 
      if      (rw <= -10) prob = 0.0;
      else if (rw >=  10) prob = 1.0;
      else                prob = EXPWERT(rw);

      printf(" (%+7.4f %2d%%)", rw, 
       	(int)(my_round(prob*100)) == 100 ? 99 : my_round(prob*100));
    }
  }
}



/* Hauptvariante ausgeben */

void VarAus(SFPOS *p, int anz)
{ 
  int i;


  printf("(");
  for (i=anz; i > anz-ZUG_AUS_ANZ && i > 0; i--) { 
    printf(" "); KoorAus(p[i-1]);
  }

  for (i=anz; i < ZUG_AUS_ANZ; i++) printf("   ");
  printf(" )");
}


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


// NewPath true <=> new variation

void Variante(ZUGIO *pzio)
{
  int i;

  Zugermittlung(pzio);

  pzio->cio.NewPath = pzio->PathLen > 0;

  if (pzio->PathLen > 0) {

    pzio->cio.ZugDa = true;
    pzio->cio.IterPathValue = pzio->PathValue;
    pzio->cio.IterPathAl    = pzio->PathAl;
    pzio->cio.IterPathBe    = pzio->PathBe;
    pzio->cio.IterPathType  = pzio->PathType;
    pzio->cio.NewPath       = true;
    
    FOR (i, pzio->PathLen) { 
/*KoorAus(pzio->Path[i]);*/
      pzio->cio.IterPath[i] = pzio->Path[i];
    }
/*printf("\n");*/
    pzio->cio.IterPathLen = pzio->PathLen;
  }

}



void ZugDauer(SPFELD *psf, REAL Restzeit, REAL *pNormal, REAL *pMax)
{
  int SteinAnz;
  REAL Dauer;


  SteinAnz = SfAnz(psf);

  Dauer = Restzeit * Zeitanteil[SteinAnz / 2 - 2];

  if (Dauer < MINDEST_ZEIT) Dauer = MINDEST_ZEIT; 
				/* > 0.0, damit bei gegn. Zeit gerechnet wird */

  *pNormal = Dauer;

  if (Restzeit >= KEINE_ZEIT &&
      Dauer * ZEITFAKTOR < Restzeit - 60 &&
      SteinAnz < 42) {

    Dauer *= ZEITFAKTOR;

  }

  *pMax = Dauer;
}


// time for solving (approximation for PP200)

const float SEL_FACTOR = 0.33;  // ratio selective/exact WLD time

void NewMoveTime(SPFELD *pb, REAL t_remain, REAL &t_normal, REAL &t_max)
{
  int  n;
  REAL t;


  n = SfAnz(pb);

  t = t_remain * Zeitanteil[n / 2 - 2];

  if (t < MINDEST_ZEIT) t = MINDEST_ZEIT; 
  // > 0.0 enables thinking on opponents' time ?!

  t_normal = t;

  if (t >= KEINE_ZEIT &&
      t * ZEITFAKTOR < t_remain - 60 &&
      n < 42) {

    t *= ZEITFAKTOR;

  }

  t_max = t;
}



void StatAus(COMZIO *pcio, ZUGIO *pzio)
{
  REAL     DauerBisher;
  ZEIT     ZeitNach;


  Zeit(&ZeitNach);
  DauerBisher = ZeitDiff(&ZeitNach, &pcio->Startzeit);

  SteinAus(pzio->cio.Partei);
  if (pzio->cio.selbst) { printf("  "); fflush(stdout); }
  else                  { printf("- "); fflush(stdout); }


  switch (pzio->SearchMode) {

  case SM_ENDGAME:   printf("%+3d,%+-3d", pzio->al, pzio->be); break;
  case SM_MIDGAME:   printf("  %2d   ", pzio->MaxTiefe); break;
  case SM_ITERATION: printf("iter %d ", pzio->be); break;
  default:         Error("unknown SearchMode");

  }

  printf("%7.1f (%9d %6d) ",
	 DauerBisher,  pcio->BewAnz,
	 (int) ((REAL)pcio->BewAnz/(DauerBisher+1e-4)));


  if (pzio->SearchMode == SM_ITERATION) {

    Error("iter endgame not supported anymore");
#if 0
    PrintIterStat();
#endif
    
    return;
  }


  PathOut(pzio->Path, pzio->PathLen);

  if (pzio->PathLen == 0) 
    printf(" (     ?     )");
  else			  
    WertAus(pzio->PathValue, pzio->PathType, pzio->PathAl, pzio->PathBe);

  if (pzio->MoveNumber >= 1) {

    printf(" (%2d/%-2d ", pzio->MoveNumber, pzio->MovesTotal);
    KoorAus(pzio->Move1); printf(") ");

  } else printf("           "); 

  printf("\r"); fflush(stdout);
}




#define EXTRAPOL(f) 						\
								\
      pzio->cio.DauerExtr = DauerBisher + f * DauerJetzt;	\
								\
      if (pzio->cio.selbst && 					\
	  DauerBisher >= AT_LEAST * pzio->cio.NormDauer &&	\
	  pzio->cio.DauerExtr > pzio->cio.MaxDauer) {		\
								\
	printf("\nEXTRAPOLATION ABORT\n");			\
        goto Ende; 						\
								\
      }



/* Eingabe: Spielfeld, Partei, LetzterZug, Restzeit
 *
 * Ausgabe: IterVar
 *
 */

SFPOS CompZug(ZUGIO *pzio)
{
  int      MaxTiefe, SteinAnz, ZugAnz, al, be, Wert, dd;
  bool	   gewonnen;
  REAL     DauerJetzt, DauerBisher;
  ZEIT     ZeitVor, ZeitNach;
  SFPOS	   BestZug;

  int komi_al, komi_be;
  bool draw_possible;
  float avg_ab;
  float eval;
  float eval_diff;
      
  Zeit(&pzio->cio.Startzeit);

  pzio->cio.BewAnz = 0;

  pzio->cio.Ausgabe	 = true;		/* Zeit ausgeben */

  pzio->cio.ZugDa        = false;		/* noch kein Zug gefunden */
  pzio->cio.search_state = S_STATE_ZUGERM;

  pzio->Sf         = pzio->cio.Sf;

  /*  SfBrett(&pzio->Sf, &pzio->Brett); */

  /*
    SfAus(&pzio->Sf, BLACK, 0);
    printf(">> %d\n", pzio->cio.Partei);
  */

  /*
    BrettAus(&pzio->Brett);
  */

  pzio->Partei     = pzio->cio.Partei;
  pzio->LetzterZug = pzio->cio.LetzterZug;


#if KILLER
  KillerAdjust(&pzio->killer, &pzio->Sf);
#endif

  BestZug  = ZUG_UNBEKANNT;

  ZugDauer(&pzio->cio.Sf, pzio->cio.Restzeit, 
	   &pzio->cio.NormDauer, &pzio->cio.MaxDauer);

  pzio->cio.DauerExtr = pzio->cio.MaxDauer;

  // compute komi window

  komi_ab(pzio->Partei, pzio->cio.game_komi, komi_al, komi_be);
  draw_possible = (komi_be - komi_al >= 2);
  
  pzio->cio.Average = 0;
  
  printf("gkomi=%f k-al=%d k-be=%d dp=%d\n",
	 pzio->cio.game_komi, komi_al, komi_be, draw_possible);

  // mirror:

  if (pzio->cio.ForcedTime) 
    pzio->cio.DauerExtr = pzio->cio.MaxDauer = 
      pzio->cio.NormDauer = pzio->cio.ForcedTime;


  if (pzio->cio.TimeFrac != 1.0) {
    pzio->cio.DauerExtr *= pzio->cio.TimeFrac;
    pzio->cio.MaxDauer  *= pzio->cio.TimeFrac;    
    pzio->cio.NormDauer *= pzio->cio.TimeFrac;
  }


  SfAus(&pzio->Sf, 0, 0);

  printf("total: %.1f sec, movetime: %.1f sec, ", 
	 pzio->cio.Restzeit, pzio->cio.NormDauer);

  SteinAnz = SfAnz(&pzio->cio.Sf);

  DauerBisher = 0.0;


  ZugAnz = SfMoeglZuegeE(&pzio->cio.Sf, pzio->cio.Partei, pzio->Zuege);
  /*  ZugAnz =  SfMoeglZuege(&pzio->cio.Sf, pzio->cio.Partei, pzio->Zuege); */

  printf("%d discs, %d move(s)\n\n", SteinAnz, ZugAnz);

  if (ZugAnz == 0) {

    printf("no move\n"); 

    pzio->cio.IterPathLen = 0;
    return ZUG_PASSEN;
  }

  if (ZugAnz == 1) {

    ZugAnz = SfMoeglZuege(&pzio->cio.Sf, pzio->cio.Partei, pzio->Zuege);

    BestZug = pzio->Zuege[MyRand() % ZugAnz];

    printf("one move: "); KoorAus(BestZug); printf("\n");

    pzio->cio.IterPath[0] = BestZug;
    pzio->cio.IterPathLen = 1;
    pzio->cio.IterPathValue = 0;
    pzio->cio.IterPathType = MIDGAME;

    return BestZug;
  }



  /* mehr als ein Zug möglich */

  pzio->ZugVorgabe = true;
  pzio->VorgabeAnz = ZugAnz;

  /*
    printf("%d Züge  ", ZugAnz); 
    FOR (i, ZugAnz) { KoorAus(pzio->Zuege[i]); printf(" "); }
    printf("\n");
  */

  MaxTiefe = MAXTIEFE0; 
  
  pzio->Ausgabe = true;

  WERT last_value = 0;
  
  FOREVER {

    //  ************* next iteration ************

    pzio->ZugVorgabe = false;  // just to be sure ...

#if 0
    {
      int i, hfree=0;


      FOR (i, pzio->hash.HashMask+1) { 
	if (pzio->hash.HashTab[i].BesterZug == ZUG_UNBEKANNT) hfree++; 
      }

      printf("%d -> %.2f\n", hfree, ((float)hfree)/HASHANZ);
    }
#endif


    Zeit(&ZeitVor);

    if (pzio->cio.p2) {

      /************* plus 2 **************/

      pzio->cio.t4t_msg[0] = 0;

      if (!pzio->cio.ForcedDepth) Error("tit-for-tat only for fixed depth");

      // endgame still perfect

      if (pzio->cio.ForcedEndDiscNum && SteinAnz >= pzio->cio.ForcedEndDiscNum &&
	  (pzio->cio.ForcedDepth && MaxTiefe > pzio->cio.ForcedDepth)) 
	goto endgame;

      if (SteinAnz + MaxTiefe >= 64) goto endgame;

      if (MaxTiefe == pzio->cio.ForcedDepth+1) {

	// determine worst move >= 60%

	const int close_value  = my_round(REAL_TO_WERT(LOGIT(0.60)));

	if (pzio->cio.IterPathValue < close_value) {

	  // val of best move < close_value => make it so :-)

	  break;
	}

	printf("look for an inferior move\n");

	float best_value = WERT_TO_REAL(pzio->PathValue);

	// midgame search

	pzio->MaxTiefe    = MaxTiefe-1;
	pzio->BewMitte    = pzio->cio.mBewMitte;
	
	pzio->Selective   = pzio->cio.Selective;
	pzio->Percentile1 = pzio->cio.Percentile1;
	pzio->Percentile2 = pzio->cio.Percentile2;
	pzio->Quiescence  = pzio->cio.Quiescence;
      
	pzio->ZugVorgabe = true;

	SFPOS moves[64];
	int mn = SfMoeglZuegeE(&pzio->Sf, pzio->Partei, moves);
	int i, best_move = pzio->cio.IterPath[0];

	printf("best move:\n");
	KoorAus(best_move); printf(" %f\n", best_value);

	int worst_val = MAXINT;

	FOR (i, mn) {

	  if (moves[i] != best_move) {

	    pzio->Modus = MODUS_ASPIRATION;
	    pzio->al = close_value-1;
	    pzio->be = WERTGEWINN+64;
	    pzio->VorgabeAnz = 1;
	    pzio->Zuege[0] = moves[i];

	    Zugermittlung(pzio);

	    if (pzio->Wert != pzio->PathValue) Error("diff. values!");

	    if (pzio->Wert >= close_value) {

	      if (pzio->Wert < worst_val) {

		// update "best" move

		worst_val = pzio->Wert;
		assert(pzio->PathLen > 0);
		pzio->cio.ZugDa = true;
		pzio->cio.IterPathValue = pzio->PathValue;
		pzio->cio.IterPathAl    = pzio->PathAl;
		pzio->cio.IterPathBe    = pzio->PathBe;
		pzio->cio.IterPathType  = pzio->PathType;
	
		int j;

		FOR (j, pzio->PathLen) { 
		  pzio->cio.IterPath[j] = pzio->Path[j];
		}
		pzio->cio.IterPathLen = pzio->PathLen;

		KoorAus(moves[i]); printf(" %f\n", WERT_TO_REAL(pzio->PathValue));
	      }
	    }
	  }
	}

	pzio->ZugVorgabe = false;

	if (best_move != pzio->cio.IterPath[0]) {

	  pzio->cio.t4t_score += WERT_TO_REAL(pzio->cio.IterPathValue) - best_value;
	  pzio->cio.t4t_updated = true;
          sprintf(pzio->cio.t4t_msg, "%.1f discs given away so far", -pzio->cio.t4t_score*10);

	}

	goto Ende;
      }
    }


    if (pzio->cio.t4t) {

      /************* tit-for-tat **************/

      pzio->cio.t4t_msg[0] = 0;

      if (!pzio->cio.ForcedDepth) Error("tit-for-tat only for fixed depth");

      // endgame still perfect

      if (pzio->cio.ForcedEndDiscNum && SteinAnz >= pzio->cio.ForcedEndDiscNum &&
	  (pzio->cio.ForcedDepth && MaxTiefe > pzio->cio.ForcedDepth)) 
	goto endgame;

      if (SteinAnz + MaxTiefe >= 64) goto endgame;

      if (MaxTiefe == pzio->cio.ForcedDepth+1) {

	const int close_value  = my_round(REAL_TO_WERT(LOGIT(0.60)));
	const int randomize_lb = my_round(REAL_TO_WERT(LOGIT(0.45)));
	const int accept_value = my_round(REAL_TO_WERT(LOGIT(0.40)));

	if (pzio->cio.IterPathValue < close_value) {

	  float r = FRAN;

	  if (r < 0.33 && pzio->cio.IterPathValue >= randomize_lb) {

	    // 10% of the time: 
	    // pick the second best move if its value is >= randomize_lb

	    float best_value = WERT_TO_REAL(pzio->PathValue);

	    printf("find 2nd best move\n");

	    // midgame search

	    pzio->MaxTiefe    = MaxTiefe-1;
	    pzio->BewMitte    = pzio->cio.mBewMitte;
	
	    pzio->Selective   = pzio->cio.Selective;
	    pzio->Percentile1 = pzio->cio.Percentile1;
	    pzio->Percentile2 = pzio->cio.Percentile2;
	    pzio->Quiescence  = pzio->cio.Quiescence;
	    
	    pzio->ZugVorgabe = true;

	    int best_move = pzio->cio.IterPath[0];
	    int mn = SfMoeglZuegeE(&pzio->Sf, pzio->Partei, pzio->Zuege);

	    int j;
	    FOR (j, mn) 
	      if (pzio->Zuege[j] == best_move) {
		pzio->Zuege[j] = pzio->Zuege[mn-1];
		mn--;
		j = -1;
		break;
	      }

	    if (j >= mn) Error("best move not found");

	    pzio->VorgabeAnz = mn;
	    Zugermittlung(pzio);

	    KoorAus(pzio->Path[0]); 
	    printf(" %f\n", WERT_TO_REAL(pzio->Wert));

	    if (pzio->Wert >= accept_value) {

	      // choose 2nd best move if value is acceptable

	      assert(pzio->PathLen > 0);
	      pzio->cio.ZugDa = true;
	      pzio->cio.IterPathValue = pzio->PathValue;
	      pzio->cio.IterPathAl    = pzio->PathAl;
	      pzio->cio.IterPathBe    = pzio->PathBe;
	      pzio->cio.IterPathType  = pzio->PathType;
	      
	      int j;
	      
	      FOR (j, pzio->PathLen) { 
		pzio->cio.IterPath[j] = pzio->Path[j];
	      }
	      pzio->cio.IterPathLen = pzio->PathLen;
	      
	      printf("pick 2nd best move: ");
	      pzio->cio.t4t_score += WERT_TO_REAL(pzio->cio.IterPathValue) - best_value;
	      pzio->cio.t4t_updated = true;
	      sprintf(pzio->cio.t4t_msg, "%.1f discs given away so far", -pzio->cio.t4t_score*10);
	      goto Ende;

	    } else break;

	  } else {

	    // val of best move < close_value => make it so :-)

	    break;

	  }
	}

	if (FRAN < 0.66) break;

	printf("look for an inferior move\n");

	int end_d = max(64-pzio->cio.ForcedEndDiscNum, pzio->cio.ForcedDepth);
	float best_value = WERT_TO_REAL(pzio->PathValue);

	// try to reach a close evaluation just before endgame

	int bad_value = 
	  my_round(REAL_TO_WERT(((pzio->cio.t4t_score-best_value)*
				 (64-end_d-SteinAnz)/SteinAnz)*
				(float(SteinAnz)/(SteinAnz+30))
				));

	printf("avg. loss per move=%.1f\n",
	       (pzio->cio.t4t_score-best_value)/(SteinAnz*0.5));
	printf("bad_value = %.1f\n", WERT_TO_REAL(bad_value));


        // -round(pzio->cio.IterPathValue/2.0);  // tit-for-tat :)

        if (FRAN < 0.85) bad_value /= 2;
	printf("bad_value = %.1f\n", WERT_TO_REAL(bad_value));

        
	// midgame search

	pzio->MaxTiefe    = MaxTiefe-1;
	pzio->BewMitte    = pzio->cio.mBewMitte;
	
	pzio->Selective   = pzio->cio.Selective;
	pzio->Percentile1 = pzio->cio.Percentile1;
	pzio->Percentile2 = pzio->cio.Percentile2;
	pzio->Quiescence  = pzio->cio.Quiescence;
      
	pzio->ZugVorgabe = true;

	SFPOS moves[64];
	int mn = SfMoeglZuegeE(&pzio->Sf, pzio->Partei, moves);
	int i, best_move = pzio->cio.IterPath[0];

	printf("best move:\n");
	KoorAus(best_move); printf(" %f\n", best_value);

	FOR (i, mn) {

	  if (moves[i] != best_move) {

	    pzio->Modus = MODUS_ASPIRATION;
	    pzio->al = bad_value-1;
	    pzio->be = bad_value;
	    pzio->VorgabeAnz = 1;
	    pzio->Zuege[0] = moves[i];

	    Zugermittlung(pzio);

	    if (pzio->Wert != pzio->PathValue) Error("diff. values!");

	    if (pzio->Wert >= bad_value) {

	      pzio->Modus = MODUS_NORMAL;
	      Zugermittlung(pzio);
	    
	      if (pzio->Wert >= bad_value &&
		  pzio->Wert < pzio->cio.IterPathValue) {

		// update "best" move

		assert(pzio->PathLen > 0);
		pzio->cio.ZugDa = true;
		pzio->cio.IterPathValue = pzio->PathValue;
		pzio->cio.IterPathAl    = pzio->PathAl;
		pzio->cio.IterPathBe    = pzio->PathBe;
		pzio->cio.IterPathType  = pzio->PathType;
	
		int j;

		FOR (j, pzio->PathLen) { 
		  pzio->cio.IterPath[j] = pzio->Path[j];
		}
		pzio->cio.IterPathLen = pzio->PathLen;

		KoorAus(moves[i]); printf(" %f\n", WERT_TO_REAL(pzio->PathValue));
	      }
	    }
	  }
	}

	pzio->ZugVorgabe = false;

	if (best_move != pzio->cio.IterPath[0]) {

	  pzio->cio.t4t_score += WERT_TO_REAL(pzio->cio.IterPathValue) - best_value;
	  pzio->cio.t4t_updated = true;
          sprintf(pzio->cio.t4t_msg, "%.1f discs given away so far", -pzio->cio.t4t_score*10);

	}

	goto Ende;
      }
    }

    /* früher durchrechnen, wenn Stellung extrem ist */

    if (!pzio->cio.Selective) {

      dd = WLD_DIFF_BRUTE;

    } else {

      dd = WLD_DIFF_SEL;

    }

    dd += pzio->cio.EarlyEndN;  // for early endgame searches

#if 0
    if (pzio->cio.EarlyEndN != 0)
      printf(">>> EarlyEndN = %d\n", pzio->cio.EarlyEndN);

    printf(">>> %d %d %d\n", dd, pzio->cio.Selective, pzio->cio.IterOutcome);

    /*printf("%d\n", MaxTiefe  + dd + GUT3_DIFF    + SteinAnz);*/
#endif

    if (pzio->cio.NoAutomaticEndgame) {

      if (pzio->cio.MaxDauer < EXACT_TIME) {  

	// no solve-search if #<64

	if (MaxTiefe + SteinAnz < 64) goto midgame; 
      }
    }

    if (pzio->cio.ForcedEndDiscNum && SteinAnz >= pzio->cio.ForcedEndDiscNum &&
	(pzio->cio.ForcedDepth && MaxTiefe > pzio->cio.ForcedDepth)) 
      goto endgame;


    // ant wipe-out avoidance: value >= 92% => 2-ply search

    if (pzio->cio.ForcedDepth) {

      if (pzio->cio.ForcedDepth == 1) {

	// ant[+]

	if (MaxTiefe > pzio->cio.ForcedDepth+1) break;
	if (MaxTiefe == pzio->cio.ForcedDepth+1 &&
	    WERT_TO_REAL(pzio->cio.IterPathValue) < LOGIT(0.92)) break;

      } else {

	if (pzio->cio.ForcedDepth && MaxTiefe > pzio->cio.ForcedDepth) break;

      }
    }

    eval = WERT_TO_REAL(pzio->cio.IterPathValue)*DISCFAKTOR;

    avg_ab = 0.5 * (komi_al + komi_be);
    eval_diff = eval - avg_ab;

    // printf("avg=%f eval=%f diff=%f\n", avg_ab, eval, eval_diff);
    
    if (!pzio->cio.ForcedDepth && 
	((SteinAnz + MaxTiefe > 57 ||
	  ZeitDiff(&ZeitVor, &pzio->cio.Startzeit) >= 
	  ENDFAC * pzio->cio.NormDauer) 		&&
	 MaxTiefe > MIN_TIEFE 				&&	   
	 ((MaxTiefe + dd + SteinAnz >= 64)   	        || 	
	  (eval_diff >= DIFF_GOOD0 && 
	   MaxTiefe  + dd + DEPTH0 + SteinAnz >= 64) 	||	      
	  (eval_diff >= DIFF_GOOD1 &&
	   MaxTiefe  + dd + DEPTH1 + SteinAnz >= 64) 	||	      
	  (eval_diff >= DIFF_GOOD2 &&
	   MaxTiefe  + dd + DEPTH2 + SteinAnz >= 64)    ||
	  (eval_diff >= DIFF_GOOD3 &&
	   MaxTiefe  + dd + DEPTH3 + SteinAnz >= 64)
	  ))) {


      // endgame search

	  endgame:

      pzio->cio.DauerExtr = -1;	// use all time

      if (pzio->cio.MaxDauer >= EXACT_TIME) goto exact; 


#if 1

      if (pzio->cio.IterOutcome && SteinAnz < 45) {


	// best first search
 
	pzio->Modus = MODUS_ITER;

	if (EXPWERT(WERT_TO_REAL(pzio->cio.IterPathValue)) >= WIN_P)
	  pzio->be = 1;
	else
	  pzio->be = 0;

	pzio->al = pzio->be - 1;

	Variante(pzio);

	if (pzio->cio.timeout) goto Ende;		// timeout

	StatAus(&pzio->cio, pzio);
	printf("\n");
  

	if (pzio->cio.IterPathValue == 0)         // draw
	  goto Ende;

	// store best move in hashtable => first move in succeeding searches
  
	{
	  HashEntry e = pzio->hash.get(pzio->Brett.Hash1, pzio->Partei);

	  pzio->hash.set(e, pzio->Brett.Hash2, 1, 0, 0, pzio->cio.IterPath[0], 0);
	  e.set_value_type(HashEntry::VALUE_UNKNOWN);
	  
	}
	
	if (abs(pzio->cio.IterPathValue) >= WERTGEWINN) {

	  if (pzio->cio.IterPathValue >= WERTGEWINN) {

	    if (pzio->be == +1) Wert = +1; else Wert = 0;

	    gewonnen = true;

	  } else {

	    if (pzio->al == -1) Wert = -1; else Wert = 0;

	    gewonnen = false;
	  }

	  goto optimal;

	} else printf("\nnot finished?\n");

      }

#endif




#if 1


#define SELEND(sel) \
      if (sel) cout << "selective (" << pzio->EndCutNiveau << ")" << endl; \
      else     cout << "exact" << endl;                         \
      if (SEL_WIDE_WINDOW) { pzio->al = -64; pzio->be = 64; }   \
      pzio->Selective  = sel;			                \
      pzio->Percentile1 = pzio->cio.Percentile1;		\
      pzio->Percentile2 = pzio->cio.Percentile2;		\
      Zeit(&ZeitVor);						\
      Variante(pzio);						\
      Zeit(&ZeitNach);						\
      DauerJetzt  = ZeitDiff(&ZeitNach, &ZeitVor);		\
      DauerBisher = ZeitDiff(&ZeitNach, &pzio->cio.Startzeit);	\
								\
      if (pzio->cio.timeout) goto Ende;		/* timeout */	\
								\
      pzio->MoveNumber = 0;					\
      StatAus(&pzio->cio, pzio);				\
      printf("\n");						\
  								\
      EXTRAPOL(ENDEXTR);

      //  selective:

      // EndCut search

      Wert = komi_al;

      for (int i=0; i < NIV_STEPS; i++) {  // was 10, now 0.6 (0.3) 3.9

	pzio->Modus = MODUS_NEGAC;
	pzio->EndCutNiveau = pzio->cio.EndCutNiveau + i * NIV_INCR;

	// new: adjust komi_al according to last iteration ->
	// searches for exact value earlier (wasting no time proving silly bounds)

	pzio->al = komi_al; pzio->be = pzio->al + 1;
	SELEND(true);

	Wert = (int) pzio->cio.IterPathValue;
	if      (Wert <= komi_al) komi_al -= 2;
	else if (Wert >= komi_al) komi_al += 2;

#if 0

	// old (search around komi_al)
	
	if (draw_possible) {

	  // allow toggling between [al-1,al] and [al,al+1]
	  
	  if (Wert <= komi_al) {

	    pzio->al = komi_al; pzio->be = pzio->al + 1;
	    SELEND(true);
	    Wert = (int) pzio->cio.IterPathValue;
	    if (Wert <= komi_al) continue;
	  }

	  pzio->al = komi_al + 1; pzio->be = pzio->al + 1;
	  SELEND(true);
	  Wert = (int) pzio->cio.IterPathValue;
	  if (Wert >= komi_be) continue;
	  Wert = komi_al;

	} else {

	  // only use [al,al+1]
	  
	  pzio->al = komi_al; pzio->be = komi_be;
	  SELEND(true);
	  
	}
#endif	

      }

#endif

	  exact:

      // 3. exact search

      pzio->al = komi_al; pzio->be = pzio->al + 2;
      pzio->Modus = MODUS_NEGAC;
      SELEND(false)

	Wert = pzio->cio.IterPathValue;

      if ((draw_possible && Wert == komi_al+1) ||
	  Wert >= 64) goto Ende;	// draw or wipe-out

      gewonnen = (Wert >= komi_be);

	  optimal:	

      if (pzio->cio.StopM && 
	  pzio->cio.StopM <= SteinAnz &&
	  SteinAnz <= pzio->cio.StopN) {

	printf("STOP\n");
	goto Ende;
      } 

      // 4. maximize result by shifting a small window

      pzio->Modus = MODUS_NEGAC;
      pzio->Selective = false;

      FOREVER {			

	if (gewonnen) { al = Wert-1; be = Wert+SCHRITT; }
	else	    { al = Wert-SCHRITT; be = Wert+1; }
	
	if (al < -64) al = -64;
	if (be >  64) be =  64;
	
	pzio->al = al; pzio->be = be;
      
	pzio->Selective = false;

	Zeit(&ZeitVor);
	Variante(pzio);
	Zeit(&ZeitNach);
	DauerJetzt  = ZeitDiff(&ZeitNach, &ZeitVor);
	DauerBisher = ZeitDiff(&ZeitNach, &pzio->cio.Startzeit);
	
	
	if (pzio->cio.timeout) goto Ende;	/* timeout */
	
	pzio->MoveNumber = 0;
	StatAus(&pzio->cio, pzio);
	printf("\n");

	EXTRAPOL(ENDEXTR);

	Wert = (int) pzio->cio.IterPathValue;

	if (abs(Wert) >= 64) break;		/* wipe-Out */
	
	if (Wert > al && Wert < be) break;	/* value exact => end */

      }


      /* optimal move determined or timeout */

      goto Ende;

    } else {

    midgame:

      // midgame search

      pzio->MaxTiefe   = MaxTiefe;
      pzio->BewMitte   = pzio->cio.mBewMitte;
      
      pzio->Modus      = MODUS_NORMAL;
      pzio->Selective  = pzio->cio.Selective;
      pzio->Percentile1 = pzio->cio.Percentile1;
      pzio->Percentile2 = pzio->cio.Percentile2;
      pzio->Quiescence = pzio->cio.Quiescence;
      
    }

    Variante(pzio);

    if (pzio->cio.ZugDa && pzio->cio.game_synchro) {

      // update komi in synchro games: around last value

      if (abs(pzio->cio.IterPathValue) >= WERTGEWINN) {

	if (pzio->cio.IterPathValue >= WERTGEWINN)
	  komi_al = pzio->cio.IterPathValue - WERTGEWINN;
	else
	  komi_al = pzio->cio.IterPathValue + WERTGEWINN;	  

      } else {

	komi_al = int(floor(WERT_TO_REAL(pzio->cio.IterPathValue)*DISCFAKTOR));

      }

      komi_be = komi_al+1;
      draw_possible = false;

      // printf("NEW KOMI: %d %d\n", komi_al, komi_be);      
    }

    if (pzio->Modus == MODUS_NORMAL && pzio->cio.ZugDa && pzio->cio.NewPath) {

      // update average for random games

      // printf(">>> IPV=%d LV=%d ", pzio->cio.IterPathValue, last_value);
      
      if (MaxTiefe == MAXTIEFE0) {
	pzio->cio.Average = pzio->cio.IterPathValue;
	pzio->cio.val1 = pzio->cio.val2 = pzio->cio.IterPathValue;
      } else {
	pzio->cio.Average = my_round(last_value * 0.5 + pzio->cio.IterPathValue * 0.5);
	pzio->cio.val1 = last_value;
	pzio->cio.val2 = pzio->cio.IterPathValue;
      }

      // printf(" AVG=%d\n", pzio->cio.Average);
      last_value = pzio->cio.IterPathValue;
    }

    Zeit(&ZeitNach);
    DauerBisher = ZeitDiff(&ZeitNach, &pzio->cio.Startzeit);

    pzio->MoveNumber = 0;
    StatAus(&pzio->cio, pzio);
    printf("\n");

    if (pzio->cio.timeout) {			/* timeout => not all moves */

      break;					/* examined => take last */

    } else

      if (pzio->fertig) break;			/* all moves examined => end */


    if (pzio->cio.IterPathLen == 0) Error("no move?");

    BestZug = pzio->cio.IterPath[0];

    if (!ZUG(BestZug)) {
      printf("%d %d %d\n", BestZug, pzio->cio.timeout, pzio->fertig);
      Error("GefZug1");
    }


    Zeit(&ZeitNach);

    
    /******** extrapolate time *********/


    DauerJetzt  = ZeitDiff(&ZeitNach, &ZeitVor);
    DauerBisher = ZeitDiff(&ZeitNach, &pzio->cio.Startzeit);


#if TEST
    { SPFELD sf=pzio->cio.Sf;
    int    i, Partei=pzio->cio.Partei;

    FOR (i, pzio->cio.IterPathLen) {

      if (ZUG(pzio->cio.IterPath[i]) && 
	  !SfSetzen(&sf, Partei, pzio->cio.IterPath[i])) 
	Error("Zug nicht möglich");
  
      Partei = GEGNER(Partei);
    }
  
    SfAus(&sf, pzio->cio.Partei, -1);

    }
#endif


    /* wipe-Out? */

    if (abs(pzio->cio.IterPathValue) >= WERTGEWINN+64) {

      printf(" WIPE-OUT "); fflush(stdout);
      break;

    }


    EXTRAPOL(MIDEXTR);

    MaxTiefe += MAXTIEFE_STEP;

#if TEST
    { int i; SPFELD Sf;

    BrettSf(&pzio->Brett, &Sf);
    FOR (i, 100) 
      if ((pzio->Brett.p[i] >= UMG && Sf.p[i] != LEER) ||
	  (pzio->Brett.p[i] < UMG &&  Sf.p[i] != pzio->Brett.p[i])) {

	SfAus(&Sf, BLACK, 0); BrettAus(&pzio->Brett);
	printf("%d: %d %d\n", i, pzio->Brett.p[i], Sf.p[i]);
	Error("Brett verändert");
      }}
#endif


  }


 Ende:

  Zeit(&ZeitNach);

  DauerBisher = ZeitDiff(&ZeitNach, &pzio->cio.Startzeit);

  if (!pzio->cio.selbst) DauerBisher = 0;

  if (pzio->cio.timeout)	printf("\nTIME\n");
  else if (pzio->fertig)	printf("\nEND\n");
  fflush(stdout);

  /*printf("%.2fs, ", DauerBisher);*/

  if (pzio->cio.IterPathLen == 0) Error("kein Zug da");

  BestZug = pzio->cio.IterPath[0];

#if 0
  printf("\nZug: "); KoorAus(BestZug);
  /* printf("  ");
     WertAus(pzio->cio.IterPathValue); printf(" ");
     VarAus(pzio->cio.IterVar, pzio->cio.IterVarPos);
  */
  printf("\n");
#endif

  return BestZug;
}



