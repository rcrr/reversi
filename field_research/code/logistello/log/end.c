// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

// fast endgame search / 1.8.92

#include "main.h"
#include "move.h"
#include "eval.h"
#include "hash.h"
#include "killer.h"
#include "order.h"
#include "end.h"
#include "fastend.h"
#include "mid.h"
#include "featurem.h"
#include "crt.h"
#include "crtg.h"
#include "pcstat.h"
#include "ffend.h"

int zaehler=0;

#define CANCEL_DEEP_VALUE 0  // 0 when using hashstamp
#define USE_OWN_HASHTAB   0

#define FASTEST_FIRST   1

#define CORNER_FIRST    0   /* 1 slower */

#if 0
#define inline		/*!!!!!!!*/
#endif

#define DIRTY		true	/* !!!!! */
#define DIRTY_STAT	false


// EndCut constants

#define NEW_ENDCUT      true
#define ENDCUT_STAT_FILE "endcutstatl"

#define NUMSTART	35  
#define NUMEND		51 // 51  
#define CHECK_DEPTH	4  

#define FIRST_LEVELS_HASH false
#define PROBING           false


#define TEST		false
#define ALBETEST	false

#define WERTAUS(s,w)	printf("%s w=%d\n", s, w)


#if	0

#define ENDE_SCHNELL	false
#define OHNE_ANZ	80
#define OPTI_ANZ	0
#define NULLFENSTER
#define WIEDERHOLUNG
#define WERTEHASH	false

#else

#define ENDE_SCHNELL	true	/* letzten Zug direkt		        */
#define NEW_FASTEND     false
#define OHNE_ANZ	58	/* !!! 58 ab hier keine Tabellen mehr   */
#define OPTI_ANZ	56	/* !!! 56 bis hier Züge optimieren	*/

#define NULLFENSTER     hi_value = max + 1
#define WIEDERHOLUNG    \
if (value > max && value < be && ZugAnz > 1) \
  value = -EndAlphaBeta1(pzio, GEGNER(Partei), -be, -value, AktZug);

#define WERTEHASH	true
#endif


static int VorTiefen[65] = {

// old

/* 0*/   5,5,5,5,5,5,5,5,5,5,
/*10*/   5,5,5,5,5,5,5,5,5,5,
/*20*/   5,5,5,5,5,5,5,5,5,5,
/*30*/   5,5,5,8,8,8,8,8,7,7,
/*40*/   7,6,5,4,3,2,2,2,2,2,
/*50*/   2,1,1,1,1,1,1,2,2,2,
/*60*/   2,2,1,1

};


static PCStat endpcstat;

#if 1

// current

static int ApproxDepths[65] = {
  /* 0*/ 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  /*20*/ 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8,8,8,8,7,
  /*40*/ 7,6,6,6,5,5,5,4,4,4,
  /*50*/ 4,4,3,3,3,0,0,0,0,0,
  /*60*/ 0,0,0,0,0
};

#else

// prev
static int ApproxDepths[65] = {
  /* 0*/ 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  /*20*/ 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  /*40*/ 0,0,5,5,5,5,5,4,4,4,
  /*50*/ 4,4,3,3,3,0,0,0,0,0,
  /*60*/ 0,0,0,0,0
};
#endif




/* Nicht EndSetzen wg. Sortierung mit Bewertungsfunktion */

#define TRY \
if (pbr->umg[AktZug] >= UMG && AktZug != HashZug) {\
    if (MAKE_MOVE(pbr, Partei, AktZug, &Delta)) {\
      ZugAnz++;\
/* Tiefe 1: */ pzio->Move1 = AktZug; pzio->MoveNumber = ZugAnz;\
      value = -EndAlphaBeta1(pzio, GEGNER(Partei), -hi_value, -max, AktZug);\
      if (value < al) value = al;	/*!!!*/\
      if (value > lo_value) {\
/* unabhängig von Ergebnis der Wiederholungssuche übernehmen!*/\
	BestZug = AktZug;\
        UNDO_MOVE(pbr, &Delta);\
/* Tiefe 1: */ FindPath(pzio, Partei, AktZug, max, hi_value, value, ENDGAME);\
	MAKE_MOVE(pbr, Partei, AktZug, &Delta);\
      }\
      WIEDERHOLUNG;\
      if (value < al) value = al;	/*!!!*/\
      UNDO_MOVE(pbr, &Delta);\
      if (value > lo_value) {\
	BestZug = AktZug;\
/* Tiefe 1: */ FindPath(pzio, Partei, AktZug, al, be, value, ENDGAME);\
\
        lo_value = value;			/* übernehmen, wenn besser */\
        if (lo_value >= be) goto Ende;		/* beta-Schnitt */\
        if (lo_value > max) max = lo_value;\
        NULLFENSTER;\
      }\
    }\
  }

int EndAlphaBeta(
  ZUGIO		*pzio,		/* Zeiger auf globale Variablen		*/
  PARTEI	Partei,		/* Spieler am Zug			*/
  int		al,		/* alpha-beta Fenster			*/
  int		be,
  int		LetzterZug
)
{
  int		i, ZugAnz, KZug; 
  SFPOS		BestZug, AktZug, HashZug;
  HashEntry	*ph = 0;
  int		value, lo_value, hi_value, max;
  KILLTAB	*Kill;
  SFPOS		*pl;
  BRETT		*pbr;
  DELTA		Delta;

#if USE_OWN_HASHTAB
  HashTab       &hd = pzio->endhash;
#else
  HashTab       &hd = pzio->hash;
#endif
  
#if TEST
  BRETT	Brett0;
#endif

  pzio->endhash0.next_stamp();
  pzio->endhash.next_stamp();
  
  pzio->cio.BewAnz++;

  pzio->fertig = false;

  pbr = &pzio->Brett;

  ZugAnz = 0; BestZug = ZUG_UNBEKANNT;

  lo_value = WERTMIN; hi_value = be; max = al;
  
  FindFreeSquares(pzio);     /* if discnumber >= OHNE_ANZ */
 
  /* compute KZug: last move or occupied square */

  if (ZUG(LetzterZug)) 

    KZug = LetzterZug; 

  else {

    /* look for occupied square -> killer move */

    FOR (KZug, 100) 
      if (pbr->p[KZug] == BLACK || pbr->p[KZug] == WHITE) break;

    if (KZug >= 100) { printf("no disc???\a\a\a\n"); KZug = D4; }

  }


  HashZug = ZUG_UNBEKANNT;


#if HASH

  ph = &hd.get(pbr->Hash1, Partei);

#if WERTEHASH

  if (HashEntry::locks_equal(ph->get_lock(), pbr->Hash2) 		    &&
      ph->get_height() == HashEntry::END_HEIGHT		 		    &&
      (pbr->SteinAnz > NUMEND || HashEntry::stamps_equal(ph->get_stamp(), hd.get_stamp())) &&
      ZUG(ph->get_best_move()) && pbr->umg[ph->get_best_move()] >= UMG  &&
      MAKE_MOVE(pbr, Partei, ph->get_best_move(), &Delta)
      ) {

    UNDO_MOVE(pbr, &Delta);

    if (ph->get_value_type() == HashEntry::VALUE_EXACT  ||
	(ph->get_value_type() == HashEntry::VALUE_LE_MINIMAX && ph->get_value() >= be) ||
	(ph->get_value_type() == HashEntry::VALUE_GE_MINIMAX && ph->get_value() <= al)) {

      /* Stellung nicht neu berechnen! */


#if TEST

      if (ph->get_height() == 0) printf("%d\n", RealeHoehe);

#endif

      FindPath(pzio, Partei, ph->get_best_move(), al, be, ph->get_value(), ENDGAME);\

      pzio->fertig = false;	/* noch nicht alles durchgerechnet     */

      return ph->get_value();

    } else  {

      /* printf("!"); fflush(stdout); */

    }
  }

#endif    





#if TEST
  if (ha > pzio->HashMask) { printf("%lu", ha); Error("hash"); }
#endif


  /* Hashzug zuerst probieren */

  AktZug = ph->get_best_move(); 

#if TEST
  if (AktZug < -2 || AktZug >= 89) Error("hashinhalt");
#endif
   

  if (HashEntry::locks_equal(ph->get_lock(), pbr->Hash2) && ZUG(AktZug)) { 
    TRY; HashZug = AktZug; 
  }

#endif


#if 0


  /* first method: use killerlist if discnum is at least OPTI_ANZ
   *               otherwise sort moves
   */


  if (pbr->SteinAnz >= OPTI_ANZ) {		/* hier immer!!! */

  
    /* Rest gemäß Killertabelle oder DefaultKill */

    if (Partei == BLACK) Kill = pzio->killer.KillWHITE; 
    else 		   Kill = pzio->killer.KillBLACK;
  
    pl = &Kill->Liste[KZug][0];


#if TEST
    {
      int i, j;

      if (64-BrettAnzBW(pbr) > pzio->killer.FreiAnz) Error("killeranz");
    }
#endif

    for (i=pzio->killer.FreiAnz; i > 0; i--) {

      AktZug = *pl++;

      TRY
	}

  } else { 		/* Züge sortieren */

    ZUGDAT ZugDat[65];
    int    Anz = 
      ZuegeSortieren(pzio, Partei, VorTiefen[pbr->SteinAnz], ZugDat); 

#if 0
    printf("\n");
    FOR (i, Anz) { KoorAus(ZugDat[i].Zug); printf(":%d ", ZugDat[i].Wert); }
    printf("\n");
#endif

    FOR (i, Anz) {

      AktZug = ZugDat[i].Zug;

      TRY
	}
  }


#else


  /* second method: use 2 killermoves and then sorted moves */

#if KILLER

  { int ki1, ki2;


  /* use killerlist for remaining moves */


  if (Partei == BLACK) Kill = pzio->killer.KillWHITE; 
  else 		       Kill = pzio->killer.KillBLACK;

  pl = &Kill->Liste[KZug][0];

#else

#error KILLER not true!

#endif


  /* only 2 killermoves (actually only one, because first move is hashmove!) */


  ki1 = ki2 = ZUG_UNBEKANNT;

  if (pzio->killer.FreiAnz >= 1) {

    ki1 = AktZug = *pl++;
    TRY

      }

  if (pzio->killer.FreiAnz >= 2) {

    ki2 = AktZug = *pl++;
    TRY

      }



  // sort moves

  {
    ZUGDAT ZugDat[65];
    int    Anz;
 
    Anz = ZuegeSortieren(pzio, Partei, VorTiefen[pbr->SteinAnz], HashZug, ZugDat);

    FOR (i, Anz) {

      AktZug = ZugDat[i].Zug;

      if (AktZug != ki1 && AktZug != ki2) TRY
					    }
  }

  }
#endif


#if 0
  { SFPOS moves[65];

  if (ZugAnz != SfMoeglZuege(&pzio->Sf, Partei, moves)) Error("too few moves!");

  }
#endif

  if (ZugAnz == 0) { 

    if (LetzterZug == ZUG_PASSEN) {		/* keiner kann */

      BEWTEST

	ANTI_RET(BEWDIFF1);

    } else {					/* passen */

      return -EndAlphaBeta1(pzio, GEGNER(Partei), -be, -al, ZUG_PASSEN);
    }
  }




 Ende:

#if TEST
  if (Tiefe == 1 && (pzio->PathLen > MAX_DEPTH || pzio->PathLen <= 0)) Error("VARPOS2");
#endif


  /* Hash- und Killer-Eintrag anpassen */

#if TEST
  if (!ZUG(BestZug)) Error("BestZug kein Zug");
#endif

#if HASH

  pzio->hash.set(*ph, pbr->Hash2, HashEntry::END_HEIGHT, al, be, BestZug, lo_value);

#if DIRTY

    // new: allow hash values, use if stamps are equal

#if CANCEL_DEEP_VALUE      
    if (pzio->Selective && pbr->SteinAnz <= NUMEND) {
      ph->set_value_type(WERT_UNBEKANNT);
    }
#endif

#endif

#endif

#if KILLER
  if (ZUG(LetzterZug)) {

#if TEST
    if (!ZUG(BestZug)) { printf("e:"); KoorAus(BestZug); }
#endif

    KillerUpdate(&pzio->killer, GEGNER(Partei), LetzterZug, BestZug);

  }
#endif

  return lo_value;
}






/* tiefere Knoten */

#define TRY1 \
if (pbr->umg[AktZug] >= UMG && AktZug != HashZug) {\
    if (MAKE_MOVE(pbr, Partei, AktZug, &Delta)) {\
      ZugAnz++;\
      value = -EndAlphaBeta1(pzio, GEGNER(Partei), -hi_value, -max, AktZug);\
      WIEDERHOLUNG;\
      UNDO_MOVE(pbr, &Delta);\
      if (value > lo_value) {\
	if (value > al || ZugAnz == 1) BestZug = AktZug;	/*!!!*/\
        lo_value = value;			/* übernehmen, wenn besser */\
        if (lo_value >= be) { \
          goto Ende;		/* beta-Schnitt */\
        }\
        if (lo_value > max) max = lo_value;\
        NULLFENSTER;\
      }\
    }\
  }

//static int num=0, zsum=0, sqt[11], znum[65], moves[65];

/*
25700 27343 1.063930 : 0 12476 2491 3973 2734 1373 1153 1456 44 0 0 

                           1    4    2    3    6    7    5   8

*/


bool check_flag = false;


int EndAlphaBeta1(
  ZUGIO		*pzio,		/* Zeiger auf globale Variablen		*/
  PARTEI	Partei,		/* Spieler am Zug			*/
  int		al,		/* alpha-beta Fenster			*/
  int		be,
  int		LetzterZug
)
{
  int		i, ZugAnz; 
  SFPOS		BestZug, AktZug, HashZug;
  HashEntry	*ph = 0;
  int		value = 0, lo_value, hi_value, max;
  KILLTAB	*Kill;
  SFPOS		*pl;
  BRETT		*pbr;
  DELTA		Delta;
  UMGEB		*pu;


#if TEST
  BRETT	Brett0;
#endif

#if 0
  pbr = &pzio->Brett;

if (pbr->SteinAnz == 54) { 
static FILE *fp=NULL;
SPFELD sf;

if (!fp) fp = fopen("rrr.sfk", "w");
BrettSf(pbr, &sf);
sf.Marke = MA_WKEIT + 50;
fSfWrite(fp, &sf, 1);
}
#endif
 
  if (--zaehler < 0) { 
    pzio->Check(&pzio->cio, pzio, false, false); 
    zaehler = CHECK_COUNT; 
  }

#if USE_OWN_HASHTAB
  HashTab &hd = pzio->endhash;
  HashTab &hd0 = pzio->endhash0;
#else
  HashTab &hd = pzio->hash;
#endif

  pbr = &pzio->Brett;
  pzio->cio.BewAnz++;

  
#if ENDE_SCHNELL

#if NEW_FASTEND

  if (pbr->SteinAnz >= 54) return FastEnd(pzio, Partei, al, be);

#endif


  if (pbr->SteinAnz >= 63) {	/* 63 -> Spielfeld fast voll */


    if (pbr->SteinAnz == 64) ANTI_RET(BEWDIFF);

    else {

      // one square free -> immediate move


      pzio->cio.BewAnz++;

      AktZug = 3168 - pbr->SteinSumme;

#if TEST
if (!ZUG(AktZug)) Error("aktzug?");
if (pbr->p[AktZug] != LEER) Error("nicht leer");
#endif


      if (pbr->StDiffBW != (value=EndSetzDiff(pbr, Partei, AktZug))) {

	ANTI_RET(Partei == BLACK ? value : -value);

      } else if (pbr->StDiffBW != (value=EndSetzDiff(pbr, GEGNER(Partei), AktZug))) {

	ANTI_RET(Partei == BLACK ? value : -value);

      } 

BEWTEST

      ANTI_RET(BEWDIFF1);
    }
  }
#endif




  ZugAnz = 0; BestZug = ZUG_UNBEKANNT;

  lo_value = WERTMIN; hi_value = be; max = al;

#if DIRTY

  if (pbr->SteinAnz <= NUMEND && pbr->SteinAnz >= NUMSTART &&
      pzio->Selective && al > -64 && be < 64 && // !!!al,be new
      (!DIRTY_STAT || LetzterZug != ZUG_UNBEKANNT)) {

#if DIRTY_STAT

#if 0
    static REAL sume1=0, sumee1=0, sumn1=0;
    static REAL sume2=0, sumee2=0, sumn2=0;

    REAL vp, vpd;
    REAL value, valued;

    value = AlphaBeta1(pzio, 4, Partei, WERTMIN, WERTMAX, ZUG_UNBEKANNT, true);

    value = WERT_TO_REAL(value);

    valued = EndAlphaBeta1(pzio, Partei, -1, 1, ZUG_UNBEKANNT);

    if (valued >= 1) {

      sume1  += value;
      sumee1 += value * value;
      sumn1  += 1;

    } else if (valued <= -1) {

      sume2  += value;
      sumee2 += value * value;
      sumn2  += 1;

    }


if (((int)sumn1+(int)(sumn2)) % 100 == 0 && sumn1 > 0 && sumn2 > 0) 
printf("p:%.2f->%.0f (%.0f: %.3f %.3f) (%.0f: %.3f %.3f)\n", 
	value, valued, 
	sumn1, sume1/sumn1, sqrt(sumee1/sumn1 - (sume1/sumn1)*(sume1/sumn1)),
	sumn2, sume2/sumn2, sqrt(sumee2/sumn2 - (sume2/sumn2)*(sume2/sumn2)));

/*
printf("### %.3f  (%d,%d)\n", valued-value, pbr->SteinAnz/8, pbr->SteinAnz %2);
*/

/*
printf("%.2f (%.2f, %.2f) (%.2f, %.2f)", vp, alp, bep, (alp-vp)/SIGMA, (vp-bep)/SIGMA);
*/

#endif

#else

    int bound;

/*
if (al < -1 || be > 1) Error("DIRTY not with [-1,1]");
*/


#if NEW_ENDCUT

    const PCStat::AppInfo *approx;

    int cd = ApproxDepths[pbr->SteinAnz];

    if (!endpcstat.loaded) {
      String name = DataPath; name += "/"; name += EndPCStatFile;
      endpcstat.read(name);
    }

    approx = endpcstat.get(pbr->SteinAnz, cd, 9);  // only height is 9

    float delta = pzio->EndCutNiveau * approx->s;

    // general [al,be]  (for random games)
   
#if 1
    // new: if cd >= 3 try cd-2 first

    if (cd >= 3) {

      const PCStat::AppInfo *approx;
      approx = endpcstat.get(pbr->SteinAnz, cd-2, 9);  // only height is 9

      bound = (int) REAL_TO_WERT((be + delta - approx->b) * approx->inva);

      if (AlphaBeta1(pzio, cd, Partei, bound-1, bound, ZUG_UNBEKANNT, true) >= bound) {
	if (be & 1) return be+1; else return be;
      }

      bound = (int) REAL_TO_WERT((al - delta - approx->b) * approx->inva);

      if (AlphaBeta1(pzio, cd, Partei, bound, bound+1, ZUG_UNBEKANNT, true) <= bound) {
	if (al & 1) return al-1; else return al;
      }
    }
#endif

    bound = (int) REAL_TO_WERT((be + delta - approx->b) * approx->inva);

#if 0
    cout << "::: " << delta << endl;
    cout << "::: " << (delta - approx->b) << endl;
    cout << "::: " << (delta - approx->b) * approx->inva << endl;

    cout << pbr->SteinAnz << " " << cd << " " 
	 << approx->inva << " " << approx->b << " " << approx->s << " " << bound << endl;
#endif

    if (AlphaBeta1(pzio, cd, Partei, bound-1, bound, ZUG_UNBEKANNT, true) >= bound) {
      if (be & 1) return be+1; else return be;
    }
    bound = (int) REAL_TO_WERT((al - delta - approx->b) * approx->inva);

#if 0
    cout << "2nd bound=" << bound << endl;
#endif

    if (AlphaBeta1(pzio, cd, Partei, bound, bound+1, ZUG_UNBEKANNT, true) <= bound) {
      if (al & 1) return al-1; else return al;
    }

#else

    // old approach
    
    bound = (int)REAL_TO_WERT(pzio->EndCutNiveau);

    if (AlphaBeta1(pzio, CHECK_DEPTH, Partei, bound-1, bound, ZUG_UNBEKANNT, true) >= bound)
      return 1;

    bound = -bound;

    if (AlphaBeta1(pzio, CHECK_DEPTH, Partei, bound, bound+1, ZUG_UNBEKANNT, true) <= bound)
      return -1;

#endif


#endif


  }

#endif



  if (pbr->SteinAnz >= OHNE_ANZ) {

#if 0

    // sort test

    ZUGDAT ZugDat[65];
    int Anz = ZuegeSortieren(pzio, Partei, VorTiefen[pbr->SteinAnz], HashZug, ZugDat, true); 
 
    FOR (i, Anz) {

      AktZug = ZugDat[i].Zug;
      TRY1;
    }

    goto AllesBesucht;
    
#endif
    
      
#if LAZY_UPDATE

    {
 
    if (pbr->SteinAnz == OHNE_ANZ) FindFreeSquares(pzio);

#if TEST
    { int i;

      FOR (i, 100) if (pbr->p[i] == LEER) {
        int j=0;

        for (j=0; pbr->FreeSquares[j]; j++) 
          if (pbr->FreeSquares[j] == i) break;
        if (!pbr->FreeSquares[j]) Error("freesq");
      }
    }
#endif

    for (pu=pbr->FreeSquares; (AktZug=*pu); pu++) { 

      if (pbr->p[AktZug] == LEER && EndSetzen(pbr, Partei, AktZug, &Delta)) {

#else

/* alle freien Positionen in Umgebungsliste? -> EndSetzen */

    alle = 64-pbr->SteinAnz <= pbr->LastIndex+1;

    {

/* jetzt keine Hash- und Killertabellen benutzen, nur noch Umgebungsliste */

      for (i=pbr->LastIndex, pu=pbr->umgl; i >= 0; i--, pu++) {

        AktZug = *pu;

        if (( alle && EndSetzen(pbr, Partei, AktZug, &Delta)) ||
            (!alle && MAKE_MOVE(pbr, Partei, AktZug, &Delta))) {
#endif

          ZugAnz++; 

          value = -EndAlphaBeta1(pzio, GEGNER(Partei), -hi_value, -max, AktZug);

  	  WIEDERHOLUNG;

#if LAZY_UPDATE

          EndZurueck(pbr, Partei, AktZug, &Delta);

#else

          if (alle) EndZurueck(pbr, Partei, AktZug, &Delta);
          else      UNDO_MOVE(pbr, &Delta);	

#endif

          if (value > lo_value) {

            lo_value = value;			/* übernehmen, wenn besser */

            if (lo_value >= be) return lo_value;	/* beta-Schnitt */

            if (lo_value > max) max = lo_value;

	    NULLFENSTER;
	  }
        }
      }
    }

/* hier weiter ... keine Züge etc. */


    goto AllesBesucht;

  } else {

    HashZug = -1;


#if HASH


#if FIRST_LEVELS_HASH

    if (pbr->SteinAnz - pzio->DiscNum0 <= hd0.get_max_height()) {

      /* position seen before => use value and move */

      ha = pbr->Hash1 & hd0.HashMask;
      if (Partei == WHITE) ha = HASH_WHITE(ha);

#if PROBING

      const int PROBE_N = 6;
      int probe_deltas[PROBE_N] = { 0 , +1, -4 , +9, -16, +25 };
      int i, min_depth=100000;
      uint4 ha1, min_ha1=0;

      FOR (i, PROBE_N) {

	ha1 = (ha+probe_deltas[i]*2) & hd0.HashMask;
	phs = &hd0.HashTab[ha1];

	if (HashEntry::locks_equal(phs->Hash2, pbr->Hash2)) {
	  min_ha1 = ha1;
	  break;
	}

	if (phs->Tiefe < min_depth) {
	  min_depth = phs->Tiefe;
	  min_ha1 = ha1;
	}

      }

      ha = min_ha1;

#endif

      phs = &hd0.HashTab[ha];

#if WERTEHASH

      if (HashEntry::locks_equal(phs->Hash2, pbr->Hash2) && 
	  phs->Tiefe == END_DEPTH	    &&
        (pbr->SteinAnz > NUMEND || STAMP_EQUAL(phs->stamp, hd.current_stamp)) &&
	  ZUG(phs->BesterZug)	            &&
	  pbr->umg[phs->BesterZug] >= UMG) {	

	if (phs->get_value_type() == HashEntry::VALUE_EXACT  ||
	    (phs->get_value_type() == HashEntry::VALUE_LE_MINIMAX && phs->Wert >= be) ||
	    (phs->get_value_type() == HashEntry::VALUE_GE_MINIMAX && phs->Wert <= al)) {

	  // value or bound known -> return

	  return phs->Wert;

	} else  {

	  /* printf("!"); fflush(stdout); */
	}
      }

#endif

    } else phs = 0;

#endif

    ph = &hd.get(pbr->Hash1, Partei);

#if TEST
    if (ha > pzio->HashMask) { printf("%lu", ha); Error("hash"); }
#endif
  

    /* ist Stellung schon im Endspiel untersucht worden? */

#if WERTEHASH

    if (HashEntry::locks_equal(ph->get_lock(), pbr->Hash2) && 
	ph->get_height() == HashEntry::END_HEIGHT	   &&
(pbr->SteinAnz > NUMEND || HashEntry::stamps_equal(ph->get_stamp(), hd.get_stamp())) &&
	ZUG(ph->get_best_move())	                   &&
	pbr->umg[ph->get_best_move()] >= UMG) {	

      if (ph->get_value_type() == HashEntry::VALUE_EXACT  ||
        (ph->get_value_type() == HashEntry::VALUE_LE_MINIMAX && ph->get_value() >= be) ||
        (ph->get_value_type() == HashEntry::VALUE_GE_MINIMAX && ph->get_value() <= al)) {

/* Stellung nicht neu berechnen! */

	return ph->get_value();

      } else  {

/* printf("!"); fflush(stdout); */
      }
    }

#endif

    

#if FASTEST_FIRST
  
  // Fastest First
  
  if (pbr->SteinAnz <= 51) {

    ZUGDAT ZugDat[65];
    int num;
    SPFELD sf;
    SFPOS moves[65];
    BrettSf(pbr, &sf);
    num = SfMoeglZuege(&sf, Partei, moves);
    
    if (num > 1) {

      int limit = my_round((be + 3) * (float)WERTFAKTOR/(float)DISCFAKTOR);
      
      if (AlphaBeta1(pzio, VorTiefen[pbr->SteinAnz], Partei, be, limit, ZUG_UNBEKANNT, true) >=
	  limit) {
	  
	// cut likely

	DELTA  Delta;

	num = ZuegeSortieren(pzio, Partei, VorTiefen[pbr->SteinAnz], ZUG_UNBEKANNT, ZugDat);

	FOR (i, num) if (ZugDat[i].Wert < limit) break;
	
	if (i > 2) {
	  
	  int j;

	  //	  printf("%2d i=%d\n", pbr->SteinAnz, i); 

	  FOR (j, i) {

	    // printf("j=%d\n", j);
	    
	    if (MAKE_MOVE(pbr, Partei, ZugDat[j].Zug, &Delta)) {
	      
	      BrettSf(pbr, &sf);
#if 0
	      KoorAus(ZugDat[j].Zug);
	      printf(" j=%d be=%d li=%d %d %d -> %d\n",
		     j,
		     be, limit,
		     SfMoeglZuege(&sf, GEGNER(Partei), moves),
		     ZugDat[j].Wert,
		     ZugDat[j].Wert - my_round(((3.0 * WERTFAKTOR)/DISCFAKTOR) * SfMoeglZuege(&sf, GEGNER(Partei), moves)));

#endif

	      ZugDat[j].Wert -=
		my_round(((1.75 * WERTFAKTOR)/DISCFAKTOR) *
			    SfMoeglZuege(&sf, GEGNER(Partei), moves));
	      
	      UNDO_MOVE(pbr, &Delta);

	    } else Error("no move?");
	  }

#if 0
	  FOR (j, num) { KoorAus(ZugDat[j].Zug); printf(" "); }
	  puts("");
#endif
	  
	  qsort((char*)ZugDat, (size_t) i, sizeof(ZUGDAT), compZUGDAT);

#if 0
	  FOR (j, num) { KoorAus(ZugDat[j].Zug); printf(" "); }
	  puts("");
	  KoorAus(ZugDat[0].Zug); puts("");
#endif

	  // printf("-> %d\n", old_best != ZugDat[0].Zug);
	  
	  FOR (i, num) { AktZug = ZugDat[i].Zug; TRY1; }
	  goto AllesBesucht;
	}
      }
    }
  }
  
#endif
  


/* Hashzug zuerst probieren */

#if FIRST_LEVELS_HASH

    if (phs) {
      AktZug = phs->BesterZug;
      if (HashEntry::locks_equal(phs->Hash2, pbr->Hash2) && ZUG(AktZug)) { 
	TRY1; HashZug = AktZug;
      }
    }

#endif


    AktZug = ph->get_best_move(); 

#if TEST
  if (AktZug < -2 || AktZug >= 89) Error("hashinhalt");
#endif
   

    if (HashEntry::locks_equal(ph->get_lock(), pbr->Hash2) && ZUG(AktZug)) { 
      TRY1; HashZug = AktZug; 
    }

#endif


#if CORNER_FIRST

    AktZug = A1; TRY1;
    AktZug = H8; TRY1;
    AktZug = A8; TRY1;
    AktZug = H1; TRY1;

#endif



    if (pbr->SteinAnz >= OPTI_ANZ) {	/* !!! 56 */

#if KILLER
  
      /* Rest gemäß Killertabelle oder DefaultKill */

      if (ZUG(LetzterZug)) {	

        if (Partei == BLACK) Kill = pzio->killer.KillWHITE; 
        else 		     Kill = pzio->killer.KillBLACK;
  
        pl = &Kill->Liste[LetzterZug][0];

      } else {

#endif

        pl = &pzio->killer.DefaultKill[0];
   
#if KILLER

      }

#endif


#if TEST
{
  int i, j;

  if (64-BrettAnzBW(pbr) > pzio->killer.FreiAnz) Error("killeranz");

  for (i=0; i <= pbr->LastIndex; i++) {
    FOR (j, pzio->killer.FreiAnz) if (pl[j] == pbr->umgl[i].UPos) break;
    if (j >= pzio->killer.FreiAnz) Error("nicht gefunden in plnull");
  }
}
#endif

      for (i=pzio->killer.FreiAnz; i > 0; i--) {

        AktZug = *pl++;

#if CORNER_FIRST
        if (!Ecke[AktZug])
#endif
          TRY1
      }

    } else { 		/* sort moves */

      ZUGDAT ZugDat[65];
      int Anz;

      if (VorTiefen[pbr->SteinAnz] == 1) {

	/* depth 1 sort */

	DELTA  Delta1;
	int    i1, imax=0, max = -(WERTMAX+1), val; 
	SFPOS  Zug1;
	SFPOS  *pl1 = &pzio->killer.DefaultKill[0];
	Square *pu = pbr->umg;  
	
	
	Anz = 0;
	
	for (i1=pzio->killer.FreiAnz; i1 > 0; i1--) {
	  
	  Zug1 = *pl1++;
	  
	  if (Zug1 != HashZug && pu[Zug1] && MAKE_MOVE(pbr, Partei, Zug1, &Delta1)) {
	    
	    val = - pzio->BewMitte(pbr, GEGNER(Partei));
	    if (val > max) { max = val; imax = Anz; } 
	    
	    UNDO_MOVE(pbr, &Delta1);
	    
	    pzio->cio.BewAnz++;
	    ZugDat[Anz].Wert = val;
	    ZugDat[Anz++].Zug = Zug1;
	    
	  }
	}
	
	if (Anz > 1 && imax != 0) {
	  
	  ZUGDAT zd;
	  
	  zd = ZugDat[0]; ZugDat[0] = ZugDat[imax]; ZugDat[imax] = zd;
	}
	
      } else {

	//printf(".%d-%d-%d", pbr->SteinAnz, VorTiefen[pbr->SteinAnz], HashZug != ZUG_UNBEKANNT); fflush(stdout);
        Anz = ZuegeSortieren(pzio, Partei, VorTiefen[pbr->SteinAnz], HashZug, ZugDat); 

      }
 
      FOR (i, Anz) {

        AktZug = ZugDat[i].Zug;

#if CORNER_FIRST
        if (!Ecke[AktZug])
#endif
          TRY1
      }
    }
  }

AllesBesucht:

  if (ZugAnz == 0) {				/* Partei kann nicht ziehen */

    if (LetzterZug == ZUG_PASSEN) {		/* keiner kann */

BEWTEST

      ANTI_RET(BEWDIFF1);

    } else {					/* passen */

      return -EndAlphaBeta1(pzio, GEGNER(Partei), -be, -al, ZUG_PASSEN);
    }
  }


Ende:

#if TEST
if (Tiefe == 1 && (pzio->PathLen > MAX_DEPTH || pzio->PathLen <= 0)) Error("VARPOS2");
#endif

if (pbr->SteinAnz < OHNE_ANZ) {

  /* Hash- und Killer-Eintrag anpassen */

#if TEST
  if (!ZUG(BestZug)) Error("BestZug kein Zug");
#endif


#if HASH

  pzio->hash.set(*ph, pbr->Hash2, HashEntry::END_HEIGHT, al, be, BestZug, lo_value);

#if DIRTY

    // new: allow hash values, use if stamps are equal
#if CANCEL_DEEP_VALUE      
    if (pzio->Selective && pbr->SteinAnz <= NUMEND)
      ph->get_value_type() = WERT_UNBEKANNT;
#endif
    
#endif

#if FIRST_LEVELS_HASH

  if (phs) {

    pzio->hash.set(*phs, pbr->Hash2, END_DEPTH, al, be, BestZug, lo_value);

      // new: allow hash values, use if stamps are equal
#if CANCEL_DEEP_VALUE      
      if (pzio->Selective && pbr->SteinAnz <= NUMEND)
      	phs->get_value_type() = WERT_UNBEKANNT;
#endif
      
  }

#endif



#endif


#if KILLER
  if (ZUG(LetzterZug)) {

#if TEST
    if (!ZUG(BestZug)) { printf("e:"); KoorAus(BestZug); }
#endif

    KillerUpdate(&pzio->killer, GEGNER(Partei), LetzterZug, BestZug);

  }
#endif

}



#if DIRTY && DIRTY_STAT


if (lo_value && pbr->SteinAnz <= 50) {

printf("\n### %.5f %d (%d)\n", WERT_TO_REAL(AlphaBeta1(pzio, 4, Partei, WERTMIN, WERTMAX, ZUG_UNBEKANNT)), sgn(lo_value), pbr->SteinAnz);

}

#endif


#if 0
if (0 && pbr->SteinAnz >= 58) {
  int fast_val = FastEnd(pzio, Partei, al, be);

  bool old_la = lo_value <= al;
  bool new_la = fast_val <= al;
  bool old_gb = lo_value >= be;
  bool new_gb = fast_val >= be;

  if (old_la != new_la || old_gb != new_gb ||
      (!old_la && !old_gb && lo_value != fast_val)) {

    SPFELD sf;
    BrettSf(pbr, &sf);
    SfAus(&sf, 0, 0); printf("%d TM\n", Partei);

    printf("#=%d a=%d b=%d  new=%d old=%d\n", pbr->SteinAnz, al, be, fast_val, lo_value);
  }
}
#endif


  return lo_value;
}



#if 0


#if PROBING

      const int PROBE_N = 6;
      int probe_deltas[PROBE_N] = { 0 , +1, -4 , +9, -16, +25 };
      int i, min_depth=100000;
      uint4 ha1, min_ha1=0;

      FOR (i, PROBE_N) {

	ha1 = (ha+probe_deltas[i]*2) & hd0.HashMask;
	phs = &hd0.HashTab[ha1];

	if (HashEntry::locks_equal(phs->Hash2, pbr->Hash2)) {
	  min_ha1 = ha1;
	  break;
	}

	if (phs->Tiefe < min_depth) {
	  min_depth = phs->Tiefe;
	  min_ha1 = ha1;
	}

      }

      ha = min_ha1;

#endif

#endif
