// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/* Alpha-Beta mit Nullfenstersuche / 10.92 */
/* Fehler: Endspielwerte in Hashtabelle 
   werden als Mittelspielwerte interpretiert -> 
   Endspielwerte werden nicht genommen 
*/

#include "main.h"
#include "mid.h"
#include "move.h"
#include "eval.h"
#include "hash.h"
#include "killer.h"
#include "fpatt.h"
#include "order.h"
#include "pcstat.h"
#include "util.h"
#include "end.h"

#include "crt.h"
#include "crtg.h"


#define PC_OLD              false   // old probcut
#define PC_EVEN             false   // pc only at even heights
#define PC_GEN_BOARDS       false   // save PC example positions
#define PC_PHASE_2_DISC_NUM 30

#define GEN_BOARDS          false   // generate example positions

#define FIRST_LEVELS_HASH  true
#define PROBING            false

#define FAST_TEST          true
#define EDGE_QS            false  // doesn't work

#define PCSTAT_FILE        true

#define QSEARCH            QSearch  // was QSearch


#define UTIL_CUT           false
#define OLD_PAIRS          false
#define NODE_STAT          false
#define SHORTCUT           true
#define SC_FACTOR          0.9

#if 1

#define L_PLY_MAKE_MOVE LastPlyLazyMove
#define L_PLY_UNDO_MOVE LastPlyLazyUndo

#else

#define L_PLY_MAKE_MOVE LazyMove
#define L_PLY_UNDO_MOVE LazyUndo

#endif


static PCStat pcstat;

inline static bool replace_ok(HashTab *phd, HashEntry *ph, BRETT *pbr, int h)
{
  if (!ph) return false;

  if (HashEntry::locks_equal(ph->get_lock(), pbr->Hash2)) {
    return (int)ph->get_height() <= h;
  }

  return true;
  
  if (phd->get_stamp() != ph->get_stamp()) return true;

  return (int)ph->get_height() <= h;
}


inline static bool hash_value_ok(HashEntry *ph, BRETT *pbr, UMGEB *pumg, int h)
{
  return 
    HashEntry::locks_equal(ph->get_lock(), pbr->Hash2) &&
    (int)ph->get_height() >= h &&
    (int)ph->get_height() != HashEntry::END_HEIGHT &&
    ZUG(ph->get_best_move()) &&
    pumg[ph->get_best_move()] >= UMG;
}


WERT QSearch(
  ZUGIO	 *pzio,
  PARTEI Partei,
  WERT	 al,
  WERT	 be
);

WERT EndCheck(  
  ZUGIO	 *pzio,
  PARTEI Partei,
  WERT	 al,
  WERT	 be
);

#if 0
#define EDGE_TEST  46

int edgetest=0, edgenum[6561];
#endif


#define BOARD_SAVE	false


#define DIRTY		true
#define DIRTY_STAT	false


/* ProbCut: 4->6, 4->8 */

#define CHECK_D1   4
#define CHECK_H1   6
#define CHECK_T1   App_4_6

#define CHECK_D2   4
#define CHECK_H2   8
#define CHECK_T2   App_4_8

/* 
  2.6  99.5% 
  2.3  99% 
  2.1  98%  
  1.5  93%  
  1.2  88%  
  1.0  84%  
  0.7  76%
*/ 

#define OLD		false

#define QUIESCENCE	true	/* examine own corner moves (or null move) */
#define QALL		false	/* examine all moves, if player has no     */
				/* corner moves but opponent		   */


#define TEST		false
#define ALBETEST	false

#define KILLER_HOEHE	5	/* < -> Killertabellen benutzen, sonst	*/
				/* Tiefe 1 oder 2 Suche			*/

#define TIEFE2_HOEHE	7	/* < -> Tiefe 1 Vorsortierung, sonst 2  */

#define ECKEN_ZUERST	true	/* nach Hashzug alle Ecken zuerst? */


#if 0				/* alle Optimierungen aus zum Testen */

#define SCHNITT		if (lo_value >= be) goto Ende;	/* beta-Schnitt */
#define NULLFENSTER  
#define WIEDERHOLUNG 
#define WERTEHASH	false

#else


#define SCHNITT		if (lo_value >= be) goto Ende;	/* beta-Schnitt */
#define NULLFENSTER     hi_value = max + 1
#define WIEDERHOLUNG    \
      if (value > max && value < be && ZugAnz > 1 /* && nRealeHoehe > 0 */)\
\
/* Wiederholungssuche */\
\
        value = -AlphaBeta1(pzio, nRealeHoehe, nPartei, -be, -value, AktZug, in_cut_search);
#define WERTEHASH	true

#endif


#ifdef KEIN_HASH
#undef WERTEHASH
#define WERTEHASH false
#endif

#if DOUBLE_INDEX
#define PU(i) (*(uint4*)((short*) pu + (i)))  /* pu + (2*index)*2 */
#else
#define PU(i) pu[i]
#endif


#define WERTAUS(s, w) printf("%s w=%d\n", s, w)


#if UTIL_CUT

// no nega-scout if util-cut is used!

#undef NULLFENSTER
#undef WIEDERHOLUNG
#define NULLFENSTER
#define WIEDERHOLUNG

#endif


int node=0;


int SortDepth[25] = {

/* 0 */   1,1,1,1,1,1,1,
/* 7 */   2,2,3,3,4,4,5,
/*14 */   5,6,6,6,7,7,6,
/*21 */   6,6,6,6

};





#if OLD_PAIRS

// old

#define APPROX_NUM   14  // number of rows
#define CHK_MAX       2  // number of columns
#define MIN_PC_HEIGHT 3

int ApproxDepths[APPROX_NUM][CHK_MAX] = {

// conservative

/*  0 */ { -1, -1 },
/*  1 */ { -1, -1 },
/*  2 */ { -1, -1 },
/*  3 */ { 1, -1 },
/*  4 */ { 2, -1 },
/*  5 */ { 1, -1 },
/*  6 */ { 2, -1 },
/*  7 */ { 3, -1 },
/*  8 */ { 4, -1 },
/*  9 */ { 3, 5  },
/* 10 */ { 4, 6  },
/* 11 */ { 3, 5  },
/* 12 */ { 4, -1 },
/* 13 */ { 5, -1 }

};


#else 

#if 0

// experiment

#define APPROX_NUM   20  // number of rows
#define CHK_MAX       2  // number of columns
#define MIN_PC_HEIGHT 3

int ApproxDepths[APPROX_NUM][CHK_MAX] = {

/*  0 */ { -1, -1 },
/*  1 */ { -1, -1 },
/*  2 */ { -1, -1 },
/*  3 */ { 1, -1 },
/*  4 */ { 2, -1 },
/*  5 */ { 1, -1 },
/*  6 */ { 2, -1 },
/*  7 */ { 1,  3 },
/*  8 */ { 2,  4 },
/*  9 */ { 3,  5 },
/* 10 */ { 2,  4 },
/* 11 */ { 3,  5 },
		
/* 12 */ { 2,  4 },
/* 13 */ { 3,  5 },

/* 14 */ { 2, 4 }, 
/* 15 */ { 3, 5 },

/* 16 */ { 4, 6 },
/* 17 */ { 5, 7 },
	 
/* 18 */ { 4, 6 },
/* 19 */ { 5, 7 }

};


#else

// current

#define APPROX_NUM   18  // number of rows
#define CHK_MAX       2  // number of columns
#define MIN_PC_HEIGHT 3

int ApproxDepths[APPROX_NUM][CHK_MAX] = {

/*  0 */ { -1, -1 },
/*  1 */ { -1, -1 },
/*  2 */ { -1, -1 },
/*  3 */ { 1, -1 },
/*  4 */ { 2, -1 },
/*  5 */ { 1, -1 },
/*  6 */ { 2, -1 },
/*  7 */ { 1,  3 },
/*  8 */ { 2,  4 },
/*  9 */ { 3,  5 },
/* 10 */ { 2,  4 },
/* 11 */ { 3,  5 },

/* 12 */ { 2,  4 },
/* 13 */ { 3,  5 },

/* 14 */ { 2,  4 }, 
/* 15 */ { 3,  5 },
/* 16 */ { 4,  6 },
/* 17 */ { 5,  7 }

};

#endif

#endif



inline static bool InVorgabe(ZUGIO *pzio, SFPOS Zug)
{
  int i;

  FOR (i, pzio->VorgabeAnz) if (pzio->Zuege[i] == Zug) break;

/*  KoorAus(Zug); printf(" vor %d, ",  i < pzio->VorgabeAnz); */

  return i < pzio->VorgabeAnz;
}



/* Pfad in Hashtabelle finden */

void FindPath(
  ZUGIO *pzio, 
  PARTEI Partei, 
  SFPOS Zug, 
  int al, int be, 
  int Wert,
  ValType Type
)
{
  int i;
  SPFELD sf;
  BRETT  Brett;
  DELTA  Delta;
  HashEntry *ph;

  //  KoorAus(Zug);
  //  cout << " findpath w=" << Wert << " " << al << " " << be << endl;

  BrettSf(&pzio->Brett, &sf);
  SfBrett(&sf, &Brett);

  pzio->PathLen = 0;
  pzio->PathValue = Wert;
  pzio->PathAl = al;
  pzio->PathBe = be;
  pzio->PathType = Type;

  FOR (i, 16) {

    if (!ZUG(Zug) || !MAKE_MOVE(&Brett, Partei, Zug, &Delta)) break; 

    pzio->Path[pzio->PathLen++] = Zug;
 
    Partei = GEGNER(Partei);

    ph = &pzio->hash.get(Brett.Hash1, Partei);

    if (!HashEntry::locks_equal(ph->get_lock(), Brett.Hash2)) { i++; break; }
    Zug = ph->get_best_move();
  }

  if (i == 0) {
SfAus(&sf, 0, 0); KoorAus(Zug); printf("%d\n", Partei);
Error("FindPath: 1. Zug geht nicht");
}

/*
printf("\n");
FOR (i, pzio->PathLen) { KoorAus(pzio->Path[i]); printf(" "); }
printf("\n");
*/
}








#if LAZY_UPDATE

/* no surrounding list -> use default killer list */

inline int Vorsort(ZUGIO *pzio, PARTEI Partei, int Tiefe, ZUGMENGE zm, ZUGDAT *ZugDat)
{
  DELTA  Delta1;
  int Anz=0, i1; 
  SFPOS Zug1;
  SFPOS *pl1 = &pzio->killer.DefaultKill[0];
  Square *pu = pzio->Brett.umg;  


  for (i1=pzio->killer.FreiAnz; i1 > 0; i1--) {

    Zug1 = *pl1++;

    if (pu[Zug1] && !ZM_GESETZT(Zug1) && 
        MAKE_MOVE(&pzio->Brett, Partei, Zug1, &Delta1)) {

      pzio->cio.BewAnz++;

      if (Tiefe < 2)

/* Tiefe 1 Vorsortierung */

        ZugDat[Anz].Wert = - pzio->BewMitte(&pzio->Brett, GEGNER(Partei));

      else {

        SFPOS Zug2; 
        SFPOS *pl2 = &pzio->killer.DefaultKill[0];
        DELTA Delta2;
        int   i2, w, max=WERTMIN;

/* Tiefe 2 Vorsortierung */

        for (i2=pzio->killer.FreiAnz; i2 > 0; i2--) {

          Zug2 = *pl2++;

	  if (pu[Zug2] && 
              MAKE_MOVE(&pzio->Brett, GEGNER(Partei), Zug2, &Delta2)) {

	    pzio->cio.BewAnz++;
	    w = pzio->BewMitte(&pzio->Brett, GEGNER(Partei));

	    if (w > max) max = w;
	    UNDO_MOVE(&pzio->Brett, &Delta2);
  	  }
        }

        ZugDat[Anz].Wert = -max;	    
      }

      UNDO_MOVE(&pzio->Brett, &Delta1);

      ZugDat[Anz++].Zug = Zug1;

    }
  }

  qsort((char*)ZugDat, (size_t) Anz, sizeof(ZUGDAT), compZUGDAT); 

  return Anz;
}


#else


inline int Vorsort(ZUGIO *pzio, PARTEI Partei, int Tiefe, ZUGMENGE zm, ZUGDAT *ZugDat)
{
  DELTA  Delta1;
  int Anz=0, i1; 
  SFPOS Zug1; 
  UMGEB *pumgl1=pzio->Brett.umgl;

  for (i1=pzio->Brett.LastIndex+1; i1 > 0; i1--) { 

    Zug1 = *pumgl1; pumgl1++;

    if (!ZM_GESETZT(Zug1) && MAKE_MOVE(&pzio->Brett, Partei, Zug1, &Delta1)) {

      pzio->cio.BewAnz++;

      if (Tiefe < 2)

/* Tiefe 1 Vorsortierung */

        ZugDat[Anz].Wert = - pzio->BewMitte(&pzio->Brett, GEGNER(Partei));

      else {

        SFPOS Zug2; 
        UMGEB *pumgl2=pzio->Brett.umgl;
        DELTA Delta2;
        int   i2, w, max=WERTMIN;

/* Tiefe 2 Vorsortierung */

        for (i2=pzio->Brett.LastIndex+1; i2 > 0; i2--) { 

          Zug2 = *pumgl2; pumgl2++;

	  if (MAKE_MOVE(&pzio->Brett, GEGNER(Partei), Zug2, &Delta2)) {

	    pzio->cio.BewAnz++;
	    w = pzio->BewMitte(&pzio->Brett, GEGNER(Partei));

	    if (w > max) max = w;
	    UNDO_MOVE(&pzio->Brett, &Delta2);
  	  }
        }

        ZugDat[Anz].Wert = -max;	    
      }

      UNDO_MOVE(&pzio->Brett, &Delta1);

      ZugDat[Anz++].Zug = Zug1;

    }
  }

  qsort((char*)ZugDat, (size_t) Anz, sizeof(ZUGDAT), compZUGDAT); 

  return Anz;
}

#endif





#define TRY \
if (pumg[AktZug] && !ZM_GESETZT(AktZug) \
    && (!pzio->ZugVorgabe || InVorgabe(pzio, AktZug)) )  {\
    if (MAKE_MOVE(pbr, Partei, AktZug, &Delta)) {\
      pzio->cio.BewAnz++;\
      ZugAnz++; ZM_SETZEN(AktZug);\
      pzio->Move1 = AktZug; pzio->MoveNumber = ZugAnz;\
\
      value = -AlphaBeta1(pzio, nRealeHoehe, nPartei, -hi_value, -max, AktZug, false);\
      /*KoorAus(AktZug); printf("(%d %d) (%d %d) -> %d\n", al, be, max, hi_value, value);*/\
      /*printf(":::%d (%d,%d) ### %d\n", ZugAnz, max, hi_value, value);*/\
      if (value > lo_value) {\
/* Pfad vor Wiederholungssuche ¸bernehmen! */\
	BestZug = AktZug;\
        UNDO_MOVE(pbr, &Delta);\
	FindPath(pzio, Partei, AktZug, al, be, value, MIDGAME);\
	MAKE_MOVE(pbr, Partei, AktZug, &Delta);\
      }\
\
      WIEDERHOLUNG;\
      UNDO_MOVE(pbr, &Delta);\
\
      if (value > lo_value) {\
\
       /*printf("hoehe=%d wert=%d alt=%d\n", RealeHoehe, value, lo_value);*/\
	FindPath(pzio, Partei, AktZug, al, be, value, MIDGAME);\
\
	BestZug = AktZug;\
        lo_value = value;		/* ¸bernehmen, wenn besser */\
	SCHNITT;\
        if (lo_value > max) max = lo_value;\
        NULLFENSTER;\
\
      }\
    }\
  }




/* NegaScout-Version */

WERT AlphaBeta(
  ZUGIO	 *pzio,		/* Zeiger auf globale Variablen	*/
  int	 RealeHoehe,	/* Abstand zum Blatt		*/
  PARTEI Partei,	/* Spieler am Zug		*/
  WERT	 al,		/* alpha-beta Fenster		*/ 
  WERT	 be,
  int    LetzterZug
)
{
  int		i, ZugAnz, nRealeHoehe, HashMove;
  PARTEI	nPartei;
  SFPOS		BestZug, AktZug, KZug;
  WERT		value, lo_value, hi_value, max;
  KILLTAB	*Kill;
  SFPOS		*pl;
  BRETT		*pbr;
  DELTA		Delta;
  HashEntry	*ph;
  Square	*pumg;
  static int 	zaehler = 0;
  ZUGMENGE	zm;
  bool          in_cut_search = false;

#if 0
int curnode;
SPFELD sf;
  curnode = node++;

  printf("%d:\n", curnode);
  { SPFELD sf;
  BrettSf(&pzio->Brett, &sf);
  SfAus(&sf, 0, 0);
  printf("p=%d Hoehe=%d, al=%d, be=%d\n", Partei, RealeHoehe, al, be);
  }  
#endif

#if 0
  int bestvalknown=false;
#endif

#if 0
  if (pzio->Quiescence) printf("### qui\n");
  if (pzio->Selective)  printf("### sel %f\n", pzio->Percentile);
#endif

#if ALBETEST
  printf("Tiefe=%d, al=%d, be=%d\n", Tiefe, al, be);
#endif

  if (--zaehler < 0) { 

    pzio->Check(&pzio->cio, pzio, false, false); 
    zaehler = CHECK_COUNT; 
  }

  pbr   = &pzio->Brett;
  pumg  = pbr->umg;

  pzio->hash0.next_stamp();
  pzio->hash.next_stamp();
  
#if ALBETEST
  { SPFELD sf;
  BrettSf(pbr, &sf);
  SfAus(&sf, 0, 0);
  printf("%d %d\n", Tiefe, Partei);
  }
#endif



  /* Spielfeld voll */

  if (pbr->SteinAnz == 64) return BewDiff(pbr, Partei);


#if HASH

  /* war Stellung schon da? */

  ph = &pzio->hash.get(pbr->Hash1, Partei);


#if WERTEHASH

  if (!pzio->ZugVorgabe &&
      hash_value_ok(ph, pbr, pumg, RealeHoehe) &&
      MAKE_MOVE(pbr, Partei, ph->get_best_move(), &Delta)) {

    UNDO_MOVE(pbr, &Delta);

    if (ph->get_value_type() == HashEntry::VALUE_EXACT  ||
       (ph->get_value_type() == HashEntry::VALUE_LE_MINIMAX && ph->get_value() >= be) ||
       (ph->get_value_type() == HashEntry::VALUE_GE_MINIMAX && ph->get_value() <= al)) {

      /* Stellung nicht neu berechnen! */


#if TEST

if (ph->get_height() == 0) printf("%d\n", RealeHoehe);

#endif

      FindPath(pzio, Partei, ph->get_best_move(), al, be, ph->get_value(), MIDGAME);\

      pzio->fertig = false;	/* noch nicht alles durchgerechnet     */
      return ph->get_value();

    } else  {

      /* printf("!"); fflush(stdout); */

    }
  }

#endif    

#endif


  if (RealeHoehe <= 0) return QSEARCH(pzio, Partei, al, be);


  ZM_LOESCHEN;

  ZugAnz = 0;

  BestZug  = ZUG_UNBEKANNT; 
  lo_value = WERTMIN; 
  hi_value = be;
  max      = al;

  nPartei = GEGNER(Partei);
  nRealeHoehe = RealeHoehe - 1;


  /* compute KZug: last move or occupied square */


  if (ZUG(LetzterZug)) 

    KZug = LetzterZug; 

  else {

    /* look for occupied square -> killer move */

   FOR (KZug, 100) 
    if (pbr->p[KZug] == BLACK || pbr->p[KZug] == WHITE) break;

   if (KZug >= 100) { printf("no disc???\a\a\a\n"); KZug = D4; }

  }



#if HASH

  AktZug = ph->get_best_move();

#if TEST
  if (AktZug < -2 || AktZug >= 89) {
    printf("%x %d\n", ha, AktZug); Error("hashinhalt1");
  }
#endif
  
  /*printf("HashZug "); KoorAus(AktZug); printf("\n");*/


  if (ZUG(AktZug) && HashEntry::locks_equal(ph->get_lock(), pbr->Hash2)) { 

    HashMove = AktZug;
    TRY 

  } else HashMove = -1;

#endif



#if KILLER

  /* use killerlist for remaining moves */


  if (Partei == BLACK) Kill = pzio->killer.KillWHITE; 
  else 		       Kill = pzio->killer.KillBLACK;

  pl = &Kill->Liste[KZug][0];

#else

#error KILLER not true!

#endif



  /* only 2 killermoves */

  for (i=0; i < pzio->killer.FreiAnz && i < 2; i++) {

    AktZug = *pl++;
    TRY
  }


  /* sort moves */

  {
    ZUGDAT ZugDat[65];
    int    Anz;

    Anz = ZuegeSortieren(
      pzio, Partei, SortDepth[RealeHoehe >= 0 ? RealeHoehe : 0], HashMove, ZugDat
          );

    FOR (i, Anz) {

      AktZug = ZugDat[i].Zug;

      TRY
    }
  }

  if (ZugAnz == 0) {				/* Partei kann nicht ziehen */

    if (LetzterZug == ZUG_PASSEN) {		/* keiner kann */

      return BewDiff(pbr, Partei);

    } else {					/* eins tiefer */

      return -AlphaBeta1(pzio, RealeHoehe, GEGNER(Partei), -be, -al, ZUG_PASSEN, false);
    }
  }




Ende:


/* Hash- und Killer-Eintrag anpassen */

#if TEST
  if (!ZUG(BestZug)) Error("BestZug auﬂerhalb");
#endif


#if HASH

if (RealeHoehe < 0) Error("Height < 0");

  pzio->hash.set(*ph, pbr->Hash2, RealeHoehe, al, be, BestZug, lo_value);

#endif


#if KILLER

  if (ZUG(BestZug)) {

    KillerUpdate(&pzio->killer, GEGNER(Partei), KZug, BestZug);    
  }

#endif

  return lo_value;
}




#if BOARD_SAVE
extern int savedepth;
#endif



#define BESSER \
      if (value > lo_value) {\
	if (value > al || ZugAnz == 1) BestZug = AktZug;\
        lo_value = value;	/* ¸bernehmen, wenn besser */\
	SCHNITT;\
        if (lo_value > max) max = lo_value;\
        NULLFENSTER;\
      }

#define TRY1 \
if (pumg[AktZug] && !ZM_GESETZT(AktZug)) {\
    if (MAKE_MOVE(pbr, Partei, AktZug, &Delta)) {\
      pzio->cio.BewAnz++;\
      ZugAnz++; ZM_SETZEN(AktZug);\
\
      value = -AlphaBeta1(pzio, nRealeHoehe, nPartei, -hi_value, -max, AktZug, in_cut_search);\
      WIEDERHOLUNG;\
      UNDO_MOVE(pbr, &Delta);\
      BESSER;\
\
    }\
  }


/* NegaScout-Version, tiefere Knoten */

WERT AlphaBeta1(
  ZUGIO	 *pzio,		/* Zeiger auf globale Variablen	*/
  int	 RealeHoehe,	/* Abstand zum Blatt		*/
  PARTEI Partei,	/* Spieler am Zug		*/
  WERT	 al,		/* alpha-beta Fenster		*/ 
  WERT	 be,
  int    LetzterZug,
  bool   in_cut_search
)
{
  int		i, ZugAnz, nRealeHoehe;
  PARTEI	nPartei;
  SFPOS		BestZug=ZUG_UNBEKANNT, AktZug;
  WERT		value, lo_value, hi_value, max;
  KILLTAB	*Kill;
  SFPOS		*pl;
  BRETT		*pbr;
  DELTA		Delta;
  HashEntry     *ph, *phs;
  Square	*pumg;
  static int 	zaehler = 0;
  ZUGMENGE	zm;

#if 0

  static int depth = -1;

  if (pzio->Brett.SteinAnz - pzio->DiscNum0 > depth) {
    depth = pzio->Brett.SteinAnz - pzio->DiscNum0;
    printf("\n\ndepth=%d\n\n", depth);
  }

#endif


#if 0
  int curnode;
  SPFELD sf;

  curnode = node++;

  printf("%d:\n", curnode);
  { SPFELD sf;
  BrettSf(&pzio->Brett, &sf);
  SfAus(&sf, 0, 0);
  printf("p=%d Hoehe=%d, al=%d, be=%d\n", Partei, RealeHoehe, al, be);
  }  
#endif

#if 0  
  static int uanz=0, ganz=0;
#endif

#if NODE_STAT
  int prev_count = pzio->cio.BewAnz;
  int mpc_nodes = 0;
#endif


  if (--zaehler < 0) { 
    pzio->Check(&pzio->cio, pzio, false, false); zaehler = CHECK_COUNT; 
  }
   
  pbr  = &pzio->Brett;
  pumg = pbr->umg;


#if ALBETEST
  { SPFELD sf;
  BrettSf(pbr, &sf);
  SfAus(&sf, 0, 0);
  printf("Hoehe=%d, al=%d, be=%d\n", RealeHoehe, al, be);
  }
#endif


#if GEN_BOARDS
  { 
    static FILE *fp;
    static bool init = false;
    const  int  MODUL = 100000;
    static int  threshold = int(0.015 * MODUL);  // generates about 1 position per 3-ply search
    SPFELD sf;
    
    if (!init) {
      init = true;
      char s[10000];
      fp = fopen("examples.sfk", "w");
      if (!fp) Error("can't open example file");
    }
    
    // generate example positions
    
    if ((random() % MODUL) < threshold) {
      
      BrettSf(pbr, &sf);
      sf.Marke = MA_WEISS_NICHT;
      if (Partei == WHITE) SfInvert(&sf);
      fSfWrite(fp, &sf, 1); 
      fflush(fp);
    }
  }
#endif

 
#if BOARD_SAVE

  if (savedepth && pbr->SteinAnz - pzio->DiscNum0 <= savedepth) {

    char s[100];
    FILE *fp;
    SPFELD sf;

    sprintf(s, "beisp.%d", pbr->SteinAnz - pzio->DiscNum0);

    if (!(fp=fopen(s, "a"))) Error("append");

    BrettSf(pbr, &sf);
    if (Partei == WHITE) SfInvert(&sf);

    sf.Marke = MA_WEISS_NICHT;

    fSfWrite(fp, &sf, 1);
    fclose(fp);

  }

#endif

  /* Spielfeld voll */

  if (pbr->SteinAnz == 64) return BewDiff(pbr, Partei);


#if FAST_TEST

  // no quiescence search - no AB call

  if (RealeHoehe <= 1) {

    pzio->fertig = false;

    if (RealeHoehe <= 0) {

      return QSEARCH(pzio, Partei, al, be);

    }

#if HASH

    // position seen before => use value and move

    ph = &pzio->hash.get(pbr->Hash1, Partei);


#if WERTEHASH

    if (hash_value_ok(ph, pbr, pumg, RealeHoehe)) {

      if (ph->get_value_type()  == HashEntry::VALUE_EXACT  ||
  (ph->get_value_type() == HashEntry::VALUE_LE_MINIMAX && ph->get_value() >= be) ||
  (ph->get_value_type() == HashEntry::VALUE_GE_MINIMAX && ph->get_value() <= al)) {

	// use value

	//	cout << "hash-value=" << ph->get_value << endl;

	return ph->get_value();

      }
    }

#endif    

    ZugAnz = 0;
    lo_value = WERTMIN;
    max = al;
    nPartei = GEGNER(Partei);

    int HashMove = ZUG_UNBEKANNT;

    if (ZUG(ph->get_best_move()) && pumg[ph->get_best_move()] >= UMG) {

      AktZug = ph->get_best_move();

      // L_PLY_MAKE/UNDO can be used since QSearch doesn't use hashvalues!

      if (L_PLY_MAKE_MOVE(pbr, Partei, AktZug, &Delta)) {

	//KoorAus(AktZug); printf(" ");

        HashMove = AktZug;
	pzio->cio.BewAnz++;
	ZugAnz++;

	value = - QSEARCH(pzio, nPartei, -be, -max);
	L_PLY_UNDO_MOVE(pbr, &Delta);\

	if (value > lo_value) {
	  BestZug = AktZug;
	  lo_value = value;	// better!

	  if (lo_value >= be) goto HashUpdateReturn;

	  if (lo_value > max) max = lo_value;
	}
      }
    }

#endif


#if KILLER
  
    // sort moves according to killer-list

    if (ZUG(LetzterZug)) {

      if (Partei == BLACK) Kill = pzio->killer.KillWHITE; 
      else 		   Kill = pzio->killer.KillBLACK;

      pl = &Kill->Liste[LetzterZug][0];

    } else {

#endif

      pl = &pzio->killer.DefaultKill[0];

#if KILLER

    }

#endif

    // RealeHoehe == 1

    for (i=pzio->killer.FreiAnz; i > 0; i--) {

      AktZug = *pl++;

      if (AktZug != HashMove && pumg[AktZug]) {
	if (L_PLY_MAKE_MOVE(pbr, Partei, AktZug, &Delta)) {

	  //KoorAus(AktZug); printf(" ");

          pzio->cio.BewAnz++;
	  ZugAnz++; 
	  value = - QSEARCH(pzio, nPartei, -be, -max);
	  L_PLY_UNDO_MOVE(pbr, &Delta);\
	  if (value > lo_value) {
	    BestZug = AktZug;
	    lo_value = value;	// better!
	    if (lo_value >= be) goto HashUpdateReturn;
	    if (lo_value > max) max = lo_value;
	  }
	}
      }
    }

    if (!ZugAnz) {

      if (LetzterZug == ZUG_PASSEN) {		// no one has a move

	return BewDiff(pbr, Partei);

      } else {					// one ply deeper

	return -AlphaBeta1(pzio, RealeHoehe, GEGNER(Partei), -be, -al, ZUG_PASSEN, in_cut_search);

      }
    }

  HashUpdateReturn:

#if HASH

    if ((!HashEntry::locks_equal(ph->get_lock(), pbr->Hash2) ||
	 (int)ph->get_height() <= RealeHoehe)) {

      // if (pbr->SteinAnz - pzio->DiscNum0 > pzio->hash.HashMaxDepth) Error("xxx");

      pzio->hash.set(*ph, pbr->Hash2, RealeHoehe, al, be, BestZug, lo_value);

    }

#endif

    // cout << "normal-return " << lo_value << endl;

    return lo_value;    

  }

#endif


#if HASH

  if (pbr->SteinAnz - pzio->DiscNum0 > (int)pzio->hash.get_max_depth()) {

    ph  = 0;
    phs = 0; 

  } else { 

#if FIRST_LEVELS_HASH

    if (pbr->SteinAnz - pzio->DiscNum0 <= (int)pzio->hash0.get_max_depth()) {

      /* position seen before => use value and move */

#if PROBING

      phs = &pzio->hash0.get_probing(pbr->Hash1, pbr->Hash2, Partei);
      
#else

      phs = &pzio->hash0.get(pbr->Hash1, Partei);

#endif


#if WERTEHASH

      if (hash_value_ok(phs, pbr, pumg, RealeHoehe)) {

	if (phs->get_value_type() == HashEntry::VALUE_EXACT  ||
    (phs->get_value_type() == HashEntry::VALUE_LE_MINIMAX && phs->get_value() >= be) ||
    (phs->get_value_type() == HashEntry::VALUE_GE_MINIMAX && phs->get_value() <= al)) {

	  /* Stellung nicht neu berechnen! */

	  pzio->fertig = false;	/* noch nicht alles durchgerechnet     */

	  //cout << "hash3 " << phs->get_value << endl;

	  return phs->get_value();

	} else  {

	  /* printf("!"); fflush(stdout); */
	  
	}
      }

#endif

    } else phs = 0;

#endif
 

    /* position seen before => use value and move */

    ph = &pzio->hash.get(pbr->Hash1, Partei);


#if WERTEHASH

#if 0
    if ((ph->get_lock() & 0x7ffffff) == (pbr->Hash2 & 0x7ffffff)) {	/* !!! */
      ganz++;
      if (ph->get_lock() != pbr->Hash2) uanz++;
      
      if ((ganz & 1023) == 0) printf("\n%d %d (%g)\n", ganz, uanz, ((float)uanz)/ganz);
    }
#endif

#if 0
    if (RealeHoehe == 1) {
      static int num=0, hit=0;

      num++;
      if (HashEntry::locks_equal(ph->get_lock(), pbr->Hash2)) hit++;

      if ((num % 1000) == 0) printf("%d %d\n", num, hit);

    }
#endif 

    if (hash_value_ok(ph, pbr, pumg, RealeHoehe)) { 

      if (ph->get_value_type() == HashEntry::VALUE_EXACT  ||
     (ph->get_value_type() == HashEntry::VALUE_LE_MINIMAX && ph->get_value() >= be) ||
     (ph->get_value_type() == HashEntry::VALUE_GE_MINIMAX && ph->get_value() <= al)) {

	/* Stellung nicht neu berechnen! */


#if TEST
	if (ph->get_height() == 0) printf("%d\n", RealeHoehe);
#endif
	
	pzio->fertig = false;	/* noch nicht alles durchgerechnet     */

	// cout << "hash-value2=" <<  ph->get_value << endl;
	return ph->get_value();

      } else  {

	/* printf("!"); fflush(stdout); */

      }
    }
  }
#endif    

#endif


  if (RealeHoehe <= 0) return QSEARCH(pzio, Partei, al, be);

#if DIRTY

  if (!in_cut_search &&

#if PC_OLD

      (RealeHoehe == CHECK_H1 || RealeHoehe == CHECK_H2)

#else

#if PC_EVEN

      ((RealeHoehe & 1) == 0) &&

#endif


      RealeHoehe >= MIN_PC_HEIGHT && RealeHoehe < APPROX_NUM 

#endif

      && pzio->Selective && (!DIRTY_STAT || LetzterZug != ZUG_UNBEKANNT)

      ) {
    static bool called = false;

#if DIRTY_STAT

    static REAL sume=0, sumee=0, sumn=0;
    static REAL vsume=0, vsumee=0, vsumn=0;

    REAL vp, vpd,
      alp=EXPWERT(WERT_TO_REAL(al)), bep=EXPWERT(WERT_TO_REAL(be));
    REAL value, valued;

    value = AlphaBeta1(pzio, 4, Partei, WERTMIN, WERTMAX, ZUG_UNBEKANNT, true);
    vp = EXPWERT(WERT_TO_REAL(value));


    valued = AlphaBeta1(pzio, 8, Partei, WERTMIN, WERTMAX, ZUG_UNBEKANNT, true);
    vpd = EXPWERT(WERT_TO_REAL(valued));


    sume += vpd - vp;
    sumee += (vpd - vp)*(vpd - vp);
    sumn += 1;

#if 0
    printf("\np:%.2f->%.2f (%f: %.3f %.3f)\n", 
	   vp, vpd, 
	   sumn, sume/sumn, sqrt(sumee/sumn - (sume/sumn)*(sume/sumn)));
#endif

    value = WERT_TO_REAL(value);
    valued = WERT_TO_REAL(valued);
    

    vsume += valued - value;
    vsumee += (valued - value)*(valued - value);
    vsumn += 1;

#if 0
    printf("v:%.2f->%.2f (%f: %.3f %.3f)\n", 
	   value, valued, 
	   vsumn, vsume/vsumn, sqrt(vsumee/vsumn - (vsume/vsumn)*(vsume/vsumn)));
#endif

    printf("\n### %.5f %.5f (%d,%d)\n", value, valued, 
	   pbr->SteinAnz/8, pbr->SteinAnz %2);


    /*
      printf("%.2f (%.2f, %.2f) (%.2f, %.2f)", vp, alp, bep, (alp-vp)/SIGMA, (vp-bep)/SIGMA);
    */

#else


    // ProbCut code

    float  alv, bev;
    int    cd, value, u_bound, l_bound;
    const PCStat::AppInfo *approx;

#if PC_OLD

    if   (RealeHoehe == CHECK_H1) { approx = &CHECK_T1[ind]; cd = CHECK_D1; }
    else                          { approx = &CHECK_T2[ind]; cd = CHECK_D2; }

#else

    float delta;
    int   k;

    FOR (k, CHK_MAX) {

      cd = ApproxDepths[RealeHoehe][k];
      if (cd < 0) break;

      if (!pcstat.loaded) {
	String name = DataPath;
	name += "/"; name += PCStatFile;
	pcstat.read(name);
      }

      approx = pcstat.get(pbr->SteinAnz, cd, RealeHoehe);

      // printf("#=%d cd=%d h=%d 1/a=%f b=%f s=%f\n", 
      //  pbr->SteinAnz, cd, RealeHoehe, approx->inva, approx->b, approx->s);

      //    printf("ind=%d h=%d cd=%d a=%f b=%f s=%f\n", ind,
      //	   RealeHoehe, cd, Approx->inva, Approx->b, Approx->s);

#endif


#if PC_GEN_BOARDS
      { 
	static FILE *fp[APPROX_NUM];
        static float threshold[APPROX_NUM] = {
	  0, 0, 0,
	  0.008,   // 1-3
	  0.016,   // 2-4
	  0.035,   // 1-5
	  0.091,   // 2-6
	  0.117,   // 3-7
	  0.244,   // 4-8
	  0.244,   // 3-9
	  0.58,    // 4-10
	  1.16,    // 5-11
	  2.0,     // 4-12
	  2.0      // 5-13
	};
      
	static bool init = false;
	SPFELD sf;
	const int MODUL = 100000;
	const float SAVE_P = 0.1;
	
	if (!init) {
	  
	  init = true;
	  
	  for (i=APPROX_NUM-1; i >= 0; i--) { 
	    
	    threshold[i] *= MODUL * SAVE_P;
	    
	    char s[10000];
	    
	    sprintf(s, "pcex.%d.%d.sfk", approx.depth, i);
	    fp[i] = fopen(s, "a");
	    
	    if (!fp[i]) Error("can't open example file");
	  }
	}
	
	// generate example positions
	
	if ((random() % MODUL) < threshold[RealeHoehe]) {
	  
	  BrettSf(pbr, &sf);
	  sf.Marke = MA_WEISS_NICHT;
	  
	  if (Partei == WHITE) SfInvert(&sf);

	  fSfWrite(fp[RealeHoehe], &sf, 1); 
	  fflush(fp[RealeHoehe]);
	}
      }
#endif


#if UTIL_CUT 

      {
	if (!called) {
	  cout << "UTIL-CUT" << endl;
	  called = true;
	}

	float 
	  a = WERT_TO_REAL(al),
	  b = WERT_TO_REAL(be),
	  s = WERT_TO_REAL(
			   AlphaBeta1(pzio, cd, Partei, WERTMIN, WERTMAX, ZUG_UNBEKANNT, true)
			   );
	  
        s = (1.0/approx->inva) * s + approx->b;

#if 0
	cout << "h=" << RealeHoehe << " cd=" << cd << " a=" << a << " b=" << b << " s=" << s 
	     << " u=" << utility(a, b, s, approx->s);
	if (utility(a, b, s, approx->s) < pzio->Percentile) cout << " -> cut";
	cout << endl;
#endif

	if (utility(a, b, s, approx->s) < pzio->Percentile1) {
	  int val = REAL_TO_WERT(s);

	  if (val >=  WERTGEWINN) return(+(WERTGEWINN-1)); 
	  if (val <= -WERTGEWINN) return(-(WERTGEWINN-1));  
	  return val;
	}
      }

#else

      // old version: two zero-window searches
      // new approach: one search with wider window

      // PROB-CUT
      
      if (!called) {
	cout << "PROB-CUT" << endl;
	called = true;
      }

      if (pbr->SteinAnz < PC_PHASE_2_DISC_NUM) {
	delta = pzio->Percentile1 * approx->s;
      } else {
	delta = pzio->Percentile2 * approx->s;
      }

      alv = WERT_TO_REAL(al);
      bev = WERT_TO_REAL(be);
      u_bound = (int) REAL_TO_WERT(( delta + bev - approx->b) * approx->inva);
      l_bound = (int) REAL_TO_WERT((-delta + alv - approx->b) * approx->inva);

      if      (u_bound >= +WERTGEWINN) u_bound = +(WERTGEWINN-1);
      else if (u_bound <= -WERTGEWINN) u_bound = -(WERTGEWINN-1);

      if      (l_bound >= +WERTGEWINN) l_bound = +(WERTGEWINN-1);
      else if (l_bound <= -WERTGEWINN) l_bound = -(WERTGEWINN-1);

      value = AlphaBeta1(pzio, cd, Partei, l_bound, u_bound, ZUG_UNBEKANNT, true);

      if (value >= u_bound && be <  WERTGEWINN) return be;
      if (value <= l_bound && al > -WERTGEWINN) return al;

#if SHORTCUT

      // check whether it is likely that the last PC check 
      // won't produce a cut => saves unecessary checks

      if (k == CHK_MAX-1 || ApproxDepths[RealeHoehe][k+1] < 0 ||
	  al <= -WERTGEWINN || be >= WERTGEWINN) continue;

      // find last cd

      int i, last_cd;

      for (i = CHK_MAX-1; i >= 0; i--)
	if (ApproxDepths[RealeHoehe][i] > 0) break;

      last_cd = ApproxDepths[RealeHoehe][i];

      const PCStat::AppInfo *last_approx = pcstat.get(pbr->SteinAnz, last_cd, RealeHoehe);
      const PCStat::AppInfo *next_approx = pcstat.get(pbr->SteinAnz, cd, last_cd);
      
      float last_bound = (delta + bev - last_approx->b) * last_approx->inva;
      int bound_be = (int)REAL_TO_WERT((last_bound - SC_FACTOR*next_approx->s - next_approx->b) * 
				       next_approx->inva);
      if      (bound_be >= +WERTGEWINN) bound_be = +(WERTGEWINN-1);
      else if (bound_be <= -WERTGEWINN) bound_be = -(WERTGEWINN-1);

      // !!!RIGHT, but not tuned with: last_bound = (-delta + alv - last_approx->b) * last_approx->inva;
      // BUGGY (found by Stephane Nicolet):
      last_bound = (int) REAL_TO_WERT((-delta + alv - last_approx->b) * last_approx->inva);
      
      int bound_al = (int)REAL_TO_WERT((last_bound + SC_FACTOR*next_approx->s - next_approx->b) * 
				       next_approx->inva);
      if      (bound_al >= +WERTGEWINN) bound_al = +(WERTGEWINN-1);
      else if (bound_al <= -WERTGEWINN) bound_al = -(WERTGEWINN-1);

#if 0
      {
	printf("#=%d al=%d be=%d cd=%d h=%d lcd=%d\n", pbr->SteinAnz, al, be, cd, RealeHoehe, last_cd);
	printf("delta=%f alv=%f bev=%f\n", delta, alv, bev);
	printf("last_b=%f last_s=%f last_inva=%f\n", last_approx->b, last_approx->s, last_approx->inva);
	printf("next_b=%f next_s=%f next_inva=%f\n", next_approx->b, next_approx->s, next_approx->inva);

	float lba= (-delta + alv - last_approx->b) * last_approx->inva;
	float lbb= (delta + bev - last_approx->b) * last_approx->inva;
	printf("lba=%f lbb=%f\n", lba, lbb);

	printf("nba=%f nbb=%f\n", 
	       (lba + SC_FACTOR*next_approx->s - next_approx->b) * next_approx->inva,
	       (lbb - SC_FACTOR*next_approx->s - next_approx->b) * next_approx->inva);
	puts("");
      }
#endif

      if (bound_al >= bound_be) {
	//printf("-");
	continue;
      } else {
	//printf("+");
      }

      if (bound_al < value && value < bound_be) break;  // cut not likely

#endif

    }

#if NODE_STAT
    puts("end");
#endif

#endif

  }

#endif
#endif


  ZM_LOESCHEN;

  ZugAnz = 0;

  BestZug  = ZUG_UNBEKANNT; 
  lo_value = WERTMIN; 
  hi_value = be;
  max      = al;


  nPartei = GEGNER(Partei);
  nRealeHoehe = RealeHoehe - 1;


#if HASH

#if FIRST_LEVELS_HASH

  if (phs) {
    AktZug = phs->get_best_move();
    if (HashEntry::locks_equal(phs->get_lock(), pbr->Hash2) && ZUG(AktZug)) { TRY1 }
  }

#endif

  if (ph) {

    AktZug = ph->get_best_move();

#if TEST
    if (AktZug < -2 || AktZug >= 89) {
      printf("%x %d\n", ha, AktZug); Error("hashinhalt1");
    }
#endif

    if (HashEntry::locks_equal(ph->get_lock(), pbr->Hash2) && ZUG(AktZug)) { TRY1 }
  }

#endif



#if ECKEN_ZUERST

  // now all corner moves

  AktZug = A1; TRY1;
  AktZug = H8; TRY1;
  AktZug = A8; TRY1;
  AktZug = H1; TRY1;

#endif


  if (RealeHoehe < KILLER_HOEHE) {

#if KILLER
  
    // Rest gem‰ﬂ Killertabelle oder DefaultKill

    if (ZUG(LetzterZug)) {

      if (Partei == BLACK) Kill = pzio->killer.KillWHITE; 
      else 		   Kill = pzio->killer.KillBLACK;

      pl = &Kill->Liste[LetzterZug][0];

    } else {

#endif

      pl = &pzio->killer.DefaultKill[0];

#if KILLER

    }

#endif


    for (i=pzio->killer.FreiAnz; i > 0; i--) {

      AktZug = *pl++;
      TRY1
	}

  } else {


    ZUGDAT ZugDat[65];
    int    Anz;

#if 1
    Anz=Vorsort(pzio, Partei, RealeHoehe < TIEFE2_HOEHE ? 1 : 2, zm, ZugDat); 
#else
    Anz=ZuegeSortieren(pzio, Partei, SortDepth[RealeHoehe >= 0 ? RealeHoehe : 0], ZugDat);
#endif


    FOR (i, Anz) {

      AktZug = ZugDat[i].Zug;

      TRY1
	}

  }

  // AllesBesucht:


  if (ZugAnz == 0) {				/* Partei kann nicht ziehen */

    if (LetzterZug == ZUG_PASSEN) {		/* keiner kann */

      return BewDiff(pbr, Partei);

    } else {					/* eins tiefer */
      /*
	printf("*\n");
      */
      return -AlphaBeta1(pzio, RealeHoehe, GEGNER(Partei), -be, -al,ZUG_PASSEN, in_cut_search);
    }
  }


 Ende:


#if NODE_STAT

  printf("h= %d total= %d mpc= %d\n", RealeHoehe, pzio->cio.BewAnz-prev_count, mpc_nodes);

#endif


  // Hash- und Killer-Eintrag anpassen


  if (1 || RealeHoehe > 0) {  // !!!

#if TEST
    if (!ZUG(BestZug)) Error("BestZug auﬂerhalb");
#endif


#if HASH

    // don't overwrite entries with less information! saves time !!!

    if (ph &&
	(!HashEntry::locks_equal(ph->get_lock(), pbr->Hash2) ||
	 (int)ph->get_height() <= RealeHoehe)) {

      // if (pbr->SteinAnz - pzio->DiscNum0 > pzio->hash.HashMaxDepth) Error("xxx");

      pzio->hash.set(*ph, pbr->Hash2, RealeHoehe, al, be, BestZug, lo_value);

    }


#if FIRST_LEVELS_HASH
 
#if 1

    // old
    
    if (phs &&
	(!HashEntry::locks_equal(phs->get_lock(), pbr->Hash2) ||
	 (int)phs->get_height() <= RealeHoehe)) {

      pzio->hash0.set(*phs, pbr->Hash2, RealeHoehe, al, be, BestZug, lo_value);
    }

#else

    // experiment
    
    // overwrite iff
    //    - lock equal but lower depth in hashtab
    //    - lock different, equal stamp, and lower depth in hashtab
    //    - lock different, stamp-diff >= 3

    if (phs) {

      if (HashEntry::locks_equal(phs->Hash2, pbr->Hash2)) {

	if ((int)phs->get_height() < RealeHoehe) 
	  SETHASH(pzio->hash, phs, pbr, RealeHoehe, al, be, BestZug, lo_value);

      } else {

	if ((STAMP_EQUAL(phs->stamp, pzio->hash0.current_stamp) &&
	     (int)phs->get_height() < RealeHoehe)  ||
	    pzio->hash0.current_stamp >= phs->stamp + 3)

	  SETHASH(pzio->hash, phs, pbr, RealeHoehe, al, be, BestZug, lo_value);
      }
    }

    
#endif
    
#endif
    
#endif


#if KILLER

    if (ZUG(LetzterZug)) {

      if (!ZUG(BestZug)) { 
	printf("n: h=%d z=%d anz=%d lo=%d ma=%d al=%d be=%d", 
	       RealeHoehe, BestZug, ZugAnz, lo_value, max, al, be); 
	KoorAus(BestZug); printf("\n"); 
      }

      KillerUpdate(&pzio->killer, GEGNER(Partei), LetzterZug, BestZug);    
    }
#endif

    /*printf("ReahleHoehe=%d Wert=%d zur¸ck\n", RealeHoehe, lo_value);*/

  } else if (RealeHoehe == 0) {

#if HASH

    if (ph && pzio->Quiescence) 
      pzio->hash.set(*ph, pbr->Hash2, 0, al, be, BestZug, lo_value);

#endif

  }

  return lo_value;
}



#define TRYQ \
\
    if (L_PLY_MAKE_MOVE(pbr, Partei, AktZug, &Delta)) {\
      pzio->cio.BewAnz++;\
\
      value = -QSearch(pzio, nPartei, -be, -max(al,lo_value));\
      L_PLY_UNDO_MOVE(pbr, &Delta);\
\
      if (value > lo_value) {\
\
	lo_value = value;\
	if (lo_value >= be) return lo_value;\
\
      }\
\
    }



// do not use hash-values!

WERT QSearch(
  ZUGIO	 *pzio,
  PARTEI Partei,
  WERT	 al,
  WERT	 be
)
{
  BRETT	*pbr = &pzio->Brett;
  SFPOS	 AktZug;


  pzio->fertig = false; 	// search not finished yet

  if (pbr->SteinAnz == 64) return BewDiff(pbr, Partei);



#if EDGE_QS

  if (edge_qs) {

  static bool table_loaded=false;
  static float *qsvals=0;
  char s[1000];

#define N 10
#define EDGE_MAX 4.3206

  if (!table_loaded) {

    // read edge-table

    table_loaded = true;

    qsvals = malloc(sizeof(qsvals[0]) * Pot3[N]);
    if (!qsvals) Error("no memory for qsvals");

    FILE *fp;

    fp = fopen("qsvals", "r");

    int i;
    FOR (i, Pot3[N]) {
      
      if (fscanf(fp, "%s %f", s, &qsvals[i]) != 2) Error("qsvals corrupt");

    }
    
    fclose(fp);
  }


  float delta = 0.8 * (1 << depth);  // threshold is depth-dependent

  if (delta < EDGE_MAX) {

    STRAHLTYP *pat = pbr->NewPatt.st;
    float *pf = &qsvals[(Pot3[N]-1)/2];
    sint1 *p=pbr->p;

#define PAT(n,x1,x2) (9*(pat[n]/(DOUBLE_INDEX ? 2 : 1))+3*p[x1]+p[x2])
#define PF(i)  (Partei == BLACK ? pf[i] : pf[-i]) 

    if (PF(PAT(PHV1A, B2, G2)) > delta ||
	PF(PAT(PHV1B, B7, G7)) > delta ||
	PF(PAT(PHV1C, B2, B7)) > delta ||
	PF(PAT(PHV1D, G2, G7)) > delta) {

      move_possible = false;

      // noisy edges => search all moves 

      int i;
      PARTEI nPartei = GEGNER(Partei);
      DELTA  Delta;
      WERT lo_value=WERTMIN, value;

      // corners first

      if (p[A1] == LEER && (p[A2] == nPartei || p[B2] == nPartei || p[B1] == nPartei)) {
        AktZug = A1; TRYQ;
      }         

      if (p[H8] == LEER && (p[H7] == nPartei || p[G7] == nPartei || p[G8] == nPartei)) {
	AktZug = H8; TRYQ;
      }         

      if (p[A8] == LEER && (p[A7] == nPartei || p[B7] == nPartei || p[B8] == nPartei)) {
	AktZug = A8; TRYQ;
      }         

      if (p[H1] == LEER && (p[H2] == nPartei || p[G2] == nPartei || p[G1] == nPartei)) {
	AktZug = H1; TRYQ;
      }         

      // remaining moves

      SFPOS *pl = &pzio->killer.DefaultKill[0];

      for (i=0; i < pzio->killer.FreiAnz; i++) {

	AktZug = *pl++;

	if (!Ecke[AktZug]) {
	  TRYQ;
	}
      }

      if (!move_possible) {

	// check for game-end

	SPFELD sf;
	SFPOS  moves[65];

	BrettSf(pbr, &sf);
	
	if (!SfMoeglZuege(&sf, GEGNER(Partei), moves)) 

	  // neither player has move => game end 

	  return BewDiff(pbr, Partei);

	else {

	  // go deeper

	  //printf("pass\n");

	  return -QS(pzio, GEGNER(Partei), -be, -al, depth);
	}
      }
    }
  }

  }
#endif


  WERT lo_value = pzio->BewMitte(pbr, Partei), value;

  if (!pzio->Quiescence || lo_value >= be) return lo_value;


  // determine corner moves 

  uint4 *pu=StrMobE8+(NUM8-1)/2, w, mobp;
  STRAHLTYP *pat = pbr->NewPatt.st;

  w  = PU(pat[PHV1A]) + PU(pat[PHV1B]) + PU(pat[PHV1C]) + PU(pat[PHV1D]) + 
       PU(pat[PD1A]) +  PU(pat[PD1B]);

  // w = MOB, MOBEB, MOBEW of edges and diagonals

      
  if (Partei == BLACK) {

    mobp = (w & MOBEB_MASK) >> 8;

#if QALL
    mobo = w & MOBEW_MASK;
#endif

  } else {

#if QALL
    mobo = (w & MOBEB_MASK) >> 8;
#endif
    mobp = w & MOBEW_MASK;

  }


  if (mobp) {	// player has corner move -> try all corner moves

    PARTEI nPartei = GEGNER(Partei);
    DELTA  Delta;

    { Square *p=pbr->p;

      if (p[A1] == LEER && (p[A2] == nPartei || p[B2] == nPartei || p[B1] == nPartei)) {
        AktZug = A1; TRYQ;
      }         
      if (p[H8] == LEER && (p[H7] == nPartei || p[G7] == nPartei || p[G8] == nPartei)) {
	AktZug = H8; TRYQ;
      }         
      if (p[A8] == LEER && (p[A7] == nPartei || p[B7] == nPartei || p[B8] == nPartei)) {
	AktZug = A8; TRYQ;
      }         
      if (p[H1] == LEER && (p[H2] == nPartei || p[G2] == nPartei || p[G1] == nPartei)) {
	AktZug = H1; TRYQ;
      }         
    }

  }


#if QALL

  if (mobo) {	// player has no corner move but opponent has -> all

    goto SearchAll;

  }

#endif


  // no corner move -> leaf

  return lo_value;
}


