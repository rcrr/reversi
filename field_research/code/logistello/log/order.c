// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/* Züge sortieren  11.93 */

#ifndef PRECOMP
#include "main.h"
#endif

#include "mid.h"
#include "move.h"
#include "eval.h"
#include "hash.h"
#include "killer.h"

#include "crt.h"
#include "crtg.h"


#if 0				/* alle Optimierungen aus zum Testen */

#define SCHNITT		if (lo_value >= be) goto Ende;	/* beta-Schnitt */
#define NULLFENSTER  
#define WIEDERHOLUNG 

#else

#define SCHNITT		if (lo_value >= be) goto Ende;	/* beta-Schnitt */
#define NULLFENSTER     hi_value = max + 1
#define WIEDERHOLUNG    \
      if (value > max && value < be && ZugAnz > 1 /* && nRealeHoehe > 0 */)\
\
/* Wiederholungssuche */\
\
        value = -SortAlphaBeta(pzio, Depth+1, nRealeHoehe, nPartei, -be, -value, AktZug);
#endif




#define BESSER \
      if (value > lo_value) {\
	BestZug = AktZug;\
        lo_value = value;	/* übernehmen, wenn besser */\
	SCHNITT;\
        if (lo_value > max) max = lo_value;\
        NULLFENSTER;\
      }


#define TRY1 \
if (pumg[AktZug] && !ZM_GESETZT(AktZug)) {\
    if (MAKE_MOVE(pbr, Partei, AktZug, &Delta)) {\
      ZugAnz++; ZM_SETZEN(AktZug);\
\
      value = -SortAlphaBeta(pzio, Depth+1, nRealeHoehe, nPartei, -hi_value, -max, AktZug);\
      WIEDERHOLUNG;\
      UNDO_MOVE(pbr, &Delta);\
      BESSER;\
\
    }\
  }



int SABBestMove;


WERT SortAlphaBeta(
  ZUGIO	 *pzio,		/* Zeiger auf globale Variablen	*/
  int    Depth,
  int	 RealeHoehe,	/* Abstand zum Blatt		*/
  PARTEI Partei,	/* Spieler am Zug		*/
  WERT	 al,		/* alpha-beta Fenster		*/ 
  WERT	 be,
  int    LetzterZug
)
{
  int		i, ZugAnz, nRealeHoehe;
  PARTEI	nPartei;
  int		BestZug, AktZug;
  WERT		value, lo_value, hi_value, max;
  KILLTAB	*Kill;
  SFPOS		*pl;
  BRETT		*pbr;
  DELTA		Delta;
  HashEntry	*ph;
  Square	*pumg;
  ZUGMENGE	zm;

/*
printf("depth=%d\n", Depth);
*/
  pbr  = &pzio->Brett;
  pumg = pbr->umg;


/* Spielfeld voll */

  if (pbr->SteinAnz == 64) { 

    pzio->cio.BewAnz++;
    return BewDiff(pbr, Partei);
  }



  if (RealeHoehe <= 0) {

    pzio->cio.BewAnz++;
    return pzio->BewMitte(pbr, Partei);

  }



  ZM_LOESCHEN;

  ZugAnz = 0;

  BestZug  = ZUG_UNBEKANNT; 
  lo_value = WERTMIN; 
  hi_value = be;
  max      = al;


  nPartei = GEGNER(Partei);
  nRealeHoehe = RealeHoehe - 1;


#if HASH

  ph = &pzio->hash.get(pbr->Hash1, Partei);

  AktZug = ph->get_best_move();

#if TEST
  if (AktZug < -2 || AktZug >= 89) {
    printf("%x %d\n", ha, AktZug); Error("hashinhalt1");
  }
#endif

  if (ZUG(AktZug) && HashEntry::locks_equal(ph->get_lock(), pbr->Hash2)) { TRY1 }

#endif


#if ECKEN_ZUERST

/* jetzt alle Eckenzüge */

  AktZug = A1; TRY1;
  AktZug = H8; TRY1;
  AktZug = A8; TRY1;
  AktZug = H1; TRY1;

#endif



#if KILLER
  
/* Rest gemäß Killertabelle oder DefaultKill */

  if (ZUG(LetzterZug)) {

    if (Partei == BLACK) Kill = pzio->killer.KillWHITE; 
    else 		 Kill = pzio->killer.KillBLACK;

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


  if (ZugAnz == 0) {				/* Partei kann nicht ziehen */

    if (LetzterZug == ZUG_PASSEN) {		/* keiner kann */

      pzio->cio.BewAnz++;
      return BewDiff(pbr, Partei);

    } else {					/* eins tiefer */

      return 
	-SortAlphaBeta(pzio, Depth+1, RealeHoehe, GEGNER(Partei), -be, -al,
		       ZUG_PASSEN);
    }
  }


Ende:

  if (Depth == 0) SABBestMove = BestZug;

  return lo_value;
}



#if LAZY_UPDATE

int ZuegeSortieren(
  ZUGIO	 *pzio,		/* Zeiger auf globale Variablen	*/
  PARTEI Partei,	/* Spieler am Zug		*/
  int	 RealeHoehe,	/* Abstand zum Blatt		*/
  int    move1,
  ZUGDAT *ZugDat
)
{
  DELTA  Delta;
  int Anz=0, i, al=WERTMIN, be=WERTMAX; 
  SFPOS Zug; 
  SFPOS *pl = &pzio->killer.DefaultKill[0];
  BRETT *pbr = &pzio->Brett;
  Square *pu = pbr->umg;  

  for (i=pzio->killer.FreiAnz; i > 0; i--) {

    Zug = *pl++;

    if (Zug != move1 && pu[Zug] && MAKE_MOVE(&pzio->Brett, Partei, Zug, &Delta)) {
      ZugDat[Anz].Wert =
        - AlphaBeta1(pzio, RealeHoehe-1, GEGNER(Partei), -be, -al, Zug);    
      ZugDat[Anz++].Zug = Zug;

      UNDO_MOVE(&pzio->Brett, &Delta);
    }
  }

  qsort((char*)ZugDat, (size_t) Anz, sizeof(ZUGDAT), compZUGDAT);
  return Anz;
}


#else

#error "lazy?"

int ZuegeSortieren(
  ZUGIO	 *pzio,		/* Zeiger auf globale Variablen	*/
  PARTEI Partei,	/* Spieler am Zug		*/
  int	 RealeHoehe,	/* Abstand zum Blatt		*/
  ZUGDAT *ZugDat
)
{
  DELTA  Delta;
  int Anz=0, i, al=WERTMIN, be=WERTMAX; 
  SFPOS Zug; 
  UMGEB *pumgl=pzio->Brett.umgl;


  for (i=pzio->Brett.LastIndex; i >= 0; i--) { 

    Zug = *pumgl; pumgl++;

    if (MAKE_MOVE(&pzio->Brett, Partei, Zug, &Delta)) {

      ZugDat[Anz].Wert =
	 -AlphaBeta1(pzio, RealeHoehe-1, GEGNER(Partei), -be, -al, Zug);	    
      ZugDat[Anz++].Zug = Zug;

      UNDO_MOVE(&pzio->Brett, &Delta);
    }
  }

  qsort((char*)ZugDat, (size_t) Anz, sizeof(ZUGDAT), compZUGDAT); 

  return Anz;
}

#endif




