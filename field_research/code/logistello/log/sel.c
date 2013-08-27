// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/* Selektive Suche / 11.93 */
/* Ansatz: flache Suche und dann nur abhängig von der Höhe */
/* die besten Züge nehmen */

obsolete


#ifndef PRECOMP
#include "main.h"
#endif

#include "sel.h"
#include "mid.h"
#include "order.h"
#include "move.h"
#include "eval.h"
#include "hash.h"
#include "killer.h"

#include "crt.h"
#include "crtg.h"

#define TEST		false

#define KILLER_HOEHE	5	/* < -> Killertabellen benutzen, sonst	*/
				/* Tiefe 1 oder 2 Suche			*/

#define TIEFE2_HOEHE	7	/* < -> Tiefe 1 Vorsortierung */

#define ECKEN_ZUERST	true	/* nach Hashzug alle Ecken zuerst? */

#define MITTEL		true	/* Bewertung wird gemittelt aus bestem Blatt */
				/* und aktueller Stellung		     */



#define START_FAKTOR	0.0	/* 0.6 */
#define END_FAKTOR	0.9	/* 0.8 */


/* in Abhängigkeit von Knotenhöhe: */

int ZugAnzahl[20] = { 
/* 0 */  1,  1,  1,  2,  3, 
/* 5 */  3,  4,  4,  4, 64,
/*10 */ 64, 64, 64, 64, 64,
/*15 */ 64, 64, 64, 64, 64,
};


REAL ZugDelta[20];


int SortierTiefe[20] = { 
/* 0 */  1,  1,  1,  1,  2, 
/* 5 */  2,  3,  4,  4,  5,
/*10 */  5,  6,  6,  7,  7,
/*15 */  8,  8,  8,  8,  8,
};




#if 0				/* alle Optimierungen aus zum Testen */

#define SCHNITT		
#define NULLFENSTER  
#define WIEDERHOLUNG 
#define WERTEHASH	false

#else

#define SCHNITT		if (lo_value >= be) goto Ende;	/* beta-Schnitt */
#define NULLFENSTER     hi_value = max + 1
#define WIEDERHOLUNG    \
      if (value > max && value < be && ZugAnz > 1 && nRealeHoehe > 0)\
\
/* Wiederholungssuche */\
\
        value = -SelAlphaBeta1(pzio, nRealeHoehe, Tiefe+1, nPartei, -be, -value, AktZug);
#define WERTEHASH	true

#endif


#ifdef KEIN_HASH
#undef WERTEHASH
#define WERTEHASH false
#endif



#define FERTIG_TIEFE	99	/* Eintrag in Hashtabelle -> keine inneren Bew. */

#define WERTAUS(s, w) printf("%s w=%d\n", s, w)




extern MOBALL MobAll;


inline static bool InVorgabe(ZUGIO *pzio, SFPOS Zug)
{
  int i;

  FOR (i, pzio->VorgabeAnz) if (pzio->Zuege[i] == Zug) break;

/*  KoorAus(Zug); printf(" vor %d, ",  i < pzio->VorgabeAnz); */

  return i < pzio->VorgabeAnz;
}




#define TRY \
if (pumg[AktZug] && !ZM_GESETZT(AktZug) \
    && (!pzio->ZugVorgabe || InVorgabe(pzio, AktZug)) )  {\
    if (Setzen(pbr, Partei, AktZug, &Delta)) {\
/*printf("t=%d ", Tiefe); KoorAus(AktZug); printf("\n");*/\
      ZugAnz++; ZM_SETZEN(AktZug);\
      pzio->Move1 = AktZug; pzio->MoveNumber = ZugAnz;\
\
      value = -SelAlphaBeta1(pzio, nRealeHoehe, Tiefe+1, nPartei, -hi_value, -max, AktZug);\
\
      WIEDERHOLUNG;\
      Zurueck(pbr, &Delta);\
\
      if (value > lo_value) {\
\
	FindPath(pzio, Partei, AktZug, al, be, value, MIDGAME);\
\
	BestZug = AktZug;\
        lo_value = value;			/* übernehmen, wenn besser */\
	SCHNITT;\
        if (lo_value > max) max = lo_value;\
        NULLFENSTER;\
\
      }\
    }\
  }



/* NegaScout-Version */

WERT SelAlphaBeta(
  ZUGIO	 *pzio,		/* Zeiger auf globale Variablen	*/
  int	 RealeHoehe,	/* Abstand zum Blatt		*/
  PARTEI Partei,	/* Spieler am Zug		*/
  WERT	 al,		/* alpha-beta Fenster		*/ 
  WERT	 be,
  int    LetzterZug
)
{
  int		i, ZugAnz, AlteBewAnz, nRealeHoehe, Tiefe=0;
  PARTEI	nPartei;
  SFPOS		BestZug, AktZug;
  WERT		value, lo_value, hi_value, max;
  KILLTAB	*Kill;
  SFPOS		*pl;
  BRETT		*pbr;
  DELTA		Delta;
  uint4		ha;
  HASHEINTRAG	*ph;
  sint1		*pumg;
  static int 	zaehler = 0;
  ZUGMENGE	zm;


#if ALBETEST
printf("Tiefe=%d, al=%d, be=%d\n", Tiefe, al, be);
#endif

  if (--zaehler < 0) { pzio->Check(&pzio->cio, pzio, false); zaehler = CHECK_COUNT; }

  pbr   = &pzio->Brett;
  pumg  = pbr->umg;



  FOR (i, RealeHoehe+1) {

    if (KILLER_HOEHE >= RealeHoehe)
      ZugDelta[i] = 0.01;
    else if (i < KILLER_HOEHE) 
      ZugDelta[i] = END_FAKTOR;
    else {
      REAL a, b;

/*      ZugDelta[i] = (0.9 * (RealeHoehe-i)) / (RealeHoehe-KILLER_HOEHE);*/

      b = (START_FAKTOR - END_FAKTOR) / 
		(RealeHoehe*RealeHoehe - KILLER_HOEHE*KILLER_HOEHE);
      a = START_FAKTOR - b * RealeHoehe*RealeHoehe;

      ZugDelta[i] = a + b * i * i;
    }

/*printf("%d: %f\n", i, ZugDelta[i]);*/
  }


#if ALBETEST
{ SPFELD sf;
BrettSf(pbr, &sf);
SfAus(&sf, 0, 0);
printf("%d %d\n", Tiefe, Partei);
}
#endif


/* Spielfeld voll */

  if (pbr->SteinAnz == 64) {

    pzio->cio.BewAnz++;
    return BewDiff(pbr, Partei);
  }



#if HASH

/* war Stellung schon da? */


  ha = pbr->Hash1 & (HASHANZ-1);
  if (Partei == WHITE) ha = HASH_WHITE(ha);

#if TEST
  if (ha >= HASHANZ) { printf("%lu", ha); Error("hash"); }
#endif
  
  ph = &pzio->hash.HashTab[ha];

#endif


  if (RealeHoehe <= 0) {


/* Blatt bewerten */

    pzio->fertig = false;	/* noch nicht alles durchgerechnet */

    pzio->cio.BewAnz++;		/* innere Bewertungen zählen */
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


  AlteBewAnz = pzio->cio.BewAnz;	/* bei Unterschied nicht fertig! */

#if HASH


  AktZug = ph->BesterZug;

#if TEST
  if (AktZug < -2 || AktZug >= 89) {
    printf("%x %d\n", ha, AktZug); Error("hashinhalt1");
  }
#endif
  
/*printf("HashZug "); KoorAus(AktZug); printf("\n");*/


  if (ZUG(AktZug) && ph->Hash2 == pbr->Hash2) { TRY }

#endif


#if ECKEN_ZUERST

/* jetzt alle Eckenzüge */

  AktZug = A1; TRY;
  AktZug = H8; TRY;
  AktZug = A8; TRY;
  AktZug = H1; TRY;

#endif



  if (RealeHoehe < KILLER_HOEHE) {


#if KILLER
  
/* Rest gemäß Killertabelle oder DefaultKill */

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



#if TEST
{
  int i, j;

  for (i=0; i <= pbr->LastIndex; i++) {
    FOR (j, pzio->killer.FreiAnz) if (pl[j] == pbr->umgl[i].UPos) break;
    if (j >= pzio->killer.FreiAnz) Error("nicht gefunden in null");
  }
}
#endif


    for (i=pzio->killer.FreiAnz; i > 0; i--) {

      AktZug = *pl++;
      TRY
    }

  } else {

    ZUGDAT ZugDat[65];
    int    Anz = ZuegeSortieren(pzio, Partei, 
			RealeHoehe < TIEFE2_HOEHE ? 1 : 2, ZugDat); 

    FOR (i, Anz) {

      AktZug = ZugDat[i].Zug;

      TRY
    }

  }



  if (ZugAnz == 0) {				/* Partei kann nicht ziehen */

    if (LetzterZug == ZUG_PASSEN) {		/* keiner kann */

      pzio->cio.BewAnz++;
      return BewDiff(pbr, Partei);

    } else {					/* eins tiefer */

      return -SelAlphaBeta1(pzio, RealeHoehe, Tiefe+1, GEGNER(Partei), -be, -al, ZUG_PASSEN);
    }
  }




Ende:

  if (RealeHoehe <= 0) {

    SETHASH(ph, pbr, RealeHoehe, al, be, ZUG_UNBEKANNT, lo_value);
    return lo_value;

  }


/* Hash- und Killer-Eintrag anpassen */

#if TEST
  if (!ZUG(BestZug)) Error("BestZug außerhalb");
#endif


#if HASH

/* alles gerechnet? */
 
  if (pzio->cio.BewAnz == AlteBewAnz) RealeHoehe = FERTIG_TIEFE;

  SETHASH(ph, pbr, RealeHoehe, al, be, BestZug, lo_value)

#endif


#if KILLER
  if (ZUG(LetzterZug)) {

    if (!ZUG(BestZug)) { printf("n:"); KoorAus(BestZug); }

    KillerUpdate(&pzio->killer, GEGNER(Partei), LetzterZug, BestZug);    
  }
#endif

  return lo_value;
}



/* Eckenvertiefung */

#if 0
      if (RealeHoehe == 1 && Ecke[AktZug]) nRealeHoehe = RealeHoehe;\
      else				   nRealeHoehe = RealeHoehe-1;\
\

#endif


#define TRY1 \
if (pumg[AktZug] && !ZM_GESETZT(AktZug)) {\
    if (Setzen(pbr, Partei, AktZug, &Delta)) {\
/*printf("t=%d ", Tiefe); KoorAus(AktZug); printf("\n");*/\
      ZugAnz++; ZM_SETZEN(AktZug);\
\
      value = -SelAlphaBeta1(pzio, nRealeHoehe, Tiefe+1, nPartei, -hi_value, -max, AktZug);\
\
      WIEDERHOLUNG;\
      Zurueck(pbr, &Delta);\
\
      if (value > lo_value) {\
\
	BestZug = AktZug;\
        lo_value = value;			/* übernehmen, wenn besser */\
	SCHNITT;\
        if (lo_value > max) max = lo_value;\
        NULLFENSTER;\
\
      }\
    }\
  }




/* NegaScout-Version, tiefere Knoten */

WERT SelAlphaBeta1(
  ZUGIO	 *pzio,		/* Zeiger auf globale Variablen	*/
  int	 RealeHoehe,	/* Abstand zum Blatt		*/
  int    Tiefe,		/* Abstand von Wurzel		*/
  PARTEI Partei,	/* Spieler am Zug		*/
  WERT	 al,		/* alpha-beta Fenster		*/ 
  WERT	 be,
  int    LetzterZug
)
{
  int		i, ZugAnz, AlteBewAnz, nRealeHoehe;
  PARTEI	nPartei;
  SFPOS		BestZug, AktZug;
  WERT		value, lo_value, hi_value, max;
  KILLTAB	*Kill;
  SFPOS		*pl;
  BRETT		*pbr;
  DELTA		Delta;
  uint4		ha;
  HASHEINTRAG	*ph;
  sint1		*pumg;
  static int 	zaehler = 0;
  ZUGMENGE	zm;

#if 0
{ SPFELD sf;

  BrettSf(&pzio->Brett, &sf);
  SfAus(&sf, Partei, -1);

printf("Tiefe=%d, al=%d, be=%d\n", Tiefe, al, be);
}
#endif

  if (--zaehler < 0) { 
    pzio->Check(&pzio->cio, pzio, false); zaehler = CHECK_COUNT; 
  }

  pbr  = &pzio->Brett;
  pumg = pbr->umg;


/* Spielfeld voll */

  if (pbr->SteinAnz == 64) { 

    pzio->cio.BewAnz++;
    return BewDiff(pbr, Partei);
  }



#if HASH

/* war Stellung schon da? */


  ha = pbr->Hash1 & (HASHANZ-1);
  if (Partei == WHITE) ha = HASH_WHITE(ha);

#if TEST
  if (ha >= HASHANZ) { printf("%lu", ha); Error("hash"); }
#endif
  
  ph = &pzio->hash.HashTab[ha];


#if WERTEHASH

  if (ph->Hash2 == pbr->Hash2 && 	/* Lock */
      ph->Tiefe >= RealeHoehe &&
      ZUG(ph->BesterZug) && pumg[ph->BesterZug] >= UMG) {

    if (ph->WertArt == WERT_EXAKT  ||
       (ph->WertArt == WERT_KLGL_MINIMAX && ph->Wert >= be) ||
       (ph->WertArt == WERT_GRGL_MINIMAX && ph->Wert <= al)) {

/* Stellung nicht neu berechnen! */

      if (ph->Tiefe != FERTIG_TIEFE) { 
	pzio->cio.BewAnz++;	/* mindestens eine innere Bewertung -> */
        pzio->fertig = false;	/* noch nicht alles durchgerechnet     */
      }

      return ph->Wert;

    } else  {

/* printf("!"); fflush(stdout); */
    }
  }

#endif    

#endif




  if (RealeHoehe <= 0) {


/* Blatt bewerten */

    pzio->fertig = false;	/* noch nicht alles durchgerechnet */
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


  AlteBewAnz = pzio->cio.BewAnz;	/* bei Unterschied nicht fertig! */


#if HASH

  AktZug = ph->BesterZug;

#if TEST
  if (AktZug < -2 || AktZug >= 89) {
    printf("%x %d\n", ha, AktZug); Error("hashinhalt1");
  }
#endif

  if (ZUG(AktZug) && ph->Hash2 == pbr->Hash2) { TRY1 }

#endif


  if (RealeHoehe < KILLER_HOEHE && Tiefe >= 5) {


#if KILLER
  
/* Rest gemäß Killertabelle oder DefaultKill */

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



#if TEST
{
  int i, j;

  for (i=0; i <= pbr->LastIndex; i++) {
    FOR (j, pzio->killer.FreiAnz) if (pl[j] == pbr->umgl[i].UPos) break;
    if (j >= pzio->killer.FreiAnz) Error("nicht gefunden in null");
  }
}
#endif


    for (i=pzio->killer.FreiAnz; i > 0; i--) {

      AktZug = *pl++;
      TRY1
    }

  } else {

    ZUGDAT ZugDat[65];
    int Anz=ZuegeSortieren(pzio, Partei, SortierTiefe[RealeHoehe], ZugDat);
    int grenze;

    
/*    if (Anz > ZugAnzahl[RealeHoehe]) Anz = ZugAnzahl[RealeHoehe]; */

    if (Anz) { 

      grenze = REAL_TO_WERT(LOGIT(
		 EXPWERT(WERT_TO_REAL(ZugDat[0].Wert)) * ZugDelta[RealeHoehe]
	       )); 
#if 0
printf("%d %f %f %f %f %d\n", 
ZugDat[0].Wert, 
EXPWERT(WERT_TO_REAL(ZugDat[0].Wert)),
ZugDelta[RealeHoehe],
EXPWERT(WERT_TO_REAL(ZugDat[0].Wert)) * ZugDelta[RealeHoehe],
LOGIT(
		 EXPWERT(WERT_TO_REAL(ZugDat[0].Wert)) * ZugDelta[RealeHoehe]
	       ),
grenze);
#endif


      FOR (i, Anz) {

        if (ZugDat[i].Wert < grenze-2) break;

        AktZug = ZugDat[i].Zug;

        TRY1
      }
    }
  }

  if (ZugAnz == 0) {				/* Partei kann nicht ziehen */

    if (LetzterZug == ZUG_PASSEN) {		/* keiner kann */

      pzio->cio.BewAnz++;
      return BewDiff(pbr, Partei);

    } else {					/* eins tiefer */

      return -SelAlphaBeta1(pzio, RealeHoehe, Tiefe+1, GEGNER(Partei), -be, -al,ZUG_PASSEN);
    }
  }




Ende:


#if MITTEL

/* Werte des Vaters und des besten Sohns werden gemittelt -> kein Alternieren */

  if (RealeHoehe == 1) lo_value = (lo_value + pzio->BewMitte(pbr, Partei)) / 2;

#endif




#if 0
  if (RealeHoehe <= 0) {

    SETHASH(ph, pbr, RealeHoehe, al, be, ZUG_UNBEKANNT, lo_value);
    return lo_value;
  }
#endif


/* Hash- und Killer-Eintrag anpassen */

#if TEST
  if (!ZUG(BestZug)) Error("BestZug außerhalb");
#endif


#if HASH

/* alles gerechnet? */

  if (pzio->cio.BewAnz == AlteBewAnz) RealeHoehe = FERTIG_TIEFE;

  SETHASH(ph, pbr, RealeHoehe, al, be, BestZug, lo_value)

#endif


#if KILLER
  if (ZUG(LetzterZug)) {

    if (!ZUG(BestZug)) { printf("n:"); KoorAus(BestZug); }

    KillerUpdate(&pzio->killer, GEGNER(Partei), LetzterZug, BestZug);    
  }
#endif


  return lo_value;
}
