// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/* Prototypen von o_plend */

#ifndef O_PLEND
#define O_PLEND

#include "move.h"

int EndAlphaBeta(
  ZUGIO *pzio,		/* Zeiger auf globale Variablen		*/
  PARTEI Partei,	/* Spieler am Zug			*/
  int al, int be,	/* alpha-beta Fenster			*/
  int LetzterZug
);


int EndAlphaBeta1(
  ZUGIO *pzio,		/* Zeiger auf globale Variablen		*/
  PARTEI Partei,	/* Spieler am Zug			*/
  int al, int be,	/* alpha-beta Fenster			*/
  int LetzterZug
);


extern int zaehler;


// macro code


#if 0

// old code

inline void FindFreeSquares(ZUGIO *pzio)
{
  int i, move;
  Square *p=pzio->Brett.p, *pf=pzio->Brett.FreeSquares;
  SFPOS *pk=pzio->killer.DefaultKill;

  for (i=pzio->killer.FreiAnz; i > 0; i--) {

    move = *pk++;

    if (p[move] == LEER) *pf++ = move;
  
  }

  *pf = 0;
}


#else

// crude sort: holes, corners, rest  -> ~ 5% faster

inline void FindFreeSquares(ZUGIO *pzio)
{
  int i, move;
  Square *p=pzio->Brett.p, *pf=pzio->Brett.FreeSquares;
  Square corner_moves[65], *pc = corner_moves;
  Square rem_moves[65], *pr = rem_moves;
  SFPOS *pk=pzio->killer.DefaultKill;

  for (i=pzio->killer.FreiAnz; i > 0; i--) {
    move = *pk++;
    if (p[move] == LEER) {
      if (p[move+1]  != LEER &&
	  p[move-1]  != LEER &&
	  p[move+10] != LEER &&
	  p[move-10] != LEER &&
	  p[move+11] != LEER &&
	  p[move-11] != LEER &&
	  p[move+9]  != LEER &&
	  p[move-9]  != LEER)
	*pf++ = move;
      else if (Ecke[move])
	*pc++ = move;
      else
	*pr++ = move;
    }      
  }

  *pc = *pr = 0;

  pc = corner_moves;
  while (*pc) *pf++ = *pc++;

  pr = rem_moves;
  while (*pr) *pf++ = *pr++;

  *pf = 0;

}


#endif



#if 0

#define SETZE(d) \
\
    if (p[Pos1=Pos+d] == gegn) {		/* gegnerischer Stein */\
\
      do { Pos1 += d; } while (p[Pos1] == gegn);	/* Strahl absuchen */\
\
      if (p[Pos1] == col) {		/* Zug möglich */\
\
        Pos1 -= d;\
\
        while (Pos1 != Pos) {		/* eingeklemmte Steine rumdrehen,   */\
					/* Pos. merken			    */\
\
	  *pgedreht++ = Pos1; p[Pos1] = eigen;\
\
	  Pos1 -= d;\
	}\
      }\
    }

#else

#define SETZE(d) \
\
    if (p[Pos+d] == gegn) {		/* gegnerischer Stein */\
\
      register Square *r_p=&p[Pos+d+d];\
\
      FOREVER {					                        \
        if (*r_p != gegn) break;		                        \
        r_p += d; \
        if (*r_p != gegn) break;		                        \
        r_p += d; \
        if (*r_p != gegn) break;		                        \
        r_p += d; \
        if (*r_p != gegn) break;		                        \
        r_p += d; \
        if (*r_p != gegn) break;		                        \
        r_p += d; \
        break; \
      }                                                                 \
      if (*r_p == col) {			 /* move possible */	\
        Pos1 = r_p - p - d;						\
        do {						                \
	  *pgedreht++ =  Pos1;						\
	  p[Pos1]     =  col;						\
	  Pos1 -= d;							\
        } while (Pos1 != Pos);						\
      }\
    }



#endif


inline bool EndSetzen(
  BRETT  *pnb,		/* Zeiger auf Brett, auf dem gesetzt wird	*/
  PARTEI eigen,		/* Partei am Zug				*/
  SFPOS	 Pos,		/* Position des zu setzenden Steins		*/
  DELTA  *pd		/* Änderungen an *pnb				*/
)
{
  int   diff, Pos1;
  register Square *p, gegn, col=eigen;
  register Square *pgedreht;	/* Zeiger auf Liste der umgedrehten Steine in *pd */  


#if TEST
BRETT Brett0;

  Brett0 = *pnb;

  if (!ZUG(Pos)) Error("Pos kein Zug in EndSetzen");

if (BrettAnz(pnb, BLACK) - BrettAnz(pnb, WHITE) != pnb->StDiffBW) {
  printf(" %d %d\n", BrettAnz(pnb, BLACK) - BrettAnz(pnb, WHITE), pnb->StDiffBW);
BrettAus(pnb);
  Error("diff ungl 0");
}

if (BrettAnzBW(pnb) != pnb->SteinAnz) { 
  printf("%d %d\n", BrettAnzBW(pnb), pnb->SteinAnz);
  Error("anz ungl 0 ");
}

  if (pnb->umg[Pos] < UMG) Error("huch");	/* keine Umgebungsposition */
#endif


  if (pnb->p[Pos] != LEER) return false;	/* schon besetzt */

  p        = pnb->p;
  pgedreht = pd->Umgedreht; 
  gegn     = GEGNER(eigen);

  SETZE(1); SETZE(-1); SETZE(10); SETZE(-10);
  SETZE(9); SETZE(-9); SETZE(11); SETZE(-11);


  if ((diff=pgedreht-pd->Umgedreht)) {	/* Setzen geht -> Umgebung anpassen */

    *pgedreht = -1;			/* Endmarkierung */

    pnb->SteinAnz++;			/* ein Stein mehr	*/
    pnb->SteinSumme += Pos;

    pd->AltStDiffBW = pnb->StDiffBW;

    if (eigen == BLACK) pnb->StDiffBW += diff+diff;
    else	        pnb->StDiffBW -= diff+diff;

    pnb->StDiffBW += eigen;		/* auch Diff. anpassen	*/

/* Stein auf Pos, muß zum Schluß geschehen; 			*/
/* sonst Probleme, wenn letzte Umgebungsposition besetzt wird	*/

    p[Pos] = eigen; 

#if TEST
if (BrettAnz(pnb, BLACK) - BrettAnz(pnb, WHITE) != pnb->StDiffBW) {
  printf(" %d %d\n", BrettAnz(pnb, BLACK) - BrettAnz(pnb, WHITE), pnb->StDiffBW);
BrettAus(&Brett0);
KoorAus(Pos);
BrettAus(pnb);
  Error("diff ungl");
}

if (BrettAnzBW(pnb) != pnb->SteinAnz) { 
  printf("%d %d\n", BrettAnzBW(pnb), pnb->SteinAnz);
  Error("anz ungl");
}
#endif

    return true;

  }

  return false;
}



/* Zug zurücknehmen */

inline void EndZurueck(BRETT *pnb, PARTEI Partei, SFPOS Pos, DELTA *pd)
{
  Square *p=pnb->p;
  Square *pgedreht=pd->Umgedreht;
  int   gegn = GEGNER(Partei);

  pnb->SteinAnz--;
  pnb->SteinSumme -= Pos;
  pnb->StDiffBW = pd->AltStDiffBW;

  while (*pgedreht >= 0) p[*pgedreht++] = gegn;

  p[Pos] = LEER;
}



/* bei 63 Steinen ist keine Anpassung mehr nötig! -> nur Steindifferenz berechnen */


#if 0


#define SETZDIFF(d) \
\
    if (p[Pos1=Pos+d] == gegn) {			/* gegnerischer Stein */\
\
      do { Pos1 += d; } while (p[Pos1] == gegn);	/* Strahl absuchen */\
\
      if (p[Pos1] == eigen) {				/* Zug möglich */\
\
        Pos1 -= d;\
\
        while (Pos1 != Pos) { StDiffBW += diff; Pos1 -= d; }\
      }\
    }



inline int EndSetzDiff(
  BRETT  *pnb,		/* Zeiger auf Brett, auf dem gesetzt wird	*/
  PARTEI eigen,		/* Partei am Zug				*/
  SFPOS	 Pos		/* Position des zu setzenden Steins		*/
)
{
  register Square gegn, *p = pnb->p;
  register int   diff, Pos1, StDiffBW;

  gegn   = GEGNER(eigen);

  diff   = eigen+eigen;		/* Steindifferenz BW bei Wechsel eines Steins */

  StDiffBW = pnb->StDiffBW;

  SETZDIFF(1); SETZDIFF(-1); SETZDIFF(10); SETZDIFF(-10);
  SETZDIFF(9); SETZDIFF(-9); SETZDIFF(11); SETZDIFF(-11);

  if (StDiffBW != pnb->StDiffBW) StDiffBW += eigen;

  return StDiffBW;
}


#else


/* f a s t e r */


#define SETZDIFF(d) \
\
    if (p[Pos+d] == opp) {			        /* gegnerischer Stein */\
\
      register int   di = diff;\
      register Square *q = &p[Pos+d+d];\
\
      while (*q == opp) { q += d; di += diff; }        /* Strahl absuchen */\
\
      if (*q == col) StDiffBW += di;			/* Zug möglich */\
\
    }



inline int EndSetzDiff(
  BRETT  *pnb,		/* Zeiger auf Brett, auf dem gesetzt wird	*/
  PARTEI eigen,		/* Partei am Zug				*/
  SFPOS	 Pos		/* Position des zu setzenden Steins		*/
)
{
  register Square col=eigen, opp=GEGNER(col), *p = pnb->p;
  register int   diff, StDiffBW;

  diff   = eigen+eigen;		/* Steindifferenz BW bei Wechsel eines Steins */

  StDiffBW = pnb->StDiffBW;

  SETZDIFF(1); SETZDIFF(-1); SETZDIFF(10); SETZDIFF(-10);
  SETZDIFF(9); SETZDIFF(-9); SETZDIFF(11); SETZDIFF(-11);

  if (StDiffBW != pnb->StDiffBW) StDiffBW += eigen;

  return StDiffBW;
}


#endif


 

#define BEWDIFF  (Partei == BLACK ? pbr->StDiffBW : -pbr->StDiffBW)

#define BEWDIFF1ALT \
  (Partei == BLACK ? \
    (pbr->StDiffBW ==  pbr->SteinAnz ?  64 : pbr->StDiffBW) :\
    (pbr->StDiffBW == -pbr->SteinAnz ? -64 :-pbr->StDiffBW))

#define BEWDIFF1 							\
  (pbr->StDiffBW == 0 ? 0 :						\
  (Partei == BLACK ? 							\
    (pbr->StDiffBW > 0 ?   pbr->StDiffBW + (64 - pbr->SteinAnz)  : 	\
			   pbr->StDiffBW - (64 - pbr->SteinAnz)) :	\
    (pbr->StDiffBW > 0 ? -(pbr->StDiffBW + (64 - pbr->SteinAnz))  : 	\
			 -(pbr->StDiffBW - (64 - pbr->SteinAnz)))))

#if TEST

#define BEWTEST 					\
{ int Wert=BEWDIFF1;					\
  if (Wert > 0) Wert += WERTGEWINN;			\
  if (Wert < 0) Wert -= WERTGEWINN;			\
  if (Wert != BewDiff(pbr, Partei)) { 			\
    printf("%d %d\n", Wert, BewDiff(pbr, Partei));	\
    Error("ungl");					\
  }							\
}

#else

#define BEWTEST

#endif


#endif
