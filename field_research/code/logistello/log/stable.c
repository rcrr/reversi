// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/* Stabilitätsmerkmale für neues Spielbrett / 3.93 */

#include "main.h"
#include "featurem.h"
#include "stable.h"
#include "crt.h"
#include "board.h"

bool f_stabneu = true;
static int  StabB, StabW;




/* Anzahl der stabilen Steine approximieren / 22.7.92 */


#ifdef xxx
/* Kantenpositionen */

static int K1[8] = { A1, A2, A3, A4, A5, A6, A7, A8 };
static int K2[8] = { H1, H2, H3, H4, H5, H6, H7, H8 };
static int K3[8] = { A1, B1, C1, D1, E1, F1, G1, H1 };
static int K4[8] = { A8, B8, C8, D8, E8, F8, G8, H8 };
#endif


static uint1 StabKant[6561+1] = {	/* Kanten-Tabelle für stabile Steine */
#include "stable.inc"
  0
};


uint1 Sicher[100];			/* Sichere Strahlen für jede Pos. */
uint1 StabDa[100];			/* Menge von stabilen Positionen */
int   StabPos[100], StabNeu[100];	/* Listen von stabilen Positionen */
int   stp, stn;				/* Längen der Listen		 */


#define W(k, w) \
  {\
    if      (pp[k] == BLACK) Nr += w;\
    else if (pp[k] == WHITE) Nr += w+w;\
  }


#define ST(k, m) \
  {\
     if ((i & m) && !pStabDa[k]) {\
\
	pStabDa[k]     = true;\
        StabPos[stp++] = k;\
     }\
  }


#define KANTE(k1, k2, k3, k4, k5, k6, k7, k8) \
\
  Nr = 0;\
\
  W(k1,  1); W(k2,   3); W(k3,   9); W(k4,   27);\
  W(k5, 81); W(k6, 243); W(k7, 729); W(k8, 2187);\
\
  if ((i=StabKant[Nr]) != 0) {	/* sind Steine stabil? */\
\
    ST(k1, 0x01); ST(k2, 0x02); ST(k3, 0x04); ST(k4, 0x08);\
    ST(k5, 0x10); ST(k6, 0x20); ST(k7, 0x40); ST(k8, 0x80);\
  }


#define STRAHL_ANZ 46	/* Anzahl der Strahlen des Spielfeldes */

#undef X
#define X(Pos) ((Pos) % 10 - 1)
#define Y(Pos) ((Pos) / 10 - 1)

#define SI(Pos) \
\
    if ((a=pp[Pos]) == BLACK || a == WHITE) {\
\
      p = &pSicher[Pos];\
\
      if (pStrahl[X(Pos)])	       *p |= (1<<2) | (1<<6);\
      if (pStrahl[8+Y(Pos)])	       *p |= (1<<0) | (1<<4);\
      if (pStrahl[16+7+X(Pos)-Y(Pos)]) *p |= (1<<3) | (1<<7);\
      if (pStrahl[31+X(Pos)+Y(Pos)])   *p |= (1<<1) | (1<<5);\
\
      if (*p == 255) {\
\
        pStabDa[Pos]   = true;\
        StabPos[stp++] = Pos;\
\
      }\
    }




/* untere Schranke für Anzahl stabiler Steine bestimmen */

void StabAnz(BRETT *pb, int *pBLACK, int *pWHITE)
{
  int   a, i, Pos, Pos0, x, y, Partei, r, diff, AnzB=0, AnzW=0, Nr;
  uint1 Strahl[STRAHL_ANZ], *pStrahl=Strahl;
  uint1 *p, *ps, *pSicher=Sicher, *pStabDa=StabDa;
  SFPOS *pp=pb->p;
  UMGEB *pu=pb->umgl;


  stp = 0;

  memset(StabDa, false, sizeof(StabDa));
  memset(Sicher, 0,     sizeof(Sicher));


/* 
  4 Kantenzahlen bestimmen und in Tabelle nachsehen, 
  ob es stabile Steine gibt
*/

  KANTE(A1, A2, A3, A4, A5, A6, A7, A8);
  KANTE(H1, H2, H3, H4, H5, H6, H7, H8);
  KANTE(A1, B1, C1, D1, E1, F1, G1, H1);
  KANTE(A8, B8, C8, D8, E8, F8, G8, H8);

 
/* Steine mit 4 vollen Strahlen sind auch stabil */

  memset(Strahl, true, sizeof(Strahl));


 /* Strahlen löschen, auf denen Umgebungspos. nicht besetzt ist (reicht) */

  a  = pb->LastIndex+1;

  FOR (i, a) {

    x = X(r=pu[i].UPos); y = Y(r);

    pStrahl[x] = pStrahl[8+y] = pStrahl[16+7+x-y] = pStrahl[31+x+y] = false;
  }


/* bei inneren besetzten Pos. testen, ob alle ihre Strahlen voll sind */


  SI(G2); SI(F2); SI(E2); SI(D2); SI(C2); SI(B2); 
  SI(G3); SI(F3); SI(E3); SI(D3); SI(C3); SI(B3);
  SI(G4); SI(F4); SI(E4); SI(D4); SI(C4); SI(B4);
  SI(G5); SI(F5); SI(E5); SI(D5); SI(C5); SI(B5);
  SI(G6); SI(F6); SI(E6); SI(D6); SI(C6); SI(B6);
  SI(G7); SI(F7); SI(E7); SI(D7); SI(C7); SI(B7);



/* solange neue stabile Steine da sind ... */

  while (stp) {

    stn = 0;

    FOR (i, stp) {				/* alle neuen abklappern */

      Pos0 = StabPos[i];

      Partei = pp[Pos0];

      if (Partei == BLACK) AnzB++; else AnzW++;	/* stabile Steine zählen */


#if TEST
if (Partei != BLACK && Partei != WHITE) Error("Partei merkst");
#endif

      FOR (r, 8) {				/* 8 Richtungen abklappern */

	Pos = Pos0 + (diff=ds[r]);
	p   = &pSicher[Pos];
	ps  = &pStabDa[Pos];
 
	while ((a=pp[Pos]) == Partei) {

/* Kette eigener Steine zuende? */


	  if (!*ps) {				/* noch nicht stabil? */


						/*   O O O O O   */	
						/*   s    <><>   */ 

	    *p |= (1<<r);			/* r eintragen */

	    if (*p == 255) {			/* alle Richtungen sicher? */

	      StabNeu[stn++] = Pos;		/* neue stabile Position */
	      *ps	     = true;

	      goto next;			/* schon stabil! */
	    }

	    					/* ~r noch nicht sicher? */
						/*   O O X X	*/
						/*   s  <-<-	*/
     
	    *p |= (1<<(r ^ 4));			/* ~r eintragen */

	    if (*p == 255) {			/* alle Richtungen sicher? */

	      StabNeu[stn++] = Pos;		/* neue stabile Position */
	      *ps	     = true;

	    }
	  }
next:
 	  Pos += diff; p += diff; ps += diff;
	}



	while ((a=pp[Pos]) == BLACK || a == WHITE) {

/* Kette eigener Steine zuende! */

	  if (!*ps) {				/* noch nicht stabil? */


	    					/* ~r noch nicht sicher? */
						/*   O O X X	*/
						/*   s  <-<-	*/
     
	    *p |= (1<<(r ^ 4));			/* ~r eintragen */

	    if (*p == 255) {			/* alle Richtungen sicher? */

	      StabNeu[stn++] = Pos;		/* neue stabile Position */
	      *ps            = true;
	    }
	  }

 	  Pos += diff; p += diff; ps += diff;
	}
      }
    }

    memcpy(StabPos, StabNeu, stn*sizeof(StabPos[0]));

    stp = stn;
  }

  *pBLACK = AnzB; *pWHITE = AnzW;
}




REAL STABDIFF(BRETT *pb)
{
  if (f_stabneu) StabAnz(pb, &StabB, &StabW);

  return StabB-StabW;
}

MERKMAL_B STABDIFF_B = { STABDIFF, "STABDIFF" };



REAL STABB(BRETT *pb)
{
  if (f_stabneu) StabAnz(pb, &StabB, &StabW);

  return StabB;
}

MERKMAL_B STABB_B = { STABB, "STABB" };



REAL STABW(BRETT *pb)
{
  if (f_stabneu) StabAnz(pb, &StabB, &StabW);

  return StabW;
}

MERKMAL_B STABW_B = { STABW, "STABW" };



/**********************************************************************/

#ifdef xxx

/* stabile Steine auf Kanten zählen, ohne Ecken */


void KANTEEINF(int *K, int *pBLACK, int *pWHITE, BRETT *pb)
{
  int i, Nr, j, Pos;


  Nr = 0;

/* Index der Kante bestimmen */

  for (i=7; i >= 0; i--) {

    Nr += Nr + Nr;

    Pos = K[i];
 
    if      (pb->p[Pos] == BLACK) Nr += 1; 
    else if (pb->p[Pos] == WHITE) Nr += 2;
  }

  if ((j=StabKant[Nr]) != 0) {	/* sind Steine stabil? */

    for (i=1; i <= 6; i++) {	/* keine Ecken! */

      if (j & (1 << i)) { 

	if (BR_INH(pb, Pos) == BLACK) (*pBLACK)++; else (*pWHITE)++;
      }
    }
  }
}



REAL STKDIFF(BRETT *pb)
{
  int AnzBLACK=0, AnzWHITE=0;

/* 
  4 Kantenzahlen bestimmen und in Tabelle nachsehen, 
  ob es stabile Steine gibt
*/


  KANTEEINF(K1, &AnzBLACK, &AnzWHITE, pb); 
  KANTEEINF(K2, &AnzBLACK, &AnzWHITE, pb); 
  KANTEEINF(K3, &AnzBLACK, &AnzWHITE, pb); 
  KANTEEINF(K4, &AnzBLACK, &AnzWHITE, pb); 

  return AnzBLACK-AnzWHITE;
}

MERKMAL_B STKDIFF_B = { STKDIFF, "STKDIFF" };
#endif




/* Stabile Richtung innerer, nicht stabiler Steine summieren */

int BitAnz(int n)
{
  int anz = 0;

  while (n) { n &= n-1; anz++; }

  return anz;
}


REAL STABRI(BRETT *pb)
{
  int su = 0, i, a;


  if (f_stabneu) StabAnz(pb, &StabB, &StabW);


  FOR (i, INNENANZ) {

    if (!StabDa[Innen[i]] && (a=BitAnz(Sicher[Innen[i]])) >= 7) {

      if (pb->p[Innen[i]] == BLACK) su ++;
      else 			    su --;
    }
  }

  return su;
}

MERKMAL_B STABRI_B = { STABRI, "STABRI" };




/********** semistabile Kantensteine *************/


#if 0

static sint1 SemiKant[6561+1] = {	/* Kanten-Tabelle für semistabile Steine */
#include "o_.sem"
  0
};


#define SKANTE(k1, k2, k3, k4, k5, k6, k7, k8) \
\
  Nr = 0;\
\
  W(k1,  1); W(k2,   3); W(k3,   9); W(k4,   27);\
  W(k5, 81); W(k6, 243); W(k7, 729); W(k8, 2187);\
\
  Wert += SemiKant[Nr];



/* Anzahldifferenz semistabiler Kantensteine bestimmen */

REAL SEMIDIFF(BRETT *pb)
{
  int   Wert=0, Nr;
  SFPOS *pp=pb->p;


  SKANTE(A1, A2, A3, A4, A5, A6, A7, A8);
  SKANTE(H1, H2, H3, H4, H5, H6, H7, H8);
  SKANTE(A1, B1, C1, D1, E1, F1, G1, H1);
  SKANTE(A8, B8, C8, D8, E8, F8, G8, H8);

  return Wert;
}


MERKMAL_B SEMIDIFF_B = { SEMIDIFF, "SEMIDIFF" };

#endif
