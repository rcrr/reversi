// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/* Zwei Bewertungsfunktionen vergleichen,	 28.3.92      */
/* zufällig Steine setzen und dann jeweils zwei Spiele machen */

#ifndef PRECOMP
#include "main.h"
#endif


#include "crt.h" 
#include "move.h"
#include "eval.h"
#include "hash.h"
#include "playgm.h"

#define _SFAUS


#define _ZEITBEGRENZUNG

#define ZUFANZ	12	/* so viele Steine zufällig da */
#define ENDANZ	48	/* ab hier wird Sieger ermittelt, >= +2: Diff. */

#define PSTART	0.55
#define VORTIEFE 8

#ifdef ZEITBEGRENZUNG
#define ZUG_DAUER	5.0
#else
#define ZUG_DAUER	1000.0
#endif

#define BEWA	BewA	/* die beiden Mittelspielbewertungsfunktionen */
#define BEWB	BewA

#define TIEFEA	9	/* maximale Rechentiefe von BEWA */
#define QUIETA	true
#define SELA	true

#define TIEFEB	8	/* maximale Rechentiefe von BEWB */
#define QUIETB	true
#define SELB	false



#define CHECK_ZEIT	0.5		/* Dauer, nach der Zeit geprüft wird */

#define V	0
#define U	1
#define G	2


/* globale Variable für zug */

int VERB = 0;

void _abort(void) { exit(1); }


ZUGIO zioA, zioB;

int altwert;
SPFELD altsf;


static ZEIT   LastCheck, StartZeit;
static int    anzBLACK=0, anzWHITE=0;
int z0;



void Check(COMZIO *pcio, ZUGIO *pzio, bool no_count)	/* Zeit-Handler */
{
  static int count = 0;
  ZEIT   AktZeit;


  if (no_count || count-- <= 0) {


    if (no_count) count = 0;
    else          count = pcio->count;


/* nur alle CHECK_ZEIT Zeit testen */ 

    CPUZeit(&AktZeit);

    if (!no_count && ZeitDiff(&AktZeit, &LastCheck) < CHECK_ZEIT) return;

    LastCheck = AktZeit;


    if (pcio->ZugDa) {

      CPUZeit(&AktZeit);

      if (ZeitDiff(&AktZeit, &StartZeit) >= pcio->MaxDauer) {


#if 0
#ifdef OUT2
printf("SIG_TIMEOUT\n");
#endif
#endif
        pcio->timeout = true;

        LONGJMP(pcio->timeout_env);
      }
    }
  }
}




/* zufälliges Spielfeld mit ZufAnz Steinen erzeugen */

void ZufSf(SPFELD *psf0, SPFELD *psf, int ZufAnz)
{
  int		zuganz;
  PARTEI	Partei;
  SFPOS		zug;
  SFPOS		Zuege[64];
 

  if (ZufAnz > 64 || ZufAnz < SfAnz(psf0)) Error("ZufAnz falsch");


  FOREVER {

    Partei = BLACK;
 
    *psf = *psf0;

    while (SfAnz(psf) < ZufAnz) {

      zuganz = SfMoeglZuege(psf, Partei, Zuege);

      if (zuganz >= 0) {

        zug = Zuege[(IRAN >> 3) % zuganz];	/* zufälliger Zug */
 
        SfSetzen(psf, Partei, zug);

      } else if (SfMoeglZuege(psf, GEGNER(Partei), Zuege) == 0) break;

      Partei = GEGNER(Partei);
    }

    if (SfAnz(psf) >= ZufAnz) break;		/* fertig? */
  }
}



/* ein Spiel durchführen, sgn(Steindifferenz(A-B)) zurück, ParteiA beginnt */

int Spiele(
  ZUGIO *pzioA, ZUGIO *pzioB,
  SPFELD *psf, 
  PARTEI ParteiA, 
  int EndAnz, 
  BEWFKTP BewA, BEWFKT BewB,
  int *panzA, int *panzB,
  REAL *ptimeA, REAL *ptimeB
)
{
  PARTEI Partei;
  int	 i, last, zuganz, Steine, r, t;
  SFPOS  Zuege[64], zug;
  ZUGIO  *pzio;
  BEWFKTP BewMitte;
  ZEIT	  EndZeit;


  last = ZUG_UNBEKANNT;

  ClearHashTab(&zioA.hash); ClearHashTab(&zioB.hash); 

altwert = WERTMIN;


  Partei = ParteiA;


  FOREVER {

#ifdef AMIGA
Chk_Abort();
#endif

    Steine = SfAnz(psf);
    zuganz = SfMoeglZuege(psf, Partei, Zuege);


    if (Partei == ParteiA) { 

      pzio = pzioA; BewMitte = BewA;

    } else {

      pzio = pzioB; BewMitte = BewB;

    }

    if (!zuganz) {		/* kein Zug möglich */

      zug = ZUG_PASSEN;

      if (!SfMoeglZuege(psf, GEGNER(Partei), Zuege)) {

	if (ParteiA == BLACK) {

	  *panzA = SfAnzBLACK(psf); *panzB = SfAnzWHITE(psf);

	} else {

	  *panzA = SfAnzWHITE(psf); *panzB = SfAnzBLACK(psf);

	}

	r = *panzA - *panzB;

	if 	(r > 0) return  1;
	else if (r < 0) return -1; 
	else		return  0;
      }

      printf("*"); fflush(stdout);


    } else {


      pzio->Sf		= *psf;
      pzio->Partei	= Partei;
      pzio->LetzterZug  = last;

      if (Steine == EndAnz || Steine == EndAnz+1) {	/* Sieger */

	pzio->cio.ZugDa   = false;		/* noch kein Zug gefunden */
	pzio->BewMitte = BewMitte; 
	pzio->Modus = MODUS_NORMAL;

	for (i=1; i <= 5; i++) {		/* iterative Suche */
	  pzio->MaxTiefe = i;
          Zugermittlung(pzio);
	}

	pzio->Modus = MODUS_GEWINN;
        printf("?"); fflush(stdout);
        Zugermittlung(pzio); 
	zug  = pzio->BestZug;
	

      } else if (Steine > EndAnz) {			/* Differenz */


	pzio->cio.ZugDa   = false;		/* noch kein Zug gefunden */
	pzio->BewMitte = BewMitte; 
	pzio->Modus = MODUS_NORMAL;

	for (i=1; i <= 5; i++) {		/* iterative Suche */
	  pzio->MaxTiefe = i;
          Zugermittlung(pzio);
	}

	pzio->Modus = MODUS_DIFFERENZ;
        printf("!"); fflush(stdout);
        Zugermittlung(pzio); 
	zug  = pzio->BestZug;

      } else {						/* Mittelspiel */

	pzio->BewMitte = BewMitte; 
	pzio->Modus = MODUS_NORMAL;


#ifdef ZEITBEGRENZUNG 

        t = 20;

#else

	if (pzio == &zioA) t = TIEFEA; else t = TIEFEB;

#endif


	if (pzio == &zioA) { 

	  pzio->Quiescence = QUIETA; 
	  pzio->Selektiv   = SELA; 

	} else { 

	  pzio->Quiescence = QUIETB; 
	  pzio->Selektiv   = SELB; 

	}


#ifdef KILLER
        KillerAdjust(&pzio->killer, &pzio->Sf);
#endif

        CPUZeit(&LastCheck); StartZeit = LastCheck;

	pzio->cio.ZugDa   = false;		/* noch kein Zug gefunden */
	pzio->cio.MaxDauer= ZUG_DAUER;

	for (i=1; i <= t; i++) {		/* iterative Suche */

	  pzio->MaxTiefe = i;

          Zugermittlung(pzio);

          if (pzio->cio.timeout) break;	/* Zeit abgelaufen */

	  zug  = pzio->BestZug;

          if (ZUG(zug)) {

            pzio->cio.ZugDa = true;			/* Zug gefunden */
          }


          if (pzio->fertig) break;			/* sonst zu tief! */

	  if (i > 20) break; 

	}

	CPUZeit(&EndZeit);

        if (Partei == ParteiA) *ptimeA += ZeitDiff(&EndZeit, &StartZeit); 
	else		       *ptimeB += ZeitDiff(&EndZeit, &StartZeit);	

        printf(":%d", pzio->MaxTiefe); fflush(stdout);

#ifdef SFAUS
if (pzio == &zioA) printf("\n(A:"); else printf("\n(B:");
printf("%d)", pzio->Wert);
#endif

#if 0
printf("(%d)", pzio->Wert);
fflush(stdout);

if (pzio == &zioA && altwert != WERTMIN && altwert != -pzio->Wert) {
  SfAus(&altsf, BLACK, 0);
  SfAus(psf, BLACK, 0);
  printf("%d\n", Partei);
}

if (pzio == &zioB) { altsf=*psf; altwert = pzio->Wert; }
#endif

 
      }

/*printf("(%.2"R_F")", pzio->Wert);*/

    }


    if (ZUG(zug)) SfSetzen(psf, Partei, zug);

#ifdef SFAUS
SfAus(psf, BLACK, 0);
#endif

    last = zug;

    Partei = GEGNER(Partei); 
  }
}





int main(int argc, char **argv)
{
  int	    retAB, retBA, 
	    sumA =0, sumB =0,
	    sumQA=0, sumQB=0,
	    anz1A, anz1B, 
	    anz2A, anz2B,
	    gewA, unent, gewB,
	    erg[3][3], sum[3][3],
	    i, j, b, spielnr, randstart, spielanz;
  SPFELD    sf0, sf, sfanf;
  PARTEI    Partei;
  REAL	    timeA=0, timeB=0, value;
  char	    s[4] = "VUG";


  if (argc != 3) Error("Aufruf: oturn srand spielanz\n");

  InitCrt();


  if (sscanf(argv[1], "%d", &randstart) != 1 || randstart < 0) 
    Error("srand falsch");

  if (sscanf(argv[2], "%d", &spielanz)  != 1 || spielanz < 1) 
    Error("spielanz falsch");

printf("%d Spiele\n", 2*spielanz);


  InitZug(&zioA, BEWA, Check, HASHBITS);
  InitZug(&zioB, BEWB, Check, HASHBITS);
  SfGrund(&sf0);

  SRAN(randstart);


  FOR (i, 3) FOR (j, 3) erg[i][j] = sum[i][j] = 0;


  FOR (spielnr, spielanz)  {

#ifdef AMIGA
Chk_Abort();
#endif

    printf("\n\n%d:\n", spielnr+1);


/* generate even position */

    do {

      ZufSf(&sf0, &sfanf, ZUFANZ);

      if (IRAN & 32) Partei = BLACK; else Partei = WHITE;

      zioA.Sf	      = sfanf;
      zioA.Partei     = Partei;
      zioA.LetzterZug = ZUG_UNBEKANNT;

      zioA.Modus      = MODUS_NORMAL;

      for (i=1; i <= VORTIEFE; i++) {	
	zioA.MaxTiefe = i;
        Zugermittlung(&zioA);

	if (i == VORTIEFE-1) value = zioA.Wert;

      }

      value = (value+zioA.Wert)/2;

printf("%.2f\n", EXPWERT(WERT_TO_REAL(value)));

    } while (EXPWERT(WERT_TO_REAL(abs(value))) > PSTART);


#ifdef SFAUS
SfAus(&sfanf, BLACK, 0);

if (Partei == BLACK) printf("BLACK\n"); else printf("WHITE\n");
#endif

    b = (IRAN & 64) != 0;

    FOR (i, 2) 

      if (i == b) {

	printf("A-B:");
  
        sf = sfanf;
        retAB = Spiele(&zioA, &zioB, &sf, Partei, ENDANZ, BEWA, BEWB, 
			&anz1A,	&anz1B, &timeA, &timeB);

        printf(" -> %d %d %d %d\n", retAB, anz1A-anz1B, anz1A, anz1B);

      } else {

	printf("B-A:");

        sf = sfanf;
        retBA = Spiele(&zioB, &zioA, &sf, Partei, ENDANZ, BEWB, BEWA, 
			&anz2B, &anz2A, &timeB, &timeA);

        printf(" -> %d %d %d %d\n", retBA, anz2B-anz2A, anz2B, anz2A);
      }


    erg[retAB+1][-retBA+1]++;

    sum[retAB+1][-retBA+1] += anz1A + anz2A - anz1B - anz2B;


    sumA += anz1A + anz2A; sumQA += anz1A*anz1A + anz2A*anz2A;
    sumB += anz1B + anz2B; sumQB += anz1B*anz1B + anz2A*anz2B;

    printf("\nStatistik von BEWA:\n\n");
    printf("  +------V------+------U------+------G------+\n");

    FOR (i, 3) {

      printf("%c |", s[i]); 

      FOR (j, 3) {
 
        if (erg[i][j]) 
	  printf("%4d (%+5.1f) |", erg[i][j], ((REAL)sum[i][j])/(2*erg[i][j])); 
	else
	  printf("     ---     |");

      }

      printf("\n");
    }


    printf("\n  +------V------+------U------+------G------+\n");

    FOR (i, 3) {

      printf("%c |", s[i]); 

      FOR (j, 3) {
 
	if (j < i) {
          if (erg[i][j] + erg[j][i]) 
	    printf("%4d (%+5.1f) |", erg[i][j]+ erg[j][i],
		   ((REAL)(sum[i][j]+sum[j][i]))/(2*(erg[i][j]+erg[j][i]))); 
	  else
	    printf("     ---     |");

	} else if (i == j) {

          if (erg[i][j]) 
	    printf("%4d (%+5.1f) |", erg[i][j],((REAL)sum[i][j])/(2*erg[i][j])); 
	  else
	    printf("     ---     |");


	} else printf("      *      |");

      }

      printf("\n");
    }


    gewA  = 2* erg[G][G] +    erg[G][U] +    erg[G][V] + erg[U][G] + erg[V][G];
    unent =    erg[G][U] + 2* erg[U][U] +    erg[V][U] + erg[U][G] + erg[U][V];
    gewB  =    erg[V][G] +    erg[V][U] + 2* erg[V][V] + erg[U][V] + erg[G][V];


    printf("\n%d (%.2f) - %d - %d (%.2f)   (%.1f%%)\n", 

      gewA, ((REAL)sumA)/2.0/(spielnr+1), 
      unent, 
      gewB, ((REAL)sumB)/2.0/(spielnr+1),

      100.0*(gewA + unent*0.5)/(gewA + unent + gewB)
    );


/* normalized: without games pairs with same results */

    gewA  -= erg[G][V] + erg[V][G];
    gewB  -= erg[G][V] + erg[V][G];
    unent -= erg[U][U];


    if (gewA + unent + gewB) {

      printf("\n%d - %d - %d   (%.1f%%)\n", 

        gewA, unent, gewB, 

        100.0*(gewA + unent*0.5)/(gewA + unent + gewB)
      );
    }






    printf("\nZeitA=%.1f ZeitB=%.1f\n",
		 timeA/2.0/(spielnr+1), 
		 timeB/2.0/(spielnr+1));

  }  

  return 0;
}
