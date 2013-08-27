// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/* Killertabelle, 22.11.91 */

#ifndef PRECOMP
#include "main.h"
#endif

#include "killer.h"
#include "move.h"
#include "crt.h"

/* empirisch (mit logit) bestimmte Reihenfolge der Felder */

static SFPOS PosTab[64] = {

/* E */
  A1, A8, H1, H8, 

/* I */
  C3, F3, C6, F6, 

/* H */
  C4, C5, F4, F5, D3, E3, D6, E6, 

/* F */
  C2, F2, B3, G3, B6, G6, C7, F7,
 
/* D */
  D2, E2, B4, G4, B5, G5, D7, E7, 

/* A */
  D1, E1, A4, H4, A5, H5, D8, E8, 

/* B */
  C1, F1, A3, H3, A6, H6, C8, F8, 

/* C */
  B1, G1, A2, H2, A7, H7, B8, G8, 
 
/* X */
  B2, G2, B7, G7,

/* Mitte */

  D4, D5, E4, E5
};


static SFPOS PosTab1[64] = {		/* zufällig */

/* D */
  B4, G4, B5, G5, E2, D7, E7, D2, 

/* Mitte */
  D4, D5, E4, E5,

/* A */
  A4, H4, A5, D1, H5, D8, E8, E1, 

/* F */
  F2, G3, B6, B3, G6, C2, C7, F7,
 
/* I */
  C3, F3, C6, F6, 

/* X */
  G7, B2, G2, B7,

/* E */
  H8, A1, A8, H1, 

/* H */
  E6, D3, C4, C5, F4, F5, E3, D6, 

/* C */
  B1, G1, G8, A2, B8, H2, A7, H7, 
 
/* B */
  F8, F1, A3, C1, H3, A6, H6, C8 

};



static void KillTabInit(KILLTAB *Kill)
{
  int Zug, i;


  FOR_SFPOS10(Zug) 

    FOR (i, 64) Kill->Liste[Zug][i] = PosTab[i];
}


static void KillTabInit1(KILLTAB *Kill)
{
  int Zug, i;


  FOR_SFPOS10(Zug) 

    FOR (i, 64) Kill->Liste[Zug][i] = PosTab1[i];
}



void InitKiller(KILLDAT *pKiller)
{
  int   i;
  SFPOS da[100];


  pKiller->FreiAnz = 64;

  pKiller->KillBLACK = (KILLTAB*) malloc(sizeof(KILLTAB));
  pKiller->KillWHITE = (KILLTAB*) malloc(sizeof(KILLTAB));

  if (!pKiller->KillBLACK || !pKiller->KillWHITE) Error("Speicher Kill");

  
  KillTabInit(pKiller->KillBLACK);
  KillTabInit(pKiller->KillWHITE);

  FOR (i, 64) pKiller->DefaultKill[i] = PosTab[i];

  FOR (i, 100) da[i] = false;

  FOR (i, 64) da[pKiller->DefaultKill[i]] = true;

  FOR_SFPOS10(i) if (!da[i]) { KoorAus(i); Error("Pos. fehlt in default-kill"); }
}


#ifdef xxx
void InitKiller1(KILLDAT *pKiller)
{
  int   i;
  SFPOS da[64];


  pKiller->FreiAnz = 64;

  pKiller->KillBLACK = (KILLTAB*) malloc(sizeof(KILLTAB));
  pKiller->KillWHITE = (KILLTAB*) malloc(sizeof(KILLTAB));

  if (!pKiller->KillBLACK || !pKiller->KillWHITE) Error("Speicher Kill");

  
  KillTabInit1(pKiller->KillBLACK);
  KillTabInit1(pKiller->KillWHITE);

  FOR (i, 64) pKiller->DefaultKill[i] = PosTab1[i];

  FOR (i, 64) da[i] = false;

  FOR (i, 64) da[pKiller->DefaultKill[i]] = true;

  FOR (i, 64) if (!da[i]) { KoorAus(i); Error("Pos. fehlt in default-kill"); }
}
#endif


void FreeKiller(KILLDAT *pKiller)
{
  free((char*)pKiller->KillBLACK);
  free((char*)pKiller->KillWHITE);
}


#ifdef xxx
static void CheckKillTab(KILLTAB *Kill, int nr)
{
  int i, j, s, da[64];
  LIST_ELEM *pl;
  SFPOS Zug;


FOR_SFPOS (Zug) {

s = 0;
  FOR (i, 64) da[i] = 0;

  pl = Kill->Erster[Zug];

  j = 0;

  do {
  
    j++;

if ((char*)pl < (char*) Kill || (char*) pl >= (char*)Kill + sizeof(KILLTAB))
  Error("pl");

    da[pl->Zug] = 1; pl = pl->next;

  } while (pl != Kill->Erster[Zug]);

if (j != 64) Error("nicht 64");

  FOR (i, 64) s += da[i];

  if (s != 64) { 

    FOR (i, 64) { printf("("); KoorAus(i); printf(" %d) ", da[i]); }
    printf(" %d -->%d\n", s, nr);
    KoorAus(Zug); Error("Zug weg");
  }
}
}
#endif



/* Pos in Feld p suchen und löschen */

static void delpos(SFPOS Pos, int FreiAnz, SFPOS *p)
{
  int i;


  FOR (i, FreiAnz) if (p[i] == Pos) break;

  if (i >= FreiAnz) Error("Pos nicht gefunden");

  for (; i < FreiAnz; i++) p[i] = p[i+1];
}



/* Pos vorne in Feld einfügen */

static void inspos(SFPOS Pos, int FreiAnz, SFPOS *p)
{
  int i;


  for (i=FreiAnz-1; i >= 0; i--) p[i+1] = p[i];
  
  p[0] = Pos;
}



/* Position von Pos in Feld p zurück, -1 falls nicht da */

static int killpos(SFPOS Pos, int FreiAnz, SFPOS *p)
{
  int i;


  FOR (i, FreiAnz) if (p[i] == Pos) break;

  if (i >= FreiAnz) return -1; else return i;
}



/* in Killertabs nur freie Positionen! */

void KillerAdjust(KILLDAT *pKiller, SPFELD *psf)
{
  int i, j;


/* 1. Alle Positionen mit Steinen aus Killtabs löschen */

  FOR_SFPOS10(i) 

    if (psf->p[i] != LEER) {

      j = killpos(i, pKiller->FreiAnz, &pKiller->DefaultKill[0]);

      if (j >= 0) {		/* Pos. da */

        delpos(i, pKiller->FreiAnz, &pKiller->DefaultKill[0]);

        FOR_SFPOS10 (j) {
	  delpos(i, pKiller->FreiAnz, &pKiller->KillBLACK->Liste[j][0]);
	  delpos(i, pKiller->FreiAnz, &pKiller->KillWHITE->Liste[j][0]);
	}

	pKiller->FreiAnz--;
      }
    }


/* 2. Freie Pos. bestimmen, die nicht in Tabs vorkommen -> vorne eintragen */

  FOR_SFPOS10(i) 

    if (psf->p[i] == LEER) {

      j = killpos(i, pKiller->FreiAnz, &pKiller->DefaultKill[0]);


      if (j < 0) {	/* Pos. nicht da -> vorne einfügen */
	
	inspos(i, pKiller->FreiAnz, &pKiller->DefaultKill[0]);

        FOR_SFPOS10 (j) {
 
	  inspos(i, pKiller->FreiAnz, &pKiller->KillBLACK->Liste[j][0]);
	  inspos(i, pKiller->FreiAnz, &pKiller->KillWHITE->Liste[j][0]);
	}
	
	pKiller->FreiAnz++;
      }
    }


/* Integritätstest */

if (64-SfAnz(psf) != pKiller->FreiAnz) {
  SfAus(psf, BLACK, 0);
  printf("%d %d ", 64-SfAnz(psf), pKiller->FreiAnz); Error("anz ungl");
}

FOR_SFPOS10 (i)
 
  if (psf->p[i] == LEER) {

    if (killpos(i, pKiller->FreiAnz, &pKiller->DefaultKill[0]) < 0) {

      printf("%d\n", i);
      FOR (i, 64) printf("%d ", pKiller->DefaultKill[i]);
      Error("1");
    } 

    FOR_SFPOS10 (j) {

      if (killpos(i, pKiller->FreiAnz, &pKiller->KillBLACK->Liste[j][0])<0) {

printf("aaa\n");
  SfAus(psf, BLACK, 0);
printf("%d %d %d\n", i, pKiller->FreiAnz, j);

        Error("2");

      }
      if (killpos(i, pKiller->FreiAnz, &pKiller->KillWHITE->Liste[j][0])<0) 
        Error("3");
    }
  }
}



/* auf gegn. Zug beste Erwiderung BestZug in entspr. Tabelle nach vorne */

void KillerUpdate(KILLDAT *pKiller, PARTEI Partei, SFPOS Zug, SFPOS BestZug)
{
  int     i, FreiAnz;
  SFPOS   *p;
  KILLTAB *Kill;


  if (!ZUG(Zug) || !ZUG(BestZug)) Error("Kein Zug in KillerUpdate");

  if (Partei == BLACK) Kill = pKiller->KillBLACK; 
  else 		       Kill = pKiller->KillWHITE;

  
  /* BestZug suchen */

  FreiAnz = pKiller->FreiAnz;
  p       = &Kill->Liste[Zug][0];

  FOR (i, FreiAnz) if (BestZug == p[i]) break;

  if (i >= FreiAnz) { 
    printf("!!!! %d ", BestZug); KoorAus(BestZug); printf(" Zug nicht in Killertabelle ?!?!?!\n"); 
    return;
  }

  /* schon bester? */

  if (i == 0) return;


  /* sonst andere eine Pos. nach hinten und BestZug nach vorne */

  for (; i >= 1; i--) p[i] = p[i-1];
  p[0] = BestZug;
}



#ifdef xxx

typedef struct { int zug; int su; } SORT;


int comp(const void *a, const void *b)
{
  return ((SORT*) b)->su - ((SORT*) a)->su;
}



void KillAuswert(KILLTAB *KillA, KILLTAB *KillB)
{
  int i, Zug;
  SORT r[64];
  LIST_ELEM *pl;


  FOR (i, 64) { r[i].su = 0; r[i].zug = i; }


  FOR (Zug, 64) {

    pl = KillA->Erster[Zug];

    do {

      r[pl->Zug].su += pl->Wert;

      pl = pl->next;

    } while (pl != KillA->Erster[Zug]);
  }

  FOR (Zug, 64) {

    pl = KillB->Erster[Zug];

    do {

      r[pl->Zug].su += pl->Wert;

      pl = pl->next;

    } while (pl != KillB->Erster[Zug]);
  }


  qsort(&r[0], 64, sizeof(SORT), comp);

printf("\n");

  FOR (i, 64) { printf("("); KoorAus(r[i].zug); printf(": %d) ", r[i].su); }

printf("\n\n");

}
#endif


