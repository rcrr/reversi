// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/* stabile Kantensteine bestimmen / 20.7.92 */

#ifndef PRECOMP
#include "main.h"
#endif

#include "sboard.h"
#include "tab.h"


#define XXX			/* Kantenausgabe */

#define NUM 8                   /* <= 8 */

#define KANTEN_ANZ (Pot3[NUM])	/* 3 hoch NUM */


typedef char KANTE[NUM];

int SchonDa[6561], NichtStabil[6561], NichtSemiStabil[6561];

void _abort(void) { exit(1); }


void KanteAus(KANTE K)
{
  int i;


  FOR (i, NUM) 

    if      (K[i] == BLACK) printf("X");
    else if (K[i] == WHITE) printf("O");
    else 		    printf("-");
}


void MaskeAus(int Maske)
{
  int i;


  FOR (i, NUM) printf("%d", (Maske & (1 << i)) != 0);
}



int KantenSetzen(KANTE KS, int Pos, PARTEI Partei)
{
  int j, veraendert = 0;


  KS[Pos] = Partei;


  if (Pos >= 2 && KS[Pos-1] == GEGNER(Partei)) {

/* nach links */ 

    j = Pos-1;

    while (j > 0 && KS[j] == GEGNER(Partei)) j--;

    if (KS[j] == Partei) {

      j++;

      while (j < Pos) { veraendert |= (1 << j); KS[j++] = Partei; }
    }
  }


  if (Pos <= NUM-3 && KS[Pos+1] == GEGNER(Partei)) {

/* nach rechts */ 

   j = Pos+1;

   while (j < NUM-1 && KS[j] == GEGNER(Partei)) j++;

   if (KS[j] == Partei) {

     j--;

     while (j > Pos) { veraendert |= (1 << j); KS[j--] = Partei; }
   }
  }

  return veraendert;
}




int Unstab(KANTE K)
{
  KANTE KS;
  int i, j, Nummer, Maske, SemiMaske;


  Nummer = 0;

  for (i=NUM-1; i >= 0; i--) {

    Nummer += Nummer + Nummer;

    if	    (K[i] == LEER)  Nummer += 1;
    else if (K[i] == BLACK) Nummer += 2;
  }

/*printf("  Nr.: %d  %d\n", Nummer, SchonDa[Nummer]);*/

  if (SchonDa[Nummer]) return NichtStabil[Nummer];

  Maske = SemiMaske = 0;

  FOR (i, NUM) {

    if (!K[i]) {

/* BLACK setzen */

      FOR (j, NUM) KS[j] = K[j];

      j = KantenSetzen(KS, i, BLACK);

      SemiMaske |= j;
      Maske     |= j | Unstab(KS);


/* WHITE setzen */

      FOR (j, NUM) KS[j] = K[j];

      j = KantenSetzen(KS, i, WHITE);

      SemiMaske |= j;
      Maske     |= j | Unstab(KS);

    }
  }

  FOR (i, NUM) if (!K[i]) { Maske &= ~(1 << i); SemiMaske &= ~(1 << i); }

  NichtStabil	  [Nummer] = Maske;
  NichtSemiStabil [Nummer] = SemiMaske;
  SchonDa	  [Nummer] = true;

  return Maske;
}



void main(void)
{
  int i, j, k, Maske, SemiMaske, Nummer, daBLACK, daWHITE;
  KANTE K;


/* noch nichts da */

  FOR (i, KANTEN_ANZ) SchonDa[i] = false;


printf("\n\n/* difference of stable discs on edge (corners=2, other=4)*/\n\n");


  FOR (i, KANTEN_ANZ) {

    int delta, diff;


    Nummer = i;

    FOR (j, NUM) {			/* Kante aus Nummer erstellen */

      k = Nummer % 3; Nummer /= 3;

      if      (k == 1) K[j] = LEER;
      else if (k == 2) K[j] = BLACK;
      else 	       K[j] = WHITE;

    }  

    if (!SchonDa[i]) Unstab(K);		/* Wenn nötig, Unstabilität ermitteln */


/* Stabil, falls Stein da und nicht unstabil */

    Nummer = i; Maske = diff = 0;

    FOR (j, NUM) {

      k = Nummer % 3; Nummer /= 3;

      if (k != 1 && (NichtStabil[i] & (1 << j)) == 0) { 

        Maske |= (1 << j);

	if (j == 0 || j == NUM-1) delta = 2; else delta = 4;

        if (k == 0) diff += delta; else diff -= delta;
      }
    }


#ifdef XXX
printf("/* ");
KanteAus(K);
printf(" -> ");
MaskeAus(Maske);
printf(" */ ");
#endif

printf("%d,\n", diff);


  }


printf("\n\n/* difference of stable discs on interior rays (corners=0, other=1)*/\n\n");



  FOR (i, KANTEN_ANZ) {

    int delta, diff;


    Nummer = i;

    FOR (j, NUM) {			/* Kante aus Nummer erstellen */

      k = Nummer % 3; Nummer /= 3;

      if      (k == 1) K[j] = LEER;
      else if (k == 2) K[j] = BLACK;
      else 	       K[j] = WHITE;

    }  

    if (!SchonDa[i]) Unstab(K);		/* Wenn nötig, Unstabilität ermitteln */


/* Stabil, falls Stein da und nicht unstabil */

    Nummer = i; Maske = diff = 0;

    FOR (j, NUM) {

      k = Nummer % 3; Nummer /= 3;

      if (k != 1 && (NichtStabil[i] & (1 << j)) == 0) { 

        Maske |= (1 << j);

	if (j == 0 || j == NUM-1) delta = 0; else delta = 1;

        if (k == 0) diff += delta; else diff -= delta;
      }
    }


#ifdef XXX
printf("/* ");
KanteAus(K);
printf(" -> ");
MaskeAus(Maske);
printf(" */ ");
#endif

printf("%d,\n", diff);


  }



}




