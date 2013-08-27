// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/* compute stable discs on edge and 2x4, 2.942 */

#include "main.h"
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


#if 0

printf("\n\n/* difference of stable discs on edge (corners=1, other=2)*/\n\n");


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

	if      (j == 0 || j == NUM-1) delta = 1; 
        else                           delta = 2;

        if (k == 0) diff -= delta; else diff += delta;
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

#endif

#if 1

printf("\n\n/* difference of stable discs on 2x4 corner pattern */\n\n");

/*

    stable discs:

        O O O O ->   O O O *   O O O O   O O O O
        O * * *      O O * *   O O * *   O O O * 
        |
        v


    values:
 
       corner
             \___________
             | 3| 4| 6| 6|
             | 4| 6|12|12|


    coding:

        0  1  2  3  4  5  6  7
       -----------------------
       A1 B1 C1 D1 A2 B2 C2 D2

*/



 
  FOR (i, KANTEN_ANZ) {

    int delta[8]= {0,0,0,0,0,2,2,2}, diff;      /* X position not always correct */


    Nummer = i;

    FOR (j, NUM) {			/* Kante aus Nummer erstellen */

      k = Nummer % 3; Nummer /= 3;

      if      (k == 1) K[NUM-1-j] = LEER;
      else if (k == 2) K[NUM-1-j] = BLACK;
      else 	       K[NUM-1-j] = WHITE;

    }  

    Nummer = i; Maske = diff = 0;


    if (K[0] != LEER) {

      Maske |= 1;

      if (K[1] == K[0]) {

        Maske |= 2;

        if (K[2] == K[0]) {

          Maske |= 4;

          if (K[3] == K[0]) {

            Maske |= 8;
	  }
	}
      }

      if (K[4] == K[0]) { 

        Maske |= 16;

        if (K[5] == K[0] && K[1] == K[0] && K[2] == K[0]) {

          Maske |= 32;

          if (K[6] == K[0] && K[3] == K[0]) Maske |= 64;

        }
      }
    }


    FOR (j, NUM) 
      if (Maske & (1<<j))
        if (K[0] == BLACK) diff += delta[j]; else diff -= delta[j]; 

#ifdef XXX
printf("/* ");
KanteAus(K);
printf(" -> ");
MaskeAus(Maske);
printf(" */ ");
#endif

printf("%d,\n", diff);


  }
#endif



}




