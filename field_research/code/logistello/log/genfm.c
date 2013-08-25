// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/* generate tables for new move-algo 7/95 */

#ifndef PRECOMP
#include "main.h"
#endif

#include "lib.h"
#include "sboard.h"
#include "crt.h"
#include "goodies.h"
#include "game.h"
#include "tab.h"
#include "patt.h"
#include "trans.h"

#define XXX 0                  /* line output */

#define KANTEN_ANZ (Pot3[Num])  /* 3 ^ Num */


typedef signed char KANTE[8];



int Num = 8;


void _abort(void)
{
  exit(10);
}

void SignalCheck(COMZIO *pcio, ZUGIO *pzio, bool no_count) {}


void KanteAus(KANTE K)
{
  int i;


  FOR (i, Num) 

    if      (K[i] == BLACK) printf("X");
    else if (K[i] == WHITE) printf("O");
    else                    printf("-");
}


void MaskeAus(int Maske)
{
  int i;


  FOR (i, Num) printf("%d", (Maske & (1 << i)) != 0);
}

int KantenSetzen(KANTE KS, int Pos, PARTEI Partei)
{
  int j, veraendert = 0;


  if (KS[Pos] != LEER) return 0;


  if (Pos >= 2 && KS[Pos-1] == GEGNER(Partei)) {

/* nach links */ 

    j = Pos-1;

    while (j > 0 && KS[j] == GEGNER(Partei)) j--;

    if (KS[j] == Partei) {

      j++;

      while (j < Pos) { veraendert |= (1 << j); KS[j++] = Partei; }
    }
  }


  if (Pos <= Num-3 && KS[Pos+1] == GEGNER(Partei)) {

/* nach rechts */ 

    j = Pos+1;

    while (j < Num-1 && KS[j] == GEGNER(Partei)) j++;

    if (KS[j] == Partei) {

      j--;

      while (j > Pos) { veraendert |= (1 << j); KS[j--] = Partei; }
    }
  }

  if (veraendert) KS[Pos] = Partei;

  return veraendert;
}



int main(int argc, char **argv)
{
  int      i, j, k, l, gamenum, pairs, player, len, pos, r, a, ka, num;
  uint4    max;
  GAME	   game1, game2;
  FILE     *fp, *fpout;
  char     *libfile=NULL, *transfile=NULL, *evalfile=NULL, newfile[200];
  SPFELD   boards[8];
  ZUGIO    zio;
  KANTE    K, K0;
  
  if (argc != 3) {

error:

fprintf(stderr, 
"*** call: ogenf col num\n");

    exit(20);
  }

  player = (toupper(argv[1][0]) == 'B') * 2 - 1;
  num = atoi(argv[2]);

  printf("/* %s  %d discs */\n\n", player == BLACK ? "black" : "white", num);

  InitZug(&zio, NULL, SignalCheck, 10);

  for (len=num; len <= num; len++) {

    Num = len;


      FOR (ka, KANTEN_ANZ) {

        int Nummer = ka, j;

        FOR (i, Num) {                      /* Kante aus Nummer erstellen */

          k = Nummer % 3; Nummer /= 3;

          if      (k == 1) K0[i] = LEER;
          else if (k == 2) K0[i] = BLACK;
          else             K0[i] = WHITE;

        } 

        FOR (pos, 8) {

          int index;

          memcpy(K, K0, sizeof(K0));

          K[pos] = LEER;  /* !!! important, see NewMove */

          l = r = 0;

          if (pos < Num &&  
              (a=KantenSetzen(K, pos, player))) {

            i = pos-1;
            while (i >= 0 && K[i] != K0[i]) i--;
            l = pos-i-1;

            i = pos+1;
            while (i < Num && K[i] != K0[i]) i++;
            r = i-pos-1;

          }

          if (pos > 0) index = r * pos + l; else index = r;

#if XXX
KanteAus(K0); 
printf("%5d %3d pos=%d l=%d r=%d: i=%d\n", ka - (KANTEN_ANZ-1)/2, player, pos, l, r,  index);
        }
#else

printf("%d,", index);
       }
printf("\n");
#endif

  }
}

  return 0;
}
