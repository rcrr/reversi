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


typedef struct {
  
  int index; int num;

} ENTRY;

#define MAX_BOARD 2000
#define MAX_PATT  2500000

int   PattNum = 0;
ENTRY *Patt;


void _abort(void) {}

int PattComp(const void *a, const void *b)
{
  return ((ENTRY*)b)->num - ((ENTRY*)a)->num;  
}

void PrintPatt(int index, int discs)
{
  int i, p[20];


  FOR (i, discs) {

    p[discs-1-i] = (index % 3) - 1;
    index /= 3;
  }

  FOR (i, discs) {

    if      (p[i] == BLACK) printf("X");
    else if (p[i] == WHITE) printf("O");
    else                    printf("-");

    if (discs == 16 && i == discs/2-1) printf("\n");
  }
}




int AppPatt(int n)
{
  int i;

  FOR (i, PattNum) 

    if (Patt[i].index == n) { Patt[i].num++; break; } 
 
  if (i >= PattNum) {

    Patt[PattNum].index = n;
    Patt[PattNum].num   = 1;
    
    PattNum++;

    if (PattNum >= MAX_PATT) Error("too many patterns");
  }

  return i;
}


#define SQSEQ sqseqC
#define DISCS 16


int main(int argc, char **argv)
{
  int      s, i, j, k, oknum, boardnum;
  FILE     *fpin;
  SPFELD   board, tboards[8];
  sint1    sqseqA[] = { A1,A2,A3,A4,A5,A6,A7,A8,B1,B2,B3,B4,B5,B6,B7,B8, 0 };
  sint1    sqseqB[] = { A1,A2,A3,A4,A5,A6,A7,A8, 0 };
  sint1    sqseqC[] = { C3,C4,C5,C6,D3,D4,D5,D6,E3,E4,E5,E6,F3,F4,F5,F6, 0 };
  
  if (argc != 2) {

error:

fprintf(stderr, 
"*** call: obpat sfk-file\n");

    exit(20);
  }


  fpin = fopen(argv[1], "r");

  if (!fpin) Error("file not found");

  Patt = (ENTRY*) malloc(sizeof(ENTRY) * MAX_PATT);

  if (!Patt) Error("no mem");


  PattNum = 0;

  boardnum = 0;

  FOREVER {

    if (fSfRead(fpin, &board, 1) != 1) break;

    boardnum++;

if (boardnum >= MAX_BOARD) break;

    if (!(boardnum % 1000)) { 
      printf("%7d %7d\r", boardnum, PattNum); fflush(stdout); 
    }

    Transform(&board, tboards);

    FOR (i, 8) { 

      s = 0;

      FOR (j, 16) 
       if (SQSEQ[j]) s = 3*s + (tboards[i].p[SQSEQ[j]]+1);
       else break;

      AppPatt(s);

    }

  }

  fclose(fpin);


  printf("%d Patterns\n", PattNum);

  qsort(Patt, PattNum, sizeof(*Patt), PattComp);

#if 1

  FOR (i, PattNum) {

    PrintPatt(Patt[i].index, DISCS); 
    printf(" %d %d%%%%\n\n", Patt[i].num, round((1000.0*Patt[i].num)/(8*boardnum)));

  }
#endif


  for (k=10;; k++) {

    fpin = fopen(argv[1], "r");

    if (!fpin) Error("file not found");

    oknum = 0;
    boardnum = 0;


    FOREVER  {

      if (fSfRead(fpin, &board, 1) != 1) break;

      boardnum++;

if (boardnum >= MAX_BOARD) break;

      Transform(&board, tboards);

      for (i=0; i < 8; i++) { 

        s = 0;

        FOR (j, 16) 
         if (SQSEQ[j]) s = 3*s + (tboards[i].p[SQSEQ[j]]+1);
         else break;

        if (AppPatt(s) >= k) break;

      }

      if (AppPatt(s) < k) oknum++;
      
    }

    fclose(fpin);

    printf("%d: %d %d %.2f\n", k, oknum, boardnum, (float)oknum*100/boardnum);

  }


  return 0;
}
