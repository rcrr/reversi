// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/* simple tournament manager, 6.95 */

#include "main.h"
#include <sys/time.h>

#include "playm.h"
#include "pmove.h"
#include "eval.h"
#include "sboard.h"

#include "goodies.h"
#include "filecom.h"
#include "crt.h"
#include "playgm.h"
#include "lib.h"
#include "fpatt.h"
#include "hash.h"


#define B_NEEDS_GO  true

#define OUT         true


#define S_LEN 1000

#define L	0
#define D	1
#define W	2



FILE *fptoA, *fpfromA, *fptoB, *fpfromB; 


void _abort(void)
{

  Enable_Abort = 0;


  printf("*** abort\a\n");


/* kill players */


  fprintf(fptoA, "setup 100\n"); fflush(fptoA);
  fprintf(fptoA, "exit\n"); fflush(fptoA);

  fprintf(fptoB, "setup 100\n"); fflush(fptoB);
  fprintf(fptoB, "exit\n"); fflush(fptoB);

  exit(1);
}




int CharsToMove(char c1, char c2)
{
  int d1, d2;

  c1 = toupper(c1);
  c2 = toupper(c2);

  if (c1 == 'P' && (c2 == 'A' || c2 == 'S')) return 0;

  d1 = c1 - 'A' + 1;
  d2 = c2 - '1' + 1;

  if (d1 < 1 || d1 > 8 || d2 < 1 || d2 > 8) return -1;

  return d2 * 10 + d1;
}




void AutoGame(
  GAME      *pgame,
  FILE      *fptoblack,
  FILE      *fpfromblack, 
  float	    *ptimeBLACK,

  FILE      *fptowhite, 
  FILE      *fpfromwhite, 
  float	    *ptimeWHITE
)
{
  char   c1, c2, c1old, c2old, s[S_LEN+1];
  SFPOS  movelist[65];
  int	 i, n, first, first2, move;
  float  movetime;
  int	 zug, letzter;
  SPFELD board;
  REAL	 Rest;
  PARTEI player;
  FILE   *fpto, *fpfrom;
  ZEIT	 StartZeit, EndZeit;


  player = PlayGame(pgame->MoveNum, pgame, &board);

  if (player != BLACK && player != WHITE) Error("game finished or corrupt");

/* send move sequence to both players */

  FOR (i, pgame->MoveNum) {

    fprintf(fptoblack, "force "); 
    fKoorAus(fptoblack, SMOVE_MOVE(pgame->Moves[i])); 
    fprintf(fptoblack, "\n");
    fflush(fptoblack);

#if 1
fKoorAus(stdout, SMOVE_MOVE(pgame->Moves[i])); fflush(stdout); 
#endif

    fprintf(fptowhite, "force "); 
    fKoorAus(fptowhite, SMOVE_MOVE(pgame->Moves[i])); 
    fprintf(fptowhite, "\n");
    fflush(fptowhite);

    sleep(1);

  }


/* finish game */


  first = 1;

#if B_NEEDS_GO

  first2 = 1;

#else

  first2 = 0;

#endif

  c1 = c2 = c1old = c2old = 0;

  *ptimeBLACK = *ptimeWHITE = 0;

  FOREVER {

    if (player == BLACK) { fpto = fptoblack; fpfrom = fpfromblack; }
    else                 { fpto = fptowhite; fpfrom = fpfromwhite; }


/* send last move or go */

    if (first) fprintf(fpto, "go\n"); 
    else {
      fprintf(fpto, "%c%c\n", c1, c2);

      if (first2 && fpto == fptoB) {
        fprintf(fpto, "go\n"); 
        first2 = 0;
      }
    }     
    fflush(fpto);
    first = 0;


/* read move and time */

    FOREVER {

      if (fgets(s, S_LEN-1, fpfrom)) {

#if OUT
printf("%s", s);
#endif

        if (sscanf(s, "My move: %c%c", &c1, &c2) == 2) {

          FOREVER {

            if (fgets(s, S_LEN-1, fpfrom)) {          
#if OUT
printf("%s", s);
#endif
              if (sscanf(s, "Seconds used: %f", &movetime) == 1) {
                goto loop_exit;
              }
	    }
          } 
        }

      } else sleep(1);

    }

loop_exit:

    if (player == BLACK) *ptimeBLACK += movetime; else *ptimeWHITE += movetime;

    c1 = toupper(c1); c2 = toupper(c2); 

    printf("%c%c", c1, c2); fflush(stdout);

    move = CharsToMove(c1, c2);

    if (move < 0) Error("move corrupt");

    if (SfMoeglZuege(&board, player, movelist)) {

      if (!SfSetzen(&board, player, move)) Error("illegal move");

      pgame->Moves[pgame->MoveNum++] = SMOVE_GEN(move, player);

    } else if (move != 0) Error("must pass");


    player = GEGNER(player);

    if (c1 == 'P' && c1 == c1old && c2 == c2old) {

      fprintf(fptoblack, "q\n"); fflush(fptoblack);
      fprintf(fptowhite, "q\n"); fflush(fptowhite);
      break;
    }

    c1old = c1; c2old = c2;
  }
}



int main(int argc, char **argv)
{
  int	    i, i1, j, j1, k, ZugAnz, Partei, argi, ZufZugAnz, SteinAnz, 
	    Zug, firstnum, game_num, is_black;
  SPFELD    board, tab;
  float	    timeA, timeB, usedA, usedB, totalA=0, totalB=0;
  SFPOS	    Zuege[65];
  ZUGDAT    ZugDat[65];
  char      *datei=NULL;
  FILE      *fpgames;
  GAME      game, game0;
  int	    AnzB[2], AnzW[2],
  	    ret1AB, ret2AB, 
	    sumA =0, sumB =0,
	    sumQA=0, sumQB=0,
	    anz1A, anz1B, 
	    anz2A, anz2B,
	    gewA, unent, gewB,
	    erg[3][3], sum[3][3];

  InitCrt();

  if (argc != 8) Error("call: omana game-file toA fromA toB fromB timeA timeB");

  fpgames = fopen(argv[1], "r");

  if (!fpgames) Error("game-file?");

  fptoA   = fopen(argv[2], "w");
  fpfromA = fopen(argv[3], "r");
  fptoB   = fopen(argv[4], "w");
  fpfromB = fopen(argv[5], "r");

  if (!fptoA || !fpfromA || !fptoB || !fpfromB) Error("open error"); 


  if (sscanf(argv[6], "%f", &timeA) != 1) Error("timeA?");
  if (sscanf(argv[7], "%f", &timeB) != 1) Error("timeB?");

  if (timeA < 10 || timeB < 10) Error("time < 10?");

  FOR (i, 3)
    FOR (j, 3)
      erg[i][j] = sum[i][j] = 0;

  for (game_num=0;;) {

    if (!fReadGame(fpgames, &game0)) break;

    is_black = (MyRand() & 64) == 0;	/* true => Program A */

printf("\nbegin: %c\n", 'B'-is_black);

    FOR (j, 2) {

/* prepare both players */

      fprintf(fptoA, "setup %d\n", (int)timeA); fflush(fptoA);
      fprintf(fptoB, "setup %d\n", (int)timeB); fflush(fptoB);

      sleep(3);

      printf("\nGame %d ", game_num+1);

      game = game0;

      if (is_black ^ j)
        AutoGame(&game, fptoA, fpfromA, &usedA, fptoB, fpfromB, &usedB);

      else
        AutoGame(&game, fptoB, fpfromB, &usedB, fptoA, fpfromA, &usedA);

      totalA += usedA;  totalB += usedB; 

      i = PlayGame(game.MoveNum, &game, &board);
      if (i != 0) Error("game corrupt");

      if (is_black ^ j) printf("\n\nA vs B\n");
      else	        printf("\n\nB vs A\n");

      Game2Tab(&game, &tab);
      fTabAus(stdout, &tab);

      printf("time used (avg.) -  A:%.1f (%.1f)  B:%.1f (%.1f)\n", 
        usedA, totalA/(game_num+1), usedB, totalB/(game_num+1)); 
     
      AnzB[j] = SfAnzBLACK(&board);
      AnzW[j] = SfAnzWHITE(&board); 

/*
      printf("  %d-%d\n\n", AnzB[j], AnzW[j]);
*/
      game_num++;
    }


    anz1A = is_black ? AnzB[0]:AnzW[0]; 
    anz1B = is_black ? AnzW[0]:AnzB[0]; 
    anz2A = is_black ? AnzW[1]:AnzB[1]; 
    anz2B = is_black ? AnzB[1]:AnzW[1]; 

    ret1AB = sgn(anz1A-anz1B);
    ret2AB = sgn(anz2A-anz2B);

    erg[ret1AB+1][ret2AB+1]++;

    sum[ret1AB+1][ret2AB+1] += anz1A + anz2A - anz1B - anz2B;

    sumA += anz1A + anz2A; sumQA += anz1A*anz1A + anz2A*anz2A;
    sumB += anz1B + anz2B; sumQB += anz1B*anz1B + anz2A*anz2B;

    printf("\nstatistics in view of program A:\n\n");
    printf("  +------L------+------D------+------W------+\n");

    FOR (i, 3) {

      char s[4] = "LDW";

      printf("%c |", s[i]); 

      FOR (j, 3) {
 
        if (erg[i][j]) 
	  printf("%4d (%+5.1f) |", erg[i][j], ((REAL)sum[i][j])/(2*erg[i][j])); 
	else
	  printf("     ---     |");

      }

      printf("\n");
    }


    printf("\n  +------L------+------D------+------W------+\n");

    FOR (i, 3) {

      char s[4] = "LDW";

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


    gewA  = 2* erg[W][W] +    erg[W][D] +    erg[W][L] + erg[D][W] + erg[L][W];
    unent =    erg[W][D] + 2* erg[D][D] +    erg[L][D] + erg[D][W] + erg[D][L];
    gewB  =    erg[L][W] +    erg[L][D] + 2* erg[L][L] + erg[D][L] + erg[W][L];


    printf("\n%d (%.2f) - %d - %d (%.2f)   (%.1f%%)\n", 

      gewA, ((REAL)sumA)/game_num, 
      unent, 
      gewB, ((REAL)sumB)/game_num,

      100.0*(gewA + unent*0.5)/(gewA + unent + gewB)
    );


/* normalized: without same result pairs */

    gewA  -= erg[W][L] + erg[L][W];
    gewB  -= erg[W][L] + erg[L][W];
    unent -= 2*erg[D][D];


    if (gewA + unent + gewB) {

      printf("\n%d - %d - %d   (%.1f%%)\n", 

        gewA, unent, gewB, 

        100.0*(gewA + unent*0.5)/(gewA + unent + gewB)
      );
    }

  }

  _abort();

  return 0;
}


