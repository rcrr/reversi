// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/* compute transpositions 5.94 */
 
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


#define TEST	false

#if TEST
#define TESTOUT(x) printf(x"\n")
#else
#define TESTOUT(x)
#endif

#define GAMEMAX	100000
#define PAIRMAX	10000

#define MOVEMIN	31
#define MOVEMAX 42


typedef struct {

  GAME 	  game;
  SPFELD  board;
  PARTEI  player;
  int     bdiscs, wdiscs;
  uint4   hash;

} PATH;


typedef struct {

  GAME 	  game1, game2;

} PAIR;







PATH *paths=NULL;
PAIR tpairs[PAIRMAX];


void _abort(void)
{
  exit(10);
}



int movenum;


int compPATH(const void *a, const void *b)
{
  uint4 ha = ((PATH*)a)->hash, hb = ((PATH*)b)->hash;

  if (hb > ha) return  1;
  if (hb < ha) return -1;

  return strncmp(((PATH*)b)->game.Moves, ((PATH*)a)->game.Moves, movenum);

}






int main(int argc, char **argv)
{
  int      i, j, k, l, gamenum, pairs;
  uint4    max;
  GAME	   game1, game2;
  FILE     *fp, *fpout;
  char     *libfile=NULL, *transfile=NULL, newfile[200];
  SPFELD   boards[8];

  if (argc != 4) {

error:

fprintf(stderr, "*** call: otranspo (-make|-inflate) oko-file trans-file\n");

    exit(20);
  }


  libfile   = argv[2];
  transfile = argv[3];


  InitCrt(); 
  InitTabs();


  if  (!strcmp(argv[1], "-make")) { 

  fp = fopen(libfile, "r");
  if (!fp) Error("can't open oko-file");

  fpout = fopen(transfile, "w");
  if (!fpout) Error("can't open trans-file");



  if (!(paths=malloc(GAMEMAX*sizeof(PATH)))) Error("no mem");


  printf("reading games ...\n"); 

  FOR (gamenum, GAMEMAX) {

if ((gamenum % 1000) == 0) { printf("%6d\r", gamenum); fflush(stdout); }

    if (!fReadPackedGame(fp, &paths[gamenum].game)) break;

  }

  printf("\nOK (%d)\n", gamenum);

  if (gamenum >= GAMEMAX) Error("too many games");

  fclose(fp);


for (movenum=MOVEMIN; movenum <= MOVEMAX; movenum++) {

  pairs = 0;

  printf("playing games (%d moves) ... ", movenum); fflush(stdout);

  FOR (i, gamenum) {

    if (movenum > paths[i].game.MoveNum) continue;

    if (paths[i].game.MoveNum < 1) 
      Error("no move!");

    if (SMOVE_MOVE(paths[i].game.Moves[0]) != D3) 
      Error("not normalized!");


    paths[i].player = 
	PlayGame(movenum, &paths[i].game, &paths[i].board);

    paths[i].bdiscs = SfAnzBLACK(&paths[i].board);
    paths[i].wdiscs = SfAnzWHITE(&paths[i].board);

    Transform(&paths[i].board, boards);

    max = 0;

    FOR (j, 8) { 

      k = Hash2Board(&boards[j]);

      if (k > max) max = k;
    } 

    paths[i].hash = max;

  }
 
  printf("OK\n");


  printf("sorting games ... "); fflush(stdout);
  qsort(paths, (size_t) gamenum, sizeof(PATH), compPATH);
  printf("OK\n");


  print
