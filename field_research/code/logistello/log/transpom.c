// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

// transposition stuff 5.94 / 7.96 

#include "main.h"
#include "lib.h"
#include "sboard.h"
#include "crt.h"
#include "goodies.h"
#include "game.h"
#include "tab.h"
#include "patt.h"
#include "trans.h"


#define TEST	false
#define UNIQ    true  /* only write different games */

#if TEST
#define TESTOUT(x) printf(x"\n")
#else
#define TESTOUT(x)
#endif

#define GAMEMAX	1500000
#define PAIRMAX	80000

#define MOVEMIN	5
#define MOVEMAX 37


typedef struct {

  NewGame game;
  SPFELD  board;
  PARTEI  player;
  int     bdiscs, wdiscs;
  uint4   hash;

} PATH;


typedef struct {

  NewGame game1, game2;
  bool seen1, seen2;
  bool stored1, stored2;

} PAIR;



typedef struct _LELEM {

  int index;
  int number;
  struct _LELEM *succ;

} LELEM;


#define HNUM 2048

LELEM *buckets[61][HNUM];



PATH *paths=0;
PAIR *tpairs=0;


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

  return 
    strncmp((char*)&(((PATH*)b)->game.get_pm(0)), 
	    (char*)&(((PATH*)a)->game.get_pm(0)), 
	    movenum);
}



/* read transposition pairs */

int readpairs(char *transfile)
{
  int tnum;
  FILE *fp;

  fp = fopen(transfile, "r");
  if (!fp) Error("can't open trans-file");

  for (tnum=0; ; tnum++) {

    if (tnum >= PAIRMAX) Error("too many pairs");

    if (!tpairs[tnum].game1.f_read_packed(fp)) break;

    if (tpairs[tnum].game1.get_value() != 100) {
      printf("%d ", tnum);
      Error("no trans-file 100!");
    }

    tpairs[tnum].game2.f_read_packed(fp);

    if (tpairs[tnum].game2.get_value() < 0 ||
	tpairs[tnum].game2.get_value() > 7) Error("no trans-file 07!");

    tpairs[tnum].seen1 = tpairs[tnum].seen2 = false;
  }


  fclose(fp);

  return tnum;
}






/* determine existing prefixes using buckets, return number */

int prefixes(char *libfile, int tnum)
{
  int  i, j=0, h, gamenum;
  FILE *fp;
  LELEM *p;
  NewGame *pg=NULL;

  fp = fopen(libfile, "r");
  if (!fp) Error("can't open lib-file");

  for (gamenum=0;; gamenum++) {

    NewGame game;
 
    if ((gamenum % 1000) == 0) { printf("%6d\r", gamenum); fflush(stdout); }

    if (!game.f_read_packed(fp)) break;

    if (game.get_move_num() > 0 && game.get_pm(0).get_move() != D3) 
      Error("not normalized!");

    h = 0;

    FOR (i, game.get_move_num()) {

      if ((p=buckets[i][h % HNUM])) {

        while (p) {

	  if      (p->number == 1) pg = &tpairs[p->index].game1;
	  else if (p->number == 2) pg = &tpairs[p->index].game2;
	  else Error("number corrupt");

	  for (j=pg->get_move_num()-1; j >= 0; j--)
	    if (game.get_pm(j) != pg->get_pm(j)) break;

	  if (j < 0) {

            if (p->number == 1) tpairs[p->index].seen1 = true;
	    else                tpairs[p->index].seen2 = true;
	  } 
  
          p = p->succ;
        }
      }

      h += game.get_pm(i).get_raw();
    }
  }

  fclose(fp);


  // move existing pairs to begin of pair-array => speed

  i = 0;

  FOR (j, tnum) 

    if (tpairs[j].seen1 && tpairs[j].seen2) tpairs[i++] = tpairs[j];

  return i;

}


static void AppendGame(NewGame *pg, int index, int number)
{
  int i, mnum=pg->get_move_num(), h=0;
  LELEM *pnew;

  FOR (i, mnum) h += pg->get_pm(i).get_raw();

  h %= HNUM;

  if (!(pnew=(LELEM*)malloc(sizeof(LELEM)))) Error("no mem");

  pnew->succ = NULL;
  pnew->index = index;
  pnew->number = number;

  if (buckets[mnum][h]) pnew->succ = buckets[mnum][h];

  buckets[mnum][h] = pnew;
}



void MakeBuckets(int tnum)
{
  int i, j;

  FOR (i, 61) 
    FOR (j, HNUM) buckets[i][j] = NULL;

  FOR (i, tnum) {
    AppendGame(&tpairs[i].game1, i, 1);
    AppendGame(&tpairs[i].game2, i, 2);
  }
}


// my quicksort (Sedgewick)


#define STACK_SIZE 1000

static int stack[STACK_SIZE], si;

#define PUSH(x) \
 { stack[si++] = x; if (si >= STACK_SIZE-1) Error("qsort stack"); }

#define POP stack[--si]


int partition(PATH *p, int l, int r)
{
  int i=l, j;
  PATH v, t;


  v = p[r]; i = l-1; j = r;
    
  for (;;) {

    while (compPATH(&p[++i], &v) < 0);
    while (compPATH(&p[--j], &v) > 0);
  
    if (i >= j) break;

    t = p[i]; p[i] = p[j]; p[j] = t;

  }

  t = p[i]; p[i] = p[r]; p[r] = t;


  return i;
}



void ownqsort(PATH *p, int n) 
{
  int i, l, r;

  l = 0; r = n-1; si = 0;

  FOREVER {

    while (r > l) {

// cout << '*' << r << ' ' << l << flush;

      i = partition(p, l, r);

      if (i-1 > r-i) { PUSH(l); PUSH(i-1); l = i+1; }
      else           { PUSH(i+1); PUSH(r); r = i-1; }

    }

    if (!si) return;
 
    r = POP; l = POP;
  }
}


void WriteUniqGame(LIBRARY *pT, NewGame &game, FILE *fpout)
{
  bool alt = game.is_alternative();

  game.is_alternative(false);

  if (AppendGameToLibrary(pT, game)) { 

#if 0
    if (!game.ok()) {
      game.f_write(stdout);  Error("\a");
    }
#endif

    game.is_alternative(alt);
    if (!game.f_write_packed(fpout)) Error("write error");
  }

  game.is_alternative(alt);

}


int main(int argc, char **argv)
{
  int      i, j, l, gamenum, pairs;
  uint4    k, max;
  NewGame  game1, game2;
  FILE     *fp, *fpout;
  char     *libfile=NULL, *transfile=NULL, *evalfile=NULL, newfile[200];
  SPFELD   boards[8];

  if (argc < 4 || argc > 5) {

    error:

    fprintf(stderr, 
      "*** call: otranspo (-make|-inflate|-reduce) oko-file trans-file [eval-file]\n");

    exit(20);
  }

  libfile   = argv[2];
  transfile = argv[3];
  evalfile  = argv[4];

  tpairs = (PAIR*) malloc(PAIRMAX*sizeof(PAIR));
  if (!tpairs) Error("mem");

  InitCrt(); 

  if  (!strcmp(argv[1], "-make")) { 

  fp = fopen(libfile, "r");
  if (!fp) Error("can't open oko-file");

  fpout = fopen(transfile, "w");
  if (!fpout) Error("can't open trans-file");

  if (!(paths=(PATH*)malloc(GAMEMAX*sizeof(PATH)))) Error("no mem");

  printf("reading games ...\n"); 

  for (gamenum=0;; gamenum++) {

    if ((gamenum % 1000) == 0) { printf("%6d\r", gamenum); fflush(stdout); }

    if (gamenum >= GAMEMAX) Error("too many games");

    if (!paths[gamenum].game.f_read_packed(fp)) break;
  }

  printf("\nOK (%d)\n", gamenum);


  fclose(fp);


  for (movenum=MOVEMIN; movenum <= MOVEMAX; movenum++) {

  pairs = 0;

  printf("playing games (%d moves) ... ", movenum); fflush(stdout);

  FOR (i, gamenum) {

    if (movenum > paths[i].game.get_move_num()) { 
      paths[i].bdiscs = 100+i;  // not equal to any other board
      paths[i].hash   = random();
      continue;
    }

    if (paths[i].game.get_move_num() < 1) Error("no move!");

    if (paths[i].game.get_pm(0).get_move() != D3) Error("not normalized!");

    paths[i].player = paths[i].game.play(movenum, paths[i].board);

    paths[i].bdiscs = SfAnzBLACK(&paths[i].board);
    paths[i].wdiscs = SfAnzWHITE(&paths[i].board);

    Transform(&paths[i].board, boards);

    max = 0;

    FOR (j, 8) { 

      k = HashEntry::sboard_lock(&boards[j]);

      if (k > max) max = k;
    } 

    paths[i].hash = max;

  }
 
  printf("OK\n");

  printf("sorting games ... "); fflush(stdout);

/*
  qsort(paths, (size_t) gamenum, sizeof(PATH), compPATH);
*/

  ownqsort(paths, gamenum);

  printf("OK\n");


  printf("searching transpositions ...\n");

  FOR (i, gamenum-1) {

    if ((i % 1000) == 0) { 
      printf("%6d game(s) %6d pair(s)\r", i, pairs); fflush(stdout); 
    }

    if (paths[i].game.get_move_num() < movenum) continue;

    while (i < gamenum-1 && !compPATH(&paths[i], &paths[i+1])) i++;

    if (i < gamenum-1 && paths[i].hash == paths[i+1].hash) {

      Transform(&paths[i].board, boards);

      for (j=i+1; j < gamenum && paths[i].hash == paths[j].hash; j++) {

	while (j < gamenum-1 && !compPATH(&paths[j], &paths[j+1])) j++;

        if (paths[i].player == paths[j].player && 
  	    paths[i].bdiscs == paths[j].bdiscs &&
	    paths[i].wdiscs == paths[j].wdiscs) {

	  FOR (k, 8) {

	    sint1 *p1 = paths[j].board.p, *p2 = boards[k].p;

	    if (p1[D4] != p2[D4] || p1[D5] != p2[D5] || 
	        p1[E4] != p2[E4] || p1[E5] != p2[E5]) continue;

	    for (l=11; l <= 88; l++) if (*p1++ != *p2++) break;
	    if (l > 88) break;
	  }

	  if (k < 8) {

	    int    player1, player2;
	    SPFELD bo[8], board1, board2;

            /* confluent only if predecessors are not equivalent */

	    player1 = paths[i].game.play(movenum-1, board1);
	    player2 = paths[j].game.play(movenum-1, board2);

	    if (player1 != player2) continue;

	    Transform(&board2, bo);

	    FOR (l, 8) {

	      int   q;
	      sint1 *p1 = board1.p, *p2 = bo[l].p;

	      for (q=11; q <= 88; q++) if (*p1++ != *p2++) break;
	      if (q > 88) break;

	    }

	    if (l < 8) continue;

            // confluent!

	    game1 = paths[i].game; 
	    game1.set_move_num(movenum); 
	    game1.get_pm(movenum).set_raw(0);
	    game1.set_value(100);

	    game2 = paths[j].game; 
	    game2.set_move_num(movenum);
	    game2.get_pm(movenum).set_raw(0);
	    game2.set_value(k);

	    game1.f_write_packed(fpout);
	    game2.f_write_packed(fpout);

	    pairs++;

/*
printf("%ux %ux %d\n", paths[i].hash, paths[j].hash, 
compPATH(&paths[i], &paths[j]));
*/

	  }
        }
      }
    }
  }

  printf("%6d game(s) %6d pair(s)\r", gamenum, pairs); fflush(stdout); 
  printf("\n");

}

  fclose(fpout);

  } else if (!strcmp(argv[1], "-inflate")) { 

    int tnum, move, player, gamenum;

#if UNIQ
    LIBRARY *pT = NewLibrary();
#endif


/* inflate library: use transfile to duplicate equivalent paths
 *
 * important: duplicate only if the two prefixes of a pair are played!
 *
 */

    tnum = readpairs(transfile);
    MakeBuckets(tnum);
    tnum = prefixes(libfile, tnum);
    MakeBuckets(tnum);

    if (evalfile) {

      sprintf(newfile, "%s.inf", evalfile);

      fpout = fopen(newfile, "w");
      if (!fpout) Error("can't open inf-file");

      fp = fopen(evalfile, "r");
      if (!fp) Error("can't open eval-file");

    } else {

      sprintf(newfile, "%s.inf", libfile);

      fpout = fopen(newfile, "w");
      if (!fpout) Error("can't open inf-file");

      fp = fopen(libfile, "r");
      if (!fp) Error("can't open lib-file");
    }


    for (gamenum=0;; gamenum++) {

      int r;
      NewGame game, gameout;


      if (!game.f_read_packed(fp)) break;

      r = game.get_value();

      if (game.get_move_num() > 0 && game.get_pm(0).get_move() != D3) 
	Error("not normalized!");

      if ((gamenum % 1000) == 0) { printf("%6d\r", gamenum); fflush(stdout); }

#if UNIQ
      WriteUniqGame(pT, game, fpout);
#else
      fWritePackedGame(fpout, &game);
#endif

      if (game.get_value() != r) puts("*");

#if 0

old

      FOR (i, tnum) if (tpairs[i].seen1 && tpairs[i].seen2) {

        if (game.get_move_num() >= tpairs[i].game1.get_move_num()) {

          for (j=tpairs[i].game1.get_move_num()-1; j >= 0; j--)
	    if (game.get_pm(j] != tpairs[i].game1.get_pm(j]) break;

	  if (j < 0) {

	    gameout = game;

	    FOR (j, tpairs[i].game1.get_move_num())
	      gameout.get_pm(j] = tpairs[i].game2.get_pm(j];

	    for (j=tpairs[i].game1.get_move_num(); j < game.get_move_num(); j++) {

	      player = SMOVE_PLAYER(game.get_pm(j]);
	      move   = SMOVE_MOVE  (game.get_pm(j]);

	      gameout.get_pm(j).set(player, Trans[tpairs[i].game2.get_value()](move));
	    }

#if UNIQ
	    WriteUniqGame(pT, gameout, fpout);
#else
	    fWritePackedGame(fpout, &gameout);
#endif

	    if (gameout.DiscDiffBW != r) puts("*");

	  }
	}

        if (game.get_move_num() >= tpairs[i].game2.get_move_num()) {

          for (j=tpairs[i].game2.get_move_num()-1; j >= 0; j--)
	    if (game.get_pm(j] != tpairs[i].game2.get_pm(j]) break;

	  if (j < 0) {

	    gameout = game;

	    FOR (j, tpairs[i].game2.get_move_num())
	      gameout.get_pm(j] = tpairs[i].game1.get_pm(j];

	    for (j=tpairs[i].game2.get_move_num(); j < game.get_move_num(); j++) {
	      player = SMOVE_PLAYER(game.get_pm(j]);
	      move   = SMOVE_MOVE  (game.get_pm(j]);
	      gameout.get_pm(j] =
		 SMOVE_GEN(Trans[TransInv[tpairs[i].game2.DiscDiffBW]](move),
			   player);
	    }

#if UNIQ
	    WriteUniqGame(pT, &gameout, fpout);
#else
	    fWritePackedGame(fpout, &gameout);
#endif

	  }
	}
      }
#endif

      { int h = 0;
        LELEM *p;

      FOR (i, game.get_move_num()) {

        NewGame *pg=NULL, *pg1, *pg2;

        if ((p=buckets[i][h % HNUM])) {

          while (p) {

	    if      (p->number == 1) pg = &tpairs[p->index].game1;
	    else if (p->number == 2) pg = &tpairs[p->index].game2;
	    else Error("number corrupt");

	    for (j=pg->get_move_num()-1; j >= 0; j--) {
	      if (game.get_pm(j) != pg->get_pm(j)) break;
            }

	    if (j < 0) {

              if (p->number == 1) {

		pg1 = &tpairs[p->index].game1;
		pg2 = &tpairs[p->index].game2;

	      } else {

		pg1 = &tpairs[p->index].game2;
		pg2 = &tpairs[p->index].game1;

              }

	      gameout = game;

	      FOR (j, pg1->get_move_num()) {
	        gameout.get_pm(j) = pg2->get_pm(j);
              }

	      for (j=pg1->get_move_num(); j < game.get_move_num(); j++) {

	        player = game.get_pm(j).get_player();
	        move   = game.get_pm(j).get_move();

                if (p->number == 1)
	        
		  gameout.get_pm(j).set(player, 
					Trans[pg2->get_value()](move));

                else

	          gameout.get_pm(j).set(player, 
					Trans[TransInv[pg1->get_value()]](move));

	      }

              // important! normalize result if there is a symmetry

	      if (pg1->get_value() != 0 && pg2->get_value() != 0) {
		gameout.unique();
		if (gameout.get_value() != r) puts("/");
	      }

#if UNIQ
	      WriteUniqGame(pT, gameout, fpout);
#else
	      fWritePackedGame(fpout, &gameout);
#endif
	      if (gameout.get_value() != r) puts("*");



#if 0
if (gameout.get_pm(1] == (sint1)SMOVE_GEN(C3, WHITE) &&
    gameout.get_pm(2] == (sint1)SMOVE_GEN(F5, BLACK) &&
    gameout.get_pm(3] == (sint1)SMOVE_GEN(F6, WHITE) &&
    gameout.get_pm(4] == (sint1)SMOVE_GEN(C4, BLACK) &&
    gameout.get_pm(5] == (sint1)SMOVE_GEN(F4, WHITE) &&
    gameout.get_pm(6] == (sint1)SMOVE_GEN(E6, BLACK) &&
    gameout.get_pm(7] == (sint1)SMOVE_GEN(D6, WHITE) &&
    gameout.get_pm(8] == (sint1)SMOVE_GEN(B3, BLACK)) {

fWriteGame(stdout, &game);
fWriteGame(stdout, &gameout);

fWriteGame(stdout, pg1);
fWriteGame(stdout, pg2);

printf("\n");
}
#endif



	    } 
  
            p = p->succ;
          }
        }

        h += game.get_pm(i).get_raw();
      }
      }
    }

  } else if (!strcmp(argv[1], "-reduce")) { 

    int tnum, move, player, gamenum;

#if UNIQ
    LIBRARY *pT = NewLibrary();
#endif


// reduce library: remove equivalent paths


    if (!evalfile) Error("-reduce works on eval-file only");

    tnum = readpairs(transfile);

    printf("%d pair(s) read\n", tnum);

    MakeBuckets(tnum);
    tnum = prefixes(libfile, tnum);
    MakeBuckets(tnum);

    printf("%d pair(s) seen\n", tnum);

#if 0
    FOR (i, tnum) {

	fWriteGame(stdout, &tpairs[i].game1);
	fWriteGame(stdout, &tpairs[i].game2);
	printf("\n");
    }
#endif

    sprintf(newfile, "%s.red", evalfile);

    fpout = fopen(newfile, "w");
    if (!fpout) Error("can't open red-file");


    fp = fopen(evalfile, "r");
    if (!fp) Error("can't open eval-file");

    for (gamenum=0; ; gamenum++) {

      NewGame game, gameout, mingame;


      if (!game.f_read_packed(fp)) break;

      if (game.get_move_num() > 0 && game.get_pm(0).get_move() != D3) 
	Error("not normalized!");


      if ((gamenum % 1000) == 0) { 
	  printf("%6d\r", gamenum); fflush(stdout); 
      }

/*fWriteGame(stdout, &game);*/

      mingame = game;

#if 0

old

      FOR (i, tnum) 

      if (tpairs[i].seen1 && tpairs[i].seen2) {

	if (game.get_move_num() >= tpairs[i].game1.get_move_num()) {

          for (j=tpairs[i].game1.get_move_num()-1; j >= 0; j--)
	    if (game.get_pm(j] != tpairs[i].game1.get_pm(j]) break;

	  if (j < 0) {

// sequence 1 matched

	    gameout = game;

	    FOR (j, tpairs[i].game1.get_move_num())
	      gameout.get_pm(j] = tpairs[i].game2.get_pm(j];

	    for (j=tpairs[i].game1.get_move_num(); j < game.get_move_num(); j++) {
	      player = game.get_pm(j).get_player();
	      move   = game.get_pm(j).get_move();

	      gameout.get_pm(j).set(
				    player, 
				    Trans[tpairs[i].game2.get_value()](move)
				   );
	    }

	    if (strcmp(gameout.Moves, mingame.Moves) < 0) {

	      mingame = gameout;

	    }
	  }
	}

        if (game.get_move_num() >= tpairs[i].game2.get_move_num()) {

          for (j=tpairs[i].game2.get_move_num()-1; j >= 0; j--)
	    if (game.get_pm(j] != tpairs[i].game2.get_pm(j]) break;

	  if (j < 0) {

// sequence 2 matched

	    gameout = game;

	    FOR (j, tpairs[i].game2.get_move_num())
	      gameout.get_pm(j] = tpairs[i].game1.get_pm(j];

	    for (j=tpairs[i].game2.get_move_num(); j < game.get_move_num(); j++) {
	      player = SMOVE_PLAYER(game.get_pm(j]);
	      move   = SMOVE_MOVE  (game.get_pm(j]);
	      gameout.get_pm(j] =
		 SMOVE_GEN(Trans[TransInv[tpairs[i].game2.get_value()]](move),
			   player);
	    }

	    if (strcmp(gameout.Moves, mingame.Moves) < 0) {

	      mingame = gameout;

	    }
	  }
	}
      }

#else

      { int h = 0;
        LELEM *p;

      FOR (i, game.get_move_num()) {

        NewGame *pg=NULL, *pg1, *pg2;

        if ((p=buckets[i][h % HNUM])) {

          while (p) {

	    if      (p->number == 1) pg = &tpairs[p->index].game1;
	    else if (p->number == 2) pg = &tpairs[p->index].game2;
	    else Error("number corrupt");

	    for (j=pg->get_move_num()-1; j >= 0; j--) {
	      if (game.get_pm(j) != pg->get_pm(j)) break;
            }

	    if (j < 0) {

              if (p->number == 1) {

		pg1 = &tpairs[p->index].game1;
		pg2 = &tpairs[p->index].game2;

	      } else {

		pg1 = &tpairs[p->index].game2;
		pg2 = &tpairs[p->index].game1;

              }

	      gameout = game;

	      FOR (j, pg1->get_move_num()) {
	        gameout.get_pm(j) = pg2->get_pm(j);
              }

	      for (j=pg1->get_move_num(); j < game.get_move_num(); j++) {

	        player = game.get_pm(j).get_player();
	        move   = game.get_pm(j).get_move();

                if (p->number == 1)
	        
		  gameout.get_pm(j).set(player, 
					Trans[pg2->get_value()](move));

                else

	          gameout.get_pm(j).set(player,
					Trans[TransInv[pg1->get_value()]](move));


	      }

	      // important! normalize result if there is a symmetry
 
	      if (pg1->get_value() != 0 && pg2->get_value() != 0) {
		gameout.unique();
	      }

	      if (strcmp((char*)&gameout.get_pm(0), (char*)&mingame.get_pm(0)) < 0) {

	        mingame = gameout;

	      }

	    } 
  
            p = p->succ;
          }
        }

        h += game.get_pm(i).get_raw();
     }
     }

#endif

#if UNIQ
      WriteUniqGame(pT, mingame, fpout);
#else
      fWritePackedGame(fpout, &mingame);
#endif

    }

  } else goto error;

  return 0;
}
