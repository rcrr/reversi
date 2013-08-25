// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/* seperate bad from good games / 5.94 */

#ifndef PRECOMP
#include "main.h"
#endif

#include "crt.h"
#include "game.h"
#include "goodies.h"


#define LOGID		"logtest"

#define RESIGN          false


#define SLEN	300


void _abort(void) { exit(10); }


int main(int argc, char **argv)
{
  FILE *fpbad, *fpgood, *fpseen, *fp;
  int  i, j, m, player, time, 
       gamenum, log_colour, diff;
  char *p, s[SLEN+2], nameblack[SLEN+2], namewhite[SLEN+2],
       moves[SLEN+2], dummy[SLEN+2];
  GAME game, *games;
  SPFELD sf;
  int  readnum=0, newnum=0, badnum=0, goodnum=0;


  if (argc == 1) {

error:

    Error("call: osep (-diff oko cor-oko | game-file seen-oko)\n\
          write games to diff.oko, rest to same.oko\n\
          or seperate new (to seen-oko) games -> bad|good.oko)");
  }


  if (argc == 4) {

    FILE *fpin1, *fpin2, *fpsame, *fpdiff;
    GAME game1, game2;


    if (strcmp(argv[1], "-diff")) goto error;


    fpin1 = fopen(argv[2], "r");
    if (!fpin1) Error("osep: can't open file1");

    fpin2 = fopen(argv[3], "r");
    if (!fpin2) Error("osep: can't open file2");

    fpsame = fopen("same.oko", "w");
    if (!fpsame) Error("osep: can't open same.oko");

    fpdiff = fopen("diff.oko", "w");
    if (!fpdiff) Error("osep: can't open diff.oko");



    FOREVER {

      if (!fReadPackedGame(fpin1, &game1)) break;

      if (!fReadPackedGame(fpin2, &game2)) Error("osep: 2nd file is shorter");

      FOR (i, 20) {

	if (i < game1.MoveNum && i < game2.MoveNum &&
	    game1.Moves[i] != game2.Moves[i]) Error("osep: games corrupt");
      }


      if (sgn(game1.DiscDiffBW) != sgn(game2.DiscDiffBW)) 

/* value has changed! */


	fWritePackedGame(fpdiff, &game2);

      else

	fWritePackedGame(fpsame, &game2);


    }


    if (fReadPackedGame(fpin2, &game2)) Error("osep: 2nd file is longer");

    return 0;

  } else if (argc != 3) goto error;


  fpbad = fopen("bad.oko", "w");
  if (!fpbad) Error("osep: can't open bad.oko");

  fpgood = fopen("good.oko", "w");
  if (!fpgood) Error("osep: can't open good.oko");


  fp = fopen(argv[1], "r");
  if (!fp) Error("osep: can't open game-file");


/* read seen games */

  fpseen = fopen(argv[2], "r");
  if (!fpseen) Error("can't open seen-oko");

  gamenum = 0;

  FOREVER {

    if (!fReadPackedGame(fpseen, &game)) break;
   
    gamenum++;

  }

  fclose(fpseen);

  printf("osep: %d game(s) seen\n", gamenum);

  games = (GAME*) malloc((gamenum+50000) * sizeof(GAME));

  if (!games) Error("osep: no mem");

  fpseen = fopen(argv[2], "r");
  if (!fpseen) Error("osep: can't open seen-oko");


  for (i=0;; i++) {

    if (!fReadPackedGame(fpseen, &games[i])) break;
   
  }

  fclose(fpseen);

  if (i != gamenum) Error("osep: seen-oko corrupt");


  fpseen = fopen(argv[2], "a");
  if (!fpseen) Error("osep: can't open seen-oko");



  FOREVER {

    if (!fgets(s, SLEN, fp)) break;

    readnum++;

    if (sscanf(s, "%s %s %s %d %s %d", 
	dummy, nameblack, namewhite, &time, moves, &diff) == 6) {

      log_colour = 0;

      if      (!strcmp(nameblack, LOGID)) log_colour = BLACK;
      else if (!strcmp(namewhite, LOGID)) log_colour = WHITE;
      else Error("no log game!");

      if (!strcmp(nameblack, LOGID) && !strcmp(namewhite, LOGID)) 
        log_colour = 0;

      {
        p = moves;

	for (i=0;; i++) {

	  if      (*p == '+') player = BLACK; 
	  else if (*p == '-') player = WHITE;
	  else break;

	  p++;

	  m = p[0] - 'a' + 1  + (p[1] - '1' + 1) * 10;

	  p += 2;
 
	  game.Moves[i] = SMOVE_GEN(m, player);

	}

	game.MoveNum = i;

	if (RESIGN || 
	    !PlayGame(i, &game, &sf)) {

	  game.DiscDiffBW = SfAnzBLACK(&sf) - SfAnzWHITE(&sf);

if ((diff >= 0 && game.DiscDiffBW < 0) || 
    (diff <  0 && game.DiscDiffBW >= 0)) Error("osep: corrupt diff");

	  if (!RESIGN) UniqueGame(&game);

	  FOR (i, gamenum) {

	    if (game.MoveNum == games[i].MoveNum) {

	      for (j=game.MoveNum-1; j >= 0; j--) {

		if (game.Moves[j] != games[i].Moves[j]) break;

	      }

	      if (j < 0) break;

	    }

	  }

	  if (i >= gamenum) {		/* new game */

	    newnum++;

/* append it */

	    games[gamenum++] = game;

	    fWritePackedGame(fpseen, &game);

/* seperate good from bad games */

	    if (RESIGN ||
		!log_colour || 
	        (log_colour == BLACK && game.DiscDiffBW <= 0) ||
	        (log_colour == WHITE && game.DiscDiffBW >= 0)) {

	      badnum++;
	      fWritePackedGame(fpbad, &game);

	    } else {

	      goodnum++;
	      fWritePackedGame(fpgood, &game);

	    }
	  }


	} else Error("osep: game not finished");

      }


    } /* else printf("*** read error: %s", s) */;


  }


  fclose(fp);
  fclose(fpbad);
  fclose(fpgood);
  fclose(fpseen);

  printf("osep: %d game(s) read, %d new game(s), %d bad, %d good\n", 
     readnum, newnum, badnum, goodnum);

  return 0;

}
