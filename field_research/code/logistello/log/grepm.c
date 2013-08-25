// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/* filter important games from ios database */

#ifndef PRECOMP
#include "main.h"
#endif

#include "crt.h"
#include "game.h"

#define SLEN	300


void _abort(void) { exit(10); }


int main(int argc, char **argv)
{
  FILE *fp;
  int  i, m, player, timestamp, time, numblack, numwhite;
  char *p, s[SLEN+2], nameblack[SLEN+2], namewhite[SLEN+2], dummy[SLEN+2],
       moves[SLEN+2], flag[SLEN+2];
  GAME game;
  SPFELD sf;
  bool f_good=false;


  if (argc != 2) {

error:

    Error("call: ogrep (-good | -bad)");
  }



  if      (!strcmp(argv[1], "-good")) f_good = true;
  else if (!strcmp(argv[1], "-bad"))  f_good = false;
  else goto error;


  fp = fopen("games.ios", "r");
  if (!fp) Error("games.ios?");

  FOREVER {

    if (!fgets(s, SLEN, fp)) break;

    if (sscanf(s, "%d %s %s %d %d %s %d %s %s", 
	&timestamp, nameblack, namewhite, &numblack, &numwhite, flag, &time,
	moves, dummy) == 9) {

      if (!strcmp(flag, "e") && 
	  (((!strcmp(nameblack, "logtest") && 
		((!f_good && numblack <= 32) || (f_good && numblack > 32)))) || 
           ((!strcmp(namewhite, "logtest") && 
		((!f_good && numwhite <= 32) || (f_good && numwhite > 32)))))) {

        p = moves;

	for (i=0;; i++) {

	  if      (*p == '+') player = BLACK; 
	  else if (*p == '-') player = WHITE;
	  else break;

	  p++;

	  m = (p[0] - '0') * 10 + p[1] - '0';

	  p += 2;
 
	  game.Moves[i] = SMOVE_GEN(m, player);


	}

	game.MoveNum = i;

	if (!PlayGame(i, &game, &sf)) {

	  game.DiscDiffBW = SfAnzBLACK(&sf) - SfAnzWHITE(&sf);

	  fWriteGame(stdout, &game);
	}
      }


    } /* else printf("*** read error: %s", s) */;


  }


  fclose(fp);

  return 0;
}
