// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

// generate a html game representation from a osp-file / 7/97

#include "main.h"
#include "sboard.h"
#include "game.h"
#include "crt.h"

const int COL_NUM = 5;

void Board2HTML(SPFELD &sf)
{
  static char *disc_files[3] = { "w", "e", "b" };
  int x, y;

  printf("<td>\n");

  FOR (y, 8) {
    FOR (x, 8) {
      printf("<img src=%s.gif>", disc_files[sf.p[y*10+x+11] + 1]);
    }

    printf("<br>\n");
  }

  printf("</td>\n");

}

void _abort(void) { exit(20); }


int main(int argc, char **argv)
{
  SPFELD tab, sf;
  GAME   game;
  int    out_num = 0, i, j;
  int    last_n = 64;

  if (argc != 1 && argc != 3) {
    fprintf(stderr, "*** usage: osp2html [-last n] < in > out\n"); exit(20);
  }

  if (argc == 3) {
    
    if (!strcmp(argv[1], "-last")) {

      last_n = atoi(argv[2]);
      if (last_n <= 0 || last_n > 60) {
	fprintf(stderr, "*** last_n?\n"); exit(20); 
      }
    }
  }

  if (fTabEin(stdin, &tab)) { fprintf(stderr, "tab-error1\n"); exit(20); }

  if (!Tab2Game(&tab, &game)) { fprintf(stderr, "tab-error2\n"); exit(20); }

#if 0
  printf("<font size=-1>\n"); 
  fWriteGame(stdout, &game);
  printf("</font>\n"); 
#endif

  SfGrund(&sf);

  printf("<table>\n");

  FOR (i, game.MoveNum+1) {

    int move   = SMOVE_MOVE(game.Moves[i]);
    int player = SMOVE_PLAYER(game.Moves[i]);

    if (i >= game.MoveNum-(last_n-1)) {

      if (out_num == 0) {
	printf("<tr>\n");
      }

      Board2HTML(sf);
      out_num++;

      if (i == game.MoveNum || out_num == COL_NUM) {

	printf("</tr>\n");
      
	printf("<tr>\n");

	FOR (j, out_num) {

	  printf("<td>\n<font size=-1>");

	  if (j < out_num-1 || (j == out_num-1 && i != game.MoveNum)) {
	    printf("%d: ", i-out_num+j+1+1);
	    if (SMOVE_PLAYER(game.Moves[i-out_num+j+1]) > 0) 
	      printf("+"); 
	    else
	      printf("-"); 
	    KoorAus(SMOVE_MOVE(game.Moves[i-out_num+j+1]));
	    
	  } else {
	    
	    sint1 moves[65];
	    
	    if (SfMoeglZuege(&sf, BLACK, moves) == 0 &&
		SfMoeglZuege(&sf, WHITE, moves) == 0) {

	      printf("<img src=b.gif> %d - <img src=w.gif> %d", SfAnzBLACK(&sf), SfAnzWHITE(&sf));
	    }
	  }

	  printf("</font></td>\n");
	}

	printf("</tr>\n");

	out_num = 0;
      }
    }

    if (i < game.MoveNum && !SfSetzen(&sf, player, move)) {
      fprintf(stderr, "%d %d invalid move", i, move); exit(20); 
    }
  }

  printf("</table>\n");

  
  return 0;
}
