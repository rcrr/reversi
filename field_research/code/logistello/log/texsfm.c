// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/* boards -> texfile, 4.94 */

#ifndef PRECOMP
#include "main.h"
#endif


#include "crt.h" 
#include "sboard.h"
#include "goodies.h"
#include "game.h"

#define MAXANZ 300

void _abort(void) { exit(1); }


int main(int argc, char **argv)
{
  int	    i, j;
  char	    s[MAXANZ], SpielerB[MAXANZ], SpielerW[MAXANZ];
  SPFELD    tab, sf;
  FILE	    *fpin, *fpout;
  GAME      Game;


  if (argc != 2) {

Fehler:
Error("call: otexsf sfk-file");
  }


  fpin = fopen(argv[1], "r");
  if (!fpin) Error("Datei nicht da");


  fpout = stdout;

{ int i=1;

  FOREVER {

    if (!fSfRead(fpin, &tab, 1)) break;

    printf("\\def\\Zuege{\n");


    FOR (j, 100) {

      int x=X100(j)+1, y=Y100(j)+1;

      if (tab.p[j] == BLACK)    
	printf("\\P{%d}{%d}{\\X}\n", x, y);
      if (tab.p[j] == WHITE)    
	printf("\\P{%d}{%d}{\\O}\n", x, y);
      if (tab.p[j] >= NUM_DISP) 
	printf("\\P{%d}{%d}{\\Z{%d}}\n", x, y, tab.p[j] - NUM_DISP);

    }
    printf("}\n");

    printf("\\Spielfeld{\\Zuege}\n"); 

  }
}
  fclose(fpin);
  fclose(fpout);


  return 0;
}

