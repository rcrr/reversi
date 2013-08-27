// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/* Spieledatei -> Texformat, 8.93 */

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
Error("Aufruf: otex osp-Datei(mit Namen)");
  }


  fpin = fopen(argv[1], "r");
  if (!fpin) Error("Datei nicht da");


  fpout = stdout;

{ int i=1;

  FOREVER {

    FOREVER {

      s[0] = 0;

      if (!fgets(s, MAXANZ, fpin)) break;

      if (s[0] == '>') break;

    }

    if (!s[0]) break;

    sprintf(SpielerB, "%s", s+1);
    if (!fgets(s, MAXANZ, fpin) || s[0] != '>') Error("2. Spieler?");

    sprintf(SpielerW, "%s", s+1);

    if (fTabEin(fpin, &tab)) Error("Spiel?");

#if 0
    if (!Tab2Game(&tab, &Game)) Error("Fehler im Spiel");

    if (PlayGame(Game.MoveNum, &Game, &sf)) {

       fprintf(stderr, "Fehler im Spiel\a\n");

    }
#endif
/*printf("%d\n", i); i++;
*/

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

    if (SpielerB[strlen(SpielerB)-1] == '\n') SpielerB[strlen(SpielerB)-1] = 0;
    if (SpielerW[strlen(SpielerW)-1] == '\n') SpielerW[strlen(SpielerW)-1] = 0;

    printf("\\Spiel{%s}{%s}{%d}{%d}{\\Zuege}\n", 
      SpielerB, SpielerW, 
      (!SfAnzWHITE(&sf)) ? 64 : SfAnzBLACK(&sf), 
      (!SfAnzBLACK(&sf)) ? 64 : SfAnzWHITE(&sf));
  }
}
  fclose(fpin);
  fclose(fpout);


  return 0;
}

