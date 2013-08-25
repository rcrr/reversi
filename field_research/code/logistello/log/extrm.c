// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/* Spielfelder gleicher Steinsteinanz aus Spieldatei extrahieren / 12.3.92 */

#ifndef PRECOMP
#include "main.h"
#endif


#include "crt.h" 
#include "sboard.h"

#define PU_LEN	100

#define PRAEFIX	"osa"

SPFELD Pu[65][PU_LEN];
int    PuAnz[65];


void PuWeg(int a)
{
  FILE *fp;
  char name[100];


  if (PuAnz[a]) {

    sprintf(name, PRAEFIX "%d.sfk", a);

    fp = fopen(name, "a");
    if (!fp) Error("oextr: kann Datei nicht zu öffnen");

    if (fSfWrite(fp, &Pu[a][0], PuAnz[a])) Error("oextr: Schreibfehler");
    fclose(fp);

    PuAnz[a] = 0; 
  }
}


void InPu(int a, SPFELD *psf)
{
  Pu[a][PuAnz[a]++] = *psf;

  if (PuAnz[a] >= PU_LEN) PuWeg(a); 
}



void _abort(void) { exit(1); }




int main(int argc, char **argv)
{
  int	 a, i, startanz, endanz;
  SPFELD sf;
  FILE   *fpin;
  char   name[100];
  int    einanz=0, ausanz=0;
  bool   f_a;


  if (argc < 5) { 

Fehler:

    Error("\
Aufruf: oextr (a | n) startanz endanz sfk-Datei[en]\n\
             a: Spielfelder an osa*.sfk anfügen\n\
             n: osa*.sfk neu anlegen\n");
  }


  if      (!strcmp(argv[1], "a")) f_a = true;
  else if (!strcmp(argv[1], "n")) f_a = false;
  else goto Fehler;

  if (sscanf(argv[2], "%d", &startanz) != 1 || startanz < 4 || startanz > 64) 
    Error("oextr: startanz falsch");  

  if (sscanf(argv[3], "%d", &endanz) != 1 || endanz < startanz || endanz > 64) 
    Error("oextr: endanz falsch");  

  FOR (i, 65) PuAnz[i] = 0;


  if (!f_a) 

/* alte Dateien löschen */

    for (i=startanz; i <= endanz; i++) {
      sprintf(name, PRAEFIX "%d.sfk", i);
      unlink(name);
    }


  for (i=4; argv[i]; i++) {

    printf("oextr: %s\n", argv[i]);

    fpin = fopen(argv[i], "r");
    if (!fpin) { printf(">>>  Datei nicht da"); continue; }


    FOREVER {

/*printf("%d ", einanz);
*/

      if (!fSfRead(fpin, &sf, 1)) break;
      einanz++;

      if ((a=SfAnz(&sf)) >= startanz && a <= endanz) {

	ausanz++;

        InPu(a, &sf);

      }
    }

    if (ferror(fpin)) Error("oextr: Lesefehler");
    fclose(fpin);

  }

  for (i=startanz; i <= endanz; i++) PuWeg(i);

  printf("oextr: %d Spielfelder eingelesen\n", einanz);
  printf("oextr: %d Spielfelder ausgegeben\n", ausanz);

  return 0;
}
  
  
  

