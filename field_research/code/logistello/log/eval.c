// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

// verschiedene Bewertungen von BLACK aus (hohe Werte)

#include "main.h"
#include "move.h"
#include "eval.h"

String ParameterFile = "evala.par";
String DataPath      = "data"; // "regbin152";

void InitBewert(void) {}


/* true <=> in Ordnung */

bool ParameterEinlesen(char *datei, int Anzahl, float *Feld)
{
  int  i, dummy;
  FILE *fp;

  fp = fopen(datei, "r");

  if (!fp) { printf(">>> parameterfile not found"); return false; }

  printf("[ read parameters '%s' ...", datei); fflush(stdout);


  FOR (i, Anzahl) {

/*
printf("%d ", i);
*/

    if (fscanf(fp, "%f,", &Feld[i]) < 1) {
      printf(">>> error in parameterfile"); return false;
    }

  }

  fscanf(fp, "%d", &dummy);

  if (!feof(fp)) { printf(">>> parameterfile too long"); return false; }

  fclose(fp);

  printf("OK ]\n");

  return true;
}




int BewDiffAlt(BRETT *pbr, PARTEI Partei)
{
  int  Wert;


  Wert = pbr->StDiffBW;

  if (abs(Wert) == pbr->SteinAnz) {	/* WIPEOUT? */
 
    if (pbr->SteinAnz + Wert) Wert =  (WERTGEWINN+64);
    else		      Wert = -(WERTGEWINN+64);

  } else {

    if      (Wert < 0) Wert -= WERTGEWINN;
    else if (Wert > 0) Wert += WERTGEWINN;

  }

  if (Partei == BLACK) return Wert; else return -Wert;
}


int BewDiff(BRETT *pbr, PARTEI Partei)
{
  int  Wert=pbr->StDiffBW;


  if (Wert == 0) return 0;	/* draw */

  if (Wert > 0) Wert = Wert + (64 - pbr->SteinAnz + WERTGEWINN);
  else 		Wert = Wert - (64 - pbr->SteinAnz + WERTGEWINN);

  if (Partei == WHITE) Wert = -Wert;

#if 0
  if (Wert != BewDiffAlt(pbr, Partei)) {
  
    SPFELD sf;

    BrettSf(pbr, &sf); 
    SfAus(&sf, 0, 0); 
    printf("alt=%d neu=%d\n", BewDiffAlt(pbr, Partei), Wert);

  }
#endif

  ANTI_RET(Wert);
}




