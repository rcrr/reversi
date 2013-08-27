// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/* Beispieldaten einlesen, 29.10.91 */

/* Format der Datei:							*/
/*									*/
/*  # Dateiname \n							*/
/*  # Merkmalsnamen (jeweils \n)					*/
/*  # \n								*/
/*  (Pos-Anzahl{0..Anzahl} Anzahl (Merkmalswert)+ \n)+	*/

#ifndef PRECOMP
#include "main.h"
#endif


#include "crt.h"

#include "logm.h"
#include "logio.h"


int BspInput(
  char   *Datei, 
  char   *InterDatei,
  MATRIX **ppX, 
  MATRIX **ppn, 
  MATRIX **ppy,
  int	 *pMerkmAnz,
  char   Merkmale[][LENMAX],
  char   input[DATMAX]
)
{
  int	c, i, zeile, merkmanz, merkmeinzel, Merkm1[VARMAX], Merkm2[VARMAX];
  FILE	*fp;
  char  s[DATMAX];


  *ppX = 0;


  fp = fopen(Datei, "rb");

  if (!fp) return 0;

  if (!fgets(s, DATMAX, fp)) Error("Filename in datafile corrupt");

  if (!sscanf(s, "#%40s", input)) Error("Filename in datafile corrupt?");

  strcpy(Merkmale[0], "INTCPT");

  merkmanz = 1;

  FOREVER {

    if (merkmanz >= VARMAX-1) Error("zu viele Merkmale");

    if (!fgets(s, DATMAX-1, fp)) Error("Merkmale?");

    if (!strcmp(s, "#\n")) break;

    i = sscanf(s, "#%40s", &Merkmale[merkmanz][0]);

    if (i == 0) Error("Fehler in Merkmalsliste");

    merkmanz++;

  }

  merkmeinzel = merkmanz;


  if (InterDatei) {		/* Interaktionen einlesen */

    int  n1, n2, a;
    FILE *fpi;


    fpi = fopen(InterDatei, "rb");

    if (!fpi) { fclose(fp); Error("InterDatei nicht zu öffnen"); }

    FOREVER {

      fscanf(fpi, " ");

      if ((a=fscanf(fpi, "(%d,%d)", &n1, &n2)) < 2) {

        if (a <= 0 && feof(fpi)) break;

        Error("Syntax Error in InterDatei");
      }

      if (n1 < 1 || n1 >= merkmeinzel || n2 < 1 || n2 >= merkmeinzel) 
	Error("Indexfehler in InterDatei");

      if (merkmanz >= VARMAX-1) Error("zu viele Merkmale");

      Merkm1[merkmanz] = n1;
      Merkm2[merkmanz] = n2;
      sprintf(Merkmale[merkmanz++], "(%d,%d)", n1, n2);

    }

    fclose(fpi);
  }

  *ppX = MatMake(BEISP_MAX, merkmanz);
  *ppn = MatMake(1, BEISP_MAX);
  *ppy = MatMake(1, BEISP_MAX);

  if (!*ppX || !*ppn || !*ppy) Error("Speicher");

  zeile = 0;

  while (!feof(fp)) {

    if (zeile >= BEISP_MAX) Error("zu viele Beispiele");

    c = fgetc(fp);
    while (c == '\n' || c == '\t' || c == ' ') c = fgetc(fp);

    while (c == '#') { fgets(s, DATMAX, fp); c = fgetc(fp); }
    ungetc(c, fp);

    if (feof(fp)) break;


    if (fscanf(fp, "%" R_F, &VEK(*ppy, zeile)) < 1) {

      if (!feof(fp)) Error("Fehler in Eingabe: # Positiver Beispiele?");
      break;
    }

    if (fscanf(fp, "%" R_F, &VEK(*ppn, zeile)) < 1) {

      if (!feof(fp)) Error("Fehler in Eingabe: Beispiel Anzahl?");
      break;
    }

/*
    if (VEK(*ppn, zeile) <= 0.0) Error("Fehler in Eingabe: Anzahl <= 0?");
*/

    MAT(*ppX, zeile, 0) = 1.0;			/* INTERCEPT */


/* Merkmalswerte einlesen */

    FOR (i, merkmeinzel-1) {
      if (fscanf(fp, "%" R_F, &MAT(*ppX, zeile, i+1)) < 1)
	Error("Fehler in der Eingabe");
/*MAT(*ppX, zeile, i+1) *= 100;*/
    }


/* etwaige Produktwerte berechnen */

    for (i=merkmeinzel; i < merkmanz; i++) {

      MAT(*ppX, zeile, i) = 
        MAT(*ppX, zeile, Merkm1[i]) * MAT(*ppX, zeile, Merkm2[i]);
    }

#if 0

/* Unentschieden raus */
   
    if (Klasse > 0.0 && Klasse < 1.0) {				/* W'keiten */

      if (WertKlasse) Error("Mix von Klasse und W'keit");

      WertWkeit = true;

      if (Klasse != 0.5) { VEK(*ppy, zeile) = Klasse; zeile++; }

    } else if (Klasse == -1.0 || Klasse == 0.0 || Klasse == 1.0) {  /* Klasse */

      if (WertWkeit) Error("Mix von Klasse und W'keit");

      WertKlasse = true;

      if      (Klasse == -1.0) { VEK(*ppy, zeile) = P_NULL; zeile++; }
      else if (Klasse ==  1.0) { VEK(*ppy, zeile) = (1.0-P_NULL); zeile++; }

    } else Error("unbekannte Klasse");

#endif

    zeile++;
  }
    
  fclose(fp);

  (*ppX)->dimy = zeile;
  (*ppn)->dimx = zeile;
  (*ppy)->dimx = zeile;

  *pMerkmAnz = merkmanz;

  return zeile;
}
