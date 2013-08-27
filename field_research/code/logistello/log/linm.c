// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/* lineare Diskriminanzfunktion / 19.12.91 */

#ifndef PRECOMP
#include "main.h"
#endif

#include "logmat.h"
#include "logio.h"
#include "logall.h"

int f_aus=0;


void _abort(void)
{
  exit(1);
}


void LinDiskr(MATRIX *pX, MATRIX *pn, MATRIX *py, MATRIX *pbeta)
{
  int i, j;
  REAL norm;


  if (pbeta->dimy != pX->dimx) Error("dim falsch");


  FOR (i, pbeta->dimx) VEK(pbeta, i) = 0.0;


/* Vektoren spiegeln und normieren */


  FOR (i, pX->dimy) {


    if (VEK(py, i) < 1.0)
      FOR (j, pX->dimx) MAT(pX, i, j) = - MAT(pX, i, j);


    norm = 0.0;
    FOR (j, pX->dimx) norm += MAT(pX, i, j) * MAT(pX, i, j);
    norm = sqrt(norm);
    FOR (j, pX->dimx) MAT(pX, i, j) /= norm;

    FOR (j, pX->dimx) VEK(pbeta, j) += MAT(pX, i, j);

  }

  norm = 0.0;
  FOR (j, pX->dimx) norm += VEK(pbeta, j) * VEK(pbeta, j);
  norm = sqrt(norm);
  FOR (j, pX->dimx) VEK(pbeta, j) /= norm;


  VekAus("beta", pbeta);
}


REAL lindis(MATRIX *pX, MATRIX *pbeta, int z)
{
  int j;
  REAL su;


  su = 0.0;

  FOR (j, pX->dimx) su += VEK(pbeta, j) * MAT(pX, z, j);

  return su;
}




int main(int argc, char **argv)
{
  int i, par;
  MATRIX *pbeta, *pX, *pn, *py;
  char testname[100];
  FILE *fp;


  if (argc < 3) 
    Error("Aufruf: olin [-a] mrk-Datei mrk-Testdateien");


  if (strcmp(argv[1], "-a") == 0) { f_aus = 1; par = 3; } else par = 2;

  if (par >= argc) 
    Error("Aufruf: log [-a] mrk-Datei mrk-Testdateien");

  i = f_aus; f_aus = 1;
  BspInput(argv[par-1], &pX, &pn, &py);
  if (!pX) Error("keine Daten");
  f_aus = i;


  i = par-1;

  while (i < argc) {

    sprintf(testname, "%s", argv[i]);

    fp = fopen(testname, "rb");
    if (!fp) Error("Datei nicht gefunden");
    fclose(fp);
    i++;
  }



  if (!(pbeta = MatMake(pX->dimx, 1))) Error("mem");

  LinDiskr(pX, pn, py, pbeta);

  



  printf("\n");
  BspInput(argv[par-1], &pX, &pn, &py);
  if (pX->dimx != pbeta->dimy) Error("Dimensionen unterschiedlich");
  Fehler(pX, pn, py, pbeta, lindis);
  printf("\n");
  
  i = par;

  while (i < argc) {

    if (strcmp(argv[par-1], argv[i])) {
      BspInput(argv[i], &pX, &pn, &py);

      if (pX->dimx != pbeta->dimy) Error("Dimensionen unterschiedlich");

      Fehler(pX, pn, py, pbeta, lindis);
      printf("\n");
    }
    i++;
  }

  printf("\n----------------------\n\n");
  
  return 0;
}
