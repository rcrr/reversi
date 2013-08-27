// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/* Merkmalsdatei aus Spielfelddatei erzeugen / 7.10.91 */

#ifndef PRECOMP
#include "main.h"
#endif

 
#include "featurem.h"
#include "crt.h"
#include "feature.h"

bool f_aus=false, f_bsp=false, f_nom=false;
int f_draw_factor = 1;
int z1;



SPFELD	*sfein, *sftest;


void _abort(void)
{
  exit(1);
}



int main(int argc, char **argv)
{
  char name[100], mrkname[100], outdat[100];
  int i, j, argind, Merkmal, Marke;
  FILE *fpin, *fpout;
  int z_gew = 0, z_ver = 0, z_remis = 0;
  REAL a;
  BRETT Brett;
  SPFELD sf;


  argind = 1;
  
  if (argc > argind && !strcmp(argv[argind], "-a")) { 
    argind++; f_aus = true; 
  }

  if (argc > argind+1 && !strcmp(argv[argind], "-df")) { 
    argind++; 
    f_draw_factor = atoi(argv[argind]);
    if (f_draw_factor < 2) goto error;
    argind++;
  }

  if (argc != argind+2) {

  error:

    Error("\
call: ofeature [-a -df draw-factor>1] feature-file sfk-file\n\
               -a: output\n\n");
 
   }

  sprintf(name, "%s", argv[argind+1]);

  InitCrt();
  InitSetzen();

  fpin = fopen(argv[argind], "rb");
  if (!fpin) Error("Merkmalsdatei nicht da");

  i = 0;

  while (!feof(fpin)) {

    if ((a=fscanf(fpin, "%20s", mrkname)) <= 0)

      if (feof(fpin)) break;
      else Error("Fehler in Merkmalsdatei");
     

if (f_aus) printf("%s\n", mrkname);

    j = 0;

    while (Merkmale[j]) {

      if (!strcmp(Merkmale[j]->Name, mrkname)) break;
      j++;
    }

    if (!Merkmale[j]) { 
      printf("\"%s\" ", mrkname); Error("unbekanntes Merkmal"); 
    }

    if (i >= MerkmalsAnz) Error("zu viele Merkmale");

    MerkmaleDa[i] = Merkmale[j];
    i++;
  }

  MerkmaleDa[i] = NULL;  
     
  fclose(fpin);



  sprintf(outdat, "%s.mrk", argv[argind+1]);

  fpout = fopen(outdat, "wb");
  if (!fpout) Error("kann nicht schreiben");


  fprintf(fpout, "# %s\n", name);

  Merkmal = 0;

  while (MerkmaleDa[Merkmal] != NULL) {
    fprintf(fpout, "#%30s\n", MerkmaleDa[Merkmal]->Name);
    Merkmal++;
  }

  fprintf(fpout, "#\n");



  printf("Datei: %s\n", name);

  fpin = fopen(name, "r");

  if (!fpin) Error("kann Datei nicht öffnen");


  i = 0;

  FOREVER {

    if (!fSfRead(fpin, &sf, 1)) break;

    if (f_aus && ((i+1) & 255) == 0) { printf("%d ", i+1); fflush(stdout); }

    i++;

    Marke = sf.Marke;

    if (Marke == MA_WEISS_NICHT) 
      Error("Beispiel nicht klassifiziert");

    if      (Marke == MA_GEWONNEN)  Marke = 100;
    else if (Marke == MA_REMIS)     Marke =  50;
    else if (Marke == MA_VERLOREN)  Marke =   0;
    else if (Marke >= MA_WKEIT && Marke <= MA_WKEIT+100) Marke -= MA_WKEIT;
    else if (Marke >= MA_DIFF && Marke <= MA_DIFF+128) 
      Marke = round((((Marke - MA_DIFF) - 64) * 50) / 64.0) + 50;
    else Error("unbekannte Marke");

/*
if (Marke > 50) Marke = 100;
if (Marke < 50) Marke = 0;
*/
    if (Marke ==   0) Marke =  1;
    if (Marke == 100) Marke = 99;
 
    if (Marke >  50) z_gew++;
    if (Marke == 50) z_remis++;
    if (Marke <  50) z_ver++;

    if (f_draw_factor > 1 && Marke == 50) {

      fprintf(fpout, "%2d %d ", f_draw_factor*Marke, f_draw_factor*100);

    } else {

      fprintf(fpout, "%2d 100 ", Marke);

    }

    Merkmal = 0;

    SfBrett(&sf, &Brett);

    while (MerkmaleDa[Merkmal] != NULL) {

      a = (MerkmaleDa[Merkmal]->fun)(&Brett);

      if (a == (REAL)((int)a)) fprintf(fpout, " %.0f", a);
      else 		       fprintf(fpout, " %.4f", a);
 
      Merkmal++;
    }
    fprintf(fpout, "\n");

  }

  if (ferror(fpout)) Error("Schreibfehler");

  fclose(fpout);
  fclose(fpin);

  if (f_aus) 
	printf(
	  "\n\n%d Zeilen, %d GEWONNEN, %d REMIS, %d VERLOREN\n\n", 
	  i, z_gew, z_remis, z_ver);

  return 0;
}
