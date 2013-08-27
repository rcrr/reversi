// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#ifndef PRECOMP
#include "main.h"
#endif


#include "crt.h" 
#include "sboard.h"
#include "playgm.h"
#include "goodies.h"
#include "eval.h"

#define BEW_MITTE	BewA
#define PARAM_DATEI	BewADatei	/*!!!*/

#define INTCPT		0		/* keine INTCPT-Anpassung */

void _abort(void) { exit(1); }

#define MMAX 		20

int main(int argc, char **argv)
{
  char	    name[200];
  int	    i, j, e, v, nr, maxnr, AnzW, Anz, argi;
  SPIELINFO Spiel[120];
  SPFELD    sf, sf0, sfin;
  FILE	    *fpin, *fpw, *fpgew, *fpver;
  int	    *NrMin, *NrMax;
  bool	    f_a=false, f_t=false, f_w=false, f_p=false, f_h=false, *M=NULL;
  int	    FAnz[100], FGesamt;
  sint1	    *p;
  float     dsum;
  int	    danz, ZugAnz;
  SFPOS     Zuege[65];
  struct { int min, max; } hist[101];
  int	    mhist[2*MMAX+1];

  if (argc != 3 && argc != 4) {
Fehler:
    Error("\
Aufruf: ohalf (-a | -t | -h | -w Anzahl | -p Parameter-Datei) sfk-Datei\"*\"\n\
          -a:  Anzahl der Spielfelder ausgeben\n\
	  -t:  Spielfelder nach Gewinn trennen (-> *.gew, *.ver)\n\
          -h:  Gemäß Histogramm symmetrisch Spielfelder auswählen (->*.his) \n\
          -w:  Anzahl Spielfelder zufällig auswählen (-> *.aus)\n\
	  -p:  Gemäß Parameter trennen\n");
  }

  argi = 2;

  if      (!strcmp(argv[1], "-a")) f_a = true;
  else if (!strcmp(argv[1], "-t")) f_t = true;
  else if (!strcmp(argv[1], "-h")) f_h = true;
  else if (!strcmp(argv[1], "-p")) {

    f_p = true;
    if (strlen(argv[2]) > DATEI_MAX-2) 
      Error("ohalf: Parameterdateiname zu lang");
    strcpy(PARAM_DATEI, argv[2]);
    argi = 3;

  } else if (!strcmp(argv[1], "-w")) {

    f_w = true;
    if (sscanf(argv[2], "%d", &AnzW) != 1 || AnzW <= 0) Error("ohalf: Anzahl?");
    argi = 3;
  }


  fpin = fopen(argv[argi], "r");
  if (!fpin) Error("ohalf: sfk-Datei nicht da");


  InitSetzen();


  if (f_h) FOR (i, 101) hist[i].min = hist[i].max = 0;

  FOR (i, 2*MMAX+1) mhist[i] = 0;

  if (f_t || f_p) {		/* trennen */

    sprintf(name, "%s.gew", argv[argi]);

    fpgew = fopen(name, "w");
    if (!fpgew) Error("ohalf: Schreibfehler");

    sprintf(name, "%s.ver", argv[argi]);

    fpver = fopen(name, "w");
    if (!fpver) Error("ohalf: Schreibfehler");

  }

  FGesamt = 0;

  dsum = 0.0; danz = 0;


  for (Anz=0; ; Anz++) {

    if (!fSfRead(fpin, &sfin, 1)) break;

#if FELDWKEIT
    p = sfin.p;

    FOR (i, 100) FAnz[i] += p[i] != LEER;

    FGesamt += 1;
#endif

/*
    { BRETT Brett;
    SfBrett(&sfin, &Brett);
    i = MOB(&Brett);
    }

    if (sfin.Marke == MA_VERLOREN || sfin.Marke <= MA_WKEIT+49) {
      mhist[i+MMAX]++;
    }
*/

    if (f_h) {

      if (sfin.Marke >= MA_WKEIT && sfin.Marke <= MA_WKEIT+100) 
        hist[sfin.Marke - MA_WKEIT].max++;
      else 
	 Error("ohalf: nicht MA_WKEIT");
    }


#if INTCPT

/* INTCPT-Anpassung */

    ZugAnz = SfMoeglZuege(&sfin, BLACK, Zuege);

    if (ZugAnz) {

      SPFELD sf;
      BRETT  Brett;
      int    w, wmin = INT_MAX;

      FOR (i, ZugAnz) {

        sf = sfin;
        if (!SfSetzen(&sf, BLACK, Zuege[i])) Error("Zug?");
        SfBrett(&sf, &Brett);

        w = BEW_MITTE(&Brett, WHITE);
        if (w < wmin) wmin = w;
      }

      SfBrett(&sfin, &Brett);

      w = BEW_MITTE(&Brett, BLACK);

/*printf("%d %d\n", w, wmin);*/

      dsum += - (w + wmin) / 2;
      danz ++;
        
    }

#endif



    if (f_t) {

      if (sfin.Marke == MA_WEISS_NICHT) Error("ohalf: Spielfeld nicht klassifiziert");


      if (sfin.Marke == MA_GEWONNEN || 
	  (sfin.Marke >= MA_WKEIT && sfin.Marke <= MA_WKEIT+100 && 
	   sfin.Marke > MA_WKEIT + 50))
	fSfWrite(fpgew, &sfin, 1);

      if (sfin.Marke == MA_VERLOREN || 
	  (sfin.Marke >= MA_WKEIT && sfin.Marke <= MA_WKEIT+100 && 
	   sfin.Marke < MA_WKEIT + 50))
	fSfWrite(fpver, &sfin, 1);

    }

    if (f_p) {

      int   Wert;
      BRETT Brett;


      if (sfin.Marke == MA_WEISS_NICHT) Error("ohalf: Spielfeld nicht klassifiziert");

      SfBrett(&sfin, &Brett);

      Wert = BEW_MITTE(&Brett, BLACK);

#if 0
{ SPFELD sf;
  BrettSf(&Brett, &sf);
  SfAus(&sf, BLACK, 0);
  printf("%d\n", Wert);
}
#endif

      if (Wert >= 0) fSfWrite(fpgew, &sfin, 1);
      else 	     fSfWrite(fpver, &sfin, 1);

    }
  }

  fclose(fpin);

/*

for (i=1; i < 2*MMAX+1; i++) mhist[i] += mhist[i-1];

FOR (i, 2*MMAX+1) printf("%d: %.2f\n", i-MMAX, ((float)mhist[i])/mhist[2*MMAX]);

FOR (i, 101) printf("%d %d\n", i, hist[i].max);

*/


  if (f_h) {

/* Histogramm um 50% symmetrisch machen */

    FOR (i, 50) 

      if (hist[i].max < hist[100-i].max) {

	hist[i].min = hist[100-i].min = hist[i].max;

      } else {

	hist[100-i].min = hist[i].min = hist[100-i].max;

      }

    FOR (i, 101) printf("%d %d\n", i, hist[i].min);

    fpin = fopen(argv[argi], "r");
    if (!fpin) Error("ohalf: sfk-Datei nicht da");

    sprintf(name, "%s.his", argv[argi]);

    fpver = fopen(name, "w");
    if (!fpver) Error("ohalf: Schreibfehler");

    fpin = fopen(argv[argi], "r");
    if (!fpin) Error("ohalf: sfk-Datei nicht da");


    for (Anz=0; ; Anz++) {

      if (!fSfRead(fpin, &sfin, 1)) break;

      if (sfin.Marke == MA_VERLOREN || sfin.Marke == MA_REMIS ||
	  sfin.Marke == MA_GEWONNEN) Error("ohalf: nicht MA_WKEIT");

      if (hist[sfin.Marke - MA_WKEIT].min > 0) {

        if (MyRand() % hist[sfin.Marke - MA_WKEIT].max < 
	    hist[sfin.Marke - MA_WKEIT].min)

	  fSfWrite(fpver, &sfin, 1);
      }
    }

    fclose(fpver); fclose(fpin);

    exit(0);
  }




#if FELDWKEIT

  if (FGesamt) {
    FOR (i, 100) {
      printf("%5.2f,", ((float)FAnz[i])/FGesamt);
      if ((i % 10) == 9) printf("\n");
    }
  }
#endif

#if INTCPT
  if (danz)  printf("%.2f %d -> %f\n", dsum, danz, dsum/danz);
#endif



  if (f_a) { printf("%d\n", Anz); exit(0); }

  if (!Anz) Error("ohalf: Anzahl == 0");

  if (f_t || f_p) { fclose(fpver); fclose(fpgew); exit(0); }


  M = (bool*) calloc(sizeof(bool), Anz);

  if (!M) Error("ohalf: nicht genug Speicher");


  if (AnzW > Anz) { 

    printf("ohalf: weniger Spielfelder als auszuwählen\n");

    AnzW = Anz;
  }


  if (AnzW > Anz/2) { v = false; e = Anz - AnzW; } else { v = true; e = AnzW; }


  FOR (i, e) {

    do { j = (IRAN >> 5) % Anz; } while (M[j]);

    M[j] = true;
  }

  


  fpin = fopen(argv[argi], "r");
  if (!fpin) Error("ohalf: Datei nicht da 2");


  sprintf(name, "%s.aus", argv[argi]);

  fpw = fopen(name, "w");
  if (!fpw) Error("ohalf: Schreibfehler");


  for (i=0; ; i++) {

    if (!fSfRead(fpin, &sfin, 1)) break;

    if (M[i] == v) fSfWrite(fpw, &sfin, 1);
  }

  if (Anz != i) Error("ohalf: Anzahl hat sich verändert");


  fclose(fpin);

  if (ferror(fpw)) Error("ohalf: Schreibfehler");
  fclose(fpw);

  free(M);

  return 0;
}

