// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/* Strahlentabellen schätzen / 8.92, 12.92, 4.93, 8.93 */

#ifndef PRECOMP
#include "main.h"
#endif

 
#include "board.h"
#include "crt.h"
#include "fpatt.h"
#include "patt.h"
#include "eval.h"

#define TEST		false

/* Einträge auf T_MAX als Betragsmaximum normieren 
 *   hierzu Einträge mit mindesten BEISP_MIN Beispielen benutzen
 */
 
#define BEISP_MIN	16	/* für Maximumsbildung */

#define FAKTOR		0.55	/* Anteil von Wert für BLACK am Zug */

#define	EXTRAPOL_ANZ	11


#define T_MAX		127	/* maximaler Tabelleneintrag   */


#define MITTEL		false	/* immer! */

int   FeldAnz[65];
float FeldDiff[65];


#undef X

int f_aus=false;

typedef struct {

  uint4 z, n; 
  int   IN[33];
  float IY[33];

} TABEINTRAG;


static TABEINTRAG *tabr1;
/*
static TABEINTRAG *tabr2;
static TABEINTRAG *tabr3;

static TABEINTRAG *tabm1;
static TABEINTRAG *tabm2;
*/

static TABEINTRAG *tabhv1;
static TABEINTRAG *tabhv2;
static TABEINTRAG *tabhv3;
static TABEINTRAG *tabhv4;

static TABEINTRAG *tabd1;
static TABEINTRAG *tabd2;
static TABEINTRAG *tabd3;
static TABEINTRAG *tabd4;
static TABEINTRAG *tabd5;
static TABEINTRAG *tabd6;


void _abort(void) { exit(1); }


inline void SteinEinfAus(FILE *fp, PARTEI Partei)
{
  if      (Partei == LEER)  fprintf(fp, "-");
  else if (Partei == BLACK) fprintf(fp, "X");
  else			    fprintf(fp, "O");
}



void MusterAus(FILE *fp, int n, int SteinAnz)
{
  int i, r, Inh[10];


  FOR (i, SteinAnz) {

    r = n % 3;
    
    if      (r == 0) Inh[i] = WHITE;
    else if (r == 1) Inh[i] = LEER;
    else             Inh[i] = BLACK;

    n /= 3;
  }

  FOR (i, SteinAnz) SteinEinfAus(fp, Inh[SteinAnz-1-i]);

}
  





int Pot3[9] = { 1, 3, 9, 27, 81, 243, 729, 2187, 6561 };  

 

#define BERECHNE(TAB, STYP, SP, STANZ, DAT, OUT) \
{\
  int i, j, k, anz=Pot3[STANZ];\
  float a, max=-1;\
  FILE *fp=NULL;\
\
  *SP = (STYP) calloc(1, anz * (IANZ+1));\
  if (!*SP) Error("BERECHNE: Speicher");\
\
  FOR (i, anz) {\
    FOR (j, IANZ+1) {\
      if (TAB[i].IN[(j * IBREITE + I0)/2] >= BEISP_MIN) {\
	a = fabs(TAB[i].IY[(j * IBREITE + I0)/2]);\
        if (a > max) max = a;\
      }\
    }\
  }\
\
  if (OUT) {\
    fp = fopen(DAT, "w"); if (!fp) Error("can't open file");\
    fprintf(fp, "max=%f\n", max);\
  }\
  if (max > 0) a = ((float)T_MAX) / max; else a = 1.0;\
  FOR (i, anz) {\
    if (OUT) { MusterAus(fp, i, STANZ); fprintf(fp, "\n"); }\
    FOR (j, IANZ+1) {\
      k = round(a * TAB[i].IY[(j * IBREITE + I0)/2]);\
      if      (k >  T_MAX) k =  T_MAX;\
      else if (k < -T_MAX) k = -T_MAX;\
      if (OUT) fprintf(fp, "%2d: %4d\n", j * IBREITE + I0, k);\
      (**SP)[j][i] = k;\
    }\
    if (OUT) fprintf(fp, "\n");\
  }\
  if (OUT) fclose(fp);\
}




void HaeufAus(char *name, TABEINTRAG *tab, 
	      void Ausgabe(FILE *fp, int i, int SteinAnz), int SteinAnz, bool aus) 
{
  int  i, j, anz, suanz, StrahlAnz, min, max;
  FILE *fp;
  double su, m[65], mm[3];


  if (aus) {

    fp = fopen(name, "w");
    if (!fp) Error("Schreibfehler");
  }
 
  StrahlAnz = Pot3[SteinAnz];

  anz = 0;

#if MITTEL
  FOR (i, 65) fprintf(fp, "%2d: %.1f\n", i, FeldDiff[i] / (FeldAnz[i] + 0.0001));
#endif

  FOR (i, StrahlAnz) {

/*printf("%d\n", i);*/

    if (aus) { 

      fprintf(fp, "/* ");

      Ausgabe(fp, i, SteinAnz); 
      fprintf(fp, " %6d */\n", tab[i].n); 
    }


/* Mittelwerte berechnen */

    min = 65; max = -1;

    FOR (j, 65) {

      if (tab[i].IN[j/2]) {

	if (j > max) max = j;
	if (j < min) min = j;

        m[j] = ((REAL)tab[i].IY[j/2]) / tab[i].IN[j/2];

      } else m[j] = 0;
    }


    if (min <= max) {


      anz++;	/* mindestens ein Beispiel da */


/* mit Mittelwerten extrapolieren */

/* vor Minimum */

      su = 0; suanz = 0;

      for (j=min; j <= min + EXTRAPOL_ANZ && j < 65; j++) {

        if (tab[i].IN[j/2]) {

  	  su += m[j]; suanz++;

        }
      }

if (!suanz) Error("anz=0 1");

      su /= suanz;

      FOR (j, min) m[j] = su;


/* hinter Maximum */

      su = 0; suanz = 0;

      for (j=max; j >= max - EXTRAPOL_ANZ && j >= 0; j--) {

        if (tab[i].IN[j/2]) {

	  su += m[j]; suanz++;

        }
      }

if (!suanz) Error("anz=0 2");

      su /= suanz;

      for (j=max+1; j <= 64; j++) m[j] = su;
    }


#if MITTEL

    FOR (j, 65) m[j] -= FeldDiff[j] / (FeldAnz[j]+0.0001);

#endif



/* mit kleinem Fenster glätten */

#define FL	(-4)
#define FR	(+3)
#define FANZ	(FR-FL+1)

    su = 0;
    for (j=FL; j <= FR; j++) su += (0+j >= 0 && 0+j <= 64) ? m[0+j] : 0;

    mm[0] = su / FANZ;

    for (j=1; j <= 64; j++) {

      su = su - (j+FL-1 >= 0 ? m[j+FL-1] : 0) + (j+FR <= 64 ? m[j+FR] : 0);

      mm[j] = su / FANZ;

    }

    if (aus) {

      FOR (j, 65) 
        fprintf(fp, "%2d: %5.1f %5.1f %d\n", 
	    j, 
  	    m[j], mm[j], tab[i].IN[j/2]);

      fprintf(fp, "\n");
    }

    FOR (j, 65) {

      if      (mm[j] > +127.0) mm[j] = +127.0;
      else if (mm[j] < -127.0) mm[j] = -127.0;

      tab[i].IY[j/2] = mm[j];
    }

  }



  FOR (i, StrahlAnz/2) {

    FOR (j, 65) {

      float r1, r2;

      r1 = tab[i].IY[j/2];
      r2 = tab[StrahlAnz-1-i].IY[j/2];

      tab[i].IY[j/2]             = FAKTOR * r1 - (1-FAKTOR) * r2;
      tab[StrahlAnz-1-i].IY[j/2] = FAKTOR * r2 - (1-FAKTOR) * r1;

    }
  }

if (aus) {

  FOR (i, StrahlAnz) {

    fprintf(fp, "/* ");

    Ausgabe(fp, i, SteinAnz); 
    fprintf(fp, " */\n"); 

    FOR (j, 65) { 

      fprintf(fp, "%2d: %.1f\n", j, tab[i].IY[j/2]);
    }
  }
  fprintf(fp, "\n/* Anzahl: %d */\n\n", anz); fclose(fp);
}


}





#define TABHOLEN(tab,anz) \
  tab = (TABEINTRAG*) calloc(sizeof(TABEINTRAG), anz);\
  if (!tab) Error("oreg: Speicher");




int main(int argc, char **argv)
{
  char name[100];
  int  i, j, k, Marke, dateinr, StrNr, Anz;
  int  z=1, AnzG=0, AnzV=0, AnzR=0;
  SPFELD sf;
  STRAHLEN Strahlen;
  FILE  *fp;
  STRAHLZEIGER sz;
  bool f_r=false;
  float Wert;


  InitCrt();
  InitSetzen();


  if (argc == 1) {
Fehler:
    Error("Aufruf: oreg sfk-Datei(en)");
  }

  StrNr = 1;


  TABHOLEN(tabr1, ANZ8);
/*
  TABHOLEN(tabr2, ANZ8);
  TABHOLEN(tabr3, ANZ8);
  TABHOLEN(tabm1, ANZ8);
  TABHOLEN(tabm2, ANZ8);
*/
  TABHOLEN(tabhv1, ANZ8);
  TABHOLEN(tabhv2, ANZ8);
  TABHOLEN(tabhv3, ANZ8);
  TABHOLEN(tabhv4, ANZ8);
  TABHOLEN(tabd1,  ANZ8);
  TABHOLEN(tabd2,  ANZ7);
  TABHOLEN(tabd3,  ANZ6);
  TABHOLEN(tabd4,  ANZ5);
  TABHOLEN(tabd5,  ANZ4);
  TABHOLEN(tabd6,  ANZ3);


  FOR (i, 65) FeldDiff[i] = FeldAnz[i] = 0;


  for (dateinr=StrNr; dateinr < argc; dateinr++) {

    sprintf(name, "%s", argv[dateinr]);
  
    printf("oreg: %s\n", name);

    fp = fopen(name, "r");

    if (!fp) { printf("*** kann Datei nicht öffnen\n"); continue; }


    
    for (i=0; ; i++) {

      j = fSfRead(fp, &sf, 1);

      if (j != 1) break;
 
      Marke = sf.Marke;

      if (Marke == MA_WEISS_NICHT) Error("Beispiel nicht klassifiziert");

      if      (Marke == MA_GEWONNEN) { Wert = +T_MAX; AnzG++; }
      else if (Marke == MA_VERLOREN) { Wert = -T_MAX; AnzV++; }
      else if (Marke == MA_REMIS)    { Wert = 0;      AnzR++; }
      else if (Marke >= MA_WKEIT && Marke <= MA_WKEIT+100) {

	if (Marke >  MA_WKEIT+50) AnzG++; 
	if (Marke <  MA_WKEIT+50) AnzV++; 
	if (Marke == MA_WKEIT+50) AnzR++; 

	Wert = (Marke - MA_WKEIT - 50) * (T_MAX / 50.0);

      } else if (Marke >= MA_DIFF && Marke <= MA_DIFF+128) {

	if (Marke >  MA_DIFF+64) AnzG++; 
	if (Marke <  MA_DIFF+64) AnzV++; 
	if (Marke == MA_DIFF+64) AnzR++; 

	Wert = (Marke - MA_DIFF - 64) * (T_MAX / 64.0);

      } else Error("unbekannte Marke");

      Anz = SfAnz(&sf);

      FeldAnz[Anz]++;
      FeldDiff[Anz] += Wert;

      z++;

      if (z > 1000000000) Error("zu viele Spielfelder\n");


      if (f_r) {


      } else {

        FOR (k, 2) {

  	  FOR (j, 4) {

	    BestimmeStrahlen(sf.p, BLACK, &Strahlen);


#define ANP(e, s) \
if (e[s].z != z) {\
  e[s].z  = z;\
  e[s].IY[Anz/2] += Wert;\
  e[s].IN[Anz/2] ++;\
  e[s].n++;\
}

	    ANP(tabr1, Strahlen.r1a);
/*
	    ANP(tabr2, Strahlen.r2a);
	    ANP(tabr3, Strahlen.r3a);

	    ANP(tabm1, Strahlen.m1a);
	    ANP(tabm2, Strahlen.m2a);
*/

	    ANP(tabhv1, Strahlen.s1);
	    ANP(tabhv2, Strahlen.s2);
	    ANP(tabhv3, Strahlen.s3);
	    ANP(tabhv4, Strahlen.s4);

	    ANP(tabd1, Strahlen.dm8);
	    ANP(tabd2, Strahlen.dm7);
	    ANP(tabd3, Strahlen.dm6);
	    ANP(tabd4, Strahlen.dm5);
	    ANP(tabd5, Strahlen.dm4);
	    ANP(tabd6, Strahlen.dm3);

	    SfDrehen(&sf);
	  }

	  SfTransponieren(&sf);
        }
      }
    }
    fclose(fp);
  }


  printf("oreg: %d gewonnen, %d remis, %d verloren\n", AnzG, AnzR, AnzV);


f_aus = true;


  printf("oreg: strr1.info\n");
  HaeufAus("strr1.info", tabr1, MusterAus, 8, f_aus);

/*
  printf("oreg: strr2.info\n");
  HaeufAus("strr2.info", tabr2, MusterAus, 8, f_aus);

  printf("oreg: strr3.info\n");
  HaeufAus("strr3.info", tabr3, MusterAus, 8, f_aus);



  printf("oreg: strm1.info\n");
  HaeufAus("strm1.info", tabm1, MusterAus, 8, f_aus);

  printf("oreg: strm2.info\n");
  HaeufAus("strm2.info", tabm2, MusterAus, 8, f_aus);
*/


  printf("oreg: strhv1.info\n");
  HaeufAus("strhv1.info", tabhv1, MusterAus, 8, f_aus);

  printf("oreg: strhv2.info\n");
  HaeufAus("strhv2.info", tabhv2, MusterAus, 8, f_aus);

  printf("oreg: strhv3.info\n");
  HaeufAus("strhv3.info", tabhv3, MusterAus, 8, f_aus);

  printf("oreg: strhv4.info\n");
  HaeufAus("strhv4.info", tabhv4, MusterAus, 8, f_aus);


  printf("oreg: strd1.info\n");
  HaeufAus("strd1.info", tabd1, MusterAus, 8, f_aus);

  printf("oreg: strd2.info\n");
  HaeufAus("strd2.info", tabd2, MusterAus, 7, f_aus);

  printf("oreg: strd3.info\n");
  HaeufAus("strd3.info", tabd3, MusterAus, 6, f_aus);

  printf("oreg: strd4.info\n");
  HaeufAus("strd4.info", tabd4, MusterAus, 5, f_aus);

  printf("oreg: strd5.info\n");
  HaeufAus("strd5.info", tabd5, MusterAus, 4, f_aus);

  printf("oreg: strd6.info\n");
  HaeufAus("strd6.info", tabd6, MusterAus, 3, f_aus);


  BERECHNE(tabr1, SW8, &sz.r1, 8, "strr1.tab", f_aus);
/*
  BERECHNE(tabr2, SW8, &sz.r2, 8, "strr2.tab", f_aus);
  BERECHNE(tabr3, SW8, &sz.r3, 8, "strr3.tab", f_aus);

  BERECHNE(tabm1, SW8, &sz.m1, 8, "strm1.tab", f_aus);
  BERECHNE(tabm2, SW8, &sz.m2, 8, "strm2.tab", f_aus);
*/

  BERECHNE(tabhv1, SW8, &sz.hv1, 8, "strhv1.tab", f_aus);
  BERECHNE(tabhv2, SW8, &sz.hv2, 8, "strhv2.tab", f_aus);
  BERECHNE(tabhv3, SW8, &sz.hv3, 8, "strhv3.tab", f_aus);
  BERECHNE(tabhv4, SW8, &sz.hv4, 8, "strhv4.tab", f_aus);

  BERECHNE(tabd1, SW8, &sz.d1, 8, "strd1.tab", f_aus);
  BERECHNE(tabd2, SW7, &sz.d2, 7, "strd2.tab", f_aus);
  BERECHNE(tabd3, SW6, &sz.d3, 6, "strd3.tab", f_aus);
  BERECHNE(tabd4, SW5, &sz.d4, 5, "strd4.tab", f_aus);
  BERECHNE(tabd5, SW4, &sz.d5, 4, "strd5.tab", f_aus);
  BERECHNE(tabd6, SW3, &sz.d6, 3, "strd6.tab", f_aus);

  StrahlenSchreiben("o_.tab0", &sz);


  return 0;
}



