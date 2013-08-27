// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/* compare static evaluation with depth 2 value, 2.95 */

#include "main.h"
#include "sboard.h"
#include "crt.h"
#include "move.h"
#include "eval.h"
#include "killer.h"
#include "fpatt.h"


#define TEST_MODUS	MODUS_NORMAL
#define EVALFUNC	EvalA
#define PARFILE         "evala.par"
#define TABFILE         "oplay.tab"


#define WFAC 1000


typedef struct {

  uint4 sum, num;

} PATT_STAT;



int VERB = 1;


PATT_STAT e[NUM8], d[NUM8];


uint4 etab[NUM8] = {
#include "edgevar.inc"
};

uint4 dtab[NUM8] = {
#include "diagvar.inc"
};






void _abort(void) { exit(1); }



void Check(COMZIO *pcio, ZUGIO *pzio, bool no_count) 
{
}



int Suche(
  ZUGIO  *pzio,
  SPFELD *psf, 
  PARTEI Partei, 
  int Tiefe
)
{
  SFPOS z;


  pzio->Sf	= *psf;
  pzio->Partei	= Partei;
  pzio->MaxTiefe= Tiefe;
  pzio->BewMitte= EVALFUNC;


  pzio->LetzterZug= ZUG_UNBEKANNT;			/* Nix anderes! */

  Zugermittlung(pzio); z = pzio->BestZug;

  return pzio->Wert;
}



void SymSign(PATT_STAT *ps)
{
  int i, j, mirror[NUM8], symm[NUM8];
  uint4 num, sum;


  FOR (i, NUM8) {

    int p[10], n = i;


    FOR (j, 8) { p[j] = n % 3; n /= 3; }

    n = 0;

    FOR (j, 8) n = n * 3 + p[j];

    symm[i] = 0;
    mirror[i] = n;
  }


  FOR (i, NUM8) {

    if (!symm[i]) {

      num = ps[i].num + ps[mirror[i]].num + 
            ps[NUM8-1-i].num + ps[mirror[NUM8-1-i]].num;

      sum = ps[i].sum + ps[mirror[i]].sum + 
            ps[NUM8-1-i].sum + ps[mirror[NUM8-1-i]].sum;

      ps[i].num = ps[mirror[i]].num = 
      ps[NUM8-1-i].num = ps[mirror[NUM8-1-i]].num = num;

      ps[i].sum = ps[mirror[i]].sum = 
      ps[NUM8-1-i].sum = ps[mirror[NUM8-1-i]].sum = sum;

      symm[i] = symm[mirror[i]] = symm[NUM8-1-i] = symm[mirror[NUM8-1-i]] = true; 

    }
  }
}



int main(int argc, char **argv)
{
  int   w0, w1, p0, p1;
  int   diff;
  PATT_STAT *pi;
  int   Tiefe, anz, i, k;
  ZUGIO zio;
  short *pat;
BRETT Brett;
  FILE *fp;
  float var;
  SPFELD sf;


  if (argc == 1) {

    Error("usage: oecmp file(s)");
    exit(20);
  }

  InitZug(&zio, EVALFUNC, Check, HASHBITS);

  strcpy(ParameterFile, PARFILE);
  strcpy(TableFile,     TABFILE);

  for (k=1; k < argc; k++) {

    fp=fopen(argv[k], "r");
    if (!fp) Error("file not found");
    fclose(fp);
  }



  for (k=1; k < argc; k++) {

  fprintf(stderr, "%s\n", argv[k]);

  fp=fopen(argv[k], "r");

  if (!fp) Error("file?");

  for (i=0;;i++) {

if (!(i % 100)) { fprintf(stderr, "%6d\r", i); fflush(stdout); }

    if (!fSfRead(fp, &sf, 1)) break;

    SfBrett(&sf, &Brett);
    w0 = EVALFUNC(&Brett, BLACK);

    zio.Modus = MODUS_NORMAL;

    w1 = Suche(&zio, &sf, BLACK, 2);

    p0 = round(EXPWERT(WERT_TO_REAL(w0))*100.0);
    p1 = round(EXPWERT(WERT_TO_REAL(w1))*100.0);


#if 0

    printf("%2d %2d -> %d\n", p0, p1, abs(p0-p1));

    if ((w0 > 0) ^ (w1 > 0) && abs(p0-p1) > 15) {

      SfAus(&sf, BLACK, 0);

    }
    printf("%d\n", w1-w0);

#endif

 


    pat = Brett.Patt.st;

    pi = &e[(NUM8-1)/2];

    diff = abs(w1 - w0) / WFAC;

#define UPE(p) \
    if (e[3280+pat[p]].sum < 2000000000) {\
      e[pat[p]+3280].num++; e[3280+pat[p]].sum += diff; \
    }

#define UPD(p) \
    if (d[3280+pat[p]].sum < 2000000000) {\
      d[pat[p]+3280].num++; d[3280+pat[p]].sum += diff; \
    }


    UPE(BS1); UPE(BS8); UPE(BSA); UPE(BSH);

    UPD(BDM8); UPD(BDP8);

#define VTAB(t,p) t[3280+pat[p]]

#if 1


    var = (VTAB(etab, BS1) + VTAB(etab, BS8) + 
           VTAB(etab, BSA) + VTAB(etab, BSH)) * 8.862e-01
 + 
          (VTAB(dtab, BDM8) + VTAB(dtab, BDP8)) * 9.363e-01
 + -3.096e+05;
;

    printf("%7d %.4f\n", abs(w1-w0), var);

#else

    printf("%6d 100 %5d %5d\n", abs(w1-w0),
            VTAB(etab, BS1) + VTAB(etab, BS8) + VTAB(etab, BSA) + VTAB(etab, BSH),
            VTAB(dtab, BDM8) + VTAB(dtab, BDP8)
    );

#endif

  }
  fclose(fp);
  }

  SymSign(e); SymSign(d);

  printf("/* edges */\n\n");

  FOR (i, NUM8) {

    int num = i, j;

    printf("/* ");

    FOR (j, 8) {			/* Kante aus Nummer erstellen */

      int k = num % 3; num /= 3;

      if      (k == 0) printf("O");
      else if (k == 2) printf("X");
      else 	       printf("-");

    }  

    printf(" : %8lu */ %8lu,\n", e[i].num, round(e[i].sum*WFAC/(e[i].num+0.01))); 

  }



  printf("\n\n\n/* diagonals */\n\n");

  FOR (i, NUM8) {

    int num = i, j;

    printf("/* ");

    FOR (j, 8) {			/* Kante aus Nummer erstellen */

      int k = num % 3; num /= 3;

      if      (k == 0) printf("O");
      else if (k == 2) printf("X");
      else 	       printf("-");

    }  

    printf(" : %8lu */ %8lu,\n", d[i].num, round(d[i].sum*WFAC/(d[i].num+0.01))); 

  }

  return 0;
}
