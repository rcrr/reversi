// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/* test symmetry of evaluation , 21.11.91 */

#include "main.h"
#include "sboard.h"
#include "crt.h"
#include "move.h"
#include "eval.h"
#include "killer.h"

#include "fpatt.h"


// small differences are OK due to rounding in table generation

#define LIMIT    10
#define ALSO_WTM 1


#define DATEI "xxx.sfk"
/*#define DATEI "o50.sfk"*/

#define TEST_MODUS	MODUS_NORMAL
#define EVALFUNC	EvalA
#define PARFILE         "logit.pardiff.par"
#define TABFILE         "new_l.tab"

#define TESTANZ		10000
#define TIEFE		7

#define _EINZELN
#define _SYMM

#define SFMAX		10000

#ifdef AMIGA
#define TESTDATEI	DATEI
#else
#define TESTDATEI	DATEI
/*#define TESTDATEI	"osfk/"DATEI*/
#endif

int VERB = 1;

int y;

void xxx() {
if (y) printf("a");
}

SPFELD *SFelder=NULL;


void _abort(void) { exit(1); }


void Check(COMZIO *pcio, ZUGIO *pzio, bool no_count, bool) 
{
  Chk_Abort();
}


WERT test_feature(BRETT *pb, PARTEI col)
{
  int val = OR_ONE(pb);

  if (col == BLACK) return val; else return -val;
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

printf("  -> "); KoorAus(z);

printf(" W=%d\n", pzio->Wert);

  return pzio->Wert;
}


#if 0
int EndSuch(
  ZUGIO  *pzio,
  SPFELD *psf, 
  PARTEI Partei, 
  int Tiefe
)
{
  int Wert;


  pzio->Modus = MODUS_GEWINN;
  Wert = Suche(pzio, psf, Partei, 0);

  if (Wert > 0) { pzio->EndAl = Wert; pzio->EndBe = WERTWIPEOUT;  }
  if (Wert < 0) { pzio->EndAl = -WERTWIPEOUT; pzio->EndBe = Wert; }

  pzio->Modus = MODUS_NEGAC;
  Suche(pzio, psf, Partei, 0);

  return pzio->Wert;
}
#endif

int NegaZeit=0;


#if 0
int NegaC(
  ZUGIO  *pzio,
  SPFELD *psf, 
  PARTEI Partei, 
  int Tiefe
)
{
  int i, Mitte, Wert, min, max, Start, Ende, z = 0;

Start = clock();

  pzio->Modus = MODUS_NEGAC;

  min = -64; max = 64;

  while (min < max) {

    Mitte = (min+max)/2;

    pzio->EndAl = Mitte-1;
    if (pzio->EndAl > 0) pzio->EndAl += WERTGEWINN;
    if (pzio->EndAl < 0) pzio->EndAl -= WERTGEWINN;

    pzio->EndBe = Mitte+1;
    if (pzio->EndBe > 0) pzio->EndBe += WERTGEWINN;
    if (pzio->EndBe < 0) pzio->EndBe -= WERTGEWINN;


printf("Al=%d Be=%d ", pzio->EndAl, pzio->EndBe); fflush(stdout);
    
    Suche(pzio, psf, Partei, i);


    Wert = pzio->Wert;

    if      (Wert >  WERTGEWINN) Wert -= WERTGEWINN;
    else if (Wert < -WERTGEWINN) Wert += WERTGEWINN;

    if (Wert == Mitte) break;

    if (Wert > Mitte) min = Wert; else max = Wert;

    printf("-> [%d,%d]\n", min, max);

z++;
if (z == 4) {
  NegaZeit += clock()-Start;
  printf(" z=%.1f\n", (REAL)(clock() - Start)/CLOCKS_PER_SEC);
}

  }
  
  return Wert;
}

#endif


void Iter(
  ZUGIO  *pzio,
  SPFELD *psf, 
  PARTEI Partei, 
  int Tiefe
)
{
  int i;


  for (i=1; i <= Tiefe; i++) {
    
    printf("%d: ", i);

    Suche(pzio, psf, Partei, i);

  }
}

#if 0
static short Diag[6561+1] = {
  //#include "diag.tab"
0
};

static short Inter[6561+1] = {
  //#include "inter.tab"
0
};

static short Strahl1[6561+1] = {
  //#include "strahl1.tab"
0
};
static short Strahl2[6561+1] = {
  //#include "strahl2.tab"
0
};
static short Strahl3[6561+1] = {
  //#include "strahl3.tab"
0
};
static short Strahl4[6561+1] = {
  //#include "strahl4.tab"
0
};
#endif

void SymmTest(sint1 *fe)
{
  int i1, i2, i3, i4, i5, i6, i7, i8, ind1, ind2;


  for (i1=0; i1<=2; i1++) 
  for (i2=0; i2<=2; i2++) 
  for (i3=0; i3<=2; i3++) 
  for (i4=0; i4<=2; i4++) 
  for (i5=0; i5<=2; i5++) 
  for (i6=0; i6<=2; i6++) 
  for (i7=0; i7<=2; i7++) 
  for (i8=0; i8<=2; i8++) {

    ind1 = i1 + 3*i2 + 9*i3 + 27*i4 + 81*i5 + 243*i6 + 729*i7 + 2187*i8;
    ind2 = i8 + 3*i7 + 9*i6 + 27*i5 + 81*i4 + 243*i3 + 729*i2 + 2187*i1;
 
    if (fe[ind1] != fe[ind2]) 
      printf("%d:%d %d:%d\n", ind1, fe[ind1], ind2, fe[ind2]);
/*{ printf("*"); fflush(stdout); }*/
  }
}


void SymmTest81(sint1 *fe)
{
  int i1, i2, i3, i4, i5, i6, i7, i8, ind1, ind2;


  for (i1=0; i1<=2; i1++) 
  for (i2=0; i2<=2; i2++) 
  for (i3=0; i3<=2; i3++) 
  for (i4=0; i4<=2; i4++) 
  for (i5=0; i5<=2; i5++) 
  for (i6=0; i6<=2; i6++) 
  for (i7=0; i7<=2; i7++) 
  for (i8=0; i8<=2; i8++) {

    ind1 = i4 + 3*i3 + 9*i2 + 27*i1 + 81*i8 + 243*i7 + 729*i6 + 2187*i5;
    ind2 = i7 + 3*i8 + 9*i6 + 27*i5 + 81*i3 + 243*i4 + 729*i2 + 2187*i1;
 
    if (fe[ind1] != fe[ind2]) 
      printf("%d:%d %d:%d\n", ind1, fe[ind1], ind2, fe[ind2]);
/*{ printf("*"); fflush(stdout); }*/
  }
}





int main(void)
{
  int   extranz=0, extrfalsch = 0;

  int   z, Tiefe, anz, i, j, k, r, r0, r1;
  uint4 Start, Ende, EinzelnClock, DanachClock, IterClock=0, 
	EinzelnKB=0, EinzelnKW=0, IterKB=0, IterKW=0;
  ZUGIO zio;
BRETT Brett, Brett0;
  int  w;



  SFelder = (SPFELD*) malloc( SFMAX * sizeof(SPFELD));

  if (!SFelder) Error("Speicher?");

  InitZug(&zio, EVALFUNC, Check, HASHBITS, HASHMAXDEPTH);

  strcpy(ParameterFile, PARFILE);
  strcpy(TableFile,     TABFILE);


  anz = SfRead(SFelder, SFMAX, TESTDATEI);

  if (!anz) Error("Datei nicht da");

  if (anz > TESTANZ) anz = TESTANZ;


  EinzelnClock = DanachClock = IterClock = 0;

  SfBrett(&SFelder[0], &Brett);

/*  w = EVALFUNC(&Brett, BLACK); */

  FOR (i, anz) {

int pos, col=WHITE, opp=-col;
DELTA delta;

    SfBrett(&SFelder[i], &Brett);

printf("."); fflush(stdout);
//SfAus(&SFelder[i], 0, 0);

    w = EVALFUNC(&Brett, BLACK);

    // printf("w=%d\n", w);
  

    FOR (k, 2) {

      FOR (j, 4) {

/*SfAus(&SFelder[i], BLACK, 0);*/
        SfBrett(&SFelder[i], &Brett);

	//printf("%d: B %d %d %d :::", i, j, k, w);

	int e = EVALFUNC(&Brett, BLACK);

        if (abs(w - e) >= LIMIT) {

	  SfAus(&SFelder[i], 0, 0);
	  printf("%d: B %d %d %d %d\n", i, j, k, w, e);

	}

#if ALSO_WTM

        SfInvert(&SFelder[i]);

/*SfAus(&SFelder[i], BLACK, 0);*/
        SfBrett(&SFelder[i], &Brett);

	//printf("%d: B %d %d %d :::", i, j, k, w);

	e = EVALFUNC(&Brett, WHITE);

        if (abs(w - e) >= LIMIT) {

	  SfAus(&SFelder[i], 0, 0);
	  printf("%d: W %d %d %d %d\n", i, j, k, w, e);

	}

        SfInvert(&SFelder[i]);
#endif

	SfDrehen(&SFelder[i]);
      }

      SfTransponieren(&SFelder[i]);
    }

  }

printf("%d boards\n", anz);

  return 0;
}
