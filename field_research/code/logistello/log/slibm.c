// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/* generate simple library, 10.93/4.94 */

#ifndef PRECOMP
#include "main.h"
#endif

 

#include "sboard.h"
#include "crt.h"
#include "move.h"
#include "eval.h"
#include "killer.h"
#include "trans.h"

#include "simlib.h"


#define BEWMITTE	BewA

#define MAX_ANZ		14	/* 16 */

#define WAHL_TIEFE	11	/* 10 */
#define BEW_TIEFE	13	/* 11 */


#define BEWMITTE        BewA

#define SELECTIVE       true
#define PERCENTILE      1.5
#define QUIESCENCE      true

#define P_ABSCHN	75
#define P_DIFF		10	/* <= Prozent Differenz zum besten Zug	*/
				/* -> expandieren			*/
#define P_SAVEDIFF	2

#define WFAKTOR		100

void _abort(void) { exit(1); }
void Check(COMZIO *pcio, ZUGIO *pzio, bool no_count) {}





int Bibl(
  ZUGIO  *pzio, 
  SPFELD *psf, 
  PARTEI Partei,
  int    MaxAnz,
  FILE   *fp
)
{
  int    i, j, ZugAnz, BestAnz;
  SFPOS  Zuege[65];
  ZUGDAT ZugDat0[65], ZugDat[65]; 
  int    P, P0;
  SPFELD sf;


  ZugAnz = SfMoeglZuegeE(psf, Partei, Zuege);

  if (!ZugAnz) {

    if (!SfMoeglZuege(psf, GEGNER(Partei), Zuege)) return 0;	/* remis */

    return -Bibl(pzio, psf, GEGNER(Partei), MaxAnz, fp);
  }


  if (SfAnz(psf) >= MaxAnz) {		/* Blatt => Stellung bewerten */
					/* und Feld ablegen           */

    pzio->Sf = *psf;
    pzio->BewMitte = pzio->cio.mBewMitte;
    pzio->Modus    = MODUS_NORMAL;

    for (j=1; j <= BEW_TIEFE; j++) {

      pzio->MaxTiefe    = j;
      pzio->Partei      = Partei;
      pzio->LetzterZug  = ZUG_UNBEKANNT;

      Zugermittlung(pzio);

#if 0
if (j == RECHEN_TIEFE) {   
  KoorAus(Zuege[i]); 
  printf(":%d ", round(100*WFAKTOR*EXPWERT(WERT_TO_REAL(-pzio->Wert))));
  fflush(stdout);
}
#endif

#if 0
printf("%2d: ", j); 
KoorAus(pzio->BestZug); 
printf(": %d\n", round(100*WFAKTOR*EXPWERT(WERT_TO_REAL(-pzio->Wert))));
#endif

    }

SfAus(psf, 0, 0);
if (Partei == BLACK) printf("BLACK\n"); else printf("WHITE\n");
printf("%d discs, %d move(s)\n", 
        SfAnz(psf), ZugAnz);
fflush(stdout);

KoorAus(pzio->BestZug); 
printf(": %d\n", round(100*WFAKTOR*EXPWERT(WERT_TO_REAL(pzio->Wert))));

    ZugDat[0].Zug = pzio->BestZug;
    ZugDat[0].Wert = round(100*WFAKTOR*EXPWERT(WERT_TO_REAL(pzio->Wert)));
    BestAnz = 1;

    goto Ablegen;

  }


  if (ZugAnz > 1) {

    FOR (i, ZugAnz) {

      sf = *psf;
      if (!SfSetzen(&sf, Partei, Zuege[i])) Error("Zug");

#if 0
SfAus(&sf, 0, 0);
if (GEGNER(Partei) == BLACK) printf("BLACK\n"); else printf("WHITE\n");
#endif

      pzio->Sf= sf;
      pzio->BewMitte = pzio->cio.mBewMitte;
      pzio->Modus    = MODUS_NORMAL;

      for (j=1; j <= WAHL_TIEFE-1; j++) {

        pzio->MaxTiefe    = j;
        pzio->Partei      = GEGNER(Partei);
        pzio->LetzterZug  = ZUG_UNBEKANNT;

        Zugermittlung(pzio);

#if 0
if (j == RECHEN_TIEFE) {   
  KoorAus(Zuege[i]); 
  printf(":%d ", round(100*WFAKTOR*EXPWERT(WERT_TO_REAL(-pzio->Wert))));
  fflush(stdout);
}
#endif

#if 0
printf("%2d: ", j); 
KoorAus(pzio->BestZug); 
printf(": %d\n", round(100*WFAKTOR*EXPWERT(WERT_TO_REAL(-pzio->Wert))));
#endif

      }

      ZugDat[i].Wert = round(100*WFAKTOR*EXPWERT(WERT_TO_REAL(-pzio->Wert)));
      ZugDat[i].Zug  = Zuege[i];
    }

  } else { ZugDat[0].Zug = Zuege[0]; ZugDat[0].Wert = 50*WFAKTOR; }

#if 0
printf("\n");
#endif

  qsort((char*)ZugDat, (size_t) ZugAnz, sizeof(ZUGDAT), compZUGDAT);

  FOR (i, ZugAnz) ZugDat0[i] = ZugDat[i];


/* Zugwerte rekursiv bestimmen */


  BestAnz = 0;

  P0 = ZugDat[0].Wert;

  FOR (i, ZugAnz) {

    sf = *psf;

    if (ZugDat[i].Wert > P_ABSCHN * WFAKTOR) {

      ZugDat[BestAnz].Zug  = ZugDat[i].Zug;
      ZugDat[BestAnz].Wert = ZugDat[i].Wert;
      BestAnz++; 
      break;

    } else if (ZugDat[i].Wert >= P0-(P_DIFF*WFAKTOR) || SfAnz(&sf) <= 5) {

      if (!SfSetzen(&sf, Partei, ZugDat[i].Zug)) Error("Zug1");
      ZugDat[BestAnz].Zug  = ZugDat[i].Zug;
      ZugDat[BestAnz].Wert = - Bibl(pzio, &sf, GEGNER(Partei), MaxAnz, fp) + 
			       50 * WFAKTOR;
      BestAnz++;

    }
  }


  qsort((char*)ZugDat, (size_t) BestAnz, sizeof(ZUGDAT), compZUGDAT);



SfAus(psf, 0, 0);
if (Partei == BLACK) printf("BLACK\n"); else printf("WHITE\n");
printf("%d discs, %d move(s)\n", 
        SfAnz(psf), ZugAnz);
fflush(stdout);

printf("shallow: ");
FOR (i, ZugAnz) { 
  KoorAus(ZugDat0[i].Zug); 
  printf(":%d ", ZugDat0[i].Wert);
}
printf("\n");

printf("deep   : ");
FOR (i, BestAnz) { 
  KoorAus(ZugDat[i].Zug); 
  printf(":%d ", ZugDat[i].Wert);
}
printf("\n");

if (ZugDat[0].Zug != ZugDat0[0].Zug) {
  FOR (i, ZugAnz) if (ZugDat0[i].Zug == ZugDat[0].Zug) break;
  printf(">>> different best move! diff=%d discnum=%d\n", 
    ZugDat0[0].Wert - ZugDat0[i].Wert, SfAnz(psf));
}

Ablegen:

  sf = *psf;
  if (Partei == WHITE) SfInvert(&sf);

/* "beste" Züge ablegen */

printf("save: ");

  FOR (i, BestAnz) {

    if (ZugDat[i].Wert >= ZugDat[0].Wert - P_SAVEDIFF*WFAKTOR) {

KoorAus(ZugDat[i].Zug); printf(" ");

      sf.Marke = MA_ZUG+ZugDat[i].Zug;
      fSfWrite(fp, &sf, 1); fflush(fp);
    }
  }

printf("\n");

  return ZugDat[0].Wert - 50 * WFAKTOR;
}



int main(int argc, char **argv)
{
  char   name[1000];
  int    i;
  ZUGIO  zio;
  FILE   *fp;
  SPFELD sf, sf1;
  PARTEI BiblPartei;


#if 1
  if (argc != 3) {

Fehler:

    Error("call: oslib id|move osp-file(startpos)");
  }



  fp = fopen(argv[2], "r");
  if (!fp) Error("can't open libfile");

  if (fTabEin(fp, &sf)) Error("startpos?");

  fclose(fp);


  FOR (i, 100) 
    if (sf.p[i] >= NUM_DISP) sf.p[i] = LEER;

  SfAus(&sf, 0, 0);
  if (sf.Marke == BLACK) printf("BLACK"); else printf("WHITE"); 
  printf("\n");

  if (strlen(argv[1]) == 2) {

    int Zug;

    if (argv[1][0] >= 'a' && argv[1][0] <= 'h' &&
        argv[1][1] >= '1' && argv[1][1] <= '8') {

      Zug = Tab8to10[(argv[1][1]-'1')*8 + argv[1][0]-'a'];
      sf1 = sf;
      if (!SfSetzen(&sf1, sf.Marke, Zug)){

	Error("Zug geht nicht");
      }

      if (sf.Marke == WHITE) SfInvert(&sf);
      sf.Marke = MA_ZUG+Zug;

      fp = fopen("o_.sl", "w");
      if (!fp) Error("can't open libfile");

      fSfWrite(fp, &sf, 1); fflush(fp);

KoorAus(Zug);
printf(" written to o_.sl\n");

      exit(0);
    }
  }


  sprintf(name, "o_.sl%s", argv[1]);

#else

  if (argc != 2) {

Fehler:

    Error("Aufruf: ob Partei(b|w)");
  }


  if (argv[1][0] == 'b') BiblPartei = BLACK; else BiblPartei = WHITE;
  sprintf(name, "o_.eb%s", argv[1]);
  SfGrund(&sf); sf.Marke = BLACK;

#endif

  fp = fopen(name, "w");
  if (!fp) Error("Bibl-Datei nicht zu öffnen");

  InitZug(&zio, BEWMITTE, Check, HASHBITS);

  zio.Selective  = SELECTIVE;
  zio.Percentile = PERCENTILE;
  zio.Quiescence = QUIESCENCE;

  Bibl(&zio, &sf, sf.Marke, MAX_ANZ, fp);

  fclose(fp);

  return 0;
}



