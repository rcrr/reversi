// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/* Parameter für selektive Suche bestimmen 11.93 */

#ifndef PRECOMP
#include "main.h"
#endif


#include "crt.h" 
#include "move.h"
#include "eval.h"
#include "sboard.h"
#include "order.h"
#include "playgm.h"
#include "trans.h"
#include "goodies.h"


#define BEW_MITTE	BewA

#define MAXANZ		10000
#define RECHEN_TIEFE	6
#define VOR_TIEFE	4

#define MAXDELTA	0.2


/* global für _abort() */

bool f_n;


/* Dateinamen */
static char name_sfk[100], name_sfk1[100], name_osp[100], name_wkt[100];


void _abort(void) 
{
  exit(1);
}

void Check(COMZIO *pcio, ZUGIO *pzio, bool no_count) {}	/* Signal-Handler */



int  Anz[MAXANZ-1];
REAL Delta[MAXANZ-1];


int main(int argc, char **argv)
{
  int	    GesZug=0, GesBetr=0, Fehler=0, i, j, t, ZugAnz, FeldAnz, AnzB, AnzW, StartNr, EndNr, Partei, ParteiEnde, 
	    VarLen, Var[65];
  ZUGIO	    zioA, zioB;
  PARTEI    Beginner;		
  ZUGIO     zio;
  SPFELD    sf, sf0, Tab;
  FILE	    *fp, *fpBrett;
  ZUGDAT    ZugDat[65], ZugDat0[65];
  SFPOS	    Zuege[65];
  REAL	    MaxDelta=-1;
  REAL      sumE=0, sumEE=0, sumn=0;

  InitCrt();

  if (argc != 2) Error("Aufruf: osel sfk-Datei\n");

  InitZug(&zio, BEW_MITTE, Check, HASHBITS);


/* Spielfelder analysieren */


  fp = fopen(argv[1], "r");

  if (!fp) Error("sfk-Datei nicht da");

  FeldAnz = 0;


  FOREVER {

    if (FeldAnz >= MAXANZ) break;

    if (!fSfRead(fp, &sf, 1)) break;

/*fTabAus(stdout, &Tab);*/

    ZugAnz = SfMoeglZuege(&sf, BLACK, Zuege);

    if (ZugAnz >= 2) {


      SfBrett(&sf, &zio.Brett);

#ifdef KILLER
      KillerAdjust(&zioA.killer, &sf);
#endif

      ZugAnz = ZuegeSortieren(&zio, BLACK, RECHEN_TIEFE, ZugDat);

      FOR (i, ZugAnz) ZugDat0[i] = ZugDat[i];

      
      ZugAnz = ZuegeSortieren(&zio, BLACK, VOR_TIEFE, ZugDat);

      FOR (i, ZugAnz) { 
        ZugDat0[i].Wert = round(EXPWERT(WERT_TO_REAL(ZugDat0[i].Wert)) * 10000);
        ZugDat[i].Wert = round(EXPWERT(WERT_TO_REAL(ZugDat[i].Wert)) * 10000);
      }

#if 0
      FOR (i, ZugAnz) { KoorAus(ZugDat0[i].Zug); printf(":%d ", ZugDat0[i].Wert); }
      printf("\n");
  
      FOR (i, ZugAnz) { KoorAus(ZugDat[i].Zug); printf(":%d ", ZugDat[i].Wert); }
      printf("\n");
#endif


sumn += 1.0;
sumE += ZugDat0[0].Wert/10000.0 - ZugDat[0].Wert/10000.0;
sumEE += (ZugDat0[0].Wert/10000.0 - ZugDat[0].Wert/10000.0) * 
         (ZugDat0[0].Wert/10000.0 - ZugDat[0].Wert/10000.0);

printf("%.2f %.2f (%.2f %.2f) ", 
  ZugDat0[0].Wert/10000.0, ZugDat[0].Wert/10000.0, 
  sumE/sumn, sqrt(sumEE/sumn - (sumE/sumn)*(sumE/sumn)));

      FOR (i, ZugAnz) if (ZugDat[i].Zug == ZugDat0[i].Zug) break;

      Anz[FeldAnz]   = i+1;
      Delta[FeldAnz] = (ZugDat[0].Wert - ZugDat[i].Wert) / 10000.0; 

      if (Delta[FeldAnz] > MAXDELTA) Fehler++;

if (MaxDelta < Delta[FeldAnz]) MaxDelta = Delta[FeldAnz];

      GesZug += ZugAnz;

      FOR (i, ZugAnz) 
        if ((ZugDat[0].Wert - ZugDat[i].Wert) / 10000.0 <= MAXDELTA) GesBetr++;

printf("%5d: Anz=%d Delta=%.4f MaxDelta=%.4f (%.4f) EAnz=%.1f (%.1f) Fehler=%.3f\n", 
FeldAnz+1, Anz[FeldAnz], Delta[FeldAnz], MaxDelta, MAXDELTA,
(REAL) GesBetr / (FeldAnz+1),  (REAL) GesZug / (FeldAnz+1),
(REAL)Fehler/(FeldAnz+1));

      FeldAnz++;



    }
  }

  fclose(fp);
  return 0;
}

