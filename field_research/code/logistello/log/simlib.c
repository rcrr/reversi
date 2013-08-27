// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/* simple library, 10.93/4.94 */

#ifndef PRECOMP
#include "main.h"
#endif



#include "sboard.h"
#include "crt.h"
#include "move.h"
#include "eval.h"
#include "trans.h"
#include "simlib.h"


SIMLIB *InitSimLib(char *BiblName)
{
  int i, Anz, max;
  FILE *fp;
  SPFELD sf;
  SIMLIB *pbi;

printf("[ ");

  pbi = (SIMLIB*) malloc(sizeof(SIMLIB));
  if (!pbi) Error("mem");

  pbi->UseLib = false;
  pbi->Boards = NULL;
  pbi->BoardNum = 0;
  pbi->MaxDiscNum = 0;

  if (!BiblName || !BiblName[0]) { 
    printf("invalid filename ]\a\n"); 
    return NULL; 
  }

  strcpy(pbi->LibName, BiblName);

  pbi->UseLib = true;

printf("reading '%s': ", BiblName); fflush(stdout);

  fp = fopen(BiblName, "r");
  if (!fp) Error("lib not found!");

  for (i=0;; i++) if (!fSfRead(fp, &sf, 1)) break;

  fclose(fp);

  Anz = i;

if (!Anz) printf(" empty lib!\a ");
else      printf(" %d position(s) ", Anz);

  pbi->Boards = (SPFELD*) malloc (Anz*sizeof(SPFELD)); 

  if (!pbi->Boards) Error("InitSimLib: no mem");

  fp = fopen(BiblName, "r");
  if (!fp) Error("lib not found");

  if (fSfRead(fp, pbi->Boards, Anz) != Anz) Error("too few positions");

  fclose(fp);


  max = -1;

  FOR (i, Anz) {

    if (max < SfAnz(&pbi->Boards[i])) max = SfAnz(&pbi->Boards[i]);

    if (!ZUG(pbi->Boards[i].Marke - MA_ZUG)) Error("error in lib!");

  }


  pbi->MaxDiscNum = max;
  pbi->BoardNum   = Anz;


printf(" %d discs maximum ]\n", max);

  return pbi;
}




SFPOS SimLibMove(SIMLIB *pbi, SPFELD *psf, PARTEI Partei)
{
  int	  i, j, ZugAnz, Zug;
  SFPOS   Zuege[65];
  SPFELD  sf, sftrans[8];


  if (!pbi->UseLib || SfAnz(psf) > pbi->MaxDiscNum) return ZUG_UNBEKANNT;

  SfAus(psf, Partei, -1); printf("%d\n", Partei);

  ZugAnz = SfMoeglZuegeE(psf, Partei, Zuege);

printf("%d move(s)\n", ZugAnz);

  if (ZugAnz == 0) return ZUG_UNBEKANNT;

  if (ZugAnz == 1) {	/* Zufallsauswahl bei Symmetrie */

    ZugAnz = SfMoeglZuege(psf, Partei, Zuege);

    return Zuege[MyRand() % ZugAnz];

  }


  sf = *psf;
  if (Partei == WHITE) SfInvert(&sf);

  Transform(&sf, sftrans);

  ZugAnz = 0;

  FOR (i, pbi->BoardNum) {

/*if (SfAnz(&pbi->Boards[i]) == 6) SfAus(&pbi->Boards[i], 0, 0);*/

    FOR (j, 8) {

      if (SfGleich(&pbi->Boards[i], &sftrans[j])) break;

    }

    if (j < 8) {

      Zug = Trans[TransInv[j]](pbi->Boards[i].Marke - MA_ZUG);

KoorAus(Zug); printf(" found\n");
 
      FOR (j, ZugAnz) if (Zuege[j] == Zug) break;

      if (j >= ZugAnz) Zuege[ZugAnz++] = Zug;
    }
  }

  if (ZugAnz) return Zuege[MyRand() % ZugAnz];
  else        return ZUG_UNBEKANNT;
}


