// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/* Spiel-Unterprogramme, 4,7.93 */

#ifndef PRECOMP
#include "main.h"
#endif

#include "crt.h"
#include "move.h"
#include "game.h"

/* Codierung der Züge nach 1..60 */

int ZugCode[100] = {
  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  
  0,   1,  2,  3,  4,  5,  6,  7,  8, 0,  
  0,   9, 10, 11, 12, 13, 14, 15, 16, 0,  
  0,  17, 18, 19, 20, 21, 22, 23, 24, 0,  
  0,  25, 26, 27, 0,  0,  28, 29, 30, 0,  
  0,  31, 32, 33, 0,  0,  34, 35, 36, 0,  
  0,  37, 38, 39, 40, 41, 42, 43, 44, 0,  
  0,  45, 46, 47, 48, 49, 50, 51, 52, 0,  
  0,  53, 54, 55, 56, 57, 58, 59, 60, 0,  
  0,  0,  0,  0,  0,  0,  0,  0,  0,  0  
};


/* Decodierung */

int CodeZug[61] = {
  0,
  11, 12, 13, 14, 15, 16, 17, 18, 
  21, 22, 23, 24, 25, 26, 27, 28, 
  31, 32, 33, 34, 35, 36, 37, 38, 
  41, 42, 43,         46, 47, 48, 
  51, 52, 53,         56, 57, 58, 
  61, 62, 63, 64, 65, 66, 67, 68, 
  71, 72, 73, 74, 75, 76, 77, 78, 
  81, 82, 83, 84, 85, 86, 87, 88 
};


#if 0
/* Tabellen für SPIEL erzeugen */

int ZugCode[100], CodeZug[61];


main()
{
  int i, j, x, y;

  j = 1;

  for (i=0; i < 100; i++) {

    x = i % 10; y = i / 10;

    if (x == 0 || x == 9 || y == 0 || y == 9) { 

      printf("0,  "); ZugCode[i] = 0;

    } else {

      if (i == 44 || i == 45 || i == 54 || i == 55) {
        printf("0,  "); ZugCode[i] = 0; 
      } else { 
	printf("%2d, ", j); ZugCode[i] = j; CodeZug[j] = i; j++; }

    }

    if (x == 9) printf("\n");
  }


  for (i=1; i < 61; i++) { 
    printf("%2d, ", CodeZug[i]);
    if ((i & 7) == 7) printf("\n");
  }
}
#endif



SZUG SZUGgen(PARTEI Partei, SFPOS Zug, bool Marke)
{
  if ((Partei != BLACK && Partei != WHITE) || 
      (Marke  != false && Marke  != true)  || 
      !ZUG(Zug)) { printf("%d %d %d ", Partei, Zug, Marke); Error("SZUGgen"); }

/* KoorAus(Zug); printf("-> %d ", ZugCode[Zug]); */

  return (Partei == WHITE ? SZUG_BIT_PARTEI : 0) |
	 (Marke           ? SZUG_BIT_MARKE  : 0) | 
	 ZugCode[Zug];
}



/* für Quicksort: Zugfolge sortieren */

int compSPIEL(const void *a, const void *b)
{
  SZUG *pa=((SPIEL*)a)->Zuege, *pb=((SPIEL*)b)->Zuege;


  while (*pa && *pb && SZUG_ZUG(*pa) == SZUG_ZUG(*pb)) { pa++; pb++; }

  if (!*pa && !*pb) return 0;

  if (!*pa) return -1;
  if (!*pb) return  1;

  return SZUG_ZUG(*pa) - SZUG_ZUG(*pb);
}



void SpieleSortieren(SPIEL *Spiele, int SpieleAnz)
{
printf("%d Spiele sortieren ...", SpieleAnz); fflush(stdout);

  qsort(Spiele, SpieleAnz, sizeof(SPIEL), compSPIEL);

printf("OK\n"); 

}



int PraefixeVergl(SPIEL *Spiele, int SpieleAnz)
{
  int   i, j, ZugAnz, doppelt=0;

printf("Präfixe vergleichen ..."); fflush(stdout);

  Spiele[0].ungl = 0;


  FOR (i, SpieleAnz-1) {


/*fSpielKlarAus(stdout, &Spiele[i]);*/
 
Chk_Abort();

    ZugAnz = Spiele[i].ZugAnz;


    FOR (j, ZugAnz) if (Spiele[i].Zuege[j] != Spiele[i+1].Zuege[j]) break;

    if (j >= ZugAnz) { 

/*
fSpielKlarAus(stdout, &Spiele[i]);
fSpielKlarAus(stdout, &Spiele[i+1]);
*/

      Spiele[i+1].ungl = -1;

      doppelt++;

    } else { 

      Spiele[i+1].ungl = j; 
    }
  }

printf("OK\n");

  if (doppelt) {

    printf("(%d Spiel(e) doppelt)\n", doppelt); 

    j = 0;

    FOR (i, SpieleAnz) {

      if (Spiele[i].ungl >= 0) Spiele[j++] = Spiele[i];

    }

    if (j != SpieleAnz - doppelt) Error("Spielanzahl?");    

    SpieleAnz = j;
  }


  Spiele[SpieleAnz].ungl = 0;		/* Ende */

  return SpieleAnz;
}




/* 0, falls Fehler auftrat */

PARTEI NachSpielen(int ZugAnz, SPIEL *pSpiel, SPFELD *psf)
{
  int	 i, Zug, Marke;
  PARTEI Partei=BLACK;


  SfGrund(psf);

/*
fSpielKlarAus(stdout, pSpiel);
*/

  if (ZugAnz > pSpiel->ZugAnz) { printf(">>> ZugAnz zu groß"); return 0; }

  FOR (i, ZugAnz) {

    Zug = SZUG_ZUG(pSpiel->Zuege[i]);

/*
KoorAus(Zug);
*/

    if (!SfSetzen(psf, Partei, Zug)) { 

      Partei = GEGNER(Partei);

      if (!SfSetzen(psf, Partei, Zug)) {
	printf(">>> Fehler in Spiel");
	return 0;
      }
 
    }

    Marke = SZUG_MARKE(pSpiel->Zuege[i]);

    pSpiel->Zuege[i] = SZUGgen(Partei, Zug, Marke);

    Partei = GEGNER(Partei);
  }

  if (i == pSpiel->ZugAnz) return SZUG_PARTEI(0);	/* Spielende */

  return SZUG_PARTEI(pSpiel->Zuege[i]);
}



bool TabToSpiel(SPFELD *pTab, SPIEL *pSpiel)
{
  int i, SteinAnz;
  SPFELD sf;
  SFPOS	 *p;


  TabToSf(pTab, &sf);

  SteinAnz = SfAnz(&sf);

  if (pTab->Marke != BLACK || SteinAnz != 4 || 
      sf.p[E4] != BLACK || sf.p[D5] != BLACK ||
      sf.p[D4] != WHITE || sf.p[E5] != WHITE)

    return false;


  FOR (i, 60) pSpiel->Zuege[i] = 0;

  p = pTab->p;

  FOR (i, 100) 
    if (p[i] >= NUM_DISP) {
      pSpiel->Zuege[p[i] - NUM_DISP -1] = SZUGgen(BLACK, i, false);

/*
KoorAus(i);
printf(" %d\n", pSpiel->Zuege[p[i] - NUM_DISP -1]);
*/

    }

  FOR (i, 60) if (pSpiel->Zuege[i] == 0) break;

  pSpiel->Zuege[i] = 0;
  pSpiel->ZugAnz = i;

  if (!NachSpielen(i, pSpiel, &sf)) return false;	/* Farben bestimmen */

  pSpiel->StDiffBW = SfAnzBLACK(&sf) - SfAnzWHITE(&sf);

  return true;
}


void SpielToTab(SPIEL *pSpiel, SPFELD *pTab)
{
  int i, Zug;


  SfGrund(pTab);

  pTab->Marke = BLACK;

  FOR (i, pSpiel->ZugAnz) {

    Zug = SZUG_ZUG(pSpiel->Zuege[i]);

    pTab->p[Zug] = NUM_DISP + i + 1;
  }
}




#define SPIEL_LAENGE	500	 

bool sSpielKlarEin(char *s, SPIEL *pSpiel)
{
  char  *p=s;
  int   i, i1, i2, Partei, Marke;
  SFPOS Zug;


  FOR (i, 60) {

    if (p[0] == ':') break;

    if      (p[0] == '!') Marke = true;
    else if (p[0] == ' ') Marke = false;
    else { printf(">>> '%s': sSpielKlarEin: nicht '!' oder ' '\n", p); return false; }

    if      (p[1] == '+') Partei = BLACK;
    else if (p[1] == '-') Partei = WHITE;
    else { printf(">>> '%s': sSpielKlarEin: nicht +-\n", p); return false; }

    if (p[2] < 'a' || p[2] > 'h' || p[3] < '1' || p[3] > '8') {
      printf(">>> '%s': sSpielKlarEin: keine Zahl\n", p); return false; 
    }
 
    Zug = Tab8to10[p[2] - 'a' + (p[3] - '1') * 8];

    pSpiel->Zuege[i] = SZUGgen(Partei, Zug, Marke);

    p += 5;
  }


  pSpiel->ZugAnz = i;

  pSpiel->Zuege[i] = 0;

  if (sscanf(p, ": %d (%d %d)", &i, &i1, &i2) != 3) {
    printf(">>> '%s' sSpielKlarEin: Ergebnis?\n", p); return false;
  }  

  pSpiel->StDiffBW = i;
  pSpiel->ungl     = i1;
  pSpiel->max      = i2;
  
  if (abs(pSpiel->StDiffBW) > 64) {
    printf(">>> sSpielKlarEin: |Steindifferenz| > 64\n"); return false;
  }

  return true;
}



bool fSpielKlarEin(FILE *fp, SPIEL *pSpiel)
{
  char s[SPIEL_LAENGE];


  s[0] = 0;

  while (!feof(fp)) {

    if (!fgets(s, SPIEL_LAENGE, fp)) return false;

    if (s[0] == ' ' || s[0] == '!') break;

  }

  return sSpielKlarEin(s, pSpiel);
}





void sSpielKlarAus(char *s, SPIEL *pSpiel)
{
  char  s1[500], t[20];
  int   i, Partei;
  SFPOS Zug;
  bool  Marke;


  s[0] = 0;

  FOR (i, pSpiel->ZugAnz) {

    Zug    = SZUG_ZUG   (pSpiel->Zuege[i]);
    Partei = SZUG_PARTEI(pSpiel->Zuege[i]);
    Marke  = SZUG_MARKE (pSpiel->Zuege[i]);

    t[0] = Marke ? '!' : ' ';
    t[1] = Partei == BLACK ? '+' : '-';

    sKoorAus(&t[2], Zug);
    sprintf(s1, "%s%s ", s, t); strcpy(s, s1);
  }

  sprintf(s1, "%s: %d (%d %d)\n", s, pSpiel->StDiffBW, pSpiel->ungl, pSpiel->max);

  strcpy(s, s1);

}


void fSpielKlarAus(FILE *fp, SPIEL *pSpiel)
{
  char  s[500];

  sSpielKlarAus(s, pSpiel);
  fprintf(fp, s);
}





bool fSpielEin(FILE *fp, SPIEL *pSpiel)
{
  pSpiel->ZugAnz   = fgetc(fp);
  pSpiel->StDiffBW = fgetc(fp);
  pSpiel->ungl     = fgetc(fp);

  if (fread((char*)pSpiel->Zuege, sizeof(pSpiel->Zuege), 1, fp) != 1) 
    return false;
/*
printf("%d %d %d\n",pSpiel->ZugAnz,  pSpiel->StDiffBW , pSpiel->ungl);
{ int i;
  FOR (i, pSpiel->ZugAnz) printf("%d ", pSpiel->Zuege[i]);
 }
puts("");

fSpielKlarAus(stdout, pSpiel);
*/

  return !feof(fp);
}


bool fSpielAus(FILE *fp, SPIEL *pSpiel)
{
  fputc(pSpiel->ZugAnz,   fp);
  fputc(pSpiel->StDiffBW, fp);
  fputc(pSpiel->ungl,     fp);

  if (fwrite((char*)pSpiel->Zuege, sizeof(pSpiel->Zuege), 1, fp) != 1)
    return false;

  return !ferror(fp);
}





/* Anzahl der Spiele in einer Datei feststellen */

int SpieleAnzahl(char *name)
{
  int   SpieleAnz;
  FILE *fp;
  SPIEL Spiel;


  SpieleAnz = 0;

  fp = fopen(name, "r");

  if (!fp) return 0;

  FOREVER {
 
    if (!fSpielEin(fp, &Spiel)) break;

    SpieleAnz++;
  }

  fclose(fp);

  return SpieleAnz;
}




/* Spiele einlesen, false <=> Fehler */

int SpieleEinlesen(char *name, SPIEL *Spiele, int AnzMax)
{
  int   SpieleAnz;
  FILE  *fp;


  SpieleAnz = 0;

  fp = fopen(name, "r");

  if (!fp) return 0;

  FOR (SpieleAnz, AnzMax) {
  
    if (!fSpielEin(fp, &Spiele[SpieleAnz])) break;

  }

  fclose(fp);

/*  return SpieleSortieren(Spiele, SpieleAnz);*/

  return SpieleAnz;
}

 

/* Spiele schreiben, false <=> Fehler */

bool SpieleSchreiben(char *name, SPIEL *Spiele, int SpieleAnz)
{
  int   i;
  FILE  *fp;


  fp = fopen(name, "w");

  if (!fp) return false;

  FOR (i, SpieleAnz) {
    if (!fSpielAus(fp, &Spiele[i])) break;

  }

  fclose(fp);

  return true;
}


