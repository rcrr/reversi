// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/* compute board transformations */

#ifndef PRECOMP
#include "main.h"
#endif

#include "crt.h"
#include "sboard.h"
#include "trans.h"


/* Transformations-Tabellen erzeugen */

#if 0
{ int i, j;
FOR (i, 8) {

printf("int T%dTab[100] = {", i);

  FOR (j, 100) {

    if (j % 10 == 0) printf("\n  ");

    printf("%2d, ", Trans[i](j));

  }

printf("\n};\n\n");

}}


/* Koordinaten-Transformationen */

static int T0(int Pos)	/* id */
{
  return Pos;
}


static int T1(int Pos)	/* x */
{
  int x, y;

  x = Pos % 10; y = Pos / 10;
  x = 9-x;
  return x + y * 10;
}


static int T2(int Pos)	/* y */
{
  int x, y;

  x = Pos % 10; y = Pos / 10;
  y = 9-y;

  return x + y * 10;
}


static int T3(int Pos)	/* xy */
{
  int x, y;

  x = Pos % 10; y = Pos / 10;
  x = 9-x;
  y = 9-y;

  return x + y * 10;
}


static int T4(int Pos)	/* t */
{
  int x, y, t;

  x = Pos % 10; y = Pos / 10;
  t = x; x = y; y = t;

  return x + y * 10;
}


static int T5(int Pos)	/* tx */
{
  int x, y, t;

  x = Pos % 10; y = Pos / 10;
  t = x; x = y; y = t;
  x = 9-x;

  return x + y * 10;
}


static int T6(int Pos)	/* ty */
{
  int x, y, t;

  x = Pos % 10; y = Pos / 10;
  t = x; x = y; y = t;
  y = 9-y;

  return x + y * 10;
}


static int T7(int Pos)	/* txy */
{
  int x, y, t;

  x = Pos % 10; y = Pos / 10;
  t = x; x = y; y = t;
  x = 9-x;
  y = 9-y;

  return x + y * 10;
}
#endif


int Schichten[64] = {

33, 34, 35, 36, 
43,         46,
53,         56,
63, 64, 65, 66,

44, 45, 
54, 55,

22, 23, 24, 25, 26, 27,
32,                 37,
42,                 47,
52,                 57,
62,                 67,
72, 73, 74, 75, 76, 77,

11, 12, 13, 14, 15, 16, 17, 18,
21,                         28,
31,                         38,
41,                         48,
51,                         58,
61,                         68,
71,                         78,
81, 82, 83, 84, 85, 86, 87, 88

};



int T0Tab[100] = {
   0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 
  10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 
  20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 
  30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 
  40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 
  50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 
  60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 
  70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 
  80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 
  90, 91, 92, 93, 94, 95, 96, 97, 98, 99 
};

int T1Tab[100] = {
   9,  8,  7,  6,  5,  4,  3,  2,  1,  0, 
  19, 18, 17, 16, 15, 14, 13, 12, 11, 10, 
  29, 28, 27, 26, 25, 24, 23, 22, 21, 20, 
  39, 38, 37, 36, 35, 34, 33, 32, 31, 30, 
  49, 48, 47, 46, 45, 44, 43, 42, 41, 40, 
  59, 58, 57, 56, 55, 54, 53, 52, 51, 50, 
  69, 68, 67, 66, 65, 64, 63, 62, 61, 60, 
  79, 78, 77, 76, 75, 74, 73, 72, 71, 70, 
  89, 88, 87, 86, 85, 84, 83, 82, 81, 80, 
  99, 98, 97, 96, 95, 94, 93, 92, 91, 90 
};

int T2Tab[100] = {
  90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 
  80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 
  70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 
  60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 
  50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 
  40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 
  30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 
  20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 
  10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 
   0,  1,  2,  3,  4,  5,  6,  7,  8,  9
};

int T3Tab[100] = {
  99, 98, 97, 96, 95, 94, 93, 92, 91, 90, 
  89, 88, 87, 86, 85, 84, 83, 82, 81, 80, 
  79, 78, 77, 76, 75, 74, 73, 72, 71, 70, 
  69, 68, 67, 66, 65, 64, 63, 62, 61, 60, 
  59, 58, 57, 56, 55, 54, 53, 52, 51, 50, 
  49, 48, 47, 46, 45, 44, 43, 42, 41, 40, 
  39, 38, 37, 36, 35, 34, 33, 32, 31, 30, 
  29, 28, 27, 26, 25, 24, 23, 22, 21, 20, 
  19, 18, 17, 16, 15, 14, 13, 12, 11, 10, 
   9,  8,  7,  6,  5,  4,  3,  2,  1,  0 
};

int T4Tab[100] = {
   0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 
   1, 11, 21, 31, 41, 51, 61, 71, 81, 91, 
   2, 12, 22, 32, 42, 52, 62, 72, 82, 92, 
   3, 13, 23, 33, 43, 53, 63, 73, 83, 93, 
   4, 14, 24, 34, 44, 54, 64, 74, 84, 94, 
   5, 15, 25, 35, 45, 55, 65, 75, 85, 95, 
   6, 16, 26, 36, 46, 56, 66, 76, 86, 96, 
   7, 17, 27, 37, 47, 57, 67, 77, 87, 97, 
   8, 18, 28, 38, 48, 58, 68, 78, 88, 98, 
   9, 19, 29, 39, 49, 59, 69, 79, 89, 99 
};

int T5Tab[100] = {
   9, 19, 29, 39, 49, 59, 69, 79, 89, 99, 
   8, 18, 28, 38, 48, 58, 68, 78, 88, 98, 
   7, 17, 27, 37, 47, 57, 67, 77, 87, 97, 
   6, 16, 26, 36, 46, 56, 66, 76, 86, 96, 
   5, 15, 25, 35, 45, 55, 65, 75, 85, 95, 
   4, 14, 24, 34, 44, 54, 64, 74, 84, 94, 
   3, 13, 23, 33, 43, 53, 63, 73, 83, 93, 
   2, 12, 22, 32, 42, 52, 62, 72, 82, 92, 
   1, 11, 21, 31, 41, 51, 61, 71, 81, 91, 
   0, 10, 20, 30, 40, 50, 60, 70, 80, 90 
};

int T6Tab[100] = {
  90, 80, 70, 60, 50, 40, 30, 20, 10,  0, 
  91, 81, 71, 61, 51, 41, 31, 21, 11,  1, 
  92, 82, 72, 62, 52, 42, 32, 22, 12,  2, 
  93, 83, 73, 63, 53, 43, 33, 23, 13,  3, 
  94, 84, 74, 64, 54, 44, 34, 24, 14,  4, 
  95, 85, 75, 65, 55, 45, 35, 25, 15,  5, 
  96, 86, 76, 66, 56, 46, 36, 26, 16,  6, 
  97, 87, 77, 67, 57, 47, 37, 27, 17,  7, 
  98, 88, 78, 68, 58, 48, 38, 28, 18,  8, 
  99, 89, 79, 69, 59, 49, 39, 29, 19,  9 
};

int T7Tab[100] = {
  99, 89, 79, 69, 59, 49, 39, 29, 19,  9, 
  98, 88, 78, 68, 58, 48, 38, 28, 18,  8, 
  97, 87, 77, 67, 57, 47, 37, 27, 17,  7, 
  96, 86, 76, 66, 56, 46, 36, 26, 16,  6, 
  95, 85, 75, 65, 55, 45, 35, 25, 15,  5, 
  94, 84, 74, 64, 54, 44, 34, 24, 14,  4, 
  93, 83, 73, 63, 53, 43, 33, 23, 13,  3, 
  92, 82, 72, 62, 52, 42, 32, 22, 12,  2, 
  91, 81, 71, 61, 51, 41, 31, 21, 11,  1, 
  90, 80, 70, 60, 50, 40, 30, 20, 10,  0 
};


static int T0(int Pos) { return T0Tab[Pos]; }
static int T1(int Pos) { return T1Tab[Pos]; }
static int T2(int Pos) { return T2Tab[Pos]; }
static int T3(int Pos) { return T3Tab[Pos]; }
static int T4(int Pos) { return T4Tab[Pos]; }
static int T5(int Pos) { return T5Tab[Pos]; }
static int T6(int Pos) { return T6Tab[Pos]; }
static int T7(int Pos) { return T7Tab[Pos]; }


int *TransTab[8] = { 
  T0Tab, T1Tab, T2Tab, T3Tab, T4Tab, T5Tab, T6Tab, T7Tab
};


/* Koordinaten-Transformationen, Inverse und Nacheinanderanwendung */


TRANS Trans[8] = { T0, T1, T2, T3, T4, T5, T6, T7 };

int TransInv[8] = { 0, 1, 2, 3, 4, 6, 5, 7 };

int TransMat[8][8] = {		/* erst y, dann x ausführen */
  {0, 1, 2, 3, 4, 5, 6, 7},
  {1, 0, 3, 2, 6, 7, 4, 5},
  {2, 3, 0, 1, 5, 4, 7, 6},
  {3, 2, 1, 0, 7, 6, 5, 4},
  {4, 5, 6, 7, 0, 1, 2, 3},
  {5, 4, 7, 6, 2, 3, 0, 1},
  {6, 7, 4, 5, 1, 0, 3, 2},
  {7, 6, 5, 4, 3, 2, 1, 0}
};


/* Integritätstest für Transformationen */

void CheckTrans(void)
{
  int i, j, k;


  FOR (i, 8) 
    FOR (j, 100) 

      if (Trans[TransInv[i]](Trans[i](j)) != j) {
	printf("%d %d\n", i, j);
	Error("TransFehler");
      }

  FOR (i, 8) 
    FOR (j, 8)
      FOR (k, 100)

        if (Trans[j](Trans[i](k)) != Trans[TransMat[i][j]](k)) {

 	  printf("%d %d %d\n", i, j, k);
	  Error("TransMatFehler");
        }
}



/* 8 Transformationen eines Spielfeldes ermitteln */


void Transform(SPFELD *pin, SPFELD pout[8])
{
  int    i, j;
  SFPOS  *po, *pi;
  TRANS  tr;


  pi = pin->p;

  FOR (i, 8) {

    tr = Trans[i];
    po = pout[i].p;

    FOR (j, 100) po[tr(j)] = pi[j];
  }
}



void TransformN(SPFELD *pin, SPFELD *pout, int n)
{
  int    i;
  SFPOS  *pi=pin->p, *po=pout->p;
  int	 *tr;

  if (n < 0 || n >= 8) Error("TransformN");

  tr = TransTab[n];

  FOR (i, 100) po[*tr++] = *pi++;
}



#if 0


static KNOT **ppvorher = NULL;


void InitNewKNOT(KNOT **ppk)	/* Listenanfang */
{
  ppvorher  = ppk;
  *ppk      = NULL;
}



KNOT *NewKNOT(void)
{
  KNOT *p;


  if (!(p=(KNOT*) malloc(sizeof(KNOT)))) Error("Speicher");

  if (ppvorher == NULL) Error("InitNewKNOT?");

  *ppvorher = p;

  ppvorher  = &p->next;

  p->next  = NULL;

  return p;
}





/* für Quicksort: maximaler Wert. minimaler Zug */

int compZUG(const void *a, const void *b)
{
  int r;


  r = ((ZUGDAT*) b)->Wert - ((ZUGDAT*) a)->Wert;

  if (r == 0) {

    r  = ((ZUGDAT*) a)->Zug - ((ZUGDAT*) b)->Zug;

  }

  return r;
}




/* Nach Spielfeld samt Transformierten in einer Knotenliste suchen, 
 * gefundene Adresse und Transformation zurück
 */

KNOT *SchonDa(
  KNOT   *Liste,
  PARTEI Partei, 
  SPFELD *psf,
  int	 *ptrans
)
{
  int    i, j, SteinAnz;
  KNOT   *p;
  SPFELD sftrans[8];


  p = Liste;

  SteinAnz = SfAnz(psf);

  Transform(psf, sftrans);


  while (p) {

    if (p->SteinAnz == SteinAnz && p->Partei == Partei) {

      FOR (j, 8) {
        FOR (i, 100) if (p->sf.p[i] != sftrans[j].p[i]) break;
        if (i >= 100) break;
      }

      if (j < 8) { *ptrans = j; return p; }
    }

    p = p->next;
  }

  return NULL;
}



/* Bibliothek einlesen */

/* Format: [ $ [ Zug(Wert) ]+ ]+ */

BIBL *HoleBibl(char *name)
{
  FILE *fp;
  KNOT *p, *p1, *pGr;
  char s[VAR_MAXLEN+10], *t;	/* +x, sonst bei scan hinten raus */
  int  i, c1, c2, d1, d2, k1, Zug, Wert;
  bool ok;
  BIBL *pbibl;



  if (!(fp=fopen(name,"rb"))) return NULL;


  if (!(pbibl=(BIBL*) malloc(sizeof(BIBL)))) Error("Speicher");

  pbibl->MaxSteinAnz = 4;

  InitNewKNOT(&pbibl->Liste);			/* Knoten-Suchliste */

  pGr = NewKNOT();

  SfGrund(&pGr->sf);
  pGr->Partei = BLACK;
  pGr->SteinAnz = SfAnz(&pGr->sf);
  pGr->ZugAnz = 0;


  do {

    ok = fgets(s, VAR_MAXLEN, fp) != NULL;


    if (ok && s[0] == VARIANTEN_START) {

/* Variante einlesen */


      p = pGr;

      t = s+1;

      FOREVER {

/* Zugformat  %c%c(%d), wobei die Zahl höchstens zwei Ziffern hat */

        while (*t == ' ' || *t == '\t') t++;

	if (!*t || *t == '\n') break;


	c1 = *t++; c2 = *t++; k1 = *t++;

	if (c1 < 'a' || c1 > 'h' || c2 < '1' || c2 > '8' || k1 != '(') 
	  Error("Bibliothek korrupt");

	d1 = *t++; d2 = *t++;

	if (d1 < '0' || d1 > '9' || (d2 != ')' && (d2 < '0' || d2 > '9')))
	  Error("Bibl: Wert außerhalb");

	if (d2 != ')' && *t++ != ')') 
	  Error("Bibliothek korrupt");

	Zug  = c1 - 'a' + 1 + (c2 - '1' + 1) * 10;

	if (Zug != ZUG_PASSEN && !ZUG(Zug))
	  Error("Bibl: Zug?");


	Wert = d1 - '0';

	if (d2 != ')') Wert = 10 * Wert + d2 - '0';



/* Zug suchen */

	FOR (i, p->ZugAnz)
	  if (Zug == p->Zuege[i].Zug) break;

	if (i >= p->ZugAnz) {			/* Zug nicht da */


/* Zug und Wert eintragen */

	  if (p->ZugAnz >= ZUGANZ_MAX) Error("zu viele Züge in einer Stellung");

	  p->Zuege[p->ZugAnz].Zug  = Zug;  
	  p->Zuege[p->ZugAnz].Wert = Wert;
	  p->ZugAnz++;


/* neuen Knoten erzeugen */

	  p1 = NewKNOT();

	  p->Zuege[p->ZugAnz-1].next = p1;	/* *p1 Nachfolger von *p */

	  p1->sf = p->sf;

	  if (ZUG(Zug)) SfSetzen(&p1->sf, p->Partei, Zug);

	  p1->Partei   = GEGNER(p->Partei);
	  p1->SteinAnz = SfAnz(&p1->sf);

	  if (p1->SteinAnz > pbibl->MaxSteinAnz) 
	    pbibl->MaxSteinAnz = p1->SteinAnz;

	  p1->ZugAnz   = 0;

	  p = p1;


	} else 

	  p = p->Zuege[i].next;
	
      }
    }

  } while (ok);
    

  fclose(fp);

  return pbibl;
}


#endif

