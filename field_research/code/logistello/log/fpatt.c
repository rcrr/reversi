// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/* Patternfeatures 5-8.93, 2.94 */

#include "main.h"
#include "featurem.h"
#include "fpatt.h"
#include "patt.h"
#include "crt.h"
#include "eval.h"
#include "board.h"


bool f_pattnew  = true;
PATTVALS PattVals;

bool TablesRead=false;
String TableFile = "oplay.tab";


static TABLES TabsB, TabsW;


#define TEST	false

#define C81(x)		((x) < 0 || (x) >= 81)
#define C6561(x)	((x) < 0 || (x) >= 6561)


#if 0

int TestMob(BRETT *pbr, PARTEI Partei)
{
  int i, j, k, Mob;
  SPFELD sf;

  BrettSf(pbr, &sf);

  Mob = 0;

  FOR (i, 100) {

    if (ZUG(i) && i != A1 && i != A8 && i != H1 && i != H8 && sf.p[i] == LEER) {

      FOR (j, 4) {

	if (sf.p[k=i+ds[j]] == GEGNER(Partei)) {
             
	  while (sf.p[k] == GEGNER(Partei)) k += ds[j];
	  if (sf.p[k] == Partei) { Mob++; continue; }
	}

	if (sf.p[k=i-ds[j]] == GEGNER(Partei)) {
             
	  while (sf.p[k] == GEGNER(Partei)) k -= ds[j];
	  if (sf.p[k] == Partei) { Mob++; continue; }
	}
      }
    }
  }

  return Mob;
}


void StrahlTest(BRETT *pbr, PATTVALS *psa, PARTEI Partei)
{
  SPFELD sf0, sf;
  int    i, MobEB, MobEW, Ecken[4] = { A1, A8, H1, H8 };

  if (psa->StrMob != TestMob(pbr, Partei) - TestMob(pbr, GEGNER(Partei))) {

    printf("%d %d %d\n", 
	psa->StrMob, TestMob(pbr, Partei), TestMob(pbr, GEGNER(Partei)));
    Error("???");
  }

  BrettSf(pbr, &sf0);

  MobEB = MobEW = 0;

  FOR (i, 4) {

    sf = sf0;
    if (SfSetzen(&sf, BLACK, Ecken[i])) MobEB++;
    sf = sf0;
    if (SfSetzen(&sf, WHITE, Ecken[i])) MobEW++;
      
  }  

  if ((psa->StrMobEB != 0) != (MobEB != 0) || 
      (psa->StrMobEW != 0) != (MobEW != 0)) {

    SfAus(&sf0, BLACK, 0);
    printf("%d %d %d %d\n", psa->StrMobEB, MobEB, psa->StrMobEW,
       MobEW);
    Error("???1");
  }
}


#endif




static void StrahlAus(char *s, int n, int Anz, sint1 *Werte)
{
  int i, j;

  printf("%10s: (%4d) ", s, Werte[n]);

  FOR (i, Anz) {

    j = n % 3;
    n /= 3;

    switch (j-1) {

      case WHITE:	printf("O"); break;
      case LEER :	printf("-"); break;
      case BLACK:	printf("X"); break;
     
    }
  }
  printf("\n");
}



/* Tabellen für Strahlmobilität */


uint4 StrMobE8[NUM8] = {
  //#include "mobtabe8.inc"
  0
};

uint4 StrMob8[NUM8] = {
  //#include "mobtab8.inc"
  0
};

uint4 StrMob7[NUM7] = {
  //#include "mobtab7.inc"
  0
};

uint4 StrMob6[NUM6] = {
  //#include "mobtab6.inc"
  0
};

uint4 StrMob5[NUM5] = {
  //#include "mobtab5.inc"
  0
};

uint4 StrMob4[NUM4] = {
  //#include "mobtab4.inc"
  0
};

uint4 StrMob3[NUM3] = {
  //#include "mobtab3.inc"
  0
};




// large pattern / 9.96


// black to move, symmetrical


inline REAL LARGE(BRETT *pb)
{
  static bool loaded = false;
  static TABLE large_tab;
  int num = Pot3[14];

  if (!loaded) {
      
    loaded = true;

    large_tab = (TABLE) malloc(num);

    if (!large_tab) Error("LARGE: no mem");

    FILE *fp = fopen("la.tab", "r");

    if (!fp) Error("LARGE: la.tab not found");

    printf("loading la.tab ..."); fflush(stdout);

    fgetc(fp);

    if (fread((TABTYPE*)large_tab, num, 1, fp) != 1) 
      Error("LARGE: read error");

    printf("OK\n");

    large_tab += (num-1)/2;

    fclose(fp);

  }

  Square *p=pb->p;

  return large_tab[S14(A1,B1,C1,D1,E1,F1,G1,H1,B2,C2,D2,E2,F2,G2)] +
	 large_tab[S14(A8,B8,C8,D8,E8,F8,G8,H8,B7,C7,D7,E7,F7,G7)] +
	 large_tab[S14(A1,A2,A3,A4,A5,A6,A7,A8,B2,B3,B4,B5,B6,B7)] +
	 large_tab[S14(H1,H2,H3,H4,H5,H6,H7,H8,G2,G3,G4,G5,G6,G7)];
}

MERKMAL_B LARGE_B = { LARGE, "LARGE" };



// 4x3 pattern / 12.96
// black to move, symmetrical


inline REAL T4X3(BRETT *pb)
{
  static bool loaded = false;
  static TABLE t4x3_tab;

  if (!loaded) {
      
    int num = Pot3[12];

    loaded = true;

    t4x3_tab = (TABLE) malloc(num);

    if (!t4x3_tab) Error("T4X3: no mem");

    FILE *fp = fopen("4x3.tab", "r");

    if (!fp) Error("T4X3: tab not found");

    printf("loading 4x3.tab ..."); fflush(stdout);

    fgetc(fp);

    if (fread((TABTYPE*)t4x3_tab, num, 1, fp) != 1) 
      Error("T4X3: read error");

    printf("OK\n");

    t4x3_tab += (num-1)/2;

    fclose(fp);

  }

  Square *p=pb->p;

  return t4x3_tab[S12(A1,B1,C1,D1,A2,B2,C2,D2,A3,B3,C3,D3)] +
	 t4x3_tab[S12(H1,G1,F1,E1,H2,G2,F2,E2,H3,G3,F3,E3)] +
	 t4x3_tab[S12(A8,B8,C8,D8,A7,B7,C7,D7,A6,B6,C6,D6)] +
	 t4x3_tab[S12(H8,G8,F8,E8,H7,G7,F7,E7,H6,G6,F6,E6)] +
         t4x3_tab[S12(A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4)] +
	 t4x3_tab[S12(H1,H2,H3,H4,G1,G2,G3,G4,F1,F2,F3,F4)] +
	 t4x3_tab[S12(A8,A7,A6,A5,B8,B7,B6,B5,C8,C7,C6,C5)] +
	 t4x3_tab[S12(H8,H7,H6,H5,G8,G7,G6,G5,F8,F7,F6,F5)];
}

MERKMAL_B T4X3_B = { T4X3, "T4X3" };


// 3x3 pattern / 12.96
// black to move, symmetrical


inline REAL T3X3(BRETT *pb)
{
  static bool loaded = false;
  static TABLE t3x3_tab;

  if (!loaded) {
      
    loaded = true;
    int num = Pot3[9];

    t3x3_tab = (TABLE) malloc(num);

    if (!t3x3_tab) Error("T3X3: no mem");

    FILE *fp = fopen("3x3.tab", "r");

    if (!fp) Error("T3X3: tab not found");

    printf("loading 3x3.tab ..."); fflush(stdout);

    fgetc(fp);

    if (fread((TABTYPE*)t3x3_tab, num, 1, fp) != 1) 
      Error("T3X3: read error");

    printf("OK\n");

    t3x3_tab += (num-1)/2;

    fclose(fp);

  }

  Square *p=pb->p;

#if 0
BrettAus(pb);
printf("A1=%d H1=%d A8=%d H8=%d\n",  
         t3x3_tab[S9(A1,B1,C1,A2,B2,C2,A3,B3,C3)],
	 t3x3_tab[S9(H1,G1,F1,H2,G2,F2,H3,G3,F3)],
	 t3x3_tab[S9(A8,B8,C8,A7,B7,C7,A6,B6,C6)],
	 t3x3_tab[S9(H8,G8,F8,H7,G7,F7,H6,G6,F6)]);
#endif

  return t3x3_tab[S9(A1,B1,C1,A2,B2,C2,A3,B3,C3)] +
	 t3x3_tab[S9(H1,G1,F1,H2,G2,F2,H3,G3,F3)] +
	 t3x3_tab[S9(A8,B8,C8,A7,B7,C7,A6,B6,C6)] +
	 t3x3_tab[S9(H8,G8,F8,H7,G7,F7,H6,G6,F6)];
}

MERKMAL_B T3X3_B = { T3X3, "T3X3" };



void FastPatt(BRETT *pb, PATTVALS *pe, PARTEI Partei)
{
  int	   sind;
  uint4    w, w1, *pu;
  Square   *p=pb->p;
  TABLES   *ptabs;
  TABTYPE  *pt;
  STRAHLTYP *pat;

  if (!TablesRead) {

    ReadTables(TableFile, &TabsB, &TabsW);

    TablesRead = true;
  }

  pat = pb->NewPatt.st;


  sind = (pb->SteinAnz - I0 + IWIDTH/2) / IWIDTH;


  if      (sind < 0)    sind = 0;
  else if (sind > INUM) sind = INUM;


  if (Partei == BLACK) ptabs = &TabsB; else ptabs = &TabsW;

/*
OOOOO:::
OOOOO:::
::::::::
::::::::
::::::::
::::::::
::::::::
::::::::
*/

#if STRB1
  pt = &(ptabs->b1)[(NUM10-1)/2];

  pe->StrB1 = pt[S10(A1,B1,C1,D1,A2,B2,C2,D2,E1,E2)] +
	      pt[S10(H1,G1,F1,E1,H2,G2,F2,E2,D1,D2)] + 
	      pt[S10(A8,B8,C8,D8,A7,B7,C7,D7,E8,E7)] +
	      pt[S10(H8,G8,F8,E8,H7,G7,F7,E7,D8,D7)] + 
	      pt[S10(A1,A2,A3,A4,B1,B2,B3,B4,A5,B5)] +
	      pt[S10(H1,H2,H3,H4,G1,G2,G3,G4,H5,G5)] +
	      pt[S10(A8,A7,A6,A5,B8,B7,B6,B5,A4,B4)] +
	      pt[S10(H8,H7,H6,H5,G8,G7,G6,G5,H4,G4)];

/*
printf("%d %d %d %d %d %d %d %d\n",
              pt[S10(A1,B1,C1,D1,A2,B2,C2,D2,E1,E2)],
	      pt[S10(H1,G1,F1,E1,H2,G2,F2,E2,D1,D2)], 
	      pt[S10(A8,B8,C8,D8,A7,B7,C7,D7,E8,E7)],
	      pt[S10(H8,G8,F8,E8,H7,G7,F7,E7,D8,D7)], 
	      pt[S10(A1,A2,A3,A4,B1,B2,B3,B4,A5,B5)],
	      pt[S10(H1,H2,H3,H4,G1,G2,G3,G4,H5,G5)],
	      pt[S10(A8,A7,A6,A5,B8,B7,B6,B5,A4,B4)],
	      pt[S10(H8,H7,H6,H5,G8,G7,G6,G5,H4,G4)]);

printf("%d %d %d\n", pt[-29524], pt[+29524], pt[0]);
*/

#endif

/*
OOOOOOOO
:O::::O:
::::::::
::::::::
::::::::
::::::::
::::::::
::::::::
*/

#if STRB2
  pt = &(ptabs->b2)[(NUM10-1)/2];

  pe->StrB2 = pt[S10(A1,B1,C1,D1,E1,F1,G1,H1,B2,G2)] +
	      pt[S10(A8,B8,C8,D8,E8,F8,G8,H8,B7,G7)] +
	      pt[S10(A1,A2,A3,A4,A5,A6,A7,A8,B2,B7)] +
	      pt[S10(H1,H2,H3,H4,H5,H6,H7,H8,G2,G7)];

/*
printf("%d %d %d %d %d %d %d %d\n",
              pt[S10(A1,B1,C1,D1,A2,B2,C2,D2,E1,E2)],
	      pt[S10(H1,G1,F1,E1,H2,G2,F2,E2,D1,D2)], 
	      pt[S10(A8,B8,C8,D8,A7,B7,C7,D7,E8,E7)],
	      pt[S10(H8,G8,F8,E8,H7,G7,F7,E7,D8,D7)], 
	      pt[S10(A1,A2,A3,A4,B1,B2,B3,B4,A5,B5)],
	      pt[S10(H1,H2,H3,H4,G1,G2,G3,G4,H5,G5)],
	      pt[S10(A8,A7,A6,A5,B8,B7,B6,B5,A4,B4)],
	      pt[S10(H8,H7,H6,H5,G8,G7,G6,G5,H4,G4)]);

printf("%d %d %d\n", pt[-29524], pt[+29524], pt[0]);
*/

#endif


/*
OOOO::::
OOOO::::
::::::::
::::::::
::::::::
::::::::
::::::::
::::::::
*/

#if STRR1
  pt = &(ptabs->r1)[sind*NUM8 + (NUM8-1)/2];

  pe->StrR1 = pt[S8(A1,B1,C1,D1,A2,B2,C2,D2)] +
	      pt[S8(H1,G1,F1,E1,H2,G2,F2,E2)] + 
	      pt[S8(A8,B8,C8,D8,A7,B7,C7,D7)] +
	      pt[S8(H8,G8,F8,E8,H7,G7,F7,E7)] + 
	      pt[S8(A1,A2,A3,A4,B1,B2,B3,B4)] +
	      pt[S8(H1,H2,H3,H4,G1,G2,G3,G4)] +
	      pt[S8(A8,A7,A6,A5,B8,B7,B6,B5)] +
	      pt[S8(H8,H7,H6,H5,G8,G7,G6,G5)];


/*
printf("%d %d %d %d %d %d %d %d\n",
pt[S8(A1,B1,C1,D1,A2,B2,C2,D2)],
	      pt[S8(H1,G1,F1,E1,H2,G2,F2,E2)], 
	      pt[S8(A8,B8,C8,D8,A7,B7,C7,D7)],
	      pt[S8(H8,G8,F8,E8,H7,G7,F7,E7)], 
	      pt[S8(A1,A2,A3,A4,B1,B2,B3,B4)],
	      pt[S8(H1,H2,H3,H4,G1,G2,G3,G4)],
	      pt[S8(A8,A7,A6,A5,B8,B7,B6,B5)],
	      pt[S8(H8,H7,H6,H5,G8,G7,G6,G5)]);
*/
#endif

/*
::::::::
OOOO::::
OOOO::::
::::::::
::::::::
::::::::
::::::::
::::::::
*/
#if STRR2
  pt = &(ptabs->r2)[sind*NUM8 +(NUM8-1)/2];
  pe->StrR2 = pt[S8(A2,B2,C2,D2,A3,B3,C3,D3)] +
		pt[S8(H2,G2,F2,E2,H3,G3,F3,E3)] +
		pt[S8(A7,B7,C7,D7,A6,B6,C6,D6)] +
		pt[S8(H7,G7,F7,E7,H6,G6,F6,E6)] +
		pt[S8(B1,B2,B3,B4,C1,C2,C3,C4)] +
		pt[S8(G1,G2,G3,G4,F1,F2,F3,F4)] +
		pt[S8(B8,B7,B6,B5,C8,C7,C6,C5)] +
		pt[S8(G8,G7,G6,G5,F8,F7,F6,F5)];
#endif

/*
::::::::
::::::::
OOOO::::
OOOO::::
::::::::
::::::::
::::::::
::::::::
*/
#if STRR3
  pt = &(ptabs->r3)[sind*NUM8 +(NUM8-1)/2];
  pe->StrR3 = pt[S8(A3,B3,C3,D3,A4,B4,C4,D4)] +
		pt[S8(H3,G3,F3,E3,H4,G4,F4,E4)] +
		pt[S8(A6,B6,C6,D6,A5,B5,C5,D5)] +
		pt[S8(H6,G6,F6,E6,H5,G5,F5,E5)] +
		pt[S8(C1,C2,C3,C4,D1,D2,D3,D4)] +
		pt[S8(F1,F2,F3,F4,E1,E2,E3,E4)] +
		pt[S8(C8,C7,C6,C5,D8,D7,D6,D5)] +
		pt[S8(F8,F7,F6,F5,E8,E7,E6,E5)];
#endif


/*
::OOOO::
::OOOO::
::::::::
::::::::
::::::::
::::::::
::::::::
::::::::
*/
#if STRM1
  pt = &(ptabs->m1)[sind*NUM8 +(NUM8-1)/2];
  pe->StrM1 = pt[S8(C1,D1,E1,F1,C2,D2,E2,F2)] +
		pt[S8(C8,D8,E8,F8,C7,D7,E7,F7)] +
		pt[S8(A3,A4,A5,A6,B3,B4,B5,B6)] +
		pt[S8(H3,H4,H5,H6,G3,G4,G5,G6)];
#endif



/*
::::::::
::OOOO::
::OOOO::
::::::::
::::::::
::::::::
::::::::
::::::::
*/
#if STRM2
  pt = &(ptabs->m2)[sind*NUM8 + (NUM8-1)/2];
  pe->StrM2 = pt[S8(C2,D2,E2,F2,C3,D3,E3,F3)] +
	      pt[S8(C7,D7,E7,F7,C6,D6,E6,F6)] +
	      pt[S8(B3,B4,B5,B6,C3,C4,C5,C6)] +
	      pt[S8(G3,G4,G5,G6,F3,F4,F5,F6)];
#endif


#if DOUBLE_INDEX
#define PT(i) pt[(i)/2]   /* pt + ((2*index)/2)*4 */
#else
#define PT(i) pt[i]
#endif



#if STRHV1
  pt = &(ptabs->hv1)[sind*NUM8 + (NUM8-1)/2];
  pe->StrHV1 = PT(pat[PHV1A]) + PT(pat[PHV1B]) + 
               PT(pat[PHV1C]) + PT(pat[PHV1D]);
#endif

/*
printf("%d: %d %d %d %d\n", sind, PT(pat[BS1]) , PT(pat[BS8]) , PT(pat[BSA]) , PT(pat[BSH]));
*/

#if STRHV2
  pt = &(ptabs->hv2)[sind*NUM8 + (NUM8-1)/2];
  pe->StrHV2 = PT(pat[PHV2A]) + PT(pat[PHV2B]) + 
               PT(pat[PHV2C]) + PT(pat[PHV2D]);
#endif

#if STRHV3
  pt = &(ptabs->hv3)[sind*NUM8 + (NUM8-1)/2];
  pe->StrHV3 = PT(pat[PHV3A]) + PT(pat[PHV3B]) + 
               PT(pat[PHV3C]) + PT(pat[PHV3D]);
#endif

#if STRHV4
  pt = &(ptabs->hv4)[sind*NUM8 + (NUM8-1)/2];
  pe->StrHV4 = PT(pat[PHV4A]) + PT(pat[PHV4B]) + 
               PT(pat[PHV4C]) + PT(pat[PHV4D]);
#endif


#if STRD1
  pt = &(ptabs->d1)[sind*NUM8 + (NUM8-1)/2];
  pe->StrD1 = PT(pat[PD1A]) + PT(pat[PD1B]);
#endif

#if STRD2
  pt = &(ptabs->d2)[sind*NUM7 + (NUM7-1)/2];
  pe->StrD2 = PT(pat[PD2A]) + PT(pat[PD2B]) + PT(pat[PD2C]) + PT(pat[PD2D]);
#endif

#if STRD3
  pt = &(ptabs->d3)[sind*NUM6 + (NUM6-1)/2];
  pe->StrD3 = PT(pat[PD3A]) + PT(pat[PD3B]) + PT(pat[PD3C]) + PT(pat[PD3D]);
#endif

#if STRD4
  pt = &(ptabs->d4)[sind*NUM5 + (NUM5-1)/2];
  pe->StrD4 = PT(pat[PD4A]) + PT(pat[PD4B]) + PT(pat[PD4C]) + PT(pat[PD4D]);
#endif

#if STRD5
  pt = &(ptabs->d5)[sind*NUM4 + (NUM4-1)/2];
  pe->StrD5 = PT(pat[PD5A]) + PT(pat[PD5B]) + PT(pat[PD5C]) + PT(pat[PD5D]);
#endif

#if STRD6
  pt = &(ptabs->d6)[sind*NUM3 + (NUM3-1)/2];
  pe->StrD6 = PT(pat[PD6A]) + PT(pat[PD6B]) + PT(pat[PD6C]) + PT(pat[PD6D]);
#endif





/* Ray Mobility */

/* Bitpartitions of table entries:
 *
 *   (8) MOB(8) MOBEB(8) MOBEW(8)       (MOB excludes MOBEB and MOBEW)
 *
 *   resp.
 *
 *   (8) MOB(8) POTMOB(8) POTMOBE(8) 	(MOB includes MOBEB and MOBEW)
 */

#if DOUBLE_INDEX
#define PU(i) (*(uint4*)((short*) pu + (i)))  /* pu + (2*index)*2 */
#else
#define PU(i) pu[i]
#endif


/* Edges and main diagonals */


  pu = StrMobE8+(NUM8-1)/2;
  w  = PU(pat[PHV1A]) + PU(pat[PHV1B]) + PU(pat[PHV1C]) + PU(pat[PHV1D]) + 
       PU(pat[PD1A]) + PU(pat[PD1B]);

/* w = MOB, MOBEB, MOBEW of edges and diagonals */

  pe->StrMobEB = (w & MOBEB_MASK) >> 8;
  pe->StrMobEW = w & MOBEW_MASK;

  pu = StrMob8+(NUM8-1)/2;
  w1 = PU(pat[PD1A]) + PU(pat[PD1B]);

/* w1 = MOB, POTMOB, POTMOBE from diagonals */



  w = (w & ~(MOBEB_MASK | MOBEW_MASK)) | (w1 & ~(MOB_MASK | POTMOBE_MASK));

/*  w = MOB of edges and diagonals without corner moves, 
 *      POTMOB of diagonals
 */

/* remaining rays of length 8 */

  w += PU(pat[PHV2A]) + PU(pat[PHV2B]) + PU(pat[PHV2C]) + PU(pat[PHV2D]) +
       PU(pat[PHV3A]) + PU(pat[PHV3B]) + PU(pat[PHV3C]) + PU(pat[PHV3D]) +
       PU(pat[PHV4A]) + PU(pat[PHV4B]) + PU(pat[PHV4C]) + PU(pat[PHV4D]);

/* length 7 */

  pu = StrMob7+(NUM7-1)/2;
  w += PU(pat[PD2A]) + PU(pat[PD2B]) + PU(pat[PD2C]) + PU(pat[PD2D]);

/* length 6 */

  pu = StrMob6+(NUM6-1)/2;
  w += PU(pat[PD3A]) + PU(pat[PD3B]) + PU(pat[PD3C]) + PU(pat[PD3D]);

/* length 5 */

  pu = StrMob5+(NUM5-1)/2;
  w += PU(pat[PD4A]) + PU(pat[PD4B]) + PU(pat[PD4C]) + PU(pat[PD4D]);


/* length 4 */


#if NEW_INDICES
 
  Error("no 4-diag");

#else

  pu = StrMob4+(NUM4-1)/2;
  w += PU(pat[PD5A]) + PU(pat[PD5B]) + PU(pat[PD5C]) + PU(pat[PD5D]);

#endif

/* length 3: no POTMOB */

  pu = StrMob3+(NUM3-1)/2;


#if 0
  w += PU(pat[PD6A]) + PU(pat[PD6B]) + PU(pat[PD6C]) + PU(pat[PD6D]);
#else
/* no mob on diagonals of length 3 => better evaluation and faster */
  w += 4*(0x00030202);
#endif


  if (Partei == BLACK) {

    pe->StrMob    = (int)((w & MOB_MASK)   >> 16) - 114;

    pe->StrPotMob = (int)(((w & POTMOB_MASK) >> 8)  +  (w & POTMOBE_MASK) - 234);
    pe->StrPotMobE =  (int)(w1 & POTMOBE_MASK) - 10;

    pe->StrMobAll    = pe->StrMob + pe->StrMobEB - pe->StrMobEW;
    pe->StrPotMobAll = pe->StrPotMob + pe->StrPotMobE;

  } else {

    pe->StrMob    = -((int)((w & MOB_MASK)   >> 16) - 114);
    pe->StrPotMob = -((int)(((w & POTMOB_MASK) >> 8) + (w & POTMOBE_MASK))- 234);
    pe->StrPotMobE = -((int)(w1 & POTMOBE_MASK) - 10);

    pe->StrMobAll    = pe->StrMob + pe->StrMobEW - pe->StrMobEB;
    pe->StrPotMobAll = pe->StrPotMob + pe->StrPotMobE;

  }

#if 0

#define CMP(s) if (pe->s != pv.s) printf(#s" %d %d\n", pe->s, pv.s)

{ PATTVALS pv;
  BRETT board;
  SPFELD sf;

  BrettSf(pb, &sf);
  SfBrett(&sf, &board);

  OldFastPatt(&board, &pv, Partei);
  
  CMP(StrHV1);
  CMP(StrHV2);
  CMP(StrHV3);
  CMP(StrHV4);
  CMP(StrD1);
  CMP(StrD2);
  CMP(StrD3);
  CMP(StrD4);
  CMP(StrMobAll);
  CMP(StrPotMobAll);

} 


#endif

}


inline REAL strb1(BRETT *pb, PARTEI Partei)
{
#if !STRB1
Error("b1 not implemented");
#endif

  if (f_pattnew) FastPatt(pb, &PattVals, Partei);

  return PattVals.StrB1;
}

inline REAL strb2(BRETT *pb, PARTEI Partei)
{
#if !STRB2
Error("b2 not implemented");
#endif

  if (f_pattnew) FastPatt(pb, &PattVals, Partei);

  return PattVals.StrB2;
}


inline REAL strr1(BRETT *pb, PARTEI Partei)
{
#if !STRR1
Error("r1 not implemented");
#endif

  if (f_pattnew) FastPatt(pb, &PattVals, Partei);

  return PattVals.StrR1;
}


inline REAL strr2(BRETT *pb, PARTEI Partei)
{
#if !STRR2
Error("r2 not implemented");
#endif

  if (f_pattnew) FastPatt(pb, &PattVals, Partei);

  return PattVals.StrR2;
}


inline REAL strr3(BRETT *pb, PARTEI Partei)
{
#if !STRR3
Error("r3 not implemented");
#endif

  if (f_pattnew) FastPatt(pb, &PattVals, Partei);

  return PattVals.StrR3;
}





inline REAL strm1(BRETT *pb, PARTEI Partei)
{
#if !STRM1
Error("m1 not implemented");
#endif

  if (f_pattnew) FastPatt(pb, &PattVals, Partei);

  return PattVals.StrM1;
}


inline REAL strm2(BRETT *pb, PARTEI Partei)
{
#if !STRM2
Error("m2 not implemented");
#endif

  if (f_pattnew) FastPatt(pb, &PattVals, Partei);

  return PattVals.StrM2;
}




inline REAL strhv1(BRETT *pb, PARTEI Partei)
{
#if !STRHV1
Error("hv1 not implemented");
#endif

 if (f_pattnew) FastPatt(pb, &PattVals, Partei);

  return PattVals.StrHV1;
}


inline REAL strhv2(BRETT *pb, PARTEI Partei)
{
#if !STRHV2
Error("hv2 not implemented");
#endif

  if (f_pattnew) FastPatt(pb, &PattVals, Partei);

  return PattVals.StrHV2;
}


inline REAL strhv3(BRETT *pb, PARTEI Partei)
{
#if !STRHV3
Error("hv3 not implemented");
#endif

  if (f_pattnew) FastPatt(pb, &PattVals, Partei);

  return PattVals.StrHV3;
}


inline REAL strhv4(BRETT *pb, PARTEI Partei)
{
#if !STRHV4
Error("hv4 not implemented");
#endif

  if (f_pattnew) FastPatt(pb, &PattVals, Partei);

  return PattVals.StrHV4;
}




inline REAL strd1(BRETT *pb, PARTEI Partei)
{
#if !STRD1
Error("d1 not implemented");
#endif

  if (f_pattnew) FastPatt(pb, &PattVals, Partei);

  return PattVals.StrD1;
}


inline REAL strd2(BRETT *pb, PARTEI Partei)
{
#if !STRD2
Error("d2 not implemented");
#endif

  if (f_pattnew) FastPatt(pb, &PattVals, Partei);

  return PattVals.StrD2;
}


inline REAL strd3(BRETT *pb, PARTEI Partei)
{
#if !STRD3
Error("d3 not implemented");
#endif

  if (f_pattnew) FastPatt(pb, &PattVals, Partei);

  return PattVals.StrD3;
}


inline REAL strd4(BRETT *pb, PARTEI Partei)
{
#if !STRD4
Error("d4 not implemented");
#endif

  if (f_pattnew) FastPatt(pb, &PattVals, Partei);

  return PattVals.StrD4;
}


inline REAL strd5(BRETT *pb, PARTEI Partei)
{
#if !STRD5
Error("d5 not implemented");
#endif

  if (f_pattnew) FastPatt(pb, &PattVals, Partei);

  return PattVals.StrD5;
}


inline REAL strd6(BRETT *pb, PARTEI Partei)
{
#if !STRD6
Error("d6 not implemented");
#endif

  if (f_pattnew) FastPatt(pb, &PattVals, Partei);

  return PattVals.StrD6;
}

REAL STRB1B(BRETT *pb) { return strb1(pb, BLACK); }
REAL STRB1W(BRETT *pb) { return strb1(pb, WHITE); }

REAL STRB2B(BRETT *pb) { return strb2(pb, BLACK); }
REAL STRB2W(BRETT *pb) { return strb2(pb, WHITE); }

REAL STRR1B(BRETT *pb) { return strr1(pb, BLACK); }
REAL STRR1W(BRETT *pb) { return strr1(pb, WHITE); }

REAL STRR2B(BRETT *pb) { return strr2(pb, BLACK); }
REAL STRR2W(BRETT *pb) { return strr2(pb, WHITE); }

REAL STRR3B(BRETT *pb) { return strr3(pb, BLACK); }
REAL STRR3W(BRETT *pb) { return strr3(pb, WHITE); }

REAL STRM1B(BRETT *pb) { return strm1(pb, BLACK); }
REAL STRM1W(BRETT *pb) { return strm1(pb, WHITE); }

REAL STRM2B(BRETT *pb) { return strm2(pb, BLACK); }
REAL STRM2W(BRETT *pb) { return strm2(pb, WHITE); }

REAL STRHV1B(BRETT *pb) { return strhv1(pb, BLACK); }
REAL STRHV1W(BRETT *pb) { return strhv1(pb, WHITE); }

REAL STRHV2B(BRETT *pb) { return strhv2(pb, BLACK); }
REAL STRHV2W(BRETT *pb) { return strhv2(pb, WHITE); }

REAL STRHV3B(BRETT *pb) { return strhv3(pb, BLACK); }
REAL STRHV3W(BRETT *pb) { return strhv3(pb, WHITE); }

REAL STRHV4B(BRETT *pb) { return strhv4(pb, BLACK); }
REAL STRHV4W(BRETT *pb) { return strhv4(pb, WHITE); }

MERKMAL_B STRB1B_B = { STRB1B, "STRB1B" };
MERKMAL_B STRB2B_B = { STRB2B, "STRB2B" };

MERKMAL_B STRR1B_B = { STRR1B, "STRR1B" };
MERKMAL_B STRR2B_B = { STRR2B, "STRR2B" };
MERKMAL_B STRR3B_B = { STRR3B, "STRR3B" };

MERKMAL_B STRM1B_B = { STRM1B, "STRM1B" };
MERKMAL_B STRM2B_B = { STRM2B, "STRM2B" };

MERKMAL_B STRHV1B_B = { STRHV1B, "STRHV1B" };
MERKMAL_B STRHV2B_B = { STRHV2B, "STRHV2B" };
MERKMAL_B STRHV3B_B = { STRHV3B, "STRHV3B" };
MERKMAL_B STRHV4B_B = { STRHV4B, "STRHV4B" };




REAL STRD1B(BRETT *pb) { return strd1(pb, BLACK); }
REAL STRD1W(BRETT *pb) { return strd1(pb, WHITE); }

REAL STRD2B(BRETT *pb) { return strd2(pb, BLACK); }
REAL STRD2W(BRETT *pb) { return strd2(pb, WHITE); }

REAL STRD3B(BRETT *pb) { return strd3(pb, BLACK); }
REAL STRD3W(BRETT *pb) { return strd3(pb, WHITE); }

REAL STRD4B(BRETT *pb) { return strd4(pb, BLACK); }
REAL STRD4W(BRETT *pb) { return strd4(pb, WHITE); }

REAL STRD5B(BRETT *pb) { return strd5(pb, BLACK); }
REAL STRD5W(BRETT *pb) { return strd5(pb, WHITE); }

REAL STRD6B(BRETT *pb) { return strd6(pb, BLACK); }
REAL STRD6W(BRETT *pb) { return strd6(pb, WHITE); }


MERKMAL_B STRD1B_B = { STRD1B, "STRD1B" };
MERKMAL_B STRD2B_B = { STRD2B, "STRD2B" };
MERKMAL_B STRD3B_B = { STRD3B, "STRD3B" };
MERKMAL_B STRD4B_B = { STRD4B, "STRD4B" };
MERKMAL_B STRD5B_B = { STRD5B, "STRD5B" };
MERKMAL_B STRD6B_B = { STRD6B, "STRD6B" };




/*************** mobility approximation using rays  *******************/


inline REAL strmob(BRETT *pb, PARTEI Partei)
{
  if (f_pattnew) FastPatt(pb, &PattVals, Partei);

  return PattVals.StrMob;
}


REAL STRMOBB(BRETT *pb) { return strmob(pb, BLACK); }
REAL STRMOBW(BRETT *pb) { return strmob(pb, WHITE); }

MERKMAL_B STRMOBB_B = { STRMOBB, "STRMOBB" };
MERKMAL_B STRMOBW_B = { STRMOBW, "STRMOBW" };



inline REAL strmobeb(BRETT *pb, PARTEI Partei)
{
  if (f_pattnew) FastPatt(pb, &PattVals, Partei);

  return PattVals.StrMobEB;
}


REAL STRMOBEBB(BRETT *pb) { return strmobeb(pb, BLACK); }
REAL STRMOBEBW(BRETT *pb) { return strmobeb(pb, WHITE); }

MERKMAL_B STRMOBEBB_B = { STRMOBEBB, "STRMOBEBB" };
MERKMAL_B STRMOBEBW_B = { STRMOBEBW, "STRMOBEBW" };


inline REAL strmobew(BRETT *pb, PARTEI Partei)
{
  if (f_pattnew) FastPatt(pb, &PattVals, Partei);

  return PattVals.StrMobEW;
}


REAL STRMOBEWB(BRETT *pb) { return strmobew(pb, BLACK); }
REAL STRMOBEWW(BRETT *pb) { return strmobew(pb, WHITE); }

MERKMAL_B STRMOBEWB_B = { STRMOBEWB, "STRMOBEWB" };
MERKMAL_B STRMOBEWW_B = { STRMOBEWW, "STRMOBEWW" };


inline REAL strmoball(BRETT *pb, PARTEI Partei)
{
  if (f_pattnew) FastPatt(pb, &PattVals, Partei);

  return PattVals.StrMobAll;
}


REAL STRMOBALLB(BRETT *pb) { return strmoball(pb, BLACK); }
REAL STRMOBALLW(BRETT *pb) { return strmoball(pb, WHITE); }

MERKMAL_B STRMOBALLB_B = { STRMOBALLB, "STRMOBALLB" };
MERKMAL_B STRMOBALLW_B = { STRMOBALLW, "STRMOBALLW" };


 


/*************** PotMob approximation with rays  *******************/


inline REAL strpotmob(BRETT *pb, PARTEI Partei)
{
  if (f_pattnew) FastPatt(pb, &PattVals, Partei);

  return PattVals.StrPotMob;
}


REAL STRPOTMOBB(BRETT *pb) { return strpotmob(pb, BLACK); }
REAL STRPOTMOBW(BRETT *pb) { return strpotmob(pb, WHITE); }

MERKMAL_B STRPOTMOBB_B = { STRPOTMOBB, "STRPOTMOBB" };
MERKMAL_B STRPOTMOBW_B = { STRPOTMOBW, "STRPOTMOBW" };




inline REAL strpotmobe(BRETT *pb, PARTEI Partei)
{
  if (f_pattnew) FastPatt(pb, &PattVals, Partei);

  return PattVals.StrPotMobE;
}


REAL STRPOTMOBEB(BRETT *pb) { return strpotmobe(pb, BLACK); }
REAL STRPOTMOBEW(BRETT *pb) { return strpotmobe(pb, WHITE); }

MERKMAL_B STRPOTMOBEB_B = { STRPOTMOBEB, "STRPOTMOBEB" };
MERKMAL_B STRPOTMOBEW_B = { STRPOTMOBEW, "STRPOTMOBEW" };





inline REAL strpotmoball(BRETT *pb, PARTEI Partei)
{
  if (f_pattnew) FastPatt(pb, &PattVals, Partei);

  return PattVals.StrPotMobAll;
}


REAL STRPOTMOBALLB(BRETT *pb) { return strpotmoball(pb, BLACK); }
REAL STRPOTMOBALLW(BRETT *pb) { return strpotmoball(pb, WHITE); }

MERKMAL_B STRPOTMOBALLB_B = { STRPOTMOBALLB, "STRPOTMOBALLB" };
MERKMAL_B STRPOTMOBALLW_B = { STRPOTMOBALLW, "STRPOTMOBALLW" };






#if 0 

/*************************************************************/


/*

R1:	  Fehler= 29.80 % (0f= 21.40 %, 1f= 8.40 %)   Diskordanz= 12.03 %

TEST-128: Fehler= 29.74 % (0f= 20.90 %, 1f= 8.84 %)   Diskordanz= 11.84 %
TEST-64:  Fehler= 29.55 % (0f= 19.80 %, 1f= 9.75 %)   Diskordanz= 11.70 %
TEST-32:  Fehler= 29.33 % (0f= 18.24 %, 1f= 11.09 %)  Diskordanz= 11.51 %
TEST-24:  Fehler= 29.22 % (0f= 17.58 %, 1f= 11.64 %)  Diskordanz= 11.39 %
TEST-20:  Fehler= 29.10 % (0f= 17.41 %, 1f= 11.69 %)  Diskordanz= 11.32 %
TEST-18:  Fehler= 29.14 % (0f= 17.27 %, 1f= 11.87 %)  Diskordanz= 11.28 %
TEST-16:  Fehler= 29.12 % (0f= 17.21 %, 1f= 11.91 %)  Diskordanz= 11.26 %
TEST-12:  Fehler= 29.32 % (0f= 17.19 %, 1f= 12.13 %)  Diskordanz= 11.25 %
TEST-8:   Fehler= 29.50 % (0f= 17.30 %, 1f= 12.20 %)  Diskordanz= 11.41 %

*/


#define F(x)	(1.0/(1+exp(-(x)/20.0)) - 0.5)



inline REAL strtest(BRETT *pb, PARTEI Partei)
{
  int	   sind;
  sint1    *pt, *p=pb->p;
  STRAHLZEIGER sz;


  if (!TablesRead) {

    ReadTables(TableFile, &szB, &szW);

    TablesRead = true;
  }


  sind = (pb->SteinAnz - I0 + IBREITE/2) / IBREITE;

  if      (sind < 0)    sind = 0;
  else if (sind > INUM) sind = INUM;

  str = pb->Strahlen.st;


  if (Partei == BLACK) sz = szB; else sz = szW;

/*
OOOO::::
OOOO::::
::::::::
::::::::
::::::::
::::::::
::::::::
::::::::
*/

  pt = &(ptabs->r1)[sind][(NUM8-1)/2];

  return
    F(pt[S8(A1,B1,C1,D1,A2,B2,C2,D2)] + pt[S8(A1,A2,A3,A4,B1,B2,B3,B4)]) +
    F(pt[S8(H1,G1,F1,E1,H2,G2,F2,E2)] + pt[S8(H1,H2,H3,H4,G1,G2,G3,G4)]) +
    F(pt[S8(A8,B8,C8,D8,A7,B7,C7,D7)] + pt[S8(A8,A7,A6,A5,B8,B7,B6,B5)]) +
    F(pt[S8(H8,G8,F8,E8,H7,G7,F7,E7)] + pt[S8(H8,H7,H6,H5,G8,G7,G6,G5)]);
}


REAL STRTESTB(BRETT *pb) { return strtest(pb, BLACK); }
REAL STRTESTW(BRETT *pb) { return strtest(pb, WHITE); }

MERKMAL_B STRTESTB_B = { STRTESTB, "STRTESTB" };
MERKMAL_B STRTESTW_B = { STRTESTW, "STRTESTW" };


 
#endif



// just return 1 iff number of empty squares is odd

REAL PARITY(BRETT *pb) 
{
  return pb->SteinAnz & 1;
}


MERKMAL_B PARITY_B = { PARITY, "PARITY" }; 



// count small odd corner regions

static int marked;


#define SQ(i,j) (corner + i*deltax + j*deltay)

#define REC(n) \
  if (bad[sq+n] == 0 && p[sq+n] == LEER) RecMark(p, sq+n, bad, deltax, deltay, mark);


static void RecMark(Square *p, int sq, sint1 *bad, int deltax, int deltay, int mark)
{
  //KoorAus(sq); printf(" marked\n");

  bad[sq] = mark; marked++;
  
  REC(1);
  REC(-1);
  REC(10);
  REC(-10);
  REC(11);
  REC(-11);
  REC(9);
  REC(-9);
}



// analyse corner region

void AnalyseRegion(Square *p, int corner, RegInfo &regi)
{
  sint1 bad_empty[100];  // bad=access to open region
  int sq, i, j, k;
  int deltax, deltay;
  int empty=0;           // number of empty squares in 4x3 region

  if (p[corner+1]  == RAND) deltax = -1;  else deltax = 1;
  if (p[corner+10] == RAND) deltay = -10; else deltay = 10;

  // init. info structure

  regi.num = 0;

  FOR (j, 4) {
    regi.regs[j].size = 0;
    regi.regs[j].color = 0;
    regi.regs[j].both = 0;
  }    

  memset(bad_empty, 1, 100);
 
  FOR (i, 4)
    FOR (j, 4) {
      sq = SQ(i,j);
      if (p[sq] == LEER) empty++;
      bad_empty[sq] = 0;
    }

  // start recursive searches from all free border squares

  j = 3;

  FOR (i, 4) {
    sq = SQ(i,j);
    if (p[sq] == LEER) {

      // recursively find all adjacent interior empty squares
      // and mark them 'bad'

      marked = 0;  
      RecMark(p, sq, bad_empty, deltax, deltay, -1);

      //KoorAus(sq);
      //printf(" %d %d\n", marked, empty);

      if (marked == empty) break;

    }

    sq = SQ(j,i);
    if (p[sq] == LEER) {

      // recursively find all adjacent interior empty squares
      // and mark them 'bad'

      marked = 0;  
      RecMark(p, sq, bad_empty, deltax, deltay, -1);
      //KoorAus(sq);
      //printf("%d %d\n", marked, empty);
      if (marked == empty) break;
    }
  }


  if (i >= 4) {   // no shortcut

    // surrounded empty regions are marked (bad[] == 0)
    // find connected regions

    FOR (i, 3)
      FOR (j, 3) {
        sq = SQ(i,j);
	if (p[sq] == LEER && bad_empty[sq] == 0) {

//KoorAus(sq); printf(" ");

	  regi.num++;
	  marked = 0;
	  RecMark(p, sq, bad_empty, deltax, deltay, regi.num);
	  regi.regs[regi.num-1].size = marked;
	}
      }
    }


    // determine sole access for each region

    FOR (i, 3)
      FOR (j, 3) {
        sq = SQ(i,j);
	if (bad_empty[sq] > 0) {

	  k = bad_empty[sq]-1;

// find colors that have possible access to square sq in direction -n

#define COLTEST(n) 						\
  if (!regi.regs[k].both && p[sq+n] != RAND && p[sq+n] != LEER && p[sq+2*n] != RAND) {\
    if (!(Ecke[sq+2*n] && p[sq+n] == p[sq+2*n])) { /* OO. in corner doesn't count for O */\
\
      if (p[sq+n] == BLACK) {					\
        if (regi.regs[k].color == WHITE) regi.regs[k].both = true;\
        regi.regs[k].color = BLACK;				\
      } else if (p[sq+n] == WHITE) {				\
        if (regi.regs[k].color == BLACK) regi.regs[k].both = true;\
        regi.regs[k].color = WHITE;				\
      }\
    }                   					\
  }

	  COLTEST(1);
	  COLTEST(-1);
	  COLTEST(10);
	  COLTEST(-10);
	  COLTEST(11);
	  COLTEST(-11);
	  COLTEST(9);
	  COLTEST(-9);
	}
      }  


#if 0
    if (regi.num > 2) {



      int x, y;

      FOR (y, 8) {

	printf("%1d ", y+1);    

	FOR (x, 8) {

	  int Pos = (y << 3) + x;

	  printf("|");

	  if	(p[Tab8to10[Pos]] == WHITE) printf("()");
	  else if	(p[Tab8to10[Pos]] == BLACK) printf("##");
	  else if   (p[Tab8to10[Pos]] == LEER) printf("  ");
	  else printf("??");
	  
	}
	
	printf("| %1d\n", y+1);
      }

      puts("->");

      FOR (y, 8) {

	printf("%1d ", y+1);    

	FOR (x, 8) {

	  int Pos = (y << 3) + x;

	  printf("|%2d", bad_empty[Tab8to10[Pos]]);

	}
	
	printf("| %1d\n", y+1);
      }
      puts("");
      puts("");

    }
#endif


//printf("both=%d col=%d\n", both, col);

}


uint1 *partab=0;

void ReadParTab(char *file)
{
  FILE *fp;
  int len;

  printf("[ loading '%s' ...", file); fflush(stdout);

  fp = fopen(file, "r");

  if (!fp) Error("can't open file");

  if (fscanf(fp, "%d", &len) != 1 || len != PARTAB_LEN) Error("length?");

  partab = (uint1*) calloc((PARTAB_LEN/4)+1,1);
  if (!partab) Error("ReadParTab: no mem!");


  int bit_num = 0, byte_num = 0, byte = 0;
  int i, val, count;

  FOREVER {

    if (fscanf(fp, "%d:%d", &val, &count) != 2) break;

    if (val < -1 || val > 1) Error("val out of range");
    if (count <= 0) Error("count <= 0");\

    FOR (i, count) {

      byte |= (val+1) << bit_num;
      bit_num += 2;
      if (bit_num >= 8) {
	bit_num = 0;
	partab[byte_num++] = byte;
	byte = 0;
      }
    }
  }

  if (bit_num != 0) partab[byte_num] = byte;

  if (!feof(fp)) Error("file corrupt");
  if (byte_num*8+bit_num != 2*PARTAB_LEN) Error("file length incorrect");

  fclose(fp);

  printf("OK ]\n");
}



// n in [ -(PARTAB_LEN-1) ... 0 ....... PARTAB_LEN-1 ]
// ->     0               PARTAB_LEN-1        0
//                              [ val is negated here ]

int GetParTabVal(int n)
{
  if (!partab) ReadParTab("partab");

  if (n >= PARTAB_LEN || n <= -PARTAB_LEN) Error("GetParTabVal: n out of range");

  if (n <= 0) {

    n += PARTAB_LEN-1;
    int val = partab[n >> 2];
    val >>= 2* (n & 3);
    val &= 3;
    return val-1;

  } else {

    n = PARTAB_LEN-1-n;
    int val = partab[n >> 2];
    val >>= 2* (n & 3);
    val &= 3;
    return -(val-1);

  }
}
 


REAL OR_BOTH(BRETT *pb)
{

#if 0
  int corners[4] = { A1, A8, H1, H8 };
  int i, num=0;
  RegInfo regi;

  FOR (i, 4) {

    AnalyseRegion(pb->p, corners[i], regi);

    if ((regi.num & 1) && regi.both) num++;

  }

  return num & 1;
#else
  Error("OR_BOTH not implemented");
  return 0;
#endif
}

MERKMAL_B OR_BOTH_B = { OR_BOTH, "OR_BOTH" }; 


#define PAT16(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,p16) \
  (3*(3*(3*(3*(3*(3*(3*(3*(3*(3*(3*(3*(3*(3*(3*\
  p[p1]+p[p2])+p[p3])+p[p4])+p[p5])+p[p6])+p[p7])+\
  p[p8])+p[p9])+p[p10])+p[p11])+p[p12])+p[p13])+p[p14])+p[p15])+p[p16])


REAL OR_ONE(BRETT *pb)
{
  int corners[4] = { A1, A8, H1, H8 };
  int i, j;
  RegInfo regi;
  int num_res=0;

#if 0

  sint1 *p = pb->p;

  int val = GetParTabVal(PAT16(A1,B1,C1,D1,A2,B2,C2,D2,A3,B3,C3,D3,A4,B4,C4,D4)) +
            GetParTabVal(PAT16(A8,B8,C8,D8,A7,B7,C7,D7,A6,B6,C6,D6,A5,B5,C5,D5)) +
            GetParTabVal(PAT16(H1,G1,F1,E1,H2,G2,F2,E2,H3,G3,F3,E3,H4,G4,F4,E4)) +
            GetParTabVal(PAT16(H8,G8,F8,E8,H7,G7,F7,E7,H6,G6,F6,E6,H5,G5,F5,E5));

  return -val;

#endif






  FOR (i, 4) {

    AnalyseRegion(pb->p, corners[i], regi);

    int num = 0;

    FOR (j, regi.num) {

      if ((regi.regs[j].size & 1) && !regi.regs[j].both) {

	if (regi.regs[j].color == BLACK) num++; else num--;
      }
    }

#if TEST
printf("i=%d %d\n", i, num);
#endif
    
    if (num > 0) num_res++;
    if (num < 0) num_res--;

  }

#if TEST

  sint1 *p = pb->p;

  int val = GetParTabVal(PAT16(A1,B1,C1,D1,A2,B2,C2,D2,A3,B3,C3,D3,A4,B4,C4,D4)) +
            GetParTabVal(PAT16(A8,B8,C8,D8,A7,B7,C7,D7,A6,B6,C6,D6,A5,B5,C5,D5)) +
            GetParTabVal(PAT16(H1,G1,F1,E1,H2,G2,F2,E2,H3,G3,F3,E3,H4,G4,F4,E4)) +
            GetParTabVal(PAT16(H8,G8,F8,E8,H7,G7,F7,E7,H6,G6,F6,E6,H5,G5,F5,E5));

#if 0
printf("1- %d %d\n", GetParTabVal(PAT16(A1,B1,C1,D1,A2,B2,C2,D2,A3,B3,C3,D3,A4,B4,C4,D4)),
       PAT16(A1,B1,C1,D1,A2,B2,C2,D2,A3,B3,C3,D3,A4,B4,C4,D4));
printf("2- %d %d\n" ,GetParTabVal(PAT16(A8,B8,C8,D8,A7,B7,C7,D7,A6,B6,C6,D6,A5,B5,C5,D5)),
       PAT16(A8,B8,C8,D8,A7,B7,C7,D7,A6,B6,C6,D6,A5,B5,C5,D5));
printf("3- %d %d\n", GetParTabVal(PAT16(H1,G1,F1,E1,H2,G2,F2,E2,H3,G3,F3,E3,H4,G4,F4,E4)),
       PAT16(H1,G1,F1,E1,H2,G2,F2,E2,H3,G3,F3,E3,H4,G4,F4,E4));
printf("4- %d %d\n", GetParTabVal(PAT16(H8,G8,F8,E8,H7,G7,F7,E7,H6,G6,F6,E6,H5,G5,F5,E5)),
       PAT16(H8,G8,F8,E8,H7,G7,F7,E7,H6,G6,F6,E6,H5,G5,F5,E5));
#endif

  val = -val;

  if (val != -num_res) {

    printf("different!\n");
    SPFELD sf;
    BrettSf(pb, &sf);
    SfAus(&sf, 0, 0);
  }
  
#endif


  return -num_res;
}


MERKMAL_B OR_ONE_B = { OR_ONE, "OR_ONE" }; 


REAL ER_ONE(BRETT *pb)
{
  int corners[4] = { A1, A8, H1, H8 };
  int i, j;
  RegInfo regi;
  int numb=0, numw=0;

  FOR (i, 4) {

    AnalyseRegion(pb->p, corners[i], regi);

    FOR (j, regi.num) {

      if (!(regi.regs[j].size & 1) && !regi.regs[j].both) {

	if (regi.regs[j].color == BLACK) numb++; else numw++;
      }
    }
  }

  return -(numb - numw);
}

MERKMAL_B ER_ONE_B = { ER_ONE, "ER_ONE" }; 




REAL DISC_DIFF(BRETT *pb)
{
  return pb->StDiffBW;
}

MERKMAL_B DISC_DIFF_B = { DISC_DIFF, "DISC_DIFF" }; 






REAL PARITY2(BRETT *pb) 
{
  int s=0;

#if 0
  BrettAus(pb);

  s = r = Region(pb, A1);
  printf("A1:%d\n", r);

  s += (r = Region(pb, A8));
  printf("A8:%d\n", r);

  s += (r = Region(pb, H1));
  printf("H1:%d\n", r);

  s += (r = Region(pb, H8));
  printf("H8:%d\n", r);
#endif

  return s;
}


MERKMAL_B PARITY2_B = { PARITY2, "PARITY2" }; 




int parity1(BRETT *pb) 
{
#if !LAZY_UPDATE

  int i, j, pos, disc, col, d[8]={-10,10,1,-1,9,-9,11,-11}, numb=0, numw=0;
  Square *pumgl=pb->umgl;
  Square *p=pb->p;


  for (i=pb->LastIndex; i >= 0; i--) {

    col = 0;
    pos = pumgl[i];

    { int x=pos % 10, y = pos/10;

      if (!((x==1||x==8)&&(y==1||y==8))) continue;
    }

    FOR (j, 8) {

      disc = p[pos+d[j]];

      if (disc == RAND) continue;
      if (disc == LEER) break;

      if (col == LEER) col = disc;
      else if (disc != col) break;

    }

    if (j >= 8) 
      if (col == BLACK) numb++; else numw++;

  }

  return numw-numb;

#else

 pb=0;
Error("no lists");
  return 0;

#endif

}



REAL PARITY1B(BRETT *pb) 
{
  int r=parity1(pb);

  if (pb->SteinAnz & 1) if (r >= 0) return +1; else return -1;
  else                  if (r <= 0) return -1; else return +1;
}


MERKMAL_B PARITY1B_B = { PARITY1B, "PARITY1B" }; 




REAL CORNERS(BRETT *pb)
{
  return pb->p[A1]+pb->p[H1]+pb->p[H8]+pb->p[A8];
}


MERKMAL_B CORNERS_B = { CORNERS, "CORNERS" }; 




REAL CENTER(BRETT *pb)
{
  int a, i, x, y, nb=0, nw=0;
  float db=0, dw=0;

  FOR (i, 100) {

    a = pb->p[i];
 
    if (a == BLACK || a == WHITE) {

      x = i % 10 - 1; y = i / 10 - 1;

      if (pb->p[i] == BLACK) { nb++; db += sqrt((x-3.5)*(x-3.5)+(y-3.5)*(y-3.5)); }
      else                   { nw++; dw += sqrt((x-3.5)*(x-3.5)+(y-3.5)*(y-3.5)); }
    }
  }

  if (nb && nw) return dw - db; else return 0;
}

MERKMAL_B CENTER_B = { CENTER, "CENTER" }; 


REAL MOBNORM(BRETT *pb)
{
  int numb, numw, movesb, movesw;
  SFPOS moves[65];
  SPFELD sf;

  numb = (pb->StDiffBW + pb->SteinAnz)/2;
  numw = pb->SteinAnz - numb;

  return STRMOBALLB(pb) * exp(((REAL)(numw - numb))/(numb+numw));



  BrettSf(pb, &sf);

  numb = (pb->StDiffBW + pb->SteinAnz)/2;
  numw = pb->SteinAnz - numb;

  movesb = SfMoeglZuege(&sf, BLACK, moves);
  movesw = SfMoeglZuege(&sf, WHITE, moves);
  
/*  if (numb && numw) return ((REAL)movesb)/numb - ((REAL)movesw)/numw;
*/
  if (movesb && movesw) 
    return ((REAL)numw)*movesb - ((REAL)numb)*movesw;
  else return 0;
}

MERKMAL_B MOBNORM_B = { MOBNORM, "MOBNORM" }; 








sint1 stab8e[NUM8] = {
  //#include "stab8e.inc"
  0
};

sint1 stab8[NUM8] = {
  //#include "stab8.inc"
  0
};

sint1 stab7[NUM7] = {
  //#include "stab7.inc"
  0
};

sint1 stab6[NUM6] = {
  //#include "stab6.inc"
  0
};

sint1 stab5[NUM5] = {
  //#include "stab5.inc"
  0
};

sint1 stab4[NUM4] = {
  //#include "stab4.inc"
  0
};

sint1 stab3[NUM3] = {
  //#include "stab3.inc"
  0
};




REAL STABILITY(BRETT *pb)
{
  sint1 *pt;
  STRAHLTYP *pat=pb->Patt.st;
  int s;



/* edges */

  pt = &(stab8e)[(NUM8-1)/2];
  s = pt[pat[BS1]] + pt[pat[BS8]] + pt[pat[BSA]] + pt[pat[BSH]];


/* other (8) */

  pt = &(stab8)[(NUM8-1)/2];
  s += pt[pat[BS2]] + pt[pat[BS7]] + pt[pat[BSB]] + pt[pat[BSG]];
  s += pt[pat[BS3]] + pt[pat[BS6]] + pt[pat[BSC]] + pt[pat[BSF]];
  s += pt[pat[BS4]] + pt[pat[BS5]] + pt[pat[BSD]] + pt[pat[BSE]];
  s += pt[pat[BDM8]] + pt[pat[BDP8]];


/* other (7..3) */

  pt = &(stab7)[(NUM7-1)/2];
  s += pt[pat[BDM7]] + pt[pat[BDM9]] + pt[pat[BDP7]] + pt[pat[BDP9]];

  pt = &(stab6)[(NUM6-1)/2];
  s += pt[pat[BDM6]] + pt[pat[BDM10]] + pt[pat[BDP6]] + pt[pat[BDP10]];

  pt = &(stab5)[(NUM5-1)/2];
  s += pt[pat[BDM5]] + pt[pat[BDM11]] + pt[pat[BDP5]] + pt[pat[BDP11]];

  pt = &(stab4)[(NUM4-1)/2];
  s += pt[pat[BDM4]] + pt[pat[BDM12]] + pt[pat[BDP4]] + pt[pat[BDP12]];

  pt = &(stab3)[(NUM3-1)/2];
  s += pt[pat[BDM3]] + pt[pat[BDM13]] + pt[pat[BDP3]] + pt[pat[BDP13]];

  return -s;
}


MERKMAL_B STABILITY_B = { STABILITY, "STABILITY" }; 




TABTYPE corner[NUM8+1] = {
/*#include "corner.inc"*/
0
};

TABTYPE edge[NUM8+1] = {
/*#include "edge.inc"*/
0
};




REAL NEWSTAB(BRETT *pb)
{
  TABTYPE *pt;
  Square   *p=pb->p;
  STRAHLTYP *pat=pb->Patt.st;
  int s;



/* edges */

  pt = &(edge)[(NUM8-1)/2];
  s = pt[pat[BS1]] + pt[pat[BS8]] + pt[pat[BSA]] + pt[pat[BSH]];


#if 1 
/* 2x4 corner */

  pt = &(corner)[(NUM8-1)/2];
  s +=        pt[S8(A1,B1,C1,D1,A2,B2,C2,D2)] +
	      pt[S8(H1,G1,F1,E1,H2,G2,F2,E2)] + 
	      pt[S8(A8,B8,C8,D8,A7,B7,C7,D7)] +
	      pt[S8(H8,G8,F8,E8,H7,G7,F7,E7)] + 
	      pt[S8(A1,A2,A3,A4,B1,B2,B3,B4)] +
	      pt[S8(H1,H2,H3,H4,G1,G2,G3,G4)] +
	      pt[S8(A8,A7,A6,A5,B8,B7,B6,B5)] +
	      pt[S8(H8,H7,H6,H5,G8,G7,G6,G5)];
#endif
  return s;
}


MERKMAL_B NEWSTAB_B = { NEWSTAB, "NEWSTAB" }; 


#define MTEST 0

/* mob approx test */


static int Check3[100] = {

  0,0,0,0,0,0,0,0,0,0,
  0,0,0,B2,0,0,G2,0,0,0,
  0,0,0,0,0,0,0,0,0,0,
  0,B2,0,0,0,0,0,0,G2,0,
  0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,
  0,B7,0,0,0,0,0,0,G7,0,
  0,0,0,0,0,0,0,0,0,0,
  0,0,0,B7,0,0,G7,0,0,0,
  0,0,0,0,0,0,0,0,0,0
};

static bool x_square[100] = {

  0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,
  0,0,1,0,0,0,0,1,0,0,
  0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,
  0,0,1,0,0,0,0,1,0,0,
  0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0
};


int mobx(BRETT *pb, int snum, int *squares)
{
  int i, j, pos, posi, col, mdiff=0;

#if MTEST
  SPFELD board;

BrettSf(pb, &board);
SfAus(&board, 0, 0);
#endif

  FOR (i, snum) {

    pos = squares[i];

    if (pb->p[pos] != BLACK && pb->p[pos] != WHITE) {

#if MTEST
KoorAus(pos); 
#endif

      for (col=WHITE; col <= BLACK; col += 2) {

        FOR (j, 4) {

          if (pb->p[pos + ds[j]] == -col && !(Check3[pos] && x_square[pos+ds[j]])) {

            posi = pos + 2*ds[j];

            while (pb->p[posi] == -col) posi += ds[j];

            if (pb->p[posi] == col) { 
#if MTEST
	      printf("(%d,%d)", col, ds[j]); 
#endif

	      mdiff += col; continue;
	    }
          }

          if (pb->p[pos - ds[j]] == -col && !(Check3[pos] && x_square[pos-ds[j]])) {

            posi = pos - 2*ds[j];

            while (pb->p[posi] == -col) posi -= ds[j];

            if (pb->p[posi] == col) { 
#if MTEST
	      printf("(%d,%d)", col, ds[j]); 
#endif
	      mdiff += col; continue;
	    }
          }
        }
      }

#if MTEST
printf(" %d\n", mdiff-mdiff0);
mdiff0=mdiff;
#endif

    }
  }

  return mdiff;
}


#define MOBN(N, NB, IA)				\
						\
REAL MOB##N(BRETT *pb)				\
{						\
  return mobx(pb, sizeof(IA)/sizeof(int), IA); 	\
}						\
						\
MERKMAL_B MOB##NB = { MOB##N, "MOB"#N };


int sq1[]   = { A1, A8, H1, H8 };
int sq2[]   = { A2, B1, G1, H2, A7, B8, G8, H7 };
int sq3[]   = { A3, A6, C1, C8, F1, F8, H3, H6 };
int sq4[]   = { A4, A5, D1, E1, D8, E8, H4, H5 };
int sq5[]   = { B2, G2, B7, G7 };
int sq6[]   = { B3, C2, B6, C7, F2, F7, G3, G6 };
int sq7[]   = { B4, B5, D2, E2, D7, E7, G4, G5 };
int sq8[]   = { C3, C6, F3, F6 };
int sq9[]   = { C4, C5, D3, E3, D6, E6, F4, F5 };
int sq10[]  = { D4, D5, E4, E5 };


MOBN(1, 1_B,  sq1)
MOBN(2, 2_B,  sq2)
MOBN(3, 3_B,  sq3)
MOBN(4, 4_B,  sq4)
MOBN(5, 5_B,  sq5)
MOBN(6, 6_B,  sq6)
MOBN(7, 7_B,  sq7)
MOBN(8, 8_B,  sq8)
MOBN(9, 9_B,  sq9)
MOBN(10, 10_B, sq10)

// no diag3-moves!

REAL MOBSUM(BRETT *pb) 
{
  int i, j, mdiff=0;

   SPFELD board1, board2;

  BrettSf(pb, &board1);

  FOR_SFPOS10(i) {

    board2 = board1;
    if ((j=Check3[i])) board2.p[j] = LEER;
    if (SfSetzen(&board2, BLACK, i)) mdiff++;    

    board2 = board1;
    if ((j=Check3[i])) board2.p[j] = LEER;
    if (SfSetzen(&board2, WHITE, i)) mdiff--;
  }

  return mdiff;
} 

MERKMAL_B MOBSUM_B = { MOBSUM, "MOBSUM" };



REAL MOBAPP(BRETT *pb)
{
  return 
 MOB1(pb)*        6.396e-01  
+MOB2(pb)*        7.006e-01  
+MOB3(pb)*        6.434e-01  
+MOB4(pb)*        5.240e-01  
+MOB5(pb)*        5.345e-01  
+MOB6(pb)*        5.812e-01  
+MOB7(pb)*        5.165e-01  
+MOB8(pb)*        5.251e-01  
+MOB9(pb)*        4.793e-01;
}

MERKMAL_B MOBAPP_B = { MOBAPP, "MOBAPP" };



REAL EVAL(BRETT *pb)
{
return 0;
//  return EvalA(pb, BLACK);
}

MERKMAL_B EVALX_B = { EVAL, "EVAL" };


