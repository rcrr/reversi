// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/* quick evaluation, 8.94 */

#include "main.h"
#include "board.h"

/* features:

[CONST]
STRMOBALLB
STRPOTMOBALLB
STRHV1B
STRHV2B
STRHV3B
STRHV4B
STRD1B
STRD2B
STRD3B
STRD4B
STRR1B

*/

#define MOB_INDEX        1
#define POTMOB_INDEX     2
#define HV1_INDEX        3
#define HV2_INDEX        4
#define HV3_INDEX        5
#define HV4_INDEX        6
#define D1_INDEX         7
#define D2_INDEX         8
#define D3_INDEX         9
#define D4_INDEX         10
#define R1_INDEX         11


#define INTFAC         1000000

#define SFAUS	false

 
#define TEST	false
#define SCHNELL true


#define AUSGABE	false
 

#ifndef PRECOMP
#include "main.h"
#endif
 
#include "move.h"
#include "eval.h"
#include "crt.h"

#include "fpatt.h"



#define MIN_ANZ		12
#define USE_ANZ		12
#define MAX_ANZ		62


#define ANZ		(MAX_ANZ-MIN_ANZ+1)
#define KOEFF_ANZ1	12

static float koeff1[ANZ*KOEFF_ANZ1];
static bool  koeff1da=false;

TABLES TabsB, TabsW;




float *pfhv1, *pfhv2, *pfhv3, *pfhv4, 
      *pfd1, *pfd2, *pfd3, *pfd4, *pfd5, *pfd6, 
      *pfr1;


/*  combine all "ray"-tables by multiplying coefficients with
 *  table-entries and adding the products
 *
 *  coefficients from 12(1)62
 *  tables from 12(4)60
 *
 *  float-tables from 12(1)62
 */

#define TABNUM  (62-12+1)

void MergeTables(float *coeff, TABLES *ptabs)
{
  int i, k, discs, sind;
  float *pf, *pk;


  printf("[ merge tables ..."); fflush(stdout);

  if (pfhv1) Error("pf-tables already initialized!");

  pfhv1 = (float*) calloc(sizeof(float), NUM8 * TABNUM);
  pfhv2 = (float*) calloc(sizeof(float), NUM8 * TABNUM);
  pfhv3 = (float*) calloc(sizeof(float), NUM8 * TABNUM);
  pfhv4 = (float*) calloc(sizeof(float), NUM8 * TABNUM);

  pfd1 = (float*) calloc(sizeof(float), NUM8 * TABNUM);
  pfd2 = (float*) calloc(sizeof(float), NUM7 * TABNUM);
  pfd3 = (float*) calloc(sizeof(float), NUM6 * TABNUM);
  pfd4 = (float*) calloc(sizeof(float), NUM5 * TABNUM);
  pfd5 = (float*) calloc(sizeof(float), NUM4 * TABNUM);
  pfd6 = (float*) calloc(sizeof(float), NUM3 * TABNUM);

#if NEW_PATT
  pfr1 = (float*) calloc(sizeof(float), NUM8 * TABNUM);
  if (!pfr1) Error("MergeTables: mem?");
#endif

  if (!pfhv1 || !pfhv2 || !pfhv3 || !pfhv4 || 
      !pfd1 || !pfd2 || !pfd3 || !pfd4 || !pfd5 || !pfd6) 

    Error("MergeTables: mem?");



#if 0

/* test symmetry */ 

FOR (k, INUM+1) {
  int d=k*NUM8;

printf("k=%d\n", k);
FOR (i, NUM8) {
  if (pfhv1[d+i] != -pfhv1[d+NUM8-1-i]) printf("1\n");
  if (ptabs->hv2[d+i] != -ptabs->hv2[d+NUM8-1-i]) printf("t2 %d %d %d\n", i, ptabs->hv2[d+i], -ptabs->hv2[d+NUM8-1-i]);
  if (pfhv2[d+i] != -pfhv2[d+NUM8-1-i]) printf("2 %d %f %f\n", i, pfhv2[d+i], -pfhv2[d+NUM8-1-i]);
  if (pfhv3[d+i] != -pfhv3[d+NUM8-1-i]) printf("3\n");
  if (pfhv4[d+i] != -pfhv4[d+NUM8-1-i]) printf("4\n");
  if (pfd1[d+i] != -pfd1[d+NUM8-1-i]) printf("d1\n");

  if (ptabs->r1[d+i] != -ptabs->r1[d+NUM8-1-i]) printf("tr1 %d %d %d\n", i, ptabs->r1[d+i], -ptabs->r1[d+NUM8-1-i]);


  if (ptabs->d1[d+i] != -ptabs->d1[d+NUM8-1-i]) printf("d1 %d %d %d\n", i, ptabs->d1[d+i], -ptabs->d1[d+NUM8-1-i]);
}}



FOR (k, INUM+1) {
  int d=k*NUM7;
printf("k=%d\n", k);
FOR (i, NUM7) {
  if (pfd2[d+i] != -pfd2[d+NUM7-1-i]) printf("d2 %d %f %f\n", i, pfd2[d+i], -pfd2[d+NUM7-1-i]);
  if (ptabs->d2[d+i] != -ptabs->d2[d+NUM7-1-i]) printf("d2 %d %d %d\n", i, ptabs->d2[d+i], -ptabs->d2[d+NUM7-1-i]);
}}



FOR (k, INUM+1) {
  int d=k*NUM6;
printf("k=%d\n", k);
FOR (i, NUM6) {
  if (pfd3[d+i] != -pfd3[d+NUM6-1-i]) printf("d3 %d %f %f\n", i, pfd3[d+i], -pfd3[d+NUM6-1-i]);
  if (ptabs->d3[d+i] != -ptabs->d3[d+NUM6-1-i]) printf("d3 %d %d %d\n", i, ptabs->d3[d+i], -ptabs->d3[d+NUM6-1-i]);
}}

FOR (k, INUM+1) {
  int d=k*NUM5;
printf("k=%d\n", k);
FOR (i, NUM5) {
  if (ptabs->d4[d+i] != -ptabs->d4[d+NUM5-1-i]) printf("td4 %d %d %d\n", i, ptabs->d4[d+i], -ptabs->d4[d+NUM5-1-i]);
  if (pfd4[d+i] != -pfd4[d+NUM5-1-i]) printf("d4 %d %f %f\n", i, pfd4[d+i], -pfd4[d+NUM5-1-i]);
}}

FOR (k, INUM+1) {
  int d=k*NUM4;
printf("k=%d\n", k);
FOR (i, NUM4) {
  if (pfd5[d+i] != -pfd5[d+NUM4-1-i]) printf("d5\n");
  if (ptabs->d5[d+i] != -ptabs->d5[d+NUM4-1-i]) printf("td5 %d %d %d\n", i, ptabs->d5[d+i], -ptabs->d5[d+NUM4-1-i]);
}}

FOR (k, INUM+1) {
  int d=k*NUM3;
printf("k=%d\n", k);
FOR (i, NUM3) {
  if (pfd6[d+i] != -pfd6[d+NUM3-1-i]) printf("d6\n");
  if (ptabs->d6[d+i] != -ptabs->d6[d+NUM3-1-i]) printf("td6 %d %d %d\n", i, ptabs->d6[d+i], -ptabs->d6[d+NUM3-1-i]);
}}

#endif




  for (discs=12; discs <= 62; discs++) {

    k = discs - 12;

    sind = (discs - I0 + IWIDTH/2) / IWIDTH;
    if      (sind < 0)    sind = 0;
    else if (sind > INUM) sind = INUM;

    pk = &coeff[(discs-I0)*KOEFF_ANZ1];


/* mobility & potential mobility */


#define ADJMOB(p, num, dm, dp)   \
  pf = &p[k * NUM##num];\
  FOR (i, NUM##num) {      \
\
/*printf("%8d: %d %f\n", i, (((StrMob##num[i] & MOB_MASK) >> MOB_SHIFT)-dm), pk[MOB_INDEX]);*/\
\
    pf[i] += pk[MOB_INDEX] * \
               ((int)((StrMob##num[i] & MOB_MASK) >> MOB_SHIFT)-dm);\
  }

#define ADJALL(p, num, dm, dp)    \
  pf = &p[k * NUM##num];\
  FOR (i, NUM##num) {      \
\
/*printf("%8d: %d %f %d %f\n", i, (((StrMob##num[i] & MOB_MASK) >> MOB_SHIFT)-dm), pk[MOB_INDEX], ((((StrMob##num[i] & POTMOB_MASK) >> POTMOB_SHIFT)-dp) + (((StrMob##num[i] & POTMOBE_MASK) >> POTMOBE_SHIFT)-dp)), pk[POTMOB_INDEX]);*/\
\
    pf[i] += pk[MOB_INDEX] *\
               ((int)((StrMob##num[i] & MOB_MASK) >> MOB_SHIFT)-dm) + \
             pk[POTMOB_INDEX] * \
               (((int)((StrMob##num[i] & POTMOB_MASK) >> POTMOB_SHIFT)-dp) + \
               ((int)((StrMob##num[i] & POTMOBE_MASK) >> POTMOBE_SHIFT)-dp));\
  }


/* edges: no potmob! */

    ADJMOB(pfhv1, 8, 3, 5);

    ADJALL(pfhv2, 8, 3, 5);
    ADJALL(pfhv3, 8, 3, 5);
    ADJALL(pfhv4, 8, 3, 5);

    ADJALL(pfd1, 8, 3, 5);
    ADJALL(pfd2, 7, 3, 4);
    ADJALL(pfd3, 6, 3, 3);
    ADJALL(pfd4, 5, 3, 2);
    ADJALL(pfd5, 4, 3, 2);

/* diagonals of length 3: no potmob! */

    ADJMOB(pfd6, 3, 3, 2);


/* pattern values */

#define ADJPAT(p,num,index,tab)   \
  pf = &p[k * NUM##num];\
  FOR (i, NUM##num)       \
    pf[i] += pk[index] * tab[sind * NUM##num + i];


    ADJPAT(pfhv1, 8, HV1_INDEX, ptabs->hv1);
    ADJPAT(pfhv2, 8, HV2_INDEX, ptabs->hv2);
    ADJPAT(pfhv3, 8, HV3_INDEX, ptabs->hv3);
    ADJPAT(pfhv4, 8, HV4_INDEX, ptabs->hv4);

    ADJPAT(pfd1, 8, D1_INDEX, ptabs->d1);
    ADJPAT(pfd2, 7, D2_INDEX, ptabs->d2);
    ADJPAT(pfd3, 6, D3_INDEX, ptabs->d3);
    ADJPAT(pfd4, 5, D4_INDEX, ptabs->d4);

#if NEW_PATT
    ADJPAT(pfr1, 8, R1_INDEX, ptabs->r1);
#endif

  }

  printf("OK ]\n");
}




/**************************************************************/

/* debug macro */

#if 0

#define P(x) \
su0=su; \
x;\
printf("%f ", su-su0);

#else 

#define P(x) x

#endif





WERT EvalA(BRETT *pb, PARTEI Partei)
{
  float su, su0, *pf;
  int	i, isu, sind;
  float *pk;
  TABTYPE *pt;
  STRAHLTYP *pat;
  SFPOS *p=pb->p;
  static TABLES TabsB, TabsW;
BRETT board;
SPFELD sf;

#if TEST
REAL su1;
#endif


#if SFAUS
{ SPFELD sf;
  BrettSf(pb, &sf);
  SfAus(&sf, 0, 0);
  printf("%d\n", Partei);
}
#endif



  if (!koeff1da) { 

    if (!ParameterEinlesen(ParameterFile, ANZ*KOEFF_ANZ1, koeff1)) {
      Error("A parameters?");
    }

    koeff1da = true;

    ReadTables(TableFile, &TabsB, &TabsW);

    MergeTables(koeff1, &TabsB);
  }



/* WIPE-OUT? */

  if ((i=pb->SteinAnz) != 0) 

    if (Partei == BLACK) {

      if (i == -pb->StDiffBW) return -(WERTGEWINN+64);

    } else {

      if (i == +pb->StDiffBW) return -(WERTGEWINN+64);

    }


  sind = (i - I0 + IWIDTH/2) / IWIDTH;
  if      (sind < 0)    sind = 0;
  else if (sind > INUM) sind = INUM;


  if      (i < MIN_ANZ) i = MIN_ANZ - MIN_ANZ;
  else if (i > MAX_ANZ) i = MAX_ANZ - MIN_ANZ;
  else                  i = i - MIN_ANZ;

  pk = &koeff1[i*KOEFF_ANZ1];

#if !NEW_PATT

  pt = &(TabsB.r1)[sind*NUM8 + (NUM8-1)/2];

  isu =       pt[S8(A1,B1,C1,D1,A2,B2,C2,D2)] +
	      pt[S8(H1,G1,F1,E1,H2,G2,F2,E2)] + 
	      pt[S8(A8,B8,C8,D8,A7,B7,C7,D7)] +
	      pt[S8(H8,G8,F8,E8,H7,G7,F7,E7)] + 
	      pt[S8(A1,A2,A3,A4,B1,B2,B3,B4)] +
	      pt[S8(H1,H2,H3,H4,G1,G2,G3,G4)] +
	      pt[S8(A8,A7,A6,A5,B8,B7,B6,B5)] +
	      pt[S8(H8,H7,H6,H5,G8,G7,G6,G5)];


#if 0
    { SPFELD sf;
      BrettSf(pb, &sf);
      SfAus(&sf, 0, 0);
    }
#endif


  P(su = isu * pk[R1_INDEX];)

  pat = pb->Patt.st;

  pf = &(pfhv1)[i*NUM8 + (NUM8-1)/2];

  P(su += pf[pat[BS1]] + pf[pat[BS8]] + pf[pat[BSA]] + pf[pat[BSH]];)

  pf = &(pfhv2)[i*NUM8 + (NUM8-1)/2];
  P(su += pf[pat[BS2]] + pf[pat[BS7]] + pf[pat[BSB]] + pf[pat[BSG]];)

  pf = &(pfhv3)[i*NUM8 + (NUM8-1)/2];
  P(su += pf[pat[BS3]] + pf[pat[BS6]] + pf[pat[BSC]] + pf[pat[BSF]];)

  pf = &(pfhv4)[i*NUM8 + (NUM8-1)/2];
  P(su += pf[pat[BS4]] + pf[pat[BS5]] + pf[pat[BSD]] + pf[pat[BSE]];)

  pf = &(pfd1)[i*NUM8 + (NUM8-1)/2];
  P(su += pf[pat[BDM8]] + pf[pat[BDP8]];)

  pf = &(pfd2)[i*NUM7 + (NUM7-1)/2];
  P(su += pf[pat[BDM7]] + pf[pat[BDM9]] + pf[pat[BDP7]] + pf[pat[BDP9]];)

  pf = &(pfd3)[i*NUM6 + (NUM6-1)/2];
  P(su += pf[pat[BDM6]] + pf[pat[BDM10]] + pf[pat[BDP6]] + pf[pat[BDP10]];)

  pf = &(pfd4)[i*NUM5 + (NUM5-1)/2];
  P(su += pf[pat[BDM5]] + pf[pat[BDM11]] + pf[pat[BDP5]] + pf[pat[BDP11]];)

  pf = &(pfd5)[i*NUM4 + (NUM4-1)/2];
  P(su += pf[pat[BDM4]] + pf[pat[BDM12]] + pf[pat[BDP4]] + pf[pat[BDP12]];)

  pf = &(pfd6)[i*NUM3 + (NUM3-1)/2];
  P(su += pf[pat[BDM3]] + pf[pat[BDM13]] + pf[pat[BDP3]] + pf[pat[BDP13]];)


#else


  pat = pb->NewPatt.st;

  pf = &(pfr1)[i*NUM8 + (NUM8-1)/2];
  su = pf[pat[PR1A]] + pf[pat[PR1B]] + pf[pat[PR1C]] + pf[pat[PR1D]] +
        pf[pat[PR1E]] + pf[pat[PR1F]] + pf[pat[PR1G]] + pf[pat[PR1H]];

  pf = &(pfhv1)[i*NUM8 + (NUM8-1)/2];
  su += pf[pat[PHV1A]] + pf[pat[PHV1B]] + pf[pat[PHV1C]] + pf[pat[PHV1D]];

  pf = &(pfhv2)[i*NUM8 + (NUM8-1)/2];
  su += pf[pat[PHV2A]] + pf[pat[PHV2B]] + pf[pat[PHV2C]] + pf[pat[PHV2D]];

  pf = &(pfhv3)[i*NUM8 + (NUM8-1)/2];
  su += pf[pat[PHV3A]] + pf[pat[PHV3B]] + pf[pat[PHV3C]] + pf[pat[PHV3D]];

  pf = &(pfhv4)[i*NUM8 + (NUM8-1)/2];
  su += pf[pat[PHV4A]] + pf[pat[PHV4B]] + pf[pat[PHV4C]] + pf[pat[PHV4D]];

  pf = &(pfd1)[i*NUM8 + (NUM8-1)/2];
  su += pf[pat[PD1A]] + pf[pat[PD1B]];

  pf = &(pfd2)[i*NUM7 + (NUM7-1)/2];
  su += pf[pat[PD2A]] + pf[pat[PD2B]] + pf[pat[PD2C]] + pf[pat[PD2D]];

  pf = &(pfd3)[i*NUM6 + (NUM6-1)/2];
  su += pf[pat[PD3A]] + pf[pat[PD3B]] + pf[pat[PD3C]] + pf[pat[PD3D]];

  pf = &(pfd4)[i*NUM5 + (NUM5-1)/2];
  su += pf[pat[PD4A]] + pf[pat[PD4B]] + pf[pat[PD4C]] + pf[pat[PD4D]];

  pf = &(pfd5)[i*NUM4 + (NUM4-1)/2];
  su += pf[pat[PD5A]] + pf[pat[PD5B]] + pf[pat[PD5C]] + pf[pat[PD5D]];

  pf = &(pfd6)[i*NUM3 + (NUM3-1)/2];
  su += pf[pat[PD6A]] + pf[pat[PD6B]] + pf[pat[PD6C]] + pf[pat[PD6D]];

#endif

  if (Partei == WHITE) su = -su;

P(su += pk[0];)    /* Intercept */

#if 0
printf("\n\n");
#endif

  isu = REAL_TO_WERT(su);

  if (isu >  WERTGEWINN) return  WERTGEWINN; 
  if (isu < -WERTGEWINN) return -WERTGEWINN;  

#if 0

{ int v = EvalA(pb, Partei);
  if (abs(v-isu) > 1) printf("%d: %d %d\n", pb->SteinAnz, v, isu);
}

#endif


  return isu;
}



