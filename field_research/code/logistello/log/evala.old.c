// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/*  B1,B2 , 3.95 */

#include "main.h"
#include "board.h"
#include "eval.h"

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
STRB1B
STRB2B

*/

#define TAB1_MAX  (3.97) //       (1.41) // was 3.97
#define TAB2_MAX  (2.7)  //       (1.64)  // was 2.7

#define CUT_OFFSET       1.1
#define CUT_TEST         false
#define CUT_DISCNUM      30


#define SLOW 0


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
#define B1_INDEX         12
#define B2_INDEX         13

#define NUM10HALF        ((NUM10-1)/2+1)

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
#define KOEFF_ANZ1	14

#define NK	(*pk++)				/* nächster Parameter       */

#if 1	/* !!! */
#define NKG0	(*pk++)				/* nächster Parameter       */
#else
#define NKG0    ((f=*pk++) > 0 ? f : 0)		/* nächster Param. aber > 0 */
#endif

/*

cutres:

[ 
12 1.4
16 1.4
20 1.4
]
24 1.4
28 1.4
32 1.3
35 1.3
39 1.5
43 1.4
48 1.5
52 1.4
56 1.1

*/



static float cut_values[MAX_ANZ-MIN_ANZ+1] = {

#if 1

 1.4, 1.4, 1.4, 1.4,  
 1.4, 1.4, 1.4, 1.4,  
 1.4, 1.4, 1.4, 1.4,  
 1.3, 1.4, 1.4, 1.4,  
 1.3, 1.3, 1.3, 1.3,  
 1.3, 1.3, 1.3, 1.3,  
 1.3, 1.3, 1.3, 1.3,  
 1.3, 1.3, 1.3, 1.3, 
 1.3, 1.3, 1.3, 1.3,  
 1.1, 1.1, 1.1, 1.1,  
 1.1, 1.1, 1.1, 1.1,  
 1.1, 1.1, 1.1, 1.1,
 1.1, 1.1, 1.1

#else

/*12*/
 1.4, 1.4, 1.4, 1.4, 
 1.4, 1.4, 1.4, 1.4,
/*20*/
 1.4, 1.4, 1.4, 1.4,
 1.4, 1.4, 1.4, 1.4,
/*28*/
 1.4, 1.4, 1.4, 1.4,
 1.4, 1.4, 1.4, 1.4,
/*36*/
 1.4, 1.4, 1.4, 1.4,
 1.4, 1.4, 1.4, 1.4,
/*44*/
 1.3, 1.3, 1.3, 1.3,
 1.2, 1.2, 1.2, 1.2,
/*52*/
 1.1, 1.1, 1.1, 1.1,
 1.1, 1.1, 1.1, 1.1,
/*60*/
 1.1, 1.1, 1.1

#endif

};


static float koeff1[ANZ*KOEFF_ANZ1];
static bool  koeff1da=false;

static TABLES TabsB, TabsW;

#if 1
typedef short   FTABTYPE;
#define FTABMAX 32760
#endif

#if 0
typedef sint1   FTABTYPE;
#define FTABMAX 127
#endif

#if 0
typedef int FTABTYPE;
#define FTABMAX 1000000
#endif


FTABTYPE *pfhv1, *pfhv2, *pfhv3, *pfhv4, 
         *pfd1, *pfd2, *pfd3, *pfd4, *pfd5, *pfd6, 
         *pfb1, *pfb2;


inline void TABSTORE(FTABTYPE *p, float delta, float TAB_MAX)
{
  int r = round((delta * FTABMAX / TAB_MAX)); 

  if      (r > +FTABMAX) { *p = +FTABMAX; printf(">"); }
  else if (r < -FTABMAX) { *p = -FTABMAX; printf("<"); }
  else                   *p = r;
}





/*  combine all "ray"-tables by multiplying coefficients with
 *  table-entries and adding the products
 *
 *  coefficients:    12(1)62
 *  tables:          12(4)60
 *
 *  FTABTYPE-tables: 12(1)62
 */

#define TABNUM  (62-12+1)

static void MergeTables(float *coeff, TABLES *ptabs)
{
  int i, k, discs, sind;
  FTABTYPE *pf;
  float *pk;


  printf("[ merge tables ..."); fflush(stdout);

  if (pfhv2) Error("pf-tables already initialized!");

  pfhv2 = (FTABTYPE*) calloc(sizeof(FTABTYPE), NUM8 * TABNUM);
  pfhv3 = (FTABTYPE*) calloc(sizeof(FTABTYPE), NUM8 * TABNUM);
  pfhv4 = (FTABTYPE*) calloc(sizeof(FTABTYPE), NUM8 * TABNUM);

  pfd1 = (FTABTYPE*) calloc(sizeof(FTABTYPE), NUM8 * TABNUM);
  pfd2 = (FTABTYPE*) calloc(sizeof(FTABTYPE), NUM7 * TABNUM);
  pfd3 = (FTABTYPE*) calloc(sizeof(FTABTYPE), NUM6 * TABNUM);
  pfd4 = (FTABTYPE*) calloc(sizeof(FTABTYPE), NUM5 * TABNUM);
  pfd5 = (FTABTYPE*) calloc(sizeof(FTABTYPE), NUM4 * TABNUM);
  pfd6 = (FTABTYPE*) calloc(sizeof(FTABTYPE), NUM3 * TABNUM);

  if (!pfhv2 || !pfhv3 || !pfhv4 || 
      !pfd1 || !pfd2 || !pfd3 || !pfd4 || !pfd5 || !pfd6) 

    Error("MergeTables: mem?");

  pfb1 = (FTABTYPE*) calloc(sizeof(FTABTYPE), NUM10HALF * TABNUM);
  if (!pfb1) Error("MergeTables: mem?");

  pfb2 = (FTABTYPE*) calloc(sizeof(FTABTYPE), NUM10HALF * TABNUM);
  if (!pfb2) Error("MergeTables: mem?");


#if 0

/* test symmetry */ 

FOR (i, NUM10) {
  if (ptabs->b1[i] != -ptabs->b1[NUM10-1-i]) printf("tb1 %d %d %d\n", i, ptabs->b1[i], -ptabs->b1[NUM10-1-i]);
  if (ptabs->b2[i] != -ptabs->b2[NUM10-1-i]) printf("tb2 %d %d %d\n", i, ptabs->b2[i], -ptabs->b1[NUM10-1-i]);

}

FOR (k, INUM+1) {
  int d=k*NUM8;

printf("k=%d\n", k);
FOR (i, NUM8) {
/*  if (pfhv1[d+i] != -pfhv1[d+NUM8-1-i]) printf("1\n"); */
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



/* only mobility */

#define ADJMOB(p, num, dm, dp, tab_max)   \
  pf = &p[k * NUM##num];\
  FOR (i, NUM##num) {      \
\
    TABSTORE(\
      &pf[i],\
      pk[MOB_INDEX] * ((int)((StrMob##num[i] & MOB_MASK) >> MOB_SHIFT)-dm),\
      tab_max\
    );\
  }



/* mobility & potential mobility */

#define ADJMOBALL(p, num, dm, dp, tab_max)    \
  pf = &p[k * NUM##num];\
  FOR (i, NUM##num) {      \
\
    TABSTORE(\
      &pf[i],\
      pk[MOB_INDEX] * ((int)((StrMob##num[i] & MOB_MASK) >> MOB_SHIFT)-dm) + \
      pk[POTMOB_INDEX] * \
         (((int)((StrMob##num[i] & POTMOB_MASK) >> POTMOB_SHIFT)-dp) + \
         ((int)((StrMob##num[i] & POTMOBE_MASK) >> POTMOBE_SHIFT)-dp)), \
      tab_max\
    );\
  }



/* mobility & potential mobility & pattern */

#define ADJALL(p, num, dm, dp, index, tab, tab_max)    \
  pf = &p[k * NUM##num];\
  FOR (i, NUM##num) {      \
\
    TABSTORE(\
      &pf[i],\
      pk[MOB_INDEX] * ((int)((StrMob##num[i] & MOB_MASK) >> MOB_SHIFT)-dm) + \
      pk[POTMOB_INDEX] * \
         (((int)((StrMob##num[i] & POTMOB_MASK ) >> POTMOB_SHIFT )-dp)  + \
          ((int)((StrMob##num[i] & POTMOBE_MASK) >> POTMOBE_SHIFT)-dp)) + \
      pk[index] * tab[sind * NUM##num + i],\
      tab_max\
    );\
  }


/* b1:  b1 pattern + r1 pattern */

    pf = &pfb1[k * NUM10HALF];
    FOR (i, NUM10HALF) {

      TABSTORE(
        &pf[i], 
/*
 *                               mask out a5,b5 squares
 *                                              v
 */
        pk[B1_INDEX] * ptabs->b1[i] + 
        pk[R1_INDEX] * ptabs->r1[sind * NUM8 + i/9],
        TAB2_MAX
      );

    }


/* b2:  edge mobility (no potmob!) + b2 pattern + hv1 pattern */

    pf = &pfb2[k * NUM10HALF];
    FOR (i, NUM10HALF) {

      TABSTORE(
        &pf[i], 
        pk[MOB_INDEX] * ((int)((StrMob8[i/9] & MOB_MASK) >> MOB_SHIFT)-3) +
/*                                       ^
 *                               mask out X squares
 *                                               v
 */
        pk[B2_INDEX]  * ptabs->b2[i] + 
        pk[HV1_INDEX] * ptabs->hv1[sind * NUM8 + i/9],
        TAB2_MAX
      );

    }

    ADJALL(pfhv2, 8, 3, 5, HV2_INDEX, ptabs->hv2, TAB1_MAX);
    ADJALL(pfhv3, 8, 3, 5, HV3_INDEX, ptabs->hv3, TAB1_MAX);
    ADJALL(pfhv4, 8, 3, 5, HV4_INDEX, ptabs->hv4, TAB1_MAX);

    ADJALL(pfd1,  8, 3, 5, D1_INDEX, ptabs->d1, TAB1_MAX);
    ADJALL(pfd2,  7, 3, 4, D2_INDEX, ptabs->d2, TAB1_MAX);
    ADJALL(pfd3,  6, 3, 3, D3_INDEX, ptabs->d3, TAB1_MAX);
    ADJALL(pfd4,  5, 3, 2, D4_INDEX, ptabs->d4, TAB1_MAX);

    ADJMOBALL(pfd5, 4, 3, 2, TAB1_MAX);

    ADJMOB(pfd6, 3, 3, 2, TAB1_MAX);  /* diagonals of length 3: no potmob! */

  }

{
  int max;
 
#define M(p,ind) \
  if (abs(p[ind]) > max) {\
    max = abs(p[ind]); \
/*    printf(#p" %d %f\n", ind, max);*/\
  }

  printf(" max: ");

  max = INT_MIN; 
  FOR (i, NUM8*TABNUM) { M(pfd1,i); } 
  printf("d1=%d ", max);

  max = INT_MIN; 
  FOR (i, NUM7*TABNUM) { M(pfd2,i); } 
  printf("d2=%d ", max);

  max = INT_MIN; 
  FOR (i, NUM6*TABNUM) { M(pfd3,i); } 
  printf("d3=%d ", max);

  max = INT_MIN; 
  FOR (i, NUM5*TABNUM) { M(pfd4,i); } 
  printf("d4=%d ", max);

  max = INT_MIN; 
  FOR (i, NUM4*TABNUM) { M(pfd5,i); } 
  printf("d5=%d ", max);

  max = INT_MIN; 
  FOR (i, NUM3*TABNUM) { M(pfd6,i); }
  printf("d6=%d ", max);

  max = INT_MIN; 
  FOR (i, NUM8*TABNUM) { M(pfhv2,i); } 
  printf("hv2=%d ", max);

  max = INT_MIN; 
  FOR (i, NUM8*TABNUM) { M(pfhv3,i); } 
  printf("hv3=%d ", max);

  max = INT_MIN; 
  FOR (i, NUM8*TABNUM) { M(pfhv4,i); } 
  printf("hv4=%d ", max);

  max = INT_MIN; 
  FOR (i, NUM10HALF*TABNUM) { M(pfb1,i ); }
  printf("b1=%d ", max);

  max = INT_MIN; 
  FOR (i, NUM10HALF*TABNUM) { M(pfb2,i ); }
  printf("b2=%d ]\n", max);
}


}



/* only 2x4 pattern */

WERT EvalASimple(BRETT *pb, PARTEI Partei)
{
  float su;
  int	i, isu;
  float *pk;



  if (!koeff1da) { 

    if (!ParameterEinlesen(ParameterFile, ANZ*KOEFF_ANZ1, koeff1)) {
      Error("A parameters?");
    }

    koeff1da = true;
  }


/* WIPE-OUT? */

  if ((i=pb->SteinAnz) != 0) 

    if (Partei == BLACK) {

      if (i == -pb->StDiffBW) return(-(WERTGEWINN+64));

    } else {

      if (i == +pb->StDiffBW) return(-(WERTGEWINN+64));

    }

  if      (i < MIN_ANZ) i = MIN_ANZ - MIN_ANZ;
  else if (i > MAX_ANZ) i = MAX_ANZ - MIN_ANZ;
  else                  i = i - MIN_ANZ;

  pk = &koeff1[i*KOEFF_ANZ1];

  f_pattnew = true;

  su = STRR1_D * pk[10];

  if (Partei == WHITE) su = -su;

  su += pk[0];

#if 1
  isu = (int) (su * WERTFAKTOR);
#else
  isu = REAL_TO_WERT(su);
#endif

  if (isu >  WERTGEWINN) return(+WERTGEWINN); 
  if (isu < -WERTGEWINN) return(-WERTGEWINN);  

  return(isu);
}














/* no table merging */

WERT EvalASlow(BRETT *pb, PARTEI Partei)
{
  float su;
  int	i, isu;
  float *pk;



  if (!koeff1da) { 

    if (!ParameterEinlesen(ParameterFile, ANZ*KOEFF_ANZ1, koeff1)) {
      Error("A parameters?");
    }

    koeff1da = true;

/*
    ReadTables(TableFile, &TabsB, &TabsW);
*/
  }


/* WIPE-OUT? */

  if ((i=pb->SteinAnz) != 0) 

    if (Partei == BLACK) {

      if (i == -pb->StDiffBW) return(-(WERTGEWINN+64));

    } else {

      if (i == +pb->StDiffBW) return(-(WERTGEWINN+64));

    }

  if      (i < MIN_ANZ) i = MIN_ANZ - MIN_ANZ;
  else if (i > MAX_ANZ) i = MAX_ANZ - MIN_ANZ;
  else                  i = i - MIN_ANZ;

  pk = &koeff1[i*KOEFF_ANZ1];

  pk++;

  su = 0;

  f_pattnew = true;

  su += STRMOBALLB(pb) * NK;

  f_pattnew = false;

  su += STRPOTMOBALL_D * NK;

  su += STRHV1_D * NKG0;
  su += STRHV2_D * NKG0;
  su += STRHV3_D * NKG0;
  su += STRHV4_D * NKG0;

  su += STRD1_D  * NKG0;
  su += STRD2_D  * NKG0;
  su += STRD3_D  * NKG0;
  su += STRD4_D  * NKG0; 

  su += STRR1_D * NKG0;

  su += STRB1_D * NKG0;
  su += STRB2_D * NKG0;

  if (Partei == WHITE) su = -su;

  su += koeff1[i*KOEFF_ANZ1+0];      /* Intercept */

#if 1
  isu = (int) (su * WERTFAKTOR);
#else
  isu = REAL_TO_WERT(su);
#endif

  if (isu >  WERTGEWINN) return(+WERTGEWINN); 
  if (isu < -WERTGEWINN) return(-WERTGEWINN);  

  return(isu);
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


static FTABTYPE 
         *pb1[65], *pb2[65], 
         *pd1[65], *pd2[65], *pd3[65], *pd4[65], *pd5[65], *pd6[65],
         *phv2[65], *phv3[65], *phv4[65];

static float CTAB1=0,  CTAB2=0, InterceptW[MAX_ANZ-MIN_ANZ+1];
static float CTAB1W=0, CTAB2W=0;
static int   CutOffsetW[MAX_ANZ-MIN_ANZ+1];

SFPOS *xx;
int dummy;
STRAHLTYP *pdummy;


WERT EvalA(BRETT *pb, PARTEI Partei)
{
  register STRAHLTYP *pat 

#if __i386__
asm("%edi")
#endif

  = pb->NewPatt.st;

  register FTABTYPE *pf

#if __i386__
asm("%ebx")
#endif
  ;

  int isu;
  float su;
  int	i, w;

#if TEST
REAL su1;
#endif

#if 0
static int num=0;
num++;
if (( num % 10000) == 0) printf("%d\n", num);
#endif

#if SFAUS

{ SPFELD sf;
  BrettSf(pb, &sf);
  SfAus(&sf, 0, 0);
  printf("%d\n", Partei);
}
#endif

#if 0

#define HTSIZE (1<<18)

static struct { uint4 lock; } ht[HTSIZE];
static int hit=0, checknum=0;
  if (ht[pb->Hash1 & (HTSIZE-1)].lock == pb->Hash2) hit++;
  checknum++;
  ht[pb->Hash1 & (HTSIZE-1)].lock = pb->Hash2;

  if (!(checknum % 10000)) printf("%d %d %f\n", checknum, hit, (float)hit/checknum);
#endif

  if (!koeff1da) { 

    if (!ParameterEinlesen(ParameterFile, ANZ*KOEFF_ANZ1, koeff1)) {
      Error("A parameters?");
    }

    koeff1da = true;

    ReadTables(TableFile, &TabsB, &TabsW);

    MergeTables(koeff1, &TabsB);

    CTAB1      = (float)TAB1_MAX / FTABMAX;
    CTAB2      = (float)TAB2_MAX / FTABMAX;
    CTAB1W     = CTAB1 * WERTFAKTOR;
    CTAB2W     = CTAB2 * WERTFAKTOR;

    FOR (i, 65) {

      pd6[i] = pfd6 + i * NUM3 + (NUM3-1)/2;
      pd5[i] = pfd5 + i * NUM4 + (NUM4-1)/2;
      pd4[i] = pfd4 + i * NUM5 + (NUM5-1)/2;
      pd3[i] = pfd3 + i * NUM6 + (NUM6-1)/2;
      pd2[i] = pfd2 + i * NUM7 + (NUM7-1)/2;
      pd1[i] = pfd1 + i * NUM8 + (NUM8-1)/2;

      phv2[i] = pfhv2 + i * NUM8 + (NUM8-1)/2;
      phv3[i] = pfhv3 + i * NUM8 + (NUM8-1)/2;
      phv4[i] = pfhv4 + i * NUM8 + (NUM8-1)/2;

      pb1[i] = pfb1 + i * NUM10HALF + NUM10HALF-1;
      pb2[i] = pfb2 + i * NUM10HALF + NUM10HALF-1;

    }

    FOR (i, MAX_ANZ - MIN_ANZ + 1) {

      InterceptW[i] = koeff1[i*KOEFF_ANZ1] * WERTFAKTOR;  
      CutOffsetW[i] = (int) rint(cut_values[i] * WERTFAKTOR);
    }
  }



/* WIPE-OUT? */

  if ((i=pb->SteinAnz) != 0) 

    if (Partei == BLACK) {

      if (i == -pb->StDiffBW) return(-(WERTGEWINN+64));

    } else {

      if (i == +pb->StDiffBW) return(-(WERTGEWINN+64));

    }

  if      (i < MIN_ANZ) i = MIN_ANZ - MIN_ANZ;
  else if (i > MAX_ANZ) i = MAX_ANZ - MIN_ANZ;
  else                  i = i - MIN_ANZ;


#if 0
{ int i;
FOR (i, 47) printf("%d: %d\n", i, pat[i]);
} 
#endif

{  register SFPOS *p

#if __i386__
asm("%esi")
#endif

 = pb->p;


/* 10 disc patterns */

/* B1 */

   pf = pb1[i];

#if DOUBLE_INDEX
#define PF(i)  (*(FTABTYPE*)((char*)pf + i))  /* pf + (2*index) */
#else
#define PF(i)  pf[i]
#endif


/* with a little help for gcc's optimizer ... */

#define PAT2(n,x1,x2) \
((w=(9*pat[n]+(DOUBLE_INDEX ? 2 : 1)*(3*p[x1]+p[x2]))) <= 0\
 ? PF(w) : (w=-w, -PF(w)))

   isu =  PAT2(PR1A, E1, E2) + PAT2(PR1B, D1, D2) + 
          PAT2(PR1C, E8, E7) + PAT2(PR1D, D8, D7) + 
          PAT2(PR1E, A5, B5) + PAT2(PR1F, H5, G5) + 
          PAT2(PR1G, A4, B4) + PAT2(PR1H, H4, G4);

#if 0
{ SPFELD sf;
  BRETT board;

  BrettSf(pb, &sf);
  SfAus(&sf, 0, 0);

  SfBrett(&sf, &board);

  BrettVergl2(pb, &board);

#if 0
#define PAT3(n,x1,x2) (9*pat[n]+3*p[x1]+p[x2])

printf("%d %d %d %d %d %d %d %d\n", 
          PAT3(PR1A, E1, E2), PAT3(PR1B, D1, D2), 
          PAT3(PR1C, E8, E7), PAT3(PR1D, D8, D7), 
          PAT3(PR1E, A5, B5), PAT3(PR1F, H5, G5), 
          PAT3(PR1G, A4, B4), PAT3(PR1H, H4, G4));
#endif
/*
printf(">>> i=%d %f   : 8=%d 10=%d\n", sind, koeff1[i*KOEFF_ANZ1], pat[PR1A], (PAT2(PR1A, E1, E2), w) );
*/
}
#endif


/* B2 */

   pf = pb2[i];

   isu += PAT2(PHV1A, B2, G2) + PAT2(PHV1B, B7, G7) + 
          PAT2(PHV1C, B2, B7) + PAT2(PHV1D, G2, G7);

#if 0
printf("%d %d %d %d\n", 
          PAT2(PHV1A, B2, G2), PAT2(PHV1B, B7, G7), 
          PAT2(PHV1C, B2, B7), PAT2(PHV1D, G2, G7));
#endif


  su = isu * CTAB2W;
  }


/* <= 8 disc patterns */

  pf = phv2[i];
  isu = PF(pat[PHV2A]) + PF(pat[PHV2B]) + PF(pat[PHV2C]) + PF(pat[PHV2D]);

  pf = phv3[i];
  isu += PF(pat[PHV3A]) + PF(pat[PHV3B]) + PF(pat[PHV3C]) + PF(pat[PHV3D]);

  pf = phv4[i];
  isu += PF(pat[PHV4A]) + PF(pat[PHV4B]) + PF(pat[PHV4C]) + PF(pat[PHV4D]);

  pf = pd1[i];
  isu += PF(pat[PD1A]) + PF(pat[PD1B]);

  pf = pd2[i];
  isu += PF(pat[PD2A]) + PF(pat[PD2B]) + PF(pat[PD2C]) + PF(pat[PD2D]);

  pf = pd3[i];
  isu += PF(pat[PD3A]) + PF(pat[PD3B]) + PF(pat[PD3C]) + PF(pat[PD3D]);

  pf = pd4[i];
  isu += PF(pat[PD4A]) + PF(pat[PD4B]) + PF(pat[PD4C]) + PF(pat[PD4D]);

  pf = pd5[i];
  isu += PF(pat[PD5A]) + PF(pat[PD5B]) + PF(pat[PD5C]) + PF(pat[PD5D]);

#if 0
/* !!! no mob on diagonals of length 3, faster and better! */
  pf = pd6[i];
  isu += PF(pat[PD6A]) + PF(pat[PD6B]) + PF(pat[PD6C]) + PF(pat[PD6D]);
#endif



  su += isu * CTAB1W;

  if (Partei == WHITE) su = -su;

  su += InterceptW[i];

  if (su >=  (float)WERTGEWINN) return(+(WERTGEWINN-1)); 
  if (su <= -(float)WERTGEWINN) return(-(WERTGEWINN-1));  

  return (WERT) su;
}



#if 0


/* lazy evaluation: 
**
**  if 10-disc-patterns yield value >= be + CUT_OFFSET*WERTFAKTOR 
**  return without computing remaining patterns
**  
*/


WERT EvalACut(BRETT *pb, PARTEI Partei, int be)
{
  register STRAHLTYP *pat=pb->NewPatt.st;
  register FTABTYPE *pf;
  register int isu, isu1;
  int	i, w;


#if TEST
REAL su1;
#endif


Error("EvalACut is buggy");

#if SFAUS

{ SPFELD sf;
  BrettSf(pb, &sf);
  SfAus(&sf, 0, 0);
  printf("%d\n", Partei);
}
#endif

#if 0

#define HTSIZE (1<<18)

static struct { uint4 lock; } ht[HTSIZE];
static int hit=0, checknum=0;
  if (ht[pb->Hash1 & (HTSIZE-1)].lock == pb->Hash2) hit++;
  checknum++;
  ht[pb->Hash1 & (HTSIZE-1)].lock = pb->Hash2;

  if (!(checknum % 10000)) printf("%d %d %f\n", checknum, hit, (float)hit/checknum);
#endif

  if (!koeff1da) { 

    if (!ParameterEinlesen(ParameterFile, ANZ*KOEFF_ANZ1, koeff1)) {
      Error("A parameters?");
    }

    koeff1da = true;

    ReadTables(TableFile, &TabsB, &TabsW);

    MergeTables(koeff1, &TabsB);

    CTAB1      = (float)TAB1_MAX / FTABMAX;
    CTAB2      = (float)TAB2_MAX / FTABMAX;
    CTAB1W     = CTAB1 * WERTFAKTOR;
    CTAB2W     = CTAB2 * WERTFAKTOR;

    FOR (i, 65) {

      pd6[i] = pfd6 + i * NUM3 + (NUM3-1)/2;
      pd5[i] = pfd5 + i * NUM4 + (NUM4-1)/2;
      pd4[i] = pfd4 + i * NUM5 + (NUM5-1)/2;
      pd3[i] = pfd3 + i * NUM6 + (NUM6-1)/2;
      pd2[i] = pfd2 + i * NUM7 + (NUM7-1)/2;
      pd1[i] = pfd1 + i * NUM8 + (NUM8-1)/2;

      phv2[i] = pfhv2 + i * NUM8 + (NUM8-1)/2;
      phv3[i] = pfhv3 + i * NUM8 + (NUM8-1)/2;
      phv4[i] = pfhv4 + i * NUM8 + (NUM8-1)/2;

      pb1[i] = pfb1 + i * NUM10HALF + NUM10HALF-1;
      pb2[i] = pfb2 + i * NUM10HALF + NUM10HALF-1;

    }

    FOR (i, MAX_ANZ - MIN_ANZ + 1) {

      InterceptW[i] = koeff1[i*KOEFF_ANZ1] * WERTFAKTOR;  
      CutOffsetW[i] = (int) rint(cut_values[i] * WERTFAKTOR);

    }
  }



/* WIPE-OUT? */

  if ((i=pb->SteinAnz) != 0) 

    if (Partei == BLACK) {

      if (i == -pb->StDiffBW) return -(WERTGEWINN+64);

    } else {

      if (i == +pb->StDiffBW) return -(WERTGEWINN+64);

    }

  if      (i < MIN_ANZ) i = MIN_ANZ - MIN_ANZ;
  else if (i > MAX_ANZ) i = MAX_ANZ - MIN_ANZ;
  else                  i = i - MIN_ANZ;


#if 0
{ int i;
FOR (i, 47) printf("%d: %d\n", i, pat[i]);
} 
#endif


{  register SFPOS *p=pb->p;


/* 10 disc patterns */

/* B1 */

   pf = pb1[i];

#define PAT2(n,x1,x2) ((w=(9*pat[n]+3*p[x1]+p[x2])) <= 0 ? pf[w] : -pf[-w])

   isu =  PAT2(PR1A, E1, E2) + PAT2(PR1B, D1, D2) + 
          PAT2(PR1C, E8, E7) + PAT2(PR1D, D8, D7) + 
          PAT2(PR1E, A5, B5) + PAT2(PR1F, H5, G5) + 
          PAT2(PR1G, A4, B4) + PAT2(PR1H, H4, G4);

#if 0
printf("%d %d %d %d %d %d %d %d\n", 
          PAT2(PR1A, E1, E2), PAT2(PR1B, D1, D2), 
          PAT2(PR1C, E8, E7), PAT2(PR1D, D8, D7), 
          PAT2(PR1E, A5, B5), PAT2(PR1F, H5, G5), 
          PAT2(PR1G, A4, B4), PAT2(PR1H, H4, G4));

printf(">>> i=%d %f   : 8=%d 10=%d\n", sind, koeff1[i*KOEFF_ANZ1], pat[PR1A], (PAT2(PR1A, E1, E2), w) );
#endif


/* B2 */

   pf = pb2[i];

   isu += PAT2(PHV1A, B2, G2) + PAT2(PHV1B, B7, G7) + 
          PAT2(PHV1C, B2, B7) + PAT2(PHV1D, G2, G7);

#if 0
printf("%d %d %d %d\n", 
          PAT2(PHV1A, B2, G2), PAT2(PHV1B, B7, G7), 
          PAT2(PHV1C, B2, B7), PAT2(PHV1D, G2, G7));
#endif

#if 0

{ static float a, min1 = WERTMAX, min2 = WERTMAX;
  static int dist1[25], dist2[25], dist3[25];
  static int num=0, num1=0, num2=0, num3=0, num10=0, num20=0, num30=0;

if (isu * CTAB2 > 1) {

  num1++;

  if (su < 0) {

    if (su <= -5) a = 4.99; else a = -su;

    dist1[(int)rint(a * 5)]++;
    num10++;

  /* printf("%f+%f = %f\n", su, isu * CTAB2, su + isu * CTAB2); */

  }

}

if (isu * CTAB2 > 2) {

  num2++;

  if (su < 0) {

    if (su <= -5) a = 4.99; else a = -su;

    dist2[(int)rint(a * 5)]++;
    num20++;

  /* printf("%f+%f = %f\n", su, isu * CTAB2, su + isu * CTAB2); */

  }
}

if (isu * CTAB2 > 3) {

  num3++;

  if (su < 0) {

    if (su <= -5) a = 4.99; else a = -su;

    dist3[(int)rint(a * 5)]++;
    num30++;

  /* printf("%f+%f = %f\n", su, isu * CTAB2, su + isu * CTAB2); */

  }

}

num++;
if ((num & 1023) == 0) { 

printf("%d (%d %d) (%d %d) (%d %d)\n", num, num1, num10, num2, num20, num3, num30);

  FOR (i, 25) printf("%d ", dist1[i]);
  printf("\n");

  FOR (i, 25) printf("%d ", dist2[i]);
  printf("\n");

  FOR (i, 25) printf("%d ", dist3[i]);
  printf("\n");

  printf("\n");

}}
#endif

  isu1 = InterceptW[i] + ((Partei == BLACK) ? (isu * CTAB2W) : -(isu * CTAB2W));


#if 0
printf("%d %f %f\n", isu1, InterceptW[i], CTAB2W);
#endif


/* <= 8 disc patterns */

  pf = phv2[i];
  isu = pf[pat[PHV2A]] + pf[pat[PHV2B]] + pf[pat[PHV2C]] + pf[pat[PHV2D]];

  pf = phv3[i];
  isu += pf[pat[PHV3A]] + pf[pat[PHV3B]] + pf[pat[PHV3C]] + pf[pat[PHV3D]];

  pf = phv4[i];
  isu += pf[pat[PHV4A]] + pf[pat[PHV4B]] + pf[pat[PHV4C]] + pf[pat[PHV4D]];



  isu1 += ((Partei == BLACK) ? (isu * CTAB1W) : -(isu * CTAB1W));


#if !CUT_TEST

  if (isu1 >= be + CutOffsetW[i]) return be;

  isu = 0;

#endif

  {



  pf = pd1[i];
  isu += pf[pat[PD1A]] + pf[pat[PD1B]];

  pf = pd2[i];
  isu += pf[pat[PD2A]] + pf[pat[PD2B]] + pf[pat[PD2C]] + pf[pat[PD2D]];

  pf = pd3[i];
  isu += pf[pat[PD3A]] + pf[pat[PD3B]] + pf[pat[PD3C]] + pf[pat[PD3D]];

  pf = pd4[i];
  isu += pf[pat[PD4A]] + pf[pat[PD4B]] + pf[pat[PD4C]] + pf[pat[PD4D]];

  pf = pd5[i];
  isu += pf[pat[PD5A]] + pf[pat[PD5B]] + pf[pat[PD5C]] + pf[pat[PD5D]];

#if 0
/* !!! no mob on diagonals of length 3, faster and better! */
  pf = pd6[i];
  isu += pf[pat[PD6A]] + pf[pat[PD6B]] + pf[pat[PD6C]] + pf[pat[PD6D]];
#endif

#if CUT_TEST

if (pb->SteinAnz == CUT_DISCNUM) { 

  int isu0 = isu1;
  static int num=0, wrong=0;

  if (Partei == BLACK) isu1 += isu * CTAB1W; else isu1 -= isu * CTAB1W; 

  if (isu0 >= be + CutOffsetW) {

    if (isu1 < be) wrong++;
    num++;

if ((num % 1000) == 0) printf("num=%d wrong=%d  %f\n", num, wrong, (float)wrong/num);

  }
}

#else

  if (Partei == BLACK) isu1 += isu * CTAB1W; else isu1 -= isu * CTAB1W; 

#endif

}

}

  if (isu1 >=  WERTGEWINN) return +(WERTGEWINN-1); 
  if (isu1 <= -WERTGEWINN) return -(WERTGEWINN-1);  

  return isu1;
}


#endif



WERT AntiEval(BRETT *pb, PARTEI Partei)
{
  int i;
  double su;

/* WIPE-OUT? */

  if ((i=pb->SteinAnz) != 0) 

    if (Partei == BLACK) {

      if (i == -pb->StDiffBW) return(+(WERTGEWINN+64));

    } else {

      if (i == +pb->StDiffBW) return(+(WERTGEWINN+64));

    }

  su = 0;

  f_pattnew = true;

  su += STRMOBALLB(pb) - 10*pb->StDiffBW;

  su *= 1000;
 
  if (Partei == BLACK) return (WERT) su; else return -(WERT)su;

}
