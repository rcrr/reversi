// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/* evaluate board using quadratic discriminant function / 7.94 */

/* features:

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
#define FEATURES	13


typedef struct {

  float S[FEATURES][FEATURES];
  float T[FEATURES];
  float I;

} PARAMS;



static PARAMS params[ANZ] = {
#include "quad.par"
};




/**************************************************************/



WERT EvalD(BRETT *pb, PARTEI Partei)
{
  REAL	su;
  int	i, j, isu;
  float f, x[FEATURES], *pS, *pT;
  sint1 *pd;
  PARAMS *pparams;

#if TEST
REAL su1;
#endif

static int ohne=false;


/* WIPE-OUT? */

  if ((i=pb->SteinAnz) != 0) 

    if (Partei == BLACK) {

      if (i == - pb->StDiffBW) return -(WERTGEWINN+64);

    } else {

      if (i ==   pb->StDiffBW) return -(WERTGEWINN+64);

    }


  if (i < MIN_ANZ) i = MIN_ANZ;
  if (i > MAX_ANZ) i = MAX_ANZ;
  if (i < USE_ANZ) i = USE_ANZ;

  i = i - MIN_ANZ;


  pparams = &params[i];


/* 1. symmetrische Merkmale */

  su = 0;

/* 2. unsymmetrische Merkmale */

  if (Partei == BLACK) {

    f_pattnew = true;

    x[0] = STRMOBALLB(pb);
    x[1] = STRPOTMOBALL_D;

    x[2] = STRHV1_D;
    x[3] = STRHV2_D;
    x[4] = STRHV3_D;
    x[5] = STRHV4_D;

    x[6] = STRD1_D;
    x[7] = STRD2_D;
    x[8] = STRD3_D;
    x[9] = STRD4_D;

    x[10] = STRR1_D;
    x[11] = STRB1_D;
    x[12] = STRB2_D;
    
  } else {

    f_pattnew = true;

    x[0] = STRMOBALLW(pb);
    x[1] = STRPOTMOBALL_D;

    x[2] = STRHV1_D;
    x[3] = STRHV2_D;
    x[4] = STRHV3_D;
    x[5] = STRHV4_D;

    x[6] = STRD1_D;
    x[7] = STRD2_D;
    x[8] = STRD3_D;
    x[9] = STRD4_D;

    x[10] = STRR1_D;
    x[11] = STRB1_D;
    x[12] = STRB2_D;
 

  }


  su = pparams->I;

  pS = &pparams->S[0][0];
  pT = &pparams->T[0];

#if 0

  FOR (i, FEATURES) {

    su += x[i] * (*pT++);

  }

#else

  FOR (i, FEATURES) {

    float *pf = pS, xi=x[i], s=0, *px=x;

    FOR (j, i) s += (*pf++) * (*px++);

    su += (s + 0.5 * (*pf) * xi + (*pT++)) * xi; 

    pS += FEATURES;
  }

#endif

  su = -su;

/*

FOR (i, ANZ) printf("%f\n", params[i].I);
printf("\n");


printf("I=%f ", su);
printf("  S=%f\n", su);
*/



  isu = su * WERTFAKTOR;




#if SFAUS
if (Partei==BLACK)
printf("%.2f %d, %d %d %d %d, %d %d %d %d %d\n",
STRMOBALLB(pb),
STRPOTMOBALL_D,
STRHV1_D,
STRHV2_D,
STRHV3_D,
STRHV4_D,
STRD1_D,
STRD2_D,
STRD3_D,
STRD4_D,
STRR1_D);
else
printf("%.2f %d, %d %d %d %d, %d %d %d %d %d\n",
STRMOBALLW(pb),
STRPOTMOBALL_D,
STRHV1_D,
STRHV2_D,
STRHV3_D,
STRHV4_D,
STRD1_D,
STRD2_D,
STRD3_D,
STRD4_D,
STRR1_D);
printf("w=%d\n\n", isu);
#endif

#if 0
if (isu == 1410) 
{
  SPFELD sf;

BrettSf(pb, &sf);
SfAus(&sf, Partei, 0xffff); printf("%d %d\n", Partei, isu);

f_mobneu = true; f_potmobneu = true;
f_strahlneu = true;

if (Partei == BLACK) {
printf("%d: %.2f %.2f %.2f %.2f %.2f %.2f %.2f %.2f %.2f %.2f\n", 
  Partei, MOB(pb), POTMOB(pb), 
STRHV1B(pb), STRHV2B(pb),STRHV3B(pb),STRHV4B(pb),
STRD1B(pb),STRD2B(pb),STRD3B(pb),STRD4B(pb));
} else {
printf("%d: %.2f %.2f %.2f %.2f %.2f %.2f %.2f %.2f %.2f %.2f\n", 
  Partei, -MOB(pb), -POTMOB(pb), 
STRHV1W(pb), STRHV2W(pb),STRHV3W(pb),STRHV4W(pb),
STRD1W(pb),STRD2W(pb),STRD3W(pb),STRD4W(pb));
}



pk = &koeff1[i*KOEFF_ANZ1];
printf("%.2f %.2f %.2f %.2f %.2f %.2f %.2f %.2f %.2f %.2f %.2f\n", 
*pk, *(pk+1), *(pk+2), *(pk+3), *(pk+4), *(pk+5), *(pk+6), *(pk+7), *(pk+8), *(pk+9), *(pk+10));

/*
f_strahlneu = true;
printf("%.2"R_F" %.2"R_F" %.2"R_F" %.2"R_F" %.2"R_F" %.2"R_F" %.2"R_F" %.2"R_F" \n",
STRAHL1B(pb), STRAHL2B(pb), STRAHL3B(pb), STRAHL4B(pb), DIAG1B(pb), DIAG2B(pb),
DIAG3B(pb), DIAG4B(pb));

f_mobneu = true;
printf("%.2"R_F"\n", MOBQ(pb));

pk = &koeff1[i*KOEFF_ANZ1];
printf("%.2"R_F" %.2"R_F" %.2"R_F" %.2"R_F" %.2"R_F" %.2"R_F" %.2"R_F" %.2"R_F" \n",
*pk, *(pk+1), *(pk+2), *(pk+3), *(pk+4), *(pk+5), *(pk+6), *(pk+7), *(pk+8), *(pk+9));
*/
}
#endif


  if (isu >  WERTGEWINN) { 

#if TEST

  SPFELD sf;

BrettSf(pb, &sf);
SfAus(&sf, Partei, 0xffff); printf("%d\n", Partei);

f_strahlneu = true;
printf("%.2"R_F" %.2"R_F" %.2"R_F" %.2"R_F" %.2"R_F" %.2"R_F" %.2"R_F" %.2"R_F" \n",
STRAHL1B(pb), STRAHL2B(pb), STRAHL3B(pb), STRAHL4B(pb), DIAG1B(pb), DIAG2B(pb),
DIAG3B(pb), DIAG4B(pb));

f_mobneu = true;
printf("%.2"R_F"\n", MOBQ(pb));

pk = &koeff1[i*KOEFF_ANZ1+1];
printf("%.2"R_F" %.2"R_F" %.2"R_F" %.2"R_F" %.2"R_F" %.2"R_F" %.2"R_F" %.2"R_F" \n",
*pk, *(pk+1), *(pk+2), *(pk+3), *(pk+4), *(pk+5), *(pk+6), *(pk+7));
     
printf("++++++++++++++++\n"); 
#endif

    return  WERTGEWINN; 
  }

  if (isu < -WERTGEWINN) { 

#if TEST
  printf("----------------\n"); 
#endif

    return -WERTGEWINN;  
  }


#if 0
{ SPFELD sf;
BrettSf(pb, &sf);
SfAus(&sf, Partei, -1); printf("partei=%d w=%d\n", 
  Partei, isu);
}
#endif



  return isu;
}



