// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#ifndef PATT_H
#define PATT_H

#include "featurem.h"
#include "board.h"
#include "tab.h"

#define NUM14	4782969
#define NUM12	531441
#define NUM10	59049
#define NUM9	19683
#define NUM8	6561
#define NUM7	2187
#define NUM6	729
#define NUM5	243
#define NUM4	81
#define NUM3	27
#define NUM2	9
#define NUM1	3
#define NUM0	1


#define PATT14(o,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14) \
2391484 o \
 (3*(3*(3*(3*(3*(3*(3*(3*(3*(3*(3*(3*(3*\
  p[p1]+p[p2])+p[p3])+p[p4])+p[p5])+p[p6])+p[p7])+\
  p[p8])+p[p9])+p[p10])+p[p11])+p[p12])+p[p13])+p[p14])

#define PATT12(o,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12) \
265720 o \
 (3*(3*(3*(3*(3*(3*(3*(3*(3*(3*(3*\
  p[p1]+p[p2])+p[p3])+p[p4])+p[p5])+p[p6])+p[p7])+\
  p[p8])+p[p9])+p[p10])+p[p11])+p[p12])

#define PATT10(o,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10) \
29524 o \
 (3*(3*(3*(3*(3*(3*(3*(3*(3*p[p1]+p[p2])+p[p3])+p[p4])+p[p5])+p[p6])+p[p7])+p[p8])+p[p9])+p[p10])

#define PATT9(o,p1,p2,p3,p4,p5,p6,p7,p8,p9) \
9841 o \
 (3*(3*(3*(3*(3*(3*(3*(3*p[p1]+p[p2])+p[p3])+p[p4])+p[p5])+p[p6])+p[p7])+p[p8])+p[p9])

#define PATT8(o,p1,p2,p3,p4,p5,p6,p7,p8) \
3280 o \
 (3*(3*(3*(3*(3*(3*(3*p[p1]+p[p2])+p[p3])+p[p4])+p[p5])+p[p6])+p[p7])+p[p8])

#define PATT7(o,p1,p2,p3,p4,p5,p6,p7) \
1093 o \
 (3*(3*(3*(3*(3*(3*p[p1]+p[p2])+p[p3])+p[p4])+p[p5])+p[p6])+p[p7])

#define PATT6(o,p1,p2,p3,p4,p5,p6) \
364  o \
 (3*(3*(3*(3*(3*p[p1]+p[p2])+p[p3])+p[p4])+p[p5])+p[p6])

#define PATT5(o,p1,p2,p3,p4,p5) \
121  o \
 (3*(3*(3*(3*p[p1]+p[p2])+p[p3])+p[p4])+p[p5])

#define PATT4(o,p1,p2,p3,p4) \
40  o \
 (3*(3*(3*p[p1]+p[p2])+p[p3])+p[p4])

#define PATT3(o,p1,p2,p3) \
13  o \
 (3*(3*p[p1]+p[p2])+p[p3])




typedef struct {

  short r1a, r1b, r1c, r1d, r1e, r1f, r1g, r1h;
  short r2a, r2b, r2c, r2d, r2e, r2f, r2g, r2h;
  short r3a, r3b, r3c, r3d, r3e, r3f, r3g, r3h;

  short m1a, m1b, m1c, m1d;
  short m2a, m2b, m2c, m2d;

  short s1,  s2,  s3,  s4,  s5,  s6,  s7,   s8;
  short sA,  sB,  sC,  sD,  sE,  sF,  sG,   sH;
  short dp3, dp4, dp5, dp6, dp7, dp8, dp9, dp10, dp11, dp12, dp13;
  short dm3, dm4, dm5, dm6, dm7, dm8, dm9, dm10, dm11, dm12, dm13;

} PATT;



typedef struct {

  short  StrB1, StrB2;
  short  StrR1, StrR2, StrR3;
  short  StrM1, StrM2;

  short	 StrHV1, StrHV2, StrHV3, StrHV4;
  short  StrD1,  StrD2,  StrD3,  StrD4, StrD5, StrD6;

  short  StrMob, StrMobEB, StrMobEW, StrPotMob, StrPotMobE;
  short  StrMobAll, StrPotMobAll;

} PATTVALS;




typedef struct {

  TABTYPE *r1,  *r2,  *r3,  *m1,  *m2;
  TABTYPE *hv1, *hv2, *hv3, *hv4;
  TABTYPE *d1,  *d2,  *d3,  *d4, *d5, *d6;
  TABTYPE *b1, *b2;
  TABTYPE *la;
  TABTYPE *t3x3, *t4x3;

} TABLES;


 
bool ReadTables(const String &FileName, TABLES *tabsb, TABLES *tabsw);
extern bool WriteTables(TABLES *psz, const String &FileName);

extern void ComputePatt(Square *p, int player, PATT *pst);
extern void BigTabWrite(FILE *fp, TABTYPE *tab, int discnum);

#endif
  
