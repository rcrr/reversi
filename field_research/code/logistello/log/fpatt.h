// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#ifndef FPATT_H
#define FPATT_H

#include "featurem.h"
#include "board.h"
#include "patt.h"
#include "eval.h"


#define	STRHV1	true
#define	STRHV2	true
#define	STRHV3	true
#define	STRHV4	true

#define	STRD1	true
#define	STRD2	true
#define	STRD3	true
#define	STRD4	true
#define	STRD5	false
#define	STRD6	false
#define	STRD7	false

#define	STRR1	true
#define	STRR2	false
#define	STRR3	false

#define	STRM1	false
#define	STRM2	false

#define STRB1   true
#define STRB2   true


#if KURZEDIAG
#define KURZTEST
#else
#define KURZTEST	Error("kurze Diagonalen nicht implementiert");
#endif



/* pattern indices */

#define BS1	0
#define BS2	1
#define BS3	2
#define BS4	3
#define BS5	4
#define BS6	5
#define BS7	6
#define BS8	7

#define BSA	8
#define BSB	9
#define BSC	10
#define BSD	11
#define BSE	12
#define BSF	13
#define BSG	14
#define BSH	15

#define BDM3	18
#define BDM4	19
#define BDM5	20
#define BDM6	21
#define BDM7	22
#define BDM8	23
#define BDM9	24
#define BDM10	25
#define BDM11	26
#define BDM12	27
#define BDM13	28

#define BDP3	33
#define BDP4	34
#define BDP5	35
#define BDP6	36
#define BDP7	37
#define BDP8	38
#define BDP9	39
#define BDP10	40
#define BDP11	41
#define BDP12	42
#define BDP13	43

#define S8(p1,p2,p3,p4,p5,p6,p7,p8) \
 (3*(3*(3*(3*(3*(3*(3*p[p1]+p[p2])+p[p3])+p[p4])+p[p5])+p[p6])+p[p7])+p[p8])

#define S9(p1,p2,p3,p4,p5,p6,p7,p8,p9) \
 (3*(3*(3*(3*(3*(3*(3*(3*p[p1]+p[p2])+p[p3])+p[p4])+p[p5])+p[p6])+p[p7])+p[p8])+p[p9])

#define S10(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10) \
 (3*(3*(3*(3*(3*(3*(3*(3*(3*p[p1]+p[p2])+p[p3])+p[p4])+p[p5])+p[p6])+p[p7])+p[p8])+p[p9])+p[p10])

#define S12(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12) \
 (3*(3*(3*(3*(3*(3*(3*(3*(3*(3*(3*p[p1]+p[p2])+p[p3])+p[p4])+p[p5])+p[p6])+p[p7])+p[p8])+p[p9])+p[p10])+p[p11])+p[p12])

#define S14(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14) \
 (3*(3*(3*(3*(3*(3*(3*(3*(3*(3*(3*(3*(3*p[p1]+p[p2])+p[p3])+p[p4])+p[p5])+p[p6])+p[p7])+p[p8])+p[p9])+p[p10])+p[p11])+p[p12])+p[p13])+p[p14])

/* used in quiescence search too */

#define MOBEB_MASK	0x0000ff00
#define MOBEW_MASK	0x000000ff

extern uint4 StrMobE8[NUM8];


/* used in eval?.c too */

#define MOB_MASK	0x00ff0000
#define MOB_SHIFT       16
#define POTMOB_MASK	0x0000ff00
#define POTMOB_SHIFT    8
#define POTMOBE_MASK	0x000000ff
#define POTMOBE_SHIFT   0

extern uint4 StrMob8[NUM8];
extern uint4 StrMob7[NUM7];
extern uint4 StrMob6[NUM6];
extern uint4 StrMob5[NUM5];
extern uint4 StrMob4[NUM4];
extern uint4 StrMob3[NUM3];




/* Für schnelle Bewertung */

extern PATTVALS PattVals;

#if STRR1
#define STRR1_D		PattVals.StrR1
#endif
#if STRR2
#define STRR2_D		PattVals.StrR2
#endif
#if STRR3
#define STRR3_D		PattVals.StrR3
#endif

#if STRB1
#define STRB1_D		PattVals.StrB1
#endif
#if STRB2
#define STRB2_D		PattVals.StrB2
#endif

#if STRM1
#define STRM1_D		PattVals.StrM1
#endif
#if STRM2
#define STRM2_D		PattVals.StrM2
#endif

#if STRHV1
#define STRHV1_D	PattVals.StrHV1
#endif
#if STRHV2
#define STRHV2_D	PattVals.StrHV2
#endif
#if STRHV3
#define STRHV3_D	PattVals.StrHV3
#endif
#if STRHV4
#define STRHV4_D	PattVals.StrHV4
#endif

#if STRD1
#define STRD1_D		PattVals.StrD1
#endif
#if STRD2
#define STRD2_D		PattVals.StrD2
#endif
#if STRD3
#define STRD3_D		PattVals.StrD3
#endif
#if STRD4
#define STRD4_D		PattVals.StrD4
#endif
#if STRD5
#define STRD5_D		PattVals.StrD5
#endif
#if STRD6
#define STRD6_D		PattVals.StrD6
#endif


#define STRMOBALL_D	PattVals.StrMobAll
#define STRMOB_D	PattVals.StrMob
#define STRMOBEB_D	PattVals.StrMobEB
#define STRMOBEW_D	PattVals.StrMobEW
#define STRPOTMOBALL_D	PattVals.StrPotMobAll
#define STRPOTMOB_D	PattVals.StrPotMob
#define STRPOTMOBE_D	PattVals.StrPotMobE


struct RegInfo {

  int  num;    // number of surrounded regions

  struct {     // for each such regions ...

    int  size;   // number of surrounded empty squares
    bool both;   // <=> both players have possible access
    int  color;  // that has no access (both = 0)

  } regs[4];
};


void AnalyseRegion(sint1 *pb, int corner, RegInfo &regi);


extern MERKMAL_B
          DISC_DIFF_B,
          STRB1B_B, STRB2B_B,
	  STRR1B_B, STRR2B_B, STRR3B_B,
	  STRM1B_B, STRM2B_B,
	  STRHV1B_B, STRHV2B_B, STRHV3B_B, STRHV4B_B,
	  STRD1B_B, STRD2B_B, STRD3B_B, STRD4B_B, STRD5B_B, STRD6B_B,
	  STRMOBALLB_B, STRMOBB_B, STRMOBEBB_B, STRMOBEWB_B,
	  STRPOTMOBALLB_B, STRPOTMOBB_B, STRPOTMOBEB_B,
	  STRALLEB_B,
	  STRECKB_B, 
	  STRRESTB_B,
	  STRTESTB_B,
	  PARITY_B,
	  PARITY2_B,
	  PARITY1B_B,
          OR_ONE_B,
          ER_ONE_B,
          OR_BOTH_B,
          CORNERS_B,
          CENTER_B,
          MOBNORM_B,
          STABILITY_B,
          NEWSTAB_B,
          MOB1_B, MOB2_B, MOB3_B, MOB4_B, MOB5_B, 
          MOB6_B, MOB7_B, MOB8_B, MOB9_B, MOB10_B, MOBSUM_B, MOBAPP_B,
          EVALX_B,
          LARGE_B,
          T3X3_B, T4X3_B
; 

extern MERKMAL
         DISC_DIFF,
         STRB1B, STRB1W,
         STRB2B, STRB2W,
	 STRR1B, STRR2B, STRR3B,
	 STRR1W, STRR2W, STRR3W,
	 STRM1B, STRM2B,
	 STRM1W, STRM2W,
	 STRHV1B, STRHV2B, STRHV3B, STRHV4B, 
	 STRHV1W, STRHV2W, STRHV3W, STRHV4W,
	 STRMOBALLB, STRMOBALLW,
	 STRMOBB, STRMOBW,
	 STRMOBEBB, STRMOBEBW, STRMOBEWB, STRMOBEWW,
	 STRPOTMOBALLB, STRPOTMOBALLW,
         STRPOTMOBB, STRPOTMOBW,
	 STRPOTMOBEB, STRPOTMOBEW,
	 STRALLEB, STRALLEW,
	 STRECKB, STRECKW, 
	 STRRESTB, STRRESTW,

	 STRTESTB, STRTESTW,

         LARGE,
         T3X3, T4X3,

	 STRD1B, STRD2B, STRD3B, STRD4B, STRD5B, STRD6B,
	 STRD1W, STRD2W, STRD3W, STRD4W, STRD5W, STRD6W,

	 PARITY,
	 PARITY2,
	 PARITY1B,
         OR_BOTH,
         OR_ONE,
         ER_ONE,
         CORNERS,
         CENTER,
         MOBNORM,
         STABILITY,
         NEWSTAB,
         MOB1, MOB2, MOB3, MOB4, MOB5, MOB6, MOB7, MOB8, MOB9, MOB10, MOBSUM,
         MOBAPP,
         EVAL 
;

extern void FastPatt	(BRETT *pb, PATTVALS *pe, PARTEI Partei);

extern bool f_pattnew;

extern String TableFile;

extern void ReadParTab(char *file);
extern int GetParTabVal(int n);
extern uint1 *partab;


const int PARTAB_LEN = 21523361;
const int TOTAL_LEN  = 43046721;  // = 3^16

// n in [ -(PARTAB_LEN-1) ... 0 ....... PARTAB_LEN-1 ]
// ->     0               PARTAB_LEN-1        0
//                              [ val is negated here ]

inline void AddParTabVal(register int n, int &sum)
{
  assert(n < PARTAB_LEN && n > -PARTAB_LEN);

  if (n <= 0) {

    n += PARTAB_LEN-1;
    register int val = partab[n >> 2];
    val >>= 2* (n & 3);
    val &= 3;
    sum += val-1;

  } else {

    n = PARTAB_LEN-1-n;
    register int val = partab[n >> 2];
    val >>= 2* (n & 3);
    val &= 3;
    sum -= val-1;

  }
}

#endif
