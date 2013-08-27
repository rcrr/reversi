// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#include "main.h"
#include "board.h"
#include "eval.h"
#include "move.h"
#include "crt.h"
#include "fpatt.h"



#define MIN_NUM 54
#define MAX_NUM 62
#define NUM (MAX_NUM-MIN_NUM+1)

const int CONFIG_NUM = NUM10;


const float Intercept[2] = { -0.24, 2.67 };
const float CenterDiff = 0.45;

static float patt1[CONFIG_NUM], patt2[CONFIG_NUM];


static void ReadTab(float *patt, char *name)
{
  FILE *fp;
  char s[1000];

  fp = fopen(name,"r");
  if (!fp) Error("can't open pattern-file");

  int i;

  FOR (i, CONFIG_NUM) {
    if (fscanf(fp, "%900s %f", s, &patt[i]) != 2) Error("pattern-file corrupt");
  }

  fclose(fp);
}


// no table merging

WERT NewEndEval(BRETT *pb, PARTEI player)
{
  float su;
  int   i, isu;
  static bool read = false;


  if (!read) { 

    ReadTab(patt1, "patt1");
    ReadTab(patt2, "patt2");
    
    read = true;
  }


/* WIPE-OUT? */

  if ((i=pb->SteinAnz) != 0) 

    if (player == BLACK) {

      if (i == -pb->StDiffBW) return(-(WERTGEWINN+64));

    } else {

      if (i == +pb->StDiffBW) return(-(WERTGEWINN+64));

    }


  if (i < MIN_NUM || i > MAX_NUM) Error("NewEndEval: disc-number out of range");


  Square *p = pb->p;
  
  su = (p[C3]+p[C4]+p[C5]+p[C6]+
	p[D3]+p[D4]+p[D5]+p[D6]+
	p[E3]+p[E4]+p[E5]+p[E6]+
	p[F3]+p[F4]+p[F5]+p[F6]) * CenterDiff;

  if (player == BLACK) {

    su += 
      patt1[PATT10(+,A1,B1,C1,D1,E1,F1,G1,H1,B2,G2)] +
      patt1[PATT10(+,A8,B8,C8,D8,E8,F8,G8,H8,B7,G7)] +
      patt1[PATT10(+,A1,A2,A3,A4,A5,A6,A7,A8,B2,B7)] +
      patt1[PATT10(+,H1,H2,H3,H4,H5,H6,H7,H8,G2,G7)] +

      patt2[PATT10(+,A1,B1,C1,D1,A2,B2,C2,D2,E1,E2)] +
      patt2[PATT10(+,H1,G1,F1,E1,H2,G2,F2,E2,D1,D2)] +
      patt2[PATT10(+,A8,B8,C8,D8,A7,B7,C7,D7,E8,E7)] + 
      patt2[PATT10(+,H8,G8,F8,E8,H7,G7,F7,E7,D8,D7)] + 
      patt2[PATT10(+,A1,A2,A3,A4,B1,B2,B3,B4,A5,B5)] +
      patt2[PATT10(+,H1,H2,H3,H4,G1,G2,G3,G4,H5,G5)] +
      patt2[PATT10(+,A8,A7,A6,A5,B8,B7,B6,B5,A4,B4)] +
      patt2[PATT10(+,H8,H7,H6,H5,G8,G7,G6,G5,H4,G4)];

  } else {

    su = -su;

    su -=
      patt1[PATT10(-,A1,B1,C1,D1,E1,F1,G1,H1,B2,G2)] +
      patt1[PATT10(-,A8,B8,C8,D8,E8,F8,G8,H8,B7,G7)] +
      patt1[PATT10(-,A1,A2,A3,A4,A5,A6,A7,A8,B2,B7)] +
      patt1[PATT10(-,H1,H2,H3,H4,H5,H6,H7,H8,G2,G7)] +
		   
      patt2[PATT10(-,A1,B1,C1,D1,A2,B2,C2,D2,E1,E2)] +
      patt2[PATT10(-,H1,G1,F1,E1,H2,G2,F2,E2,D1,D2)] +
      patt2[PATT10(-,A8,B8,C8,D8,A7,B7,C7,D7,E8,E7)] + 
      patt2[PATT10(-,H8,G8,F8,E8,H7,G7,F7,E7,D8,D7)] + 
      patt2[PATT10(-,A1,A2,A3,A4,B1,B2,B3,B4,A5,B5)] +
      patt2[PATT10(-,H1,H2,H3,H4,G1,G2,G3,G4,H5,G5)] +
      patt2[PATT10(-,A8,A7,A6,A5,B8,B7,B6,B5,A4,B4)] +
      patt2[PATT10(-,H8,H7,H6,H5,G8,G7,G6,G5,H4,G4)];
  }

  su += Intercept[pb->SteinAnz & 1];

  isu = (int) (su * 1000);
  if (isu >  WERTGEWINN) return(+WERTGEWINN); 
  if (isu < -WERTGEWINN) return(-WERTGEWINN);  

  return(isu);
}





REAL NEWENDEVAL(BRETT *pb)
{
  return NewEndEval(pb, BLACK);
}

MERKMAL_B NEWENDEVAL_B = { NEWENDEVAL, "NEWENDEVAL" };


