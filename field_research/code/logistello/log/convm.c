// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#include "main.h"
#include "sboard.h"

#define WIN   true        /* only won positions */

/*

   A  B  C  D  E  F  G  H
1 | 0| 0| 0| 1| 1| 1| 2| 2| 1
2 | 9| 9| 9|10|10|10|11| 2| 2
3 | 8|15|16|16|16|17|11| 3| 3
4 | 8|15|19|()|##|17|11| 3| 4
5 | 8|15|19|##|()|17|12| 3| 5
6 | 7|14|19|18|18|18|12| 4| 6
7 | 7|14|14|13|13|13|12| 4| 7
8 | 7| 6| 6| 6| 5| 5| 5| 4| 8
   A  B  C  D  E  F  G  H

*/

void _abort(void) {}

char A, *B;

void A2B(void)
{
  if (A=='0') { B="   "; return; }
  if (A=='1') { B="  o"; return; }
  if (A=='2') { B="  x"; return; }
  if (A=='3') { B=" o "; return; }
  if (A=='4') { B=" oo"; return; }
  if (A=='5') { B=" ox"; return; }
  if (A=='6') { B=" x "; return; }
  if (A=='7') { B=" xo"; return; }
  if (A=='8') { B=" xx"; return; }
  if (A=='9') { B="o  "; return; }
  if (A=='A') { B="o o"; return; }
  if (A=='B') { B="o x"; return; }
  if (A=='C') { B="oo "; return; }
  if (A=='D') { B="ooo"; return; }
  if (A=='E') { B="oox"; return; }
  if (A=='F') { B="ox "; return; }
  if (A=='G') { B="oxo"; return; }
  if (A=='H') { B="oxx"; return; }
  if (A=='I') { B="x  "; return; }
  if (A=='J') { B="x o"; return; }
  if (A=='K') { B="x x"; return; }
  if (A=='L') { B="xo "; return; }
  if (A=='M') { B="xoo"; return; }
  if (A=='N') { B="xox"; return; }
  if (A=='P') { B="xx "; return; }
  if (A=='Q') { B="xxo"; return; }
  if (A=='R') { B="xxx"; return; }

  printf("Error1\n");
  exit(10);
}


void Convert(char *s)
{
  int i, k, bl, wh;
  char D, *Q1, *Q2, *P[21], b[100], s1[200], s2[200], s3[200], s4[200], s5[200];
  SPFELD sf;


  for (k=0; k < 20; k++) {

    A = s[k];

    A2B();

    P[k] = B;

  }

  D = s[20];

  if      (D=='0') { Q1="oo"; Q2="oo"; }
  else if (D=='1') { Q1="oo"; Q2="xo"; }
  else if (D=='2') { Q1="oo"; Q2="ox"; }
  else if (D=='3') { Q1="oo"; Q2="xx"; }
  else if (D=='4') { Q1="ox"; Q2="oo"; }
  else if (D=='5') { Q1="ox"; Q2="xo"; }
  else if (D=='6') { Q1="ox"; Q2="ox"; }
  else if (D=='7') { Q1="ox"; Q2="xx"; }
  else if (D=='8') { Q1="xo"; Q2="oo"; }
  else if (D=='9') { Q1="xo"; Q2="xo"; }
  else if (D=='A') { Q1="xo"; Q2="ox"; }
  else if (D=='B') { Q1="xo"; Q2="xx"; }
  else if (D=='C') { Q1="xx"; Q2="oo"; }
  else if (D=='D') { Q1="xx"; Q2="xo"; }
  else if (D=='E') { Q1="xx"; Q2="ox"; }
  else if (D=='F') { Q1="xx"; Q2="xx"; }
  else { printf("Error2\n"); exit(10); }

  sprintf(b, "%s%s%c%c%s%s%c%c%c%c%s%c%c%c%c%c%c%s%c%c%c%c%c%c%s%c%c%c%c%c%c%c%c%c%c%c%c%c%c%c%c%c%c%c%c%c%c%c%c%c%c%c\n", 
    P[0], P[1], P[2][0], P[2][1],
    P[9], P[10], P[11][0], P[2][2],
    P[8][2], P[15][2], P[16], P[17][0], P[11][1], P[3][0],
    P[8][1], P[15][1], P[19][2], Q1, P[17][1], P[11][2], P[3][1],
    P[8][0], P[15][0], P[19][1], Q2, P[17][2], P[12][0], P[3][2], 
    P[7][2], P[14][2], P[19][0], P[18][2],  P[18][1], P[18][0], P[12][1], P[4][0],
    P[7][1], P[14][1], P[14][0], P[13][2],  P[13][1], P[13][0], P[12][2], P[4][1],
    P[7][0], P[6][2], P[6][1], P[6][0], P[5][2], P[5][1], P[5][0], P[4][2]);


  FOR (i, 64) {

    int x,y,j;

    x = i % 8; y = i / 8;
    j = x+1+ y*10+10;

    if (b[i] == ' ') sf.p[j] =  0;
    if (b[i] == 'x') sf.p[j] = +1;
    if (b[i] == 'o') sf.p[j] = -1;

  }

  sscanf(s, "%s %d-%d %s %s %s %s", s1, &bl, &wh, s2, s3, s4, s5);

#if WIN

  if (((SfAnz(&sf) & 1) && wh <= 32) || (!(SfAnz(&sf) & 1) && bl <= 32)) return;

#endif


  if (SfAnz(&sf) & 1) printf("\n-> ()\n"); else printf("\n-> ##\n");

  SfAus(&sf, 0, 0);


  printf("##:() %d:%d  [%s%s%s]\n", bl, wh, s3, s4, s5);
 

}


int main(void)
{
  char s[1000+1];

  for (;;) {

    if (!fgets(s, 1000, stdin)) break;

    Convert(s);


  }

  return 0;
}



