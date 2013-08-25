// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/* check quiescense features, 5.95 */

#include "main.h"
#include "sboard.h"

#define SMAX 1000


void _abort(void) {}


int main(int argc, char **argv)
{
  int i;
  char s[SMAX+2];
  FILE *fpv, *fpb;
  float v0, v1, v2, v3, v4, r1, r2;
  SPFELD board;


  fpv = fopen("erg", "r");
  if (!fpv) Error("erg?");

  fpb = fopen("boards.sfk", "r");
  if (!fpb) Error("boards.sfk");


  FOREVER {


    if (!fSfRead(fpb, &board, 1)) break;

    if (!fgets(s, SMAX, fpv)) break;

    if (sscanf(s, "%f %f %f %f %f", &v0, &v1, &v2, &v3, &v4) != 5) 
      Error("sscanf");

    r1 = v0; r2 = v4;

#if 0
    if (fabs(r1-r2) >= 0.5) {
#else
    if ((r1 > 0) ^ (r2 > 0)) {
#endif

      SfAus(&board, 0, 0); 

      printf("r1=%f  r2=%f\n", r1, r2); 
    }

  }

  fclose(fpv); fclose(fpb);

  return 0;
}
