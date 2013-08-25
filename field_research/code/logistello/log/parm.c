// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#include "main.h"


#define PARNUM	11


float max[PARNUM];

float val[60][PARNUM];


int main(void)
{
  char s[200];
  int i, j, k;



  FOR (i, PARNUM) max[i] = -1;


  j = 0;
    

  while (!feof(stdin)) {

fgets(s, 100, stdin);
 
    FOR (i, PARNUM) {

      if (scanf("%f\n", &val[j][i]) != 1) {

        if (i != 0) { printf("*** missing data\n"); exit(10); }

        break;

      }

      val[j][i] /= 0.55132;

      if (fabs(val[j][i]) > max[i]) max[i] = fabs(val[j][i]);

    }

    if (i < PARNUM) break;

    j++;

    if (j >= 60) { printf("*** to much data\n"); exit(10); }

  }


  FOR (i, PARNUM) {

    printf("%%max=%.4f\n", max[i]);

    printf("\\setpatt\n\\plot ");

    FOR (k, j) {

      printf("%d %.2f", 12+k, val[k][i] /*/max[i]*/);

      if (k != j-1) printf(" "); else printf(" /");

    }

    printf("\n\n");

  }



  return 0;
}
